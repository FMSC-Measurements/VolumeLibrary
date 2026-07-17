//#include "pch.h"
#include "VolumeLibrary.h"
#include "WoodlandBiomass.h"
#include "VolumeCalculators\JenkinsBiomass.h"



TreeOutput VolumeLibrary::CalculateVolume(const VolumeCalculationOptions options, const TreeMeasurment tree, std::optional<MerchRules> maybe_merchRules)
{
	auto& volumeCalculator = volumeCalculatorFactory_.MakeVolumeCalculator(options);

	auto merchRules = (maybe_merchRules.has_value()) ? maybe_merchRules.value() : merchRulesResolver_.GetMerchRules(options);
	
	//region 7 (BLM) saw top diameter
	if (options.region == 7) merchRules.minTopDibSaw = tree.dbh * 0.184 + 2.24;
	//check override parameters for stump, sawTopDib, nonsawTopDib
	if (tree.stumpHeightOverride > 0.0) merchRules.stumpHeight = tree.stumpHeightOverride;
	if (tree.minTopDibSawOverride > 0.0) merchRules.minTopDibSaw = tree.minTopDibSawOverride;
	if (tree.minTopDibNonSawOverride > 0.0) merchRules.minTopDibNonSaw = tree.minTopDibNonSawOverride;

	auto treeOutput = volumeCalculator.CalculateVolume(options,tree, merchRules);

	//biomass calculation using NSVB 
	WeightFactorAndRefData refSpeciesData = getSpeciesWtfactorAndRefData(options.region, options.forest, options.fiaCode);
	
	double moistContent = 0.0;
	if (tree.isLive) {
		moistContent = (refSpeciesData.weightFactorNonsaw - refSpeciesData.weightFactorDry) / refSpeciesData.weightFactorDry;
	}
	else {
		moistContent = (refSpeciesData.weightFactorDead - refSpeciesData.weightFactorDry) / refSpeciesData.weightFactorDry;
	}

	double mcFactor = 1.0 + moistContent;

	double carbonFraction = refSpeciesData.carbonFraction;

	//for trees with DBH only measurement, using Jenkins method to calculate biomass
	if (tree.dbh > 0.0 && tree.totalHeight == 0.0 && 
		tree.merchHeightSaw == 0.0 && tree.merchHeightNonsaw == 0.0 &&
		tree.heightToTopBroken==0.0 && tree.referenceHeight) {

		BiomassOutput treeBiomass = jenkins(options.fiaCode, tree.dbh);
		treeOutput.dryBio = treeBiomass;
		treeOutput.greenBio = scale(treeOutput.dryBio, mcFactor);
		treeOutput.carbonContent = treeOutput.dryBio.aboveGroundTotal * carbonFraction;

		return treeOutput;
	}

	if (refSpeciesData.jenkinsSpeciesGroupCD != 10) {
		//call NSVB to get volume and biomass for non-woodland species
		NationalScaleVolumeBiomass nsvb = NationalScaleVolumeBiomass(options);
		TreeOutput nsvbOutput = nsvb.CalculateVolumeBiomass(options, tree, merchRules);

		double weightFactor = (options.primaryProduct == 1) ? refSpeciesData.weightFactorSaw : refSpeciesData.weightFactorNonsaw;
		weightFactor = (tree.isLive) ? weightFactor : refSpeciesData.weightFactorDead;

		//Adjust nsvb biomass based on the vol from VOLEQ and NSVB
		double cubicfootPrimary = treeOutput.grossCubicFootPrimary;
		double greenWeightPrimary = treeOutput.greenWeightPrimary;
		double cubicfootSecondary = treeOutput.grossCubicFootSecondary;
		double greenWeightSecondary = treeOutput.greenWeightSecondary;
		double dryWeightPrimary = treeOutput.dryWeightPrimary;
		double dryWeightSecondary = treeOutput.dryWeightSecondary;

		if (cubicfootPrimary > 0.0 && greenWeightPrimary == 0.0) {
			greenWeightPrimary = cubicfootPrimary * weightFactor;
			dryWeightPrimary = cubicfootPrimary * refSpeciesData.weightFactorDry;

			treeOutput.greenWeightPrimary = greenWeightPrimary;
			treeOutput.dryWeightPrimary = dryWeightPrimary;
		}

		if (cubicfootSecondary > 0.0 && greenWeightSecondary == 0.0) {
			if (tree.isLive) greenWeightSecondary = cubicfootSecondary * refSpeciesData.weightFactorNonsaw;
			else greenWeightSecondary = cubicfootSecondary * refSpeciesData.weightFactorDead;
			dryWeightSecondary = cubicfootSecondary * refSpeciesData.weightFactorDry;

			treeOutput.greenWeightSecondary = greenWeightSecondary;
			treeOutput.dryWeightSecondary = dryWeightSecondary;
		}

		double merchWeight = greenWeightPrimary + greenWeightSecondary;
		double nsvbMerchWeight = nsvbOutput.greenWeightPrimary + nsvbOutput.greenWeightSecondary;
		double ratioPrimary = 1.0;
		if (merchWeight > 0.0) ratioPrimary = greenWeightPrimary / merchWeight;
		
		double ratioWood = 1.0;
		double volFactor = 1.0;

		if (nsvbMerchWeight > 0.0 && merchWeight > 0.0) {
			volFactor = merchWeight / nsvbMerchWeight;
			ratioWood = (nsvbOutput.greenBio.stemPrimaryWood + nsvbOutput.greenBio.stemSecondaryWood) / nsvbMerchWeight;
		}
		else {
			double totalStemWeight = treeOutput.totalCubicFoot * weightFactor;

			if (nsvbOutput.greenBio.stemBarkTotal + nsvbOutput.greenBio.stemWoodTotal > 0.0) {
				volFactor = totalStemWeight / (nsvbOutput.greenBio.stemBarkTotal + nsvbOutput.greenBio.stemWoodTotal);
				ratioWood = nsvbOutput.greenBio.stemWoodTotal / (nsvbOutput.greenBio.stemBarkTotal + nsvbOutput.greenBio.stemWoodTotal);
			}
		}

		if (volFactor <= 0.0) volFactor = 1.0;
		treeOutput.greenBio = scale(nsvbOutput.greenBio, volFactor);
		treeOutput.dryBio = scale(nsvbOutput.dryBio, volFactor);

		//reset biomass conponent value
		if (treeOutput.grossCubicFootPrimary > 0.0) {
			treeOutput.greenBio.stemPrimaryWood = treeOutput.greenWeightPrimary * ratioWood;
			treeOutput.greenBio.stemPrimaryBark = treeOutput.greenWeightPrimary * (1.0 - ratioWood);
			treeOutput.dryBio.stemPrimaryWood = treeOutput.dryWeightPrimary * ratioWood;
			treeOutput.dryBio.stemPrimaryBark = treeOutput.dryWeightPrimary * (1.0 - ratioWood);
		}
		else {
			treeOutput.greenBio.stemPrimaryWood = 0.0;
			treeOutput.greenBio.stemPrimaryBark = 0.0;
			treeOutput.dryBio.stemPrimaryWood = 0.0;
			treeOutput.dryBio.stemPrimaryBark = 0.0;
		}

		if (treeOutput.grossCubicFootSecondary > 0.0) {
			treeOutput.greenBio.stemSecondaryWood = treeOutput.greenWeightSecondary * ratioWood;
			treeOutput.greenBio.stemSecondaryBark = treeOutput.greenWeightSecondary * (1.0 - ratioWood);
			treeOutput.dryBio.stemSecondaryWood = treeOutput.dryWeightSecondary * ratioWood;
			treeOutput.dryBio.stemSecondaryBark = treeOutput.dryWeightSecondary * (1.0 - ratioWood);
		}
		else {
			treeOutput.greenBio.stemSecondaryWood = 0.0;
			treeOutput.greenBio.stemSecondaryBark = 0.0;
			treeOutput.dryBio.stemSecondaryWood = 0.0;
			treeOutput.dryBio.stemSecondaryBark = 0.0;
		}

		if (treeOutput.tipCubicFoot > 0.0) {
			double tipWeight = treeOutput.tipCubicFoot * weightFactor;
			treeOutput.greenBio.stemTipWood = tipWeight * ratioWood;
			treeOutput.greenBio.stemTipBark = tipWeight * (1.0 - ratioWood);
			treeOutput.dryBio.stemTipWood = treeOutput.greenBio.stemTipWood / (1.0 + mcFactor);
			treeOutput.dryBio.stemTipBark = treeOutput.greenBio.stemTipBark / (1.0 + mcFactor);
		}

		double stemGreenWeightDiff = treeOutput.greenBio.stemWoodTotal + treeOutput.greenBio.stemBarkTotal -
			(treeOutput.greenBio.stumpWood + treeOutput.greenBio.stumpBark + treeOutput.greenBio.stemPrimaryWood +
			treeOutput.greenBio.stemPrimaryBark + treeOutput.greenBio.stemTipWood + treeOutput.greenBio.stemTipBark);
		double stemDryWeightDiff = treeOutput.dryBio.stemWoodTotal + treeOutput.dryBio.stemBarkTotal -
			(treeOutput.dryBio.stumpWood + treeOutput.dryBio.stumpBark + treeOutput.dryBio.stemPrimaryWood +
			treeOutput.dryBio.stemPrimaryBark + treeOutput.dryBio.stemTipWood + treeOutput.dryBio.stemTipBark);
		treeOutput.greenBio.branches += stemGreenWeightDiff;
		treeOutput.dryBio.branches += stemDryWeightDiff;

		treeOutput.carbonContent = treeOutput.dryBio.aboveGroundTotal * refSpeciesData.carbonFraction;
	}
	else {
		//for woodland species
		BiomassOutput woodlandTreeBiomass = woodlandBiomass(options, tree, treeOutput.totalCubicFoot);

		//adjust broken top
		if (tree.heightToTopBroken > 0.0 && tree.heightToTopBroken < tree.totalHeight) {
			int ecoProvince = NationalScaleVolumeBiomass::getEcoProvince(options.region, options.forest, options.district);
			double crownRatio = tree.crownRatio;
			if (tree.isLive && crownRatio == 0.0) crownRatio = 1.0;
			double branchRemain = NationalScaleVolumeBiomass::getBranchRem(ecoProvince, options.fiaCode, tree.totalHeight, tree.heightToTopBroken, crownRatio);
			woodlandTreeBiomass.foliage = branchRemain * woodlandTreeBiomass.foliage;
		}
		//adjust for CULL
		if (tree.isLive && tree.cull > 0.0) {
			double decayProp = 0.92;
			if (options.fiaCode >= 300) decayProp = 0.52;
			double cullFactor = 1.0 - tree.cull / 100 * (1.0 - decayProp);
			woodlandTreeBiomass = scale(woodlandTreeBiomass, cullFactor);
		}
		else if (!tree.isLive) {
			woodlandTreeBiomass.foliage = 0.0;
			int decaycd = tree.decaycd;
			if (decaycd == 0) decaycd = 3;
			double deadCarbonFrac = 0.5;
			double denProp = NationalScaleVolumeBiomass::getDecayDenProp(refSpeciesData.softHard, decaycd, deadCarbonFrac);
			woodlandTreeBiomass = scale(woodlandTreeBiomass, denProp);

		}
		treeOutput.dryBio = woodlandTreeBiomass;

		//convert dry to green
		treeOutput.greenBio = scale(treeOutput.dryBio, mcFactor);
		treeOutput.carbonContent = treeOutput.dryBio.aboveGroundTotal * carbonFraction;
		treeOutput.greenWeightPrimary = treeOutput.dryWeightPrimary * mcFactor;
		treeOutput.greenWeightSecondary = treeOutput.dryWeightSecondary * mcFactor;
	}

	return treeOutput;

}

std::string VolumeLibrary::GetVolumeEquationNumber(VolumeCalculationOptions options)
{
	VolumeEquation volumeEquation = VolumeEquationResolver::GetVolumeEquation(options);
	return volumeEquation.volEqStr;
}
