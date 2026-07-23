//#include "pch.h"
#include "VolumeLibrary.h"
#include "WoodlandBiomass.h"
#include "VolumeCalculators\JenkinsBiomass.h"



TreeOutput VolumeLibrary::CalculateVolume(const VolumeCalculationOptions options, const TreeMeasurment tree, std::optional<MerchRules> maybe_merchRules)
{
	if (tree.dbh < 1.0 && tree.drc < 1.0) {
		throw std::invalid_argument("DBH less than one!");
	}

	WeightFactorAndRefData refSpeciesData = getSpeciesWtfactorAndRefData(options.region, options.forest, options.fiaCode);

	double weightFactor = (options.primaryProduct == 1) ? refSpeciesData.weightFactorSaw : refSpeciesData.weightFactorNonsaw;
	weightFactor = (tree.isLive) ? weightFactor : refSpeciesData.weightFactorDead;

	double moistContent = 0.0;
	moistContent = (weightFactor - refSpeciesData.weightFactorDry) / refSpeciesData.weightFactorDry;
	double mcFactor = 1.0 + moistContent;

	double carbonFraction = refSpeciesData.carbonFraction;

	//for trees with DBH only measurement, using Jenkins method to calculate biomass
	if (tree.dbh > 0.0 && tree.totalHeight == 0.0 &&
		tree.merchHeightSaw == 0.0 && tree.merchHeightNonsaw == 0.0 &&
		tree.heightToTopBroken == 0.0 && tree.referenceHeight == 0.0) {

		TreeOutput treeOutput;
		BiomassOutput treeBiomass = jenkins(options.fiaCode, tree.dbh);
		treeOutput.dryBio = treeBiomass;
		treeOutput.greenBio = scale(treeOutput.dryBio, mcFactor);
		treeOutput.carbonContent = treeOutput.dryBio.aboveGroundTotal * carbonFraction;

		return treeOutput;
	}

	auto merchRules = (maybe_merchRules.has_value()) ? maybe_merchRules.value() : merchRulesResolver_.GetMerchRules(options);
	
	//region 7 (BLM) saw top diameter
	if (options.region == 7) merchRules.minTopDibSaw = tree.dbh * 0.184 + 2.24;
	//check override parameters for stump, sawTopDib, nonsawTopDib
	if (tree.stumpHeightOverride > 0.0) merchRules.stumpHeight = tree.stumpHeightOverride;
	if (tree.minTopDibSawOverride > 0.0) merchRules.minTopDibSaw = tree.minTopDibSawOverride;
	if (tree.minTopDibNonSawOverride > 0.0) merchRules.minTopDibNonSaw = tree.minTopDibNonSawOverride;

	//for FIA with NSVB equation, just call the NSVB calculation
	if (options.volumeCalculationOptions == VolumeCalculationOptions::VolumeCalculationType::FIA 
		&& options.volumeEquationNumberOverride.substr(0, 3) == "NVB"
		&& refSpeciesData.jenkinsSpeciesGroupCD != 10)
	{
		NationalScaleVolumeBiomass nsvb = NationalScaleVolumeBiomass(options.volumeEquationNumberOverride, options);
		TreeOutput nsvbOutput = nsvb.CalculateVolumeBiomass(options, tree, merchRules);

		return nsvbOutput;
	}

	//Calculate volume for Cruise, FVS, and FIA using volume library profile model and Direct volume equation
	//volume calculation using Profile model or Direct volume estimator
	auto& volumeCalculator = volumeCalculatorFactory_.MakeVolumeCalculator(options);
	auto treeOutput = volumeCalculator.CalculateVolume(options,tree, merchRules);

	//biomass calculation using NSVB 
	if (refSpeciesData.jenkinsSpeciesGroupCD != 10) {
		//call NSVB to get volume and biomass for non-woodland species
		NationalScaleVolumeBiomass nsvb = NationalScaleVolumeBiomass(options);
		TreeOutput nsvbOutput = nsvb.CalculateVolumeBiomass(options, tree, merchRules);

		if (nsvbOutput.dryBio.aboveGroundTotal > 0.0) {
			mcFactor = nsvbOutput.greenBio.aboveGroundTotal / nsvbOutput.dryBio.aboveGroundTotal;
		}

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

		double merchStemDryWeight = treeOutput.dryBio.stemPrimaryBark + treeOutput.dryBio.stemPrimaryWood +
			treeOutput.dryBio.stemSecondaryBark + treeOutput.dryBio.stemSecondaryWood;
		//reset biomass conponent value
		if (treeOutput.grossCubicFootPrimary > 0.0) {
			treeOutput.greenBio.stemPrimaryWood = treeOutput.greenWeightPrimary * ratioWood;
			treeOutput.greenBio.stemPrimaryBark = treeOutput.greenWeightPrimary * (1.0 - ratioWood);
			treeOutput.dryBio.stemPrimaryWood = ratioPrimary * merchStemDryWeight * ratioWood;
			treeOutput.dryBio.stemPrimaryBark = ratioPrimary * merchStemDryWeight * (1.0 - ratioWood);
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
			treeOutput.dryBio.stemSecondaryWood = (1.0 - ratioPrimary) * merchStemDryWeight * ratioWood;
			treeOutput.dryBio.stemSecondaryBark = (1.0 - ratioPrimary) * merchStemDryWeight * (1.0 - ratioWood);
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
			treeOutput.dryBio.stemTipWood = treeOutput.greenBio.stemTipWood / mcFactor;
			treeOutput.dryBio.stemTipBark = treeOutput.greenBio.stemTipBark / mcFactor;
		}

		//the difference should be add to branches
		//double stemGreenWeightDiff = treeOutput.greenBio.stemWoodTotal + treeOutput.greenBio.stemBarkTotal -
		//	(treeOutput.greenBio.stumpWood + treeOutput.greenBio.stumpBark + treeOutput.greenBio.stemPrimaryWood +
		//	treeOutput.greenBio.stemPrimaryBark + treeOutput.greenBio.stemTipWood + treeOutput.greenBio.stemTipBark);
		//double stemDryWeightDiff = treeOutput.dryBio.stemWoodTotal + treeOutput.dryBio.stemBarkTotal -
		//	(treeOutput.dryBio.stumpWood + treeOutput.dryBio.stumpBark + treeOutput.dryBio.stemPrimaryWood +
		//	treeOutput.dryBio.stemPrimaryBark + treeOutput.dryBio.stemTipWood + treeOutput.dryBio.stemTipBark);
		//treeOutput.greenBio.branches += stemGreenWeightDiff;
		//treeOutput.dryBio.branches += stemDryWeightDiff;

		//adjust other biomass components
		treeOutput.greenBio.stemBarkTotal = 
			treeOutput.greenBio.stumpBark + treeOutput.greenBio.stemPrimaryBark +
			treeOutput.greenBio.stemSecondaryBark + treeOutput.greenBio.stemTipBark;
		treeOutput.greenBio.stemWoodTotal = 
			treeOutput.greenBio.stumpWood + treeOutput.greenBio.stemPrimaryWood +
			treeOutput.greenBio.stemSecondaryWood + treeOutput.greenBio.stemTipWood;
		treeOutput.dryBio.stemBarkTotal = 
			treeOutput.dryBio.stumpBark + treeOutput.dryBio.stemPrimaryBark +
			treeOutput.dryBio.stemSecondaryBark + treeOutput.dryBio.stemTipBark;
		treeOutput.dryBio.stemWoodTotal = 
			treeOutput.dryBio.stumpWood + treeOutput.dryBio.stemPrimaryWood +
			treeOutput.dryBio.stemSecondaryWood + treeOutput.dryBio.stemTipWood;

		treeOutput.greenBio.aboveGroundTotal = treeOutput.greenBio.stemBarkTotal + treeOutput.greenBio.stemWoodTotal + treeOutput.greenBio.branches;
		treeOutput.dryBio.aboveGroundTotal = treeOutput.dryBio.stemBarkTotal + treeOutput.dryBio.stemWoodTotal + treeOutput.dryBio.branches;

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

double VolumeLibrary::GetHeightAtDiameter(const std::string& volumeEquationNumber, TreeMeasurment tree, double diameter)
{
	VolumeCalculationOptions vco;
	vco.volumeEquationNumberOverride = volumeEquationNumber;
	auto& volumeCalculator = volumeCalculatorFactory_.MakeVolumeCalculator(vco);

	return volumeCalculator.GetHeightAtDiameter(vco, tree, diameter);
}

double VolumeLibrary::GetDiameterAtHeight(const std::string& volumeEquationNumber, TreeMeasurment tree, double height)
{
	VolumeCalculationOptions vco;
	vco.volumeEquationNumberOverride = volumeEquationNumber;
	auto& volumeCalculator = volumeCalculatorFactory_.MakeVolumeCalculator(vco);

	return volumeCalculator.GetDiameterAtHeight(vco, tree, height);
}

int VolumeLibrary::GetNumberOfLogs(VolumeCalculationOptions options, TreeMeasurment tree, std::optional<MerchRules> maybe_merchRules)
{
	int numberOfLogs = 0;
	double merchLength = 0.0;
	auto merchRules = (maybe_merchRules.has_value()) ? maybe_merchRules.value() : merchRulesResolver_.GetMerchRules(options);

	//region 7 (BLM) saw top diameter
	if (options.region == 7) merchRules.minTopDibSaw = tree.dbh * 0.184 + 2.24;
	//check override parameters for stump, sawTopDib, nonsawTopDib
	if (tree.stumpHeightOverride > 0.0) merchRules.stumpHeight = tree.stumpHeightOverride;
	if (tree.minTopDibSawOverride > 0.0) merchRules.minTopDibSaw = tree.minTopDibSawOverride;

	if (tree.merchHeightSaw > 0.0) {
		merchLength = tree.merchHeightSaw - merchRules.stumpHeight;
	}
	else {
		auto& volumeCalculator = volumeCalculatorFactory_.MakeVolumeCalculator(options);
		double heightToSawTopDib = volumeCalculator.GetHeightAtDiameter(options, tree, merchRules.minTopDibSaw);
		if (tree.heightToTopBroken > 0.0 && heightToSawTopDib > tree.heightToTopBroken) {
			heightToSawTopDib = tree.heightToTopBroken;
		}
		merchLength = heightToSawTopDib - merchRules.stumpHeight;
	}

	std::vector<double> logs = ProfileVolumeCalculator::getLogs(merchLength, merchRules, numberOfLogs);

	return numberOfLogs;
}