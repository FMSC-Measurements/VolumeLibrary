#include "NationalScaleVolumeBiomassTaperModel.h"

void NationalScaleVolumeBiomassTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
	if(volEq.volEqStr != "223DVEW122") nsvb.setIbToObRatio(tree.dbh, tree.totalHeight);
}

double NationalScaleVolumeBiomassTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
	return nsvb.getDiaAtHeight(tree.dbh, tree.totalHeight, height);
}

double NationalScaleVolumeBiomassTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob)
{
	return nsvb.getHeightAtDiameter(tree.dbh, tree.totalHeight, diameter);
}

StemVolume NationalScaleVolumeBiomassTaperModel::GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
	StemVolume result = { 0.0,0.0,0.0,0.0 };
	TreeOutput out = nsvb.CalculateVolumeBiomass(vco, tree, merchRules);
	result.stumpVol = out.stumpCubicFoot;
	result.primaryVol = out.grossCubicFootPrimary;
	result.topwoodVol = out.grossCubicFootSecondary;
	result.tipVol = out.tipCubicFoot;
	//result.volCalculated = true;

	return result;
}