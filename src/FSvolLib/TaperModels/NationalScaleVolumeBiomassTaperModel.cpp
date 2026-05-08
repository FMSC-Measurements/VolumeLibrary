#include "NationalScaleVolumeBiomassTaperModel.h"

void NationalScaleVolumeBiomassTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
	if(volEq.volEqStr != "223DVEW122") nsvb.setIbToObRatio(tree.dbh, tree.totalHeight);
}

double NationalScaleVolumeBiomassTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
	return nsvb.getDiaAtHeight(tree.dbh, tree.totalHeight, height);
}

double NationalScaleVolumeBiomassTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter)
{
	return nsvb.getHeightAtDiameter(tree.dbh, tree.totalHeight, diameter);
}

std::array<double, 4> NationalScaleVolumeBiomassTaperModel::GetStemVolumes(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
	std::array<double, 4> result = { 0.0,0.0,0.0,0.0 };
	TreeOutput out = nsvb.CalculateVolumeBiomass(vco, tree, merchRules);
	result[0] = out.stumpCubicFoot;
	result[1] = out.grossCubicFootPrimary;
	result[2] = out.grossCubicFootSecondary;
	result[3] = out.tipCubicFoot;
	return result;
}