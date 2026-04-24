#include "NationalScaleVolumeBiomassTaperModel.h"

void NationalScaleVolumeBiomassTaperModel::InitializeOnTree(TreeMeasurment tree)
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