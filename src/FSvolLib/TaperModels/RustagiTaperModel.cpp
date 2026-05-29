#include "RustagiTaperModel.h"

double RustagiTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
	double dib = 0.0;
	return R4MAT_Taper(volEqStr, tree.dbh, tree.totalHeight, height, dib);
}

double RustagiTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob)
{
	double htup = 0.0;
	return R4MAT_Taper(volEqStr, tree.dbh, tree.totalHeight, htup, diameter);
}