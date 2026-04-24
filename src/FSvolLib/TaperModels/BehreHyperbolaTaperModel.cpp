#include "BehreHyperbolaTaperModel.h"

BehreHyperbolaTaperModel::BehreHyperbolaTaperModel(VolumeEquation volumeEquation, MerchRules merchRules)
    : TaperModel(), volEqStr(volumeEquation.GetVolumeEquationNumber())
{
    topDibSaw = merchRules.minTopDibSaw;
}

double BehreHyperbolaTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    double DBHOB = tree.dbh;
    double HTTOT = tree.totalHeight;
    double TLH = 0.0;
    if (tree.merchHeightUnit != TreeMeasurment::MerchHeightUnit::FEET) TLH = tree.totalHeight;
    double HTUP = height;
    int FCLASS = tree.formClass;
    double TOP = topDibSaw;

    return BEHtaper(volEqStr, DBHOB, HTTOT, TLH, HTUP, FCLASS, TOP);
}

double BehreHyperbolaTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter)
{
    double dbh = tree.dbh;
    double totalHeight = tree.totalHeight;

    // Check if diameter is effectively zero
    if (diameter <= 0.1) {
        return totalHeight;
    }

    // Binary search for the height that gives the target diameter
    double lowHeight = 0.0;
    double highHeight = totalHeight;
    double tolerance = 0.01;  // 0.01 feet tolerance

    while (highHeight - lowHeight > tolerance) {
        double midHeight = (lowHeight + highHeight) / 2.0;
        double calcDiameter = GetDiameterAtHeight(tree, midHeight);

        if (calcDiameter > diameter) {
            lowHeight = midHeight;
        }
        else {
            highHeight = midHeight;
        }
    }

    return (lowHeight + highHeight) / 2.0;

}
