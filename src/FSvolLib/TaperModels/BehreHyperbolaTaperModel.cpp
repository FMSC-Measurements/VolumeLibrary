#include "BehreHyperbolaTaperModel.h"
#include "../DefaultFormClassForFVS.h"

BehreHyperbolaTaperModel::BehreHyperbolaTaperModel(VolumeEquation volumeEquation)
    : TaperModel(), volEqStr(volumeEquation.GetVolumeEquationNumber())
{
    
}


void BehreHyperbolaTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    topDibSaw = merchRules.minTopDibSaw;
    formClass = tree.formClass;
    if (tree.formClass == 0) {
        if (vco.volumeCalculationOptions == VolumeCalculationOptions::VolumeCalculationType::CRUISE) {
            throw std::invalid_argument("Form Class missing");
        }
        else {
            formClass = GetFormClass(volEqStr, vco.forest, tree.dbh);
        }
    }
}

double BehreHyperbolaTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    double totalLogHeight = 0.0;
    if (tree.merchHeightUnit != TreeMeasurment::MerchHeightUnit::FEET) totalLogHeight = tree.totalHeight;

    return BEHtaper(volEqStr, tree.dbh, tree.totalHeight, totalLogHeight, height, formClass, topDibSaw);
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
