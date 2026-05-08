#include "DeMarsTaperModel.h"

void DeMarsTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    topDibSaw = merchRules.minTopDibSaw;
    if (tree.merchHeightUnit != TreeMeasurment::MerchHeightUnit::FEET)
    {
        //log height, the merchHeightSaw is number of logs * 10, then get the estimated total height
        double logLength = 16.3;
        if (tree.merchHeightUnit == TreeMeasurment::MerchHeightUnit::LOGS32) logLength = 32.6;
        totalHeight = ((tree.merchHeightSaw / 10.0) * logLength + merchRules.stumpHeight) / (1.0 + (2.0 / 3.0) * merchRules.minTopDibSaw / tree.dbh);
    }
}

double DeMarsTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    if (tree.totalHeight > 0.0) totalHeight = tree.totalHeight;

	return R10TAP(tree.dbh, totalHeight, height);
}

double DeMarsTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter)
{
    // This model doesn't have a direct inverse function
    // Need to iterate to find height at diameter
    double dbh = tree.dbh;
    double totalHeight = tree.totalHeight;

    // Check if diameter is larger than DBH
    if (diameter >= dbh) {
        return 4.5;  // Return breast height
    }

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