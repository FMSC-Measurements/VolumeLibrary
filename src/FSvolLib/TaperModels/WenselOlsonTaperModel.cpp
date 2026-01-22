//#include "..\pch.h"
#include "WenselOlsonTaperModel.h"

WenselOlsonTaperModel::WenselOlsonTaperModel(VolumeEquation volumeEquation)
    : TaperModel(), fiaCode_(volumeEquation.fiaCode)
{
    int speciesIndex = GetSpeciesIndex(fiaCode_);

    if (speciesIndex < 0 || speciesIndex >= 9) {
        throw std::invalid_argument("Invalid FVS species code for Wensel-Olson model");
    }

    c_ = COEF_C[speciesIndex];
    b_ = COEF_B[speciesIndex];

}

int WenselOlsonTaperModel::GetSpeciesIndex(int fvsSpeciesCode)
{
    // Map FVS species codes to coefficient array indices
    // Based on VOLEQ(8:10) values from r5tap.f
    switch (fvsSpeciesCode) {
        case 202: return 0;  // Douglas Fir
        case 122: return 1;  // Ponderosa Pine
        case 117: return 2;  // Sugar Pine
        case 15:  return 3;  // White Fir
        case 20:  return 4;  // Red Fir
        case 81:  return 5;  // Incense Cedar
        case 116: return 6;  // Jeffrey Pine
        case 108: return 7;  // Lodgepole Pine
        case 211: return 8;  // Redwood
        default:  return -1; // Invalid species
    }
}

double WenselOlsonTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    double dbh = tree.dbh;
    double totalHeight = tree.totalHeight;
    
    // Check if height is above total height
    if (height > totalHeight) {
        return 0.0;
    }
    
    double dibcor = 0.0;
    
    // Calculate diameter based on height position
    if (height >= 4.499) {
        // Upper stem calculation using Wensel and Krumland model
        double term1 = c_[0];
        double term2 = c_[2] + c_[3] * dbh + c_[4] * totalHeight;
        
        // Special constraint for White Fir (species index 3)
        if (fiaCode_ == 15  && term2 > -1.0) {
            term2 = -1.0;
        }
        
        double term3 = std::pow((height - 1.0) / (totalHeight - 1.0), c_[1]);
        
        // Calculate term4 with exponential
        double dm = 0.0;  // Diameter modifier (not used in basic calculation)
        double expTerm = std::exp(c_[0] / term2 - dm / (dbh * term2));
        double term4 = std::log(1.0 - term3 * (1.0 - expTerm));
        
        dibcor = dbh * (term1 - term2 * term4);
    }
    else {
        // Stump portion calculation
        double term1 = (1.0 - b_[0]) * dbh;
        double term2 = std::exp(b_[1] * (4.5 - height));
        
        dibcor = term1 * term2;
    }
    
    return dibcor;
}

double WenselOlsonTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter)
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
