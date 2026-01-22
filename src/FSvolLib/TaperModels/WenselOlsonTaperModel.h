#pragma once

#include "..\VolumeEquation.h"
#include "TaperModel.h"
#include <array>
#include <cmath>

class WenselOlsonTaperModel : public TaperModel
{
private:
    // Coefficients for Wensel and Krumland model
    // Species: DF(0), PP(1), SP(2), WF(3), RF(4), IC(5), JP(6), LP(7), RW(8)
    static constexpr std::array<std::array<double, 5>, 9> COEF_C = {{
        {0.84292, 0.97062, -0.38163, -0.0074002, 0.0},        // Douglas Fir
        {0.87278, 1.26066, -1.91214, 0.020445, 0.0},          // Ponderosa Pine
        {0.90051, 0.91588, -0.92964, 0.0077119, -0.0011019},  // Sugar Pine
        {0.86039, 1.45196, -2.42273, -0.15848, 0.036947},     // White Fir
        {0.87927, 0.91350, -0.56617, -0.014480, 0.0037262},   // Red Fir
        {1.0, 0.31550, -0.34316, 0.0, -0.00039283},           // Incense Cedar
        {0.82932, 1.50831, -4.08016, 0.047053, 0.0},          // Jeffrey Pine
        {1.0, 0.84257, -0.98434, 0.0, 0.0},                   // Lodgepole Pine
        {0.955, 0.387, -0.362, -0.00581, 0.00122}             // Redwood
    }};

    // Bark coefficients (B1, B2)
    static constexpr std::array<std::array<double, 2>, 9> COEF_B = {{
        {0.1420, 0.04302},   // Douglas Fir
        {0.1031, 0.03068},   // Ponderosa Pine
        {0.0743, 0.02936},   // Sugar Pine
        {0.0844, 0.03320},   // White Fir
        {0.1105, 0.05061},   // Red Fir
        {0.1177, 0.03894},   // Incense Cedar
        {0.1472, 0.03880},   // Jeffrey Pine
        {0.0147, 0.03223},   // Lodgepole Pine
        {0.153, 0.035}       // Redwood
    }};

    static constexpr double E = 2.7182818284;

    // Map FVS species code to coefficient array index
    static int GetSpeciesIndex(int fiaCode);

    const int fiaCode_;

    std::array<double, 5> c_;
    std::array<double, 2> b_;

public:
    WenselOlsonTaperModel(VolumeEquation volumeEquation);

    void InitializeOnTree(TreeMeasurment tree) override {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;
    
    double GetHeightAtDiameter(TreeMeasurment tree, double diameter) override;
};
