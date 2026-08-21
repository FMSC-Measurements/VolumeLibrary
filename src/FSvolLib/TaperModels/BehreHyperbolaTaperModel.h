#pragma once
#include "..\VolumeEquation.h"
#include "..\Models\MerchRules.h"
#include "TaperModel.h"
#include <array>
#include <cmath>

class BehreHyperbolaTaperModel : public TaperModel
{
private:
    VolumeEquation volumeEquation_;
    std::string volEqStr_;
    double topDibSaw_ = 6.0;
    int formClass_ = 0;
    double formClassHeight_ = 17.3;
    double dbhIb_;
    double d17_;
    double stumpHeight_ = 1.0;
    double maxLogLength_ = 16.0;
    double trim_ = 0.3;
    double formClassLogLengthWithTrim_ = 16.3;

    int blmProfile = 10;
    int blmTaperEq = 56;

    double A = 0.62;
    double B = 0.38;

// BLMTHT: 4x10 coefficients for Behr's hyperbola (B0,B1,B2,B3)
    static constexpr std::array<std::array<double, 4>, 10> BLMTHT = { {
            // J=1..10 (Fortran); C++ indices 0..9
            // 01 zone 01
            { 0.6448,   -0.00196,  0.0,        0.0        },
            // 01 zone 02
            { 0.6096,   -0.00196,  0.0,        0.0        },
            // 10, 11
            { 0.31385,   0.0,      0.002985,  -0.00003386 },
            // 13
            { 0.4779,    0.0,      0.0,        0.0        },
            // 14
            { 0.5455,   -0.00196,  0.0,        0.0        },
            // 31 zone 1, 33
            { 0.45648,   0.00289,  0.0,        0.0        },
            // 32, 34, 35
            { 0.6014,    0.0,      0.0,        0.0        },
            // 48 (hemlock) corrected B3=0.00000546
            { 0.54568,   0.0,      0.0,        0.00000546 },
            // 51, 54, 55
            { 0.4606,    0.0,      0.0,        0.0        },
            // ALL OTHER SPECIES
            { 0.6200,    0.0,      0.0,        0.0        }
        } };

 
    double BehrTaper(TreeMeasurment tree, double stemUpperHeight);

public:
    BehreHyperbolaTaperModel(VolumeEquation volumeEquation);

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override; // {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob = false) override;

    StemVolume GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override; // { return { 0.0,0.0,0.0,0.0 }; };

};