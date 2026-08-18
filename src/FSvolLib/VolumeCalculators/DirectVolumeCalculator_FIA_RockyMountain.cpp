#include "DirectVolumeCalculator_FIA_RockyMountain.h"

TreeOutput ChojnackyWoodlandVol(int fiaCode, double drc, double totalHeight)
{
    TreeOutput out;

    auto V1 = [](double B0, double B1, double B2, double X) {
        return B0 + B1 * X + B2 * X * X;
        };

    auto V2 = [](double B0, double B1, double B2, double X0, double X){
        return B0 + B1 * X + B2 * (3.0 * X0 * X0 - 2.0 * X0 * X0 * X0 / X);
    };

    // Small trees DIA < 3
    if (drc < 3.0) {
        out.totalCubicFoot = 0.1;
        return out;
    }

    double D2H = drc*drc*totalHeight;
    double X = D2H / 1000.0;

    double B0, B1, B2, X0;

    // Species-specific coefficients (1.5-inch equations)
    if (fiaCode == 63 || fiaCode == 66) {
        B0 = 0.0255;  B1 = 1.7479;  B2 = 0.1994;  X0 = 4.0021;
    }
    else if (fiaCode == 65 || fiaCode == 69) {
        B0 = -0.0192; B1 = 2.1297;  B2 = 0.1100;  X0 = 2.5757;
    }
    else if (fiaCode == 106) {
        B0 = -0.0594; B1 = 2.6358;  B2 = 0.3248;  X0 = 2.0773;
    }
    else {
        out.errflag = 6;
        return out;
    }

    double CV15;
    if (X <= X0)
        CV15 = V1(B0, B1, B2, X);
    else
        CV15 = V2(B0, B1, B2, X0, X);

    // Calculate volume to 3" top if needed
    if (fiaCode == 63 || fiaCode == 66) {
        B0 = -0.0601; B1 = 1.3846; B2 = 0.1566; X0 = 5.2101;
    }
    else if (fiaCode == 65 || fiaCode == 69) {
        B0 = -0.1063; B1 = 1.4373; B2 = 0.1324; X0 = 4.0243;
    }
    else if (fiaCode == 106) {
        B0 = -0.1231; B1 = 2.0741; B2 = 0.1831; X0 = 3.5503;
    }

    double CV3;
    if (X <= X0)
        CV3 = V1(B0, B1, B2, X);
    else
        CV3 = V2(B0, B1, B2, X0, X);

    out.totalCubicFoot = CV15;
    out.grossCubicFootPrimary = CV3;

    return out;
}