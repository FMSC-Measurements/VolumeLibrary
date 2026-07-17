#pragma once
#include "..\VolumeEquation.h"
#include "TaperModel.h"
#include <array>
#include <cmath>

class RustagiTaperModel : public TaperModel
{
private:
    std::string volEqStr;
// Store CFCOEF in the exact order as the Fortran DATA statement.
// Fortran fills 2D arrays column-major: (1,1),(2,1)...(20,1),(1,2)...(20,7).
// We'll access with a helper to mimic CFCOEF(II, col).
    static constexpr std::array<double, 140> CFCOEF = {
        0.5563,0.4496,0.2359,0.8435,1.1152,0.5405,1.2552,
        0.4302,0.4727,2.0461,0.6294,1.2467,0.5334,0.5301,0.8315,
        0.5028,0.7572,0.6310,0.4850,0.6076,-0.0636,-0.3956,
        -0.7237,-0.2800,-0.3678,-0.5908,-0.1727,-0.5229,-0.0508,-0.5080,
        -0.0939,-0.1615,-0.0071,0.2327,0.0967,-0.0942,-0.4527,-0.1026,
        -0.3636,-0.1730,0.9900,1.2756,1.6343,1.0264,0.9828,
        1.3374,0.9435,1.3824,1.0615,0.9113,0.9886,0.9013,1.0127,0.8748,
        0.8293,1.0600,1.1502,1.0230,1.2324,1.0585,
        -0.2,-0.041,-0.041,-0.041,-0.191,-0.191,-0.131,-0.131,
        -0.159,-0.041,-0.488,-0.200,-0.200,-0.365,-0.365,-0.365,-0.143,
        -0.143,0.000,-0.153,0.964,0.884,0.884,0.884,0.943,0.943,
        0.886,0.886,0.832,0.884,0.894,0.964,0.964,0.887,0.887,0.887,
        0.933,0.933,0.887,0.883,
        -0.201,-0.041,-0.041,-0.041,-0.192,-0.192,-0.159,
        -0.159,-0.055,-0.041,-0.445,-0.201,-0.201,-0.367,-0.367,-0.367,
        -0.144,-0.144,0.000,-0.143,0.968,0.888,0.888,0.888,
        0.947,0.947,0.891,0.891,0.837,0.888,0.897,0.968,0.968,
        0.891,0.891,0.891,0.937,0.937,0.893,0.886
    };

    // Fortran-like accessor: CFCOEF(II, col) where II in [1..20], col in [1..7]
    inline double CF(int II, int col) {
        // Flat index for column-major layout: (II-1) + (col-1)*20
        return CFCOEF[(II - 1) + (col - 1) * 20];
    }

    // Map VOLEQ to II (1..20). Returns 0 if not found.
    int mapVOLEQtoII(const std::string& VOLEQ) {
        if (VOLEQ.size() < 10) return 0;
        std::string prefix = VOLEQ.substr(0, 3);     // Fortran (1:3)
        std::string suffix = VOLEQ.substr(7, 3);     // Fortran (8:10)

        if (suffix == "746") return 1;
        else if (suffix == "202" && prefix == "400") return 2;
        else if (suffix == "202" && prefix == "405") return 3;
        else if (suffix == "202" && prefix == "401") return 4;
        else if (suffix == "019" && prefix == "400") return 5;
        else if (suffix == "019" && prefix == "405") return 6;
        else if (suffix == "015" && prefix == "400") return 7;
        else if (suffix == "015" && prefix == "401") return 8;
        else if (suffix == "081") return 9;
        else if (suffix == "073") return 10;
        else if (suffix == "122" && prefix == "403") return 11;
        else if (suffix == "108" && prefix == "400") return 12;
        else if (suffix == "108" && prefix == "401") return 13;
        else if (suffix == "122" && prefix == "401") return 14;
        else if (suffix == "122" && prefix == "402") return 15;
        else if (suffix == "122" && prefix == "400") return 16;
        else if (suffix == "093" && prefix == "400") return 17;
        else if (suffix == "093" && prefix == "407") return 18;
        else if (suffix == "020") return 19;
        else if (suffix == "117") return 20;
        else return 0;
    }

    double R4MAT_Taper(
        const std::string& VOLEQ,
        double DBHOB,
        double HTTOT,
        double HTUP,
        double DIB,
        bool calcCF0 = false)
    {
        int ERRFLAG = 0;
        double STUMPD = 0.0;
        double BUTTCF = 0.0;
        double CF0 = 0.0;
        double B = 0.0;
        
        // Basic input checks (same as Fortran)
        if (DBHOB < 1.0) {
            ERRFLAG = 3;
            return 0;
        }
        // Proposed modification to prevent div by zero (same as your code)
        if (HTTOT <= 5.0) {
            ERRFLAG = 4;
            return 0;
        }

        // Determine index II from VOLEQ
        int II = mapVOLEQtoII(VOLEQ);
        if (II == 0) {
            ERRFLAG = 1;
            return 0;
        }

        // Mathis anchor at 1 ft
        double THT = HTTOT - 1.0;

        // Compute intermediates
        double HT67 = CF(II, 1) * std::pow(DBHOB, CF(II, 2)) * std::pow(THT, CF(II, 3));
        BUTTCF = CF(II, 5) * DBHOB + CF(II, 4);
        STUMPD = std::sqrt((BUTTCF * BUTTCF * THT) / (THT - 4.0));
        double D67 = CF(II, 7) * DBHOB * (2.0 / 3.0) + CF(II, 6);
        CF0 = 0.002727 * (HT67 * STUMPD * STUMPD + D67 * D67 * THT);
        if (calcCF0) return CF0;

        double F = CF0 / (0.005454 * STUMPD * STUMPD * THT);
        B = (1.0 - F) / (2.0 * F);

        // Forward: given HTUP, compute DIB
        if (HTUP > 0.0 && HTUP < HTTOT) {
            if (HTUP <= 1.0) {
                DIB = STUMPD;
            }
            else {
                double PHT = HTUP - 1.0; // height above 1 ft
                DIB = STUMPD * std::pow((THT - PHT) / THT, B);
            }
            return DIB;
        }
        // Inverse: given DIB, compute HTUP
        else if (DIB > 0.0 && DIB < STUMPD) {
            double PHT = THT - THT * std::pow(DIB / STUMPD, 1.0 / B);
            HTUP = PHT + 1.0;
            return HTUP;
        }

        return ERRFLAG;
    }
public:
    RustagiTaperModel(VolumeEquation volumeEquation)
        : TaperModel(), volEqStr(volumeEquation.volEqStr)
    {}

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob = false) override;

    StemVolume GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override { return { 0.0,0.0,0.0,0.0 }; };
};