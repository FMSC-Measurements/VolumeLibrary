#include <cmath>
#include <cstddef>
#include <string>
#include <array>
#include <stdexcept>
#include "DirectVolumeCalculator_BIA.h"

//VOLEQ: I00DVEW000
// C++ translation of Fortran:
//   SUBROUTINE VolEq_Johnson(dbh, ht, FCLASS, VOL)
//
// Inputs:
//   dbh     : diameter at breast height (inches)
//   ht      : total height (feet)
//   FCLASS  : integer Form Class (e.g., 78, 80); if < 10, considered "not provided"
// Outputs:
//   VOL     : array of length >= 4; sets VOL(4) in Fortran => VOL[3] in C++
//
// Notes:
// - Fortran LOG() is the natural logarithm; we use std::log.
// - Fortran REAL() casts are not needed; we work entirely in double.
// - If FCLASS < 10 (i.e., no form class), default to 0.72 per Fortran logic.
// - DIB16 = dbh * formclass, capped at dbh.
// - If dbh < 1.0, returns 0 volume (VOL[3] unchanged by design).
TreeOutput VolEq_Johnson(TreeMeasurment tree)
//inline void VolEq_Johnson(double dbh, double ht, int FCLASS, double VOL[15])
{
    TreeOutput out;
    double dbh = tree.dbh;
    double ht = tree.totalHeight;
    int FCLASS = tree.formClass;

    // Minimum DBH for cubic volume (as in Fortran)
    const double MinCUFTDBH = 1.0;

    // Form class: if not provided (<10), set to 0 then default to 0.72
    double formclass = (FCLASS < 10) ? 0.0 : static_cast<double>(FCLASS) / 100.0;
    if (formclass == 0.0) formclass = 0.72;

    // Derived inside-bark diameter at 16 ft
    double DIB16 = dbh * formclass;
    if (DIB16 > dbh) DIB16 = dbh;

    // Early exit for tiny trees
    if (dbh < MinCUFTDBH) {
        // Fortran returns without changing VOL; here we can write explicit zero to VOL(4)
        // to match typical expectations. Comment this out if you prefer not to set.
        // VOL[3] = 0.0;
        out.errflag = 3;
        return out;
    }

    // Butt section volume
    double BUTT = 0.0;
    if (dbh < 6.0) {
        // For small DBH (< 6")
        BUTT = ((dbh - 4.0) * 0.5) *
            (-4.68329 + (18.7668 * formclass) - (12.18 * std::pow(formclass, 2.0)));
    }
    else {
        // For DBH ≥ 6"
        BUTT = 8.436
            - 2.608 * dbh
            + 0.070242 * std::pow(dbh, 2.0)
            + 3.1278 * DIB16
            - 12.18 * std::pow(formclass, 2.0);
    }

    double CalcCUFTVolume = 0.0;

    // If DIB16 < 4 or height ≤ 17, only butt section contributes
    if (DIB16 < 4.0 || ht <= 17.0) {
        CalcCUFTVolume = BUTT;
    }
    else {
        // Otherwise add upper stem contribution
        const double A = (ht - 17.0) * std::pow(DIB16, 2.0) * 0.022716;
        const double B = (DIB16 - 4.0) / (DIB16 - 1.96);
        const double C3 = std::pow(DIB16 - 1.96, 2.0) / std::pow(DIB16, 2.0);
        const double C = std::log(C3); // natural log
        const double D = 0.51 * (DIB16 - 4.0) / DIB16;

        const double UPPER = A * (B - 1.401656 - (1.040816 * C) + D);

        CalcCUFTVolume = BUTT + UPPER;
    }

    // Fortran sets VOL(4) = CalcCUFTVolume
    out.grossCubicFootPrimary = CalcCUFTVolume;
    return out;
}

// Canadian equation used by BIA eastern region
// VOLEQ: C00DVEE*** (where *** is species three digid code)
// Fortran -> C++ VOL index mapping:
// VOL(1)  -> VOL[0]   // Total cubic volume (Vtcf)
// VOL(2)  -> VOL[1]   // Merchantable board foot volume (Vmbf), rounded
// VOL(4)  -> VOL[3]   // Merchantable cubic volume (Vmcf), rounded
// VOL(7)  -> VOL[6]   // Topwood cubic volume (Vtops - Vmcf)
// VOL(15) -> VOL[14]  // Not used here (commented out in original)

TreeOutput Voleq_Honer(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;
    double DBH = tree.dbh;
    double HT = tree.totalHeight;
    double MTOPP = merchRules.minTopDibSaw;
    double MTOPS = merchRules.minTopDibNonSaw;
    if (vco.primaryProduct != 1) MTOPP = MTOPS;
    std::array<double, 15> VOL;
    VOL.fill(0.0);

    // --- Parse species code from VOLEQ(8:10) (Fortran 1-based) -> C++ substr(7,3) ---
    int ispc = 0;
    if (VOLEQ.size() >= 10) {
        try {
            ispc = std::stoi(VOLEQ.substr(7, 3));
        }
        catch (...) {
            ispc = 0;
        }
    }

    // Defaults per Fortran
    if (MTOPP <= 0.0) {
        MTOPP = (ispc < 300 ? 7.0 : 9.0); // softwood vs hardwood default
    }
    if (MTOPS <= 0.0) {
        MTOPS = 4.0;
    }

    // Minimums (per Fortran variables; set to 0 so checks become top-diameter-only)
    const double MinCUFTDBH = 0.0;
    const double MinCUFTTop = MTOPP;
    const double MinBDFTDBH = 0.0;
    double MinBDFTTop = MTOPP;
    if (MinBDFTTop <= 0.0) MinBDFTTop = 7.0;

    // Honer parameter arrays (length 29)
    static const std::array<double, 29> HonerTotalCUFT1 = {
        2.139, 2.139, 1.44, 1.588, 1.226, 0.897, 0.71, 0.691, 4.167, 1.112,
        1.046, 1.046, 1.046, 1.046, 1.046, 1.449, 2.222, 2.222, 0.959, 0.033,
        0.033, -0.312, -0.312, -0.312, 0.033, 1.512, 0.948, 1.046, 1.046
    };
    static const std::array<double, 29> HonerTotalCUFT2 = {
        301.634, 301.634, 342.175, 333.364, 315.832, 348.53, 355.623, 363.676, 244.906, 350.092,
        383.972, 383.972, 383.972, 383.972, 383.972, 344.754, 300.373, 300.373, 334.829, 393.336,
        393.336, 436.683, 436.683, 436.683, 393.336, 336.972, 401.456, 383.972, 383.972
    };
    static const std::array<double, 29> HonerMerchCUFT1 = {
        0.9352, 0.9352, 0.9611, 0.9644, 0.9644, 0.9635, 0.9672, 0.9735, 0.9645, 0.9645,
        0.9057, 0.9057, 0.9057, 0.9057, 0.9057, 0.8778, 0.9087, 0.9087, 0.9057, 0.9057,
        0.9057, 0.9354, 0.9354, 0.9354, 0.9057, 0.9057, 0.9057, 0.9057, 0.9057
    };
    static const std::array<double, 29> HonerMerchCUFT2 = {
        -0.0395, -0.0395, -0.2456, -0.0995, -0.0995, -0.15, -0.0393, -0.2346, -0.1616, -0.1616,
        -0.0708, -0.0708, -0.0708, -0.0708, -0.0708, -0.2417, -0.3049, -0.3049, -0.0708, -0.0708,
        -0.0708, 0.0957, 0.0957, 0.0957, -0.0708, -0.0708, -0.0708, -0.0708, -0.0708
    };
    static const std::array<double, 29> HonerMerchCUFT3 = {
        -0.8147, -0.8147, -0.6801, -0.7658, -0.7658, -0.8081, -1.0523, -0.7378, -0.7945, -0.7945,
        -0.8375, -0.8375, -0.8375, -0.8375, -0.8375, -0.5247, -0.5107, -0.5107, -0.8375, -0.8375,
        -0.8375, -1.1613, -1.1613, -1.1613, -0.8375, -0.8375, -0.8375, -0.8375, -0.8375
    };
    static const std::array<double, 3> HonerBDFT = { 5.4332, -1.6281, -4.471 };
    static const std::array<double, 5> HonerAll = { 1.046, 383.972, 0.9604, -0.166, -0.7868 };

    // WARNING: assumed stump height 0.5 ft
    const double Stump = 0.5;

    // SpeciesIndex selection (matching Fortran SELECT CASE as closely as possible)
    // Initialize to DEFAULT (15), so any case that doesn't set SpeciesIndex uses "MISC"
    int SpeciesIndex = 15;
    // aCoeff/bCoeff and txtEqn were present in Fortran but are not used in calculations
    // We'll set SpeciesIndex only where explicitly done in Fortran
    if (ispc == 12) {
        SpeciesIndex = 1;   // Balsam fir
    }
    else if (ispc >= 89 && ispc <= 100) {
        SpeciesIndex = 2;   // Spruce spp (and Jack Pine per Fortran block)
    }
    else if (ispc == 105) {
        SpeciesIndex = 3;   // Jack Pine (explicit)
    }
    else if (ispc == 129) {
        SpeciesIndex = 4;   // Eastern White Pine
    }
    else if (ispc == 241) {
        SpeciesIndex = 5;   // Northern White Cedar
    }
    else if (ispc == 261) {
        SpeciesIndex = 6;   // Hemlock
    }
    else if (ispc == 316) {
        SpeciesIndex = 7;   // Red Maple
    }
    else if (ispc == 371) {
        SpeciesIndex = 8;   // Yellow Birch
    }
    else if (ispc == 375) {
        SpeciesIndex = 9;   // Paper/White/Gray Birch
    }
    else if (ispc == 531) {
        SpeciesIndex = 10;  // Beech
    }
    else if (ispc >= 540 && ispc <= 545) {
        SpeciesIndex = 12;  // Eastern Ash
    }
    else if (ispc >= 740 && ispc <= 747) {
        SpeciesIndex = 13;  // Aspen/Cottonwood
    }
    else if (ispc == 762) {
        SpeciesIndex = 14;  // Black Cherry
    }
    else if (ispc == 751 || ispc == 990 || ispc == 999) {
        SpeciesIndex = 16;  // All Hardwoods (misc common)
    }
    // (For ispc == 315 or 318, the Fortran didn't set SpeciesIndex; we keep default "MISC" (15).)

    // Load species parameters B(1..5)
    const double B1 = (SpeciesIndex >= 1 && SpeciesIndex <= 29) ? HonerTotalCUFT1[SpeciesIndex - 1] : HonerAll[0];
    const double B2 = (SpeciesIndex >= 1 && SpeciesIndex <= 29) ? HonerTotalCUFT2[SpeciesIndex - 1] : HonerAll[1];
    const double M1 = (SpeciesIndex >= 1 && SpeciesIndex <= 29) ? HonerMerchCUFT1[SpeciesIndex - 1] : HonerAll[2];
    const double M2 = (SpeciesIndex >= 1 && SpeciesIndex <= 29) ? HonerMerchCUFT2[SpeciesIndex - 1] : HonerAll[3];
    const double M3 = (SpeciesIndex >= 1 && SpeciesIndex <= 29) ? HonerMerchCUFT3[SpeciesIndex - 1] : HonerAll[4];

    // ---- Computations ----
    const double D = DBH;
    const double Tht = HT;

    // Total cubic ft (Vtcf) only if Tht > 0
    if (Tht <= 0.0) {
        // In Fortran this does a RETURN with no error set; we mirror that.
        out.errflag = 4;
        return out;
    }
    const double Vtcf = (D * D) / (B1 + (B2 / Tht));
    VOL[0] = Vtcf; // VOL(1)

    // Merchantable cubic ft (Vmcf) to top MTOPP (MinCUFTTop)
    const double Tcu = MinCUFTTop;
    double Vmcf = 0.0;
    if (D > Tcu) {
        // X2 uses top diameter and stump adjustment
        const double X2 = std::pow(Tcu / D, 2.0) * (1.0 + Stump / Tht);
        Vmcf = Vtcf * (M1 + M2 * X2 + M3 * X2 * X2);

        // Optional "topwood" segment from MTOPP down to MTOPS:
        if (MTOPS > 0.0 && MTOPP > MTOPS) {
            const double X3 = std::pow(MTOPS / D, 2.0) * (1.0 + Stump / Tht);
            const double Vtops = Vtcf * (M1 + M2 * X3 + M3 * X3 * X3);
            VOL[6] = Vtops - Vmcf; // VOL(7) = topwood cubic volume
            // NOTE: In the Fortran, VOL(15) potential 'total-topwood' (Vtcf - Vtops) was commented out.
            // If desired, you could set VOL[14] = Vtcf - Vtops here.
        }
    }
    else {
        Vmcf = 0.0;
    }

    // Round Vmcf to two decimals (ANINT(Vmcf*100)/100) and set VOL(4)
    const double VmcfRounded = std::nearbyint(Vmcf * 100.0) / 100.0;
    VOL[3] = VmcfRounded;

    // Merchantable board foot (Vmbf) to MinBDFTTop (default 7.0 if <=0)
    const double Tbd = MinBDFTTop;
    double Vmbf = 0.0;
    if (D > MinBDFTDBH) {
        if (Tht > 0.0 && Tbd > 0.0 && D > Tbd) {
            const double X1 = std::pow(Tbd / D, 2.0) * (1.0 + Stump / Tht);
            Vmbf = Vtcf * (HonerBDFT[0] + HonerBDFT[1] * X1 + HonerBDFT[2] * X1 * X1);
        }
        else {
            Vmbf = 0.0;
        }
    }
    else {
        Vmbf = 0.0;
    }

    // Round Vmbf to two decimals and set VOL(2)
    const double VmbfRounded = std::nearbyint(Vmbf * 100.0) / 100.0;
    VOL[1] = VmbfRounded;

    // Ensure non-negative outputs (optional; Fortran does not clamp here explicitly)
    if (VOL[0] < 0.0) VOL[0] = 0.0;
    if (VOL[1] < 0.0) VOL[1] = 0.0;
    if (VOL[3] < 0.0) VOL[3] = 0.0;
    if (VOL[6] < 0.0) VOL[6] = 0.0;
    out.totalCubicFoot = VOL[0];
    out.grossBoardFootPrimary = VOL[1];
    out.grossCubicFootPrimary = VOL[3];
    out.grossCubicFootSecondary = VOL[6];
    return out;
}