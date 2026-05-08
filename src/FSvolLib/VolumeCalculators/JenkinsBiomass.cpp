#include "JenkinsBiomass.h"
#include "..\WeightFactorAndRefDataResolver.h"
#include <cmath>
#include <array>
#include <vector>
#include <algorithm>
#include <cstddef>

// --- Helper: linear search over first `last` entries ---
// Mimics the Fortran SEARCH: returns index (1-based) if found, 0 if not.
// Sets errflag=1 when not found.
inline int searchLinear(int last, const int* ary, int spc) {
    for (int i = 0; i < last; ++i) {
        if (ary[i] == spc) {
            return i + 1; // 1-based index, matching Fortran DONE
        }
    }
    return 0;
}

// --- STUMPCOEF: compute DOBB, DIBA, DIBB given SPCD ---
inline void stumpCoef(int spcd, double& DOBB, double& DIBA, double& DIBB) {
    // Species canonical list (splist) of length 23
    static const std::array<int, 23> splist = {
        129,125,105,90,94,95,12,261,241,802,
        833,531,371,318,317,544,543,375,743,746,
        950,740,970
    };

    // Group arrays
    static const std::array<int, 55> spgrp123 = {
        10,11,12,14,15,16,17,18,19,20,
        21,22,40,41,42,43,50,51,52,53,
        54,55,56,57,64,67,68,70,71,72,
        73,81,200,201,202,211,212,220,221,222,
        223,230,231,232,240,241,242,250,251,252,
        260,261,262,263,264
    };

    static const std::array<int, 39> spgrp4 = {
        100,101,102,103,104,105,107,108,109,110,
        111,112,113,114,115,116,117,118,119,120,
        121,122,123,124,125,126,127,128,129,130,
        131,132,135,136,137,139,142,144,299
    };

    static const std::array<int, 9> spgrp5 = { 90,91,92,93,94,95,96,97,98 };

    static const std::array<int, 27> spgrp6 = {
        350,351,352,353,355,740,741,742,743,744,
        745,746,747,748,749,752,753,920,921,922,
        923,924,925,926,927,928,929
    };

    static const std::array<int, 18> spgrp7 = {
        310,311,312,313,315,316,317,319,320,370,
        371,372,373,374,375,377,378,379
    };

    static const std::array<int, 59> spgrp9 = {
        314,318,400,401,402,403,404,405,406,407,
        408,409,410,411,412,413,531,800,801,802,
        804,805,806,807,808,809,811,812,813,815,
        816,817,818,819,820,821,822,823,824,825,
        826,827,828,830,831,832,833,834,835,836,
        837,838,839,840,841,842,844,845,851
    };

    static const std::array<int, 38> spgrp10 = {
        58,59,60,61,62,63,65,66,69,106,
        133,134,138,140,141,143,300,303,304,321,
        322,363,475,523,755,756,757,758,803,810,
        814,829,843,846,847,867,902,990
    };

    // Coefficient arrays (length 23)
    static const std::array<double, 23> dob_b = {
        .11694,.08091,.08076,.14525,.16903,
        .12147,.15359,.12667,.18850,.14872,
        .12798,.15113,.15350,.12111,.11585,
        .12766,.17376,.11655,.06834,.09658,
        .14413,.17123,.16638
    };

    static const std::array<double, 23> dib_a = {
        .91385,.90698,.90973,.94804,.95487,
        .94122,.93793,.91400,.94698,.91130,
        .92267,.96731,.94423,.93818,.94181,
        .91979,.93502,.93763,.91625,.91882,
        .92442,.92736,.93257
    };

    static const std::array<double, 23> dib_b = {
        .11182,.08469,.07926,.13722,.15664,
        .11781,.14553,.11975,.18702,.14907,
        .12506,.14082,.14335,.11424,.10740,
        .12152,.17071,.10640,.06478,.08593,
        .14240,.17626,.15803
    };

    int SPN = spcd;
    DOBB = 0.0;
    DIBA = 1.0;
    DIBB = 0.0;

    while (true) {
        // Try direct match in splist
        int idx = 0; // 1-based like Fortran
        for (std::size_t i = 0; i < splist.size(); ++i) {
            if (SPN == splist[i]) {
                idx = static_cast<int>(i) + 1;
                break;
            }
        }

        if (idx == 0) {
            // Not found in splist: search groups with the same LAST limits as Fortran.
            int err = 0;
            int done = 0;

            // Groups 1,2,3 → map to 261
            done = searchLinear(55, spgrp123.data(), SPN);
            if (done > 0) { SPN = 261; continue; }

            // Group 4 → map to 125 (NOTE: only first 34 entries searched per original code)
            done = searchLinear(34, spgrp4.data(), SPN);
            if (done > 0) { SPN = 125; continue; }

            // Group 5 → map to 90
            done = searchLinear(9, spgrp5.data(), SPN);
            if (done > 0) { SPN = 90; continue; }

            // Group 6 → map to 746
            done = searchLinear(27, spgrp6.data(), SPN);
            if (done > 0) { SPN = 746; continue; }

            // Group 7 → map to 317
            done = searchLinear(18, spgrp7.data(), SPN);
            if (done > 0) { SPN = 317; continue; }

            // Group 9 → map to 833
            done = searchLinear(59, spgrp9.data(), SPN);
            if (done > 0) { SPN = 833; continue; }

            // Group 10 → map to 833
            done = searchLinear(38, spgrp10.data(), SPN);
            if (done > 0) { SPN = 833; continue; }

            // Default to 544 and try again
            SPN = 544;
            continue;
        }

        // Found a canonical species in splist: pull coefficients (convert 1-based idx)
        const int i = idx - 1;
        DOBB = dob_b[i];
        DIBA = dib_a[i];
        DIBB = dib_b[i];
        break;
    }
}

// --- STUMPDIA: predict stump DOB/DIB at height ---
inline void stumpDia(int spn, double dbh, double stumpht,
    double& stumpdib, double& stumpdob)
{
    // Early exit per Fortran: no changes to outputs when height is out of range
    if (stumpht > 4.5 || stumpht < 0.0001) return;

    double dob_b = 0.0, dib_a = 1.0, dib_b = 0.0;
    stumpCoef(spn, dob_b, dib_a, dib_b);

    // Fortran formulas:
    // stumpdob = dbh + dbh*dob_b*(4.5-stumpht)/(stumpht+1.0)
    // stumpdib = dbh*dib_a + dbh*dib_b*(4.5-stumpht)/(stumpht+1.0)
    stumpdob = dbh + dbh * dob_b * (4.5 - stumpht) / (stumpht + 1.0);
    stumpdib = dbh * dib_a + dbh * dib_b * (4.5 - stumpht) / (stumpht + 1.0);
}

// --- RAILEVOL: cubic foot volume via Raile integration (IB/OB) ---
StumpVolume raileVol(int spn, double dbh, double htup)
{
    StumpVolume out;
    double volIB;
    double volOB;

    // Constant: π/4 / 144 ≈ 0.0054541539
    constexpr double K = 0.0054541539;

    if (htup < 0.01) htup = 1.0;

    double dob_b = 0.0, dib_a = 1.0, dib_b = 0.0;
    stumpCoef(spn, dob_b, dib_a, dib_b);

    const double termIB_top =
        std::pow(dib_a - dib_b, 2) * htup
        + 11.0 * dib_b * (dib_a - dib_b) * std::log(htup + 1.0)
        - 30.25 / (htup + 1.0) * std::pow(dib_b, 2);

    const double termIB_bottom =
        std::pow(dib_a - dib_b, 2) * 0.0
        + 11.0 * dib_b * (dib_a - dib_b) * std::log(0.0 + 1.0)
        - 30.25 / (0.0 + 1.0) * std::pow(dib_b, 2);

    volIB = K * dbh * dbh * (termIB_top - termIB_bottom);

    // NOTE: The original Fortran has ((HTUP - DOB_B)**2 * 0) in the lower limit,
    // whereas symmetry suggests ((1 - DOB_B)**2 * 0). We preserve your code.
    const double termOB_top =
        std::pow(1.0 - dob_b, 2) * htup
        + 11.0 * dob_b * (1.0 - dob_b) * std::log(htup + 1.0)
        - 30.25 / (htup + 1.0) * std::pow(dob_b, 2);

    const double termOB_bottom =
        std::pow(htup - dob_b, 2) * 0.0
        + 11.0 * dob_b * (1.0 - dob_b) * std::log(0.0 + 1.0)
        - 30.25 / (0.0 + 1.0) * std::pow(dob_b, 2);

    volOB = K * dbh * dbh * (termOB_top - termOB_bottom);
    out.woodVol = volIB;
    out.barkVol = volOB - volIB;
    return out;
}

BiomassOutput jenkins(int SPEC, double DBHOB)
//void JENKINS(int SPEC, double DBHOB)
{
    BiomassOutput out;
    StumpVolume stumpVol;
    RefSpeciesData spcdRefData;
    std::array<double, 8> BIOMS;
    BIOMS.fill(0.0);

    // --- Local variables (matching Fortran names/types) ---
    int SPCD = SPEC;
    int SPGRPCD = 0;
    int SPCLS = 0;            // 0=softwood, 1=hardwood (per Fortran usage)
    //int FIRST, LAST, HALF, DONE;

    double DBHIN, B0 = 0.0, B1 = 0.0, DBHCM;
    double FOL = 0.0, ROOT = 0.0, WOOD = 0.0, BARK = 0.0, STMTOT = 0.0;
    double ABT = 0.0, BRANCHES = 0.0, CROWN = 0.0, STUMP = 0.0;
    double A0F = 0.0, A1F = 0.0, A0R = 0.0, A1R = 0.0, A0W = 0.0, A1W = 0.0, A0B = 0.0, A1B = 0.0;
    double FOLRATIO = 0.0, ROTRATIO = 0.0, WDRATIO = 0.0, BKRATIO = 0.0;
    double KG2LB = 2.20462;

    double STUMPVIB = 0.0, STUMPVOB = 0.0, STUMPHT = 1.0;
    double WDEN = 0.0, BDEN = 0.0;

    // --- Coefficients for above-ground total biomass (Jenkins 10 species groups) ---
    // COEF: rows (group_code, B0, B1)
    static const struct { int grp; double B0; double B1; } COEF[10] = {
        {  1, -2.0336, 2.2592 }, // cedar/larch
        {  2, -2.2304, 2.4435 }, // Douglas-fir
        {  3, -2.5384, 2.4814 }, // true fir/hemlock
        {  4, -2.5356, 2.4349 }, // pine
        {  5, -2.0773, 2.3323 }, // spruce
        {  6, -2.2094, 2.3867 }, // aspen/alder/cottonwood/willow
        {  7, -1.9123, 2.3651 }, // soft maple/birch
        {  8, -2.4800, 2.4835 }, // mixed hardwood
        {  9, -2.0127, 2.4342 }, // hard maple/oak/hickory/beech
        { 10, -0.7152, 1.7029 }  // woodland (juniper/oak/mesquite)
    };

    // --- Determine species class & group code ---
    if (SPCD < 10) {
        // The SPCD is Jenkins group code (1..10)
        SPCLS = 1;                // default hardwood for groups >= 6
        if (SPCD < 6) SPCLS = 0;  // groups 1..5 are softwood
        if (SPCD == 0) SPCD = 10; // map 0 → 10 per Fortran
        SPGRPCD = SPCD;
    }
    else {
        spcdRefData = getRefSpeciesData(SPCD);
        SPCLS = spcdRefData.softHard;
        SPGRPCD = spcdRefData.jenkinsSpeciesGroupCD;
        WDEN = spcdRefData.WDSG;
        BDEN = spcdRefData.barkDen;
    }

    // --- DBH in inches & cm ---
    DBHIN = DBHOB;
    DBHCM = 2.54 * DBHIN;

    // For woodland species (group 10), DRC→DBH conversion (commented as in Fortran)
    // The conversion from DRC to DBH should be done before calling Jenkins
    // if (SPGRPCD == 10) DBHCM = std::exp(-0.35031 + 1.03991 * std::log(DBHCM));

    // --- Stump biomass from Raile volumes (only for DBH ≥ 5 in) ---
    STUMP = 0.0;
    if (DBHIN >= 5.0) {
        stumpVol = raileVol(SPCD, DBHIN, STUMPHT);
        // volumes in ft^3, densities in lb/ft^3 → mass in lb
        STUMP = stumpVol.woodVol * WDEN + stumpVol.barkVol * BDEN;
        // Convert lb → kg to sum with Jenkins components (which are computed in kg)
        STUMP /= KG2LB;
    }

    // --- Find B0,B1 for this group ---
    // (Fortran would leave B0/B1 uninitialized if not found; we choose group 10 fallback)
    bool found = false;
    for (const auto& row : COEF) {
        if (row.grp == SPGRPCD) {
            B0 = row.B0;
            B1 = row.B1;
            found = true;
            break;
        }
    }
    if (!found) {
        // Fallback to woodland group coefficients (group 10) if group code not matched
        B0 = COEF[9].B0;
        B1 = COEF[9].B1;
    }

    // --- Above-ground total biomass (kg), Jenkins: ABT = exp(B0 + B1*log(DBH_cm)) ---
    ABT = std::exp(B0 + B1 * std::log(DBHCM));

    // --- Component ratio coefficients by species class ---
    if (SPCLS == 0) {
        // Softwood
        A0F = -2.9584;  A1F = 4.4766;
        A0R = -1.5619;  A1R = 0.6614;
        A0B = -2.0980;  A1B = -1.1432;
        A0W = -0.3737;  A1W = -1.8055;
    }
    else if (SPCLS == 1) {
        // Hardwood
        A0F = -4.0813;  A1F = 5.8816;
        A0R = -1.6911;  A1R = 0.8160;
        A0B = -2.0129;  A1B = -1.6805;
        A0W = -0.3065;  A1W = -5.4240;
    }

    // --- Component ratios ---
    FOLRATIO = std::exp(A0F + A1F / DBHCM);
    ROTRATIO = std::exp(A0R + A1R / DBHCM);
    BKRATIO = std::exp(A0B + A1B / DBHCM);
    WDRATIO = std::exp(A0W + A1W / DBHCM);

    // --- Components in kg ---
    FOL = ABT * FOLRATIO;
    ROOT = ABT * ROTRATIO;

    if (DBHIN < 5.0) {
        BARK = 0.0;
        WOOD = 0.0;
        STMTOT = 0.0;
        BRANCHES = ABT - FOL;
        CROWN = ABT;
    }
    else {
        BARK = ABT * BKRATIO;
        WOOD = ABT * WDRATIO;
        STMTOT = WOOD + BARK;
        BRANCHES = ABT - STMTOT - FOL - STUMP;
        CROWN = ABT - STMTOT - STUMP;
    }

    // --- Convert to lb and populate BIOMS(8) ---
    // 1 ABOVE GROUND TOTAL
    // 2 STEM WOOD
    // 3 STEM BARK
    // 4 FOLIAGE
    // 5 ROOTS
    // 6 BRANCHES
    // 7 CROWN
    // 8 STEM WOOD AND BARK
    BIOMS[0] = ABT * KG2LB;
    BIOMS[1] = WOOD * KG2LB;
    BIOMS[2] = BARK * KG2LB;
    BIOMS[3] = FOL * KG2LB;
    BIOMS[4] = ROOT * KG2LB;
    BIOMS[5] = BRANCHES * KG2LB;
    BIOMS[6] = CROWN * KG2LB;
    BIOMS[7] = STMTOT * KG2LB;
    
    out.aboveGroundTotal = BIOMS[0] - BIOMS[3];
    out.branches = BIOMS[5];
    out.foliage = BIOMS[3];
    out.stemPrimaryWood = BIOMS[1];
    out.stemPrimaryBark = BIOMS[2];
    out.stemTopAndLimb = BIOMS[6];
    return out;
}
