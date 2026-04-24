#include <array>
#include <cmath>
#include <cstddef>
#include <string>
#include <algorithm>
#include "..\VolumeCalculators\DirectVolumeCalculator_R1.h"

// ---- Constants ported from FORTRAN DATA blocks ----

// BFVOL(species 1..15, table 1..5, coeff 1..4)
// Unspecified coefficients default to 0.0
static const double BFVOL[15][5][4] = {
    // Species 1..15 (AS, CW, GF, AF, WL, S, WBP, LP, WP, PP, DF, C, H, J, PY)
    // Table 1 (4 coeffs), other tables mostly empty except where defined.
    // 1: AS
    { { 1.197, -18.544, 1.216, -21.309 }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
    // 2: CW
    { { 1.046, -15.966, 1.140, -46.735 }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
    // 3: GF
    { { 1.293, -34.127, 1.218, 10.603  }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
    // 4: AF
    { { 1.011, -11.403, 0.694, 124.425 }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
    // 5: WL
    { { 0.997, -29.790, 0.841, 85.150  }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
    // 6: S
    { { 1.149, -11.851, 1.158, 1.620   }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
    // 7: WBP
    { { 1.208, -8.085,  1.103, 14.111  },
      { 0.0135436, -33.632, 0, 0 },            // Table 2
      { 1.1656,    -13.5219, 0, 0 },           // Table 3
      { 0,0,0,0 }, {0,0,0,0} },
      // 8: LP
      { { 1.208, -8.085,  1.103, 14.111  },
        { 0.0135436, -33.632, 0, 0 },            // Table 2
        { 1.1656,    -13.5219, 0, 0 },           // Table 3
        { 0,0,0,0 }, {0,0,0,0} },
        // 9: WP
        { { 1.189, -26.729, 1.181, -32.516 }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
        // 10: PP
        { { 1.201, -50.340, 1.595, -298.784 },
          { 1.6399,  -1.01249, -4.60157, 0 },      // Table 2
          { 1.8422,   1.11986, -2.78156, 0 },      // Table 3
          { 0.17148, -1.97703, 7.034, 0 },         // Table 4
          { 0.13412, -1.65764, 7.4384, 0 } },      // Table 5
          // 11: DF
          { { 1.003, -25.332, 1.011, -9.522  },
            { 0.848633, 0.000102364, -9.84419, 0 },  // Table 2
            { 0,0,0,0 }, {0,0,0,0}, {0,0,0,0} },
            // 12: C
            { { 0.878, -10.742, 0.799, -4.064  }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
            // 13: H
            { { 1.203, -37.314, 1.306, -50.680 }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
            // 14: J
            { { 1.208, -8.085,  1.103, 14.111  }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
            // 15: PY
            { { 0,0,0,0 }, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} },
};

// CBVOLE(species 1..15, coeff 1..11)
static const double CBVOLE[15][11] = {
    // 1: AS
    {0.3482, -0.0384, 0.001427, -0.842503, 0.224, -0.343, 0.217, 1.071, 0.0, 0.0, 0.0},
    // 2: CW
    {0.1064, -0.00778, 0.000176, -0.265342, 0.204, -0.749, 0.194, 4.285, 0.0, 0.0, 0.0},
    // 3: GF
    {0.3386, -0.03359, 0.001109, -0.918645, 0.219, -0.563, 0.197, 9.969, 0.2153, -0.00167, 0.50},
    // 4: AF
    {0.4529, -0.052,   0.002003, -1.113416, 0.183, 1.449,  0.117, 26.222, 0.2153,  0.00167, 0.67},
    // 5: WL
    {0.4172, -0.04693, 0.001782, -1.086592, 0.17, -0.056,  0.132, 19.409, 0.1922,  0.09023, 0.35},
    // 6: S
    {0.2619, -0.02345, 0.000671, -0.716502, 0.214, 0.48,   0.174, 19.041, 0.2306,  0.14528, 0.35},
    // 7: WB
    {0.6808, -0.07974, 0.003113, -1.692512, 0.221, 1.052,  0.197, 5.369,  0.2306,  0.14528, 0.35},
    // 8: LP
    {0.6808, -0.07974, 0.003113, -1.692512, 0.221, 1.052,  0.197, 5.369,  0.2306,  0.14528, 0.35},
    // 9: WP
    {0.4544, -0.05119, 0.001945, -1.14765,  0.206, 0.166,  0.194, 4.508,  0.2306,  0.14528, 0.35},
    // 10: PP
    {0.4041, -0.04535, 0.001726, -1.054732, 0.203, -1.656, 0.218, -9.637, 0.2306,  0.14528, 0.25},
    // 11: DF
    {0.5125, -0.05817, 0.002208, -1.320519, 0.178, 0.437,  0.165, 7.702,  0.1795,  0.16949, 0.47},
    // 12: C
    {0.3349, -0.03565, 0.001273, -0.851441, 0.174, 1.141,  0.146, 8.931,  0.1922,  0.09023, 0.67},
    // 13: H
    {0.2213, -0.01913, 0.000533, -0.635045, 0.209, -0.991, 0.210, 2.544,  0.2153, -0.00167, 0.43},
    // 14: J
    {0.0,     0.0,     0.0,      0.0,       0.211, -0.597, 0.211, -0.597, 0.0,     0.0,      0.0},
    // 15: PY
    {0.0,     0.0,     0.0,      0.0,       0.211, -0.597, 0.211, -0.597, 0.0,     0.0,      0.0},
};

// AMRLNE(species 1..13, coeff 1..3) for R1LOGS
static const double AMRLNE[13][3] = {
    {0.0,      0.0,       0.0},           // 1 AS
    {0.0,      0.0,       0.0},           // 2 CW
    {0.769102, 8.17961,  -39.81773},      // 3 GF
    {0.617455, 9.00450,  -33.62005},      // 4 AF
    {0.767457, 11.0658,  -53.23026},      // 5 WL
    {0.788744, 6.25054,  -36.74818},      // 6 S
    {0.722147, 17.8226,  -67.2513},       // 7 WBP
    {0.722147, 17.8226,  -67.2513},       // 8 LP
    {0.596947, 14.6879,  -49.13522},      // 9 WP
    {0.838089, 7.29928,  -41.14457},      // 10 PP
    {0.745249, 6.12528,  -31.79443},      // 11 DF
    {0.741795, 6.29675,  -33.11715},      // 12 C
    {0.716799, 10.5844,  -46.78366},      // 13 H
};

// Helper: map VOLEQ to species index (0..14) and table (JTAB = 1..5)
static bool mapSpeciesAndTable(const std::string& VOLEQ, int& ispec_idx, int& jtab) {
    if (VOLEQ.size() < 10) return false;
    const std::string s23 = VOLEQ.substr(1, 2);   // FORTRAN (2:3)
    const std::string s810 = VOLEQ.substr(7, 3);  // FORTRAN (8:10)

    if (s23 == "02") {
        if (s810 == "746") ispec_idx = 0;
        else if (s810 == "740") ispec_idx = 1;
        else if (s810 == "017") ispec_idx = 2;
        else if (s810 == "019") ispec_idx = 3;
        else if (s810 == "070" || s810 == "073") ispec_idx = 4;
        else if (s810 == "090" || s810 == "093") ispec_idx = 5;
        else if (s810 == "101") ispec_idx = 6;
        else if (s810 == "108") ispec_idx = 7;
        else if (s810 == "119") ispec_idx = 8;
        else if (s810 == "122") ispec_idx = 9;
        else if (s810 == "202") ispec_idx = 10;
        else if (s810 == "240" || s810 == "242") ispec_idx = 11;
        else if (s810 == "260" || s810 == "263") ispec_idx = 12;
        else if (s810 == "060") ispec_idx = 13;
        else if (s810 == "106") ispec_idx = 14;
        else return false;
        jtab = 1;
        return true;
    }
    else if (s23 == "03") {
        if (s810 == "101") ispec_idx = 6;
        else if (s810 == "108") ispec_idx = 7;
        else if (s810 == "122") ispec_idx = 9;
        else if (s810 == "202") ispec_idx = 10;
        else return false;
        jtab = 2;
        return true;
    }
    else if (s23 == "04") {
        if (s810 == "108") ispec_idx = 7;
        else if (s810 == "122") ispec_idx = 9;
        else return false;
        jtab = 3;
        return true;
    }
    else if (s23 == "05") {
        ispec_idx = 9;  // PP
        jtab = 4;
        return true;
    }
    else if (s23 == "06") {
        ispec_idx = 9;  // PP
        jtab = 5;
        return true;
    }
    return false;
}

// Number of 16.5' logs (English), R1LOGS translation
double R1LOGS(int ISPEC_1based, double DBHOB, double HTTOT, int KLASS, int IWHEN) {
    // MEASUR=0 (English) only
    double amerch = 0.0;
    if (ISPEC_1based >= 1 && ISPEC_1based <= 13) {
        // AS/CW zero
        if (ISPEC_1based == 1 || ISPEC_1based == 2) amerch = 0.0;
        else {
            const auto& c = AMRLNE[ISPEC_1based - 1];
            amerch = c[0] * HTTOT + c[1] * std::sqrt(DBHOB) + c[2];
        }
    }
    else {
        amerch = 0.0;
    }

    // Deduct for dead LP or dead WP before Jan 1, 1994
    if ((ISPEC_1based == 8 || (ISPEC_1based == 9 && IWHEN < 9400000)) && KLASS == 2) {
        amerch -= 16.5;
    }
    if (amerch < 0.0) amerch = 0.0;

    return amerch / 16.5;
}

TreeOutput R1KEMP(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree)
{
    TreeOutput out;
    out.errflag = 0;
    double HTTOT = tree.totalHeight;
    double DBHOB = tree.dbh;
    char LIVE = 'L';
    if(!tree.isLive) LIVE = 'D';
    int PROD = vco.primaryProduct;
    int IWHEN = 9500000;

    // Derived values
    const double D2H100 = (DBHOB * DBHOB) * HTTOT / 100.0;

    // Map species and table
    int ispec_idx = -1;
    int jtab = -1;
    if (!mapSpeciesAndTable(VOLEQ, ispec_idx, jtab)) {
        out.errflag = 1;
        return out;
    }
    const int ISPEC_1based = ispec_idx + 1;

    // KLASS determination
    int KLASS = 4;
    if (LIVE == 'D' && ISPEC_1based == 8) {
        KLASS = 2;
    }
    else if (LIVE == 'D') {
        KLASS = 1;
    }
    else if (PROD == 2) {
        KLASS = 3;
    }
    else {
        KLASS = 4;
    }

    // ---- Board foot gross (BFGRS) ----
    double BFGRS = 0.0;
    switch (jtab) {
    case 1: { // Table 01
        const auto& c = BFVOL[ispec_idx][0];
        if (DBHOB < 21.0) BFGRS = c[0] * D2H100 + c[1];
        else              BFGRS = c[2] * D2H100 + c[3];
        break;
    }
    case 2: { // Table 02
        if (ISPEC_1based == 8 || ISPEC_1based == 7) {
            const auto& c = BFVOL[ispec_idx][1];
            BFGRS = DBHOB * DBHOB * HTTOT * c[0] + c[1];
        }
        else if (ISPEC_1based == 10) {
            const auto& c = BFVOL[9][1];
            BFGRS = std::pow(10.0,
                c[0] * std::log10(DBHOB * DBHOB * HTTOT) +
                c[1] * std::log10(DBHOB) + c[2]) * 10.0;
        }
        else if (ISPEC_1based == 11) {
            const auto& c = BFVOL[10][1];
            BFGRS = c[0] * D2H100 + c[1] * D2H100 * D2H100 + c[2];
        }
        else {
            // Fall back to Table 01
            const auto& c = BFVOL[ispec_idx][0];
            if (DBHOB < 21.0) BFGRS = c[0] * D2H100 + c[1];
            else              BFGRS = c[2] * D2H100 + c[3];
        }
        break;
    }
    case 3: { // Table 03
        if (ISPEC_1based == 10) {
            const auto& c = BFVOL[9][2];
            BFGRS = std::pow(10.0,
                c[0] * std::log10(DBHOB) +
                c[1] * std::log10(HTTOT) + c[2]) * 10.0;
        }
        else if (ISPEC_1based == 8 || ISPEC_1based == 7) {
            const auto& c = BFVOL[ispec_idx][2];
            BFGRS = c[0] * D2H100 + c[1];
        }
        else {
            const auto& c = BFVOL[ispec_idx][0];
            if (DBHOB < 21.0) BFGRS = c[0] * D2H100 + c[1];
            else              BFGRS = c[2] * D2H100 + c[3];
        }
        break;
    }
    case 4: { // Table 04 (PP only)
        const auto& c = BFVOL[9][3];
        BFGRS = (DBHOB * DBHOB) * c[0] + DBHOB * c[1] + c[2];
        BFGRS *= 10.0;
        break;
    }
    case 5: { // Table 05 (PP only)
        const auto& c = BFVOL[9][4];
        BFGRS = (DBHOB * DBHOB) * c[0] + DBHOB * c[1] + c[2];
        BFGRS *= 10.0;
        break;
    }
    default:
        out.errflag = 1; // shouldn't happen
        return out;
    }

    // Deduct for dead LP or dead WP (WP before 1994)
    if (KLASS == 2 && (ISPEC_1based == 8 || (ISPEC_1based == 9 && IWHEN < 9400000))) {
        BFGRS -= 20.0;
    }
    // Minimums
    BFGRS = std::max(BFGRS, 10.0);
    if ((ISPEC_1based == 8 || (ISPEC_1based == 9 && IWHEN < 9400000)) &&
        KLASS == 2 && BFGRS < 30.0) {
        BFGRS = 30.0;
    }

    // ---- Cubic gross (CBGRS) ----
    double CBGRS = 0.0;
    if (ISPEC_1based == 14 || ISPEC_1based == 15) {
        if (DBHOB < 5.0) {
            CBGRS = 0.0;
        }
        else if (DBHOB <= 20.5) {
            CBGRS = CBVOLE[ispec_idx][4] * D2H100 + CBVOLE[ispec_idx][5];
        }
        else {
            CBGRS = CBVOLE[ispec_idx][6] * D2H100 + CBVOLE[ispec_idx][7];
        }
    }
    else {
        if (DBHOB < 5.0) {
            CBGRS = (CBVOLE[ispec_idx][8] * D2H100 + CBVOLE[ispec_idx][9]) *
                CBVOLE[ispec_idx][10];
        }
        else if (DBHOB <= 9.5) {
            CBGRS = D2H100 * (CBVOLE[ispec_idx][0] * DBHOB +
                CBVOLE[ispec_idx][1] * DBHOB * DBHOB +
                CBVOLE[ispec_idx][2] * DBHOB * DBHOB * DBHOB +
                CBVOLE[ispec_idx][3]);
        }
        else if (DBHOB <= 20.5) {
            CBGRS = CBVOLE[ispec_idx][4] * D2H100 + CBVOLE[ispec_idx][5];
        }
        else {
            CBGRS = CBVOLE[ispec_idx][6] * D2H100 + CBVOLE[ispec_idx][7];
        }
    }
    // Deduct for dead LP/WP before 1994
    if (KLASS == 2 && (ISPEC_1based == 8 || (ISPEC_1based == 9 && IWHEN < 9400000))) {
        CBGRS -= 3.8;
    }
    // Cubic minimums (gross)
    if (KLASS == 2 && (ISPEC_1based == 8 || (ISPEC_1based == 9 && IWHEN < 9400000))) {
        CBGRS = std::max(CBGRS, 4.3);
    }
    else if (KLASS <= 2) {
        CBGRS = std::max(CBGRS, 1.6);
    }
    else if (KLASS == 3) {
        CBGRS = std::max(CBGRS, 2.4);
    }
    else {
        CBGRS = std::max(CBGRS, 0.1);
    }

    // ---- Board foot net (BFNET) ----
    // Original FORTRAN defect logic commented out; BFNET starts at 0 then min is enforced.
    //double BFNET = 0.0;
    //BFNET = std::max(BFNET, 10.0);
    //if (ISPEC_1based == 8 && KLASS == 2 && BFNET < 30.0) {
    //    BFNET = 30.0;
    //}

    //// ---- Cubic sawlog (CBSAW) ----
    //if (NONSAW > 100) NONSAW = 100;
    //double CBSAW = CBGRS - (NONSAW / 100.0) * CBGRS;

    //// Sawlog minimums (net)
    //if (KLASS == 2 && (ISPEC_1based == 8 || (ISPEC_1based == 9 && IWHEN < 9400000))) {
    //    CBSAW = std::max(CBSAW, 4.3);
    //}
    //else if (KLASS <= 2) {
    //    CBSAW = std::max(CBSAW, 0.5);
    //}
    //else if (KLASS == 3) {
    //    CBSAW = std::max(CBSAW, 2.4);
    //}
    //else {
    //    CBSAW = std::max(CBSAW, 0.1);
    //}

    // ---- Number of logs ----
    if (ISPEC_1based < 14) {
        out.numberOfLogs = R1LOGS(ISPEC_1based, DBHOB, HTTOT, KLASS, IWHEN);
    }

    // ---- Write volumes to VOL array ----
    out.totalCubicFoot = CBGRS;  // VOL(1)
    out.grossBoardFootPrimary = BFGRS;  // VOL(2)
    out.grossCubicFootPrimary = CBGRS;  // VOL(4) (same as gross cubic)
    
    // VOL(6..15) remain 0.0 as in original

    return out;
}

//-------------------------------------------------------------------------
//R1Allen.f

// =========================
// Utility: Behré hyperbola integral function (ratio usage)
// =========================
static inline double bhre(double L1, double L2, double AHAT, double BHAT) {
    const double ALB1 = AHAT * L1 + BHAT;
    const double ALB2 = AHAT * L2 + BHAT;
    // Note: off by factor pi/(a^3), but cancels out in ratios (as per FORTRAN comment)
    return (ALB2 - ALB1)
        - 2.0 * BHAT * (std::log(ALB2) - std::log(ALB1))
        - (BHAT * BHAT) / ALB2
        + (BHAT * BHAT) / ALB1;
}

// =========================
// Coefficients and tables
// =========================

// Bark ratio lookup (ISPC 1..11)
static const double BRATIO11[11] = {
    0.964, 0.851, 0.867, 0.915, 0.934, 0.950, 0.969, 0.956, 0.937, 0.890, 0.934
};

// For R1ALLENB (board foot)
static const double D2HBRK[11] = {
    16917.1, 19231.8, 15763.5, 10016.7, 14883.9, 29922.0,
     4567.8,  6024.9, 19425.9, 14034.0, 14883.9
};

static std::array<double, 100> build_HDRATM() {
    std::array<double, 100> H{};
    // Per FORTRAN DATA:
    // 10.0, 10.0, 9.0, 8.0, 7.8, 7.65, 7.5, 7.25, 7.0, 6.75, 6.5, 6.25,
    // 18*6.0, 10*5.5, 10*5.0, 10*4.5, 10*4.0, 10*3.5, 10*3.0, 10*2.5
    double init[] = { 10.0,10.0,9.0,8.0,7.8,7.65,7.5,7.25,7.0,6.75,6.5,6.25 };
    std::size_t k = 0;
    for (double v : init) { H[k++] = v; }
    for (int i = 0; i < 18; i++) H[k++] = 6.0;
    for (int i = 0; i < 10; i++) H[k++] = 5.5;
    for (int i = 0; i < 10; i++) H[k++] = 5.0;
    for (int i = 0; i < 10; i++) H[k++] = 4.5;
    for (int i = 0; i < 10; i++) H[k++] = 4.0;
    for (int i = 0; i < 10; i++) H[k++] = 3.5;
    for (int i = 0; i < 10; i++) H[k++] = 3.0;
    for (int i = 0; i < 10; i++) H[k++] = 2.5;
    return H;
}
static const std::array<double, 100> HDRATM = build_HDRATM();

static const double COFBVS[11] = {
    0.01031,0.008423,0.008423,0.009523,0.009523,0.008421,
    0.01031,0.009523,0.009523,0.008423,0.009523
};

// BFVEQS[species 1..11][coeff 1..7]
static const double BFVEQS[11][7] = {
    {-26.729, 0.0, 0.0, 0.01189, 0.0, 0.0, 0.0},
    {-29.790, 0.0, 0.0, 0.00997, 0.0, 0.0, 0.0},
    {-25.332, 0.0, 0.0, 0.01003, 0.0, 0.0, 0.0},
    {-34.127, 0.0, 0.0, 0.01293, 0.0, 0.0, 0.0},
    {-37.314, 0.0, 0.0, 0.01203, 0.0, 0.0, 0.0},
    {-10.742, 0.0, 0.0, 0.00878, 0.0, 0.0, 0.0},
    { -8.085, 0.0, 0.0, 0.01208, 0.0, 0.0, 0.0},
    {-11.851, 0.0, 0.0, 0.01149, 0.0, 0.0, 0.0},
    {-11.403, 0.0, 0.0, 0.01011, 0.0, 0.0, 0.0},
    {-50.340, 0.0, 0.0, 0.01201, 0.0, 0.0, 0.0},
    {-37.314, 0.0, 0.0, 0.01203, 0.0, 0.0, 0.0}
};

// BFVEQL[species 1..11][coeff 1..7]
static const double BFVEQL[11][7] = {
    {-32.516, 0.0, 0.0, 0.01181, 0.0, 0.0, 0.0},
    { 85.150, 0.0, 0.0, 0.00841, 0.0, 0.0, 0.0},
    { -9.522, 0.0, 0.0, 0.01011, 0.0, 0.0, 0.0},
    { 10.603, 0.0, 0.0, 0.01218, 0.0, 0.0, 0.0},
    {-50.680, 0.0, 0.0, 0.01306, 0.0, 0.0, 0.0},
    { -4.064, 0.0, 0.0, 0.00799, 0.0, 0.0, 0.0},
    { 14.111, 0.0, 0.0, 0.01103, 0.0, 0.0, 0.0},
    {  1.620, 0.0, 0.0, 0.01158, 0.0, 0.0, 0.0},
    {124.425, 0.0, 0.0, 0.00694, 0.0, 0.0, 0.0},
    {-298.784,0.0, 0.0, 0.01595, 0.0, 0.0, 0.0},
    {-50.680, 0.0, 0.0, 0.01306, 0.0, 0.0, 0.0}
};

static const int IBTRAN[11] = { 0,0,0,0,0,0,0,0,0,0,0 };
static const double BTRAN[11] = {
    20.5,20.5,20.5,20.5,20.5,20.5,20.5,20.5,20.5,20.5,20.5
};

// For TOTVOL (cubic total)
static const double CFVEQS[11][7] = {
    {0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0},
    {0,0,0,0,0,0,0},
    {0.030288,0.0,0.0,0.002213,0.0,0.0,0.0},
    {0,0,0,0,0,0,0}
};

static const double CFVEQL[11][7] = {
    {0,0,0,0.00233,0,0,0},
    {0,0,0,0.00184,0,0,0},
    {0,0,0.003865,0.001714,0,0,0},
    {0,0,0,0.00234,0,0,0},
    {0,0,0,0.00219,0,0,0},
    {0,0,0,0.00205,0,0,0},
    {0,0,0,0.0,0.002782,1.9041,1.0488}, // species 7 special exponents
    {0,0,0.003865,0.001714,0,0,0},
    {0,0,0.003865,0.001714,0,0,0},
    {-1.557103,0,0,0.002474,0,0,0},
    {0,0,0,0.00219,0,0,0}
};

static const int ICTRAN[11] = { 0,0,0,0,0,0,0,0,0,1,0 };
static const double CTRAN[11] = { 0,0,0,0,0,0,0,0,0,6000.0,0 };

// =========================
// TOTVOL function (cubic total volume)
// =========================
static inline double TOTVOL(int ISPC_1based, double DBHOB, double HTTOT, double D2H) {
    // species index 1..11 only
    if (ISPC_1based < 1 || ISPC_1based > 11) return 0.0;
    int idx = ISPC_1based - 1;

    double TSIZE = DBHOB;
    if (ICTRAN[idx] > 0) TSIZE = DBHOB * DBHOB * HTTOT;

    double TERM1 = 0.0;
    if (TSIZE < CTRAN[idx]) {
        if (CFVEQS[idx][4] > 0.0) {
            TERM1 = CFVEQS[idx][4] * std::pow(DBHOB, CFVEQS[idx][5]) *
                std::pow(HTTOT, CFVEQS[idx][6]);
        }
        return CFVEQS[idx][0]
            + CFVEQS[idx][1] * DBHOB
            + CFVEQS[idx][2] * DBHOB * HTTOT
            + CFVEQS[idx][3] * D2H
            + TERM1;
    }
    else {
        if (CFVEQL[idx][4] > 0.0) {
            TERM1 = CFVEQL[idx][4] * std::pow(DBHOB, CFVEQL[idx][5]) *
                std::pow(HTTOT, CFVEQL[idx][6]);
        }
        return CFVEQL[idx][0]
            + CFVEQL[idx][1] * DBHOB
            + CFVEQL[idx][2] * DBHOB * HTTOT
            + CFVEQL[idx][3] * D2H
            + TERM1;
    }
}

// =========================
// Helpers: VOLEQ → species index (R1ALLENC/R1ALLENB)
// =========================
static inline bool map_ispc_R1ALLENC(const std::string& VOLEQ, int& ISPC) {
    if (VOLEQ.size() < 10) return false;
    const std::string code = VOLEQ.substr(7, 3); // FORTRAN (8:10)
    if (code == "119") ISPC = 1;
    else if (code == "073") ISPC = 2;
    else if (code == "202") ISPC = 3;
    else if (code == "017") ISPC = 4;
    else if (code == "263" || code == "260") ISPC = 5;
    else if (code == "242" || code == "240") ISPC = 6;
    else if (code == "108") ISPC = 7;
    else if (code == "093" || code == "090") ISPC = 8;
    else if (code == "019") ISPC = 9;
    else if (code == "122") ISPC = 10;
    else if (code == "999") ISPC = 11;
    else if (code == "375") ISPC = 12; // Paper birch special case
    else if (code == "740") ISPC = 13; // Cottonwood special case
    else return false;
    return true;
}

//static inline bool map_ispc_R1ALLENB(const std::string& VOLEQ, int& ISPC, bool& early_return_zero) {
//    if (VOLEQ.size() < 10) return false;
//    const std::string code = VOLEQ.substr(7, 3); // (8:10)
//    early_return_zero = false;
//    if (code == "119") ISPC = 1;
//    else if (code == "073") ISPC = 2;
//    else if (code == "202") ISPC = 3;
//    else if (code == "017") ISPC = 4;
//    else if (code == "263" || code == "260") ISPC = 5;
//    else if (code == "242" || code == "240") ISPC = 6;
//    else if (code == "108") ISPC = 7;
//    else if (code == "093" || code == "090") ISPC = 8;
//    else if (code == "019") ISPC = 9;
//    else if (code == "122") ISPC = 10;
//    else if (code == "999") ISPC = 11;
//    else if (code == "375") { early_return_zero = true; return true; }
//    else if (code == "740") { early_return_zero = true; return true; }
//    else return false;
//    return true;
//}

// =========================
// R1ALLENC: cubic volumes (merchantable and total)
// =========================
TreeOutput R1ALLENC(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{   // out
    // Initialization
    TreeOutput out;
    double DBHOB = tree.dbh;
    double HTTOT = tree.totalHeight;
    double TOPD = merchRules.minTopDibSaw;
    if (tree.minTopDibSawOverride > 0.0) TOPD = tree.minTopDibSawOverride;
    //reset TOPD for nonSaw prod
    if (vco.primaryProduct != 1)
    {
        TOPD = merchRules.minTopDibNonSaw;
        if (tree.minTopDibNonSawOverride > 0.0) TOPD = tree.minTopDibNonSawOverride;
    }
    double BTR = merchRules.barkThicknessRatio;
    double CFSTMP = merchRules.stumpHeight;
    if (tree.stumpHeightOverride > 0.0) CFSTMP = tree.stumpHeightOverride;

    double CUVOL = 0.0;
    double TCVOL = 0.0;
    bool TKILL = false;
    int ITHT = 0;
    int ERRFLAG = 0;

    int ISPC = 0;
    if (!map_ispc_R1ALLENC(VOLEQ, ISPC)) {
        out.errflag = 1;
        out.totalCubicFoot = 0.0;
        out.grossCubicFootPrimary = 0.0;
        return out;
    }

    const double D2H = DBHOB * DBHOB * HTTOT;

    // Paper birch (ISPC=12) – North Central Station equation (7/2001)
    if (ISPC == 12) {
        if (DBHOB < 5.0) {
            CUVOL = 0.0;
        }
        else if (DBHOB >= 5.0 && DBHOB < 11.0) {
            CUVOL = 0.988264 + 0.002732 * D2H;
        }
        else { // DBHOB >= 11.0
            CUVOL = 2.512836 + 0.002446 * D2H;
        }
        TCVOL = CUVOL;
        out.totalCubicFoot = TCVOL;
        out.grossCubicFootPrimary = CUVOL;
        return out;
    }
    // Cottonwood (ISPC=13) – Edminster RN RM-351 (7/2001)
    if (ISPC == 13) {
        CUVOL = 0.00142526 * std::pow(D2H, 1.0636);
        TCVOL = CUVOL;
        out.totalCubicFoot = TCVOL;
        out.grossCubicFootPrimary = CUVOL;
        return out;
    }

    // All other species
    const double DBHMIN = 6.0;
    const double BARK = (BTR > 0.0) ? BTR : BRATIO11[ISPC - 1];
    const double VMAX = TOTVOL(ISPC, DBHOB, HTTOT, D2H);

    if (VMAX < 0.0) {
        CUVOL = 0.0;
        TCVOL = 0.0;
        out.totalCubicFoot = 0.0;
        out.grossCubicFootPrimary = 0.0;
        return out;
    }

    // Initialize total cubic volume
    TCVOL = VMAX;

    // Behré hyperbola parameter estimation
    double BHAT = VMAX / (0.00545415 * DBHOB * DBHOB * BARK * BARK * HTTOT);
    if (BHAT > 0.95) BHAT = 0.95;
    double AHAT = 0.44277 - 0.99167 / BHAT - 1.43237 * std::log(BHAT)
        + 1.68581 * std::sqrt(BHAT) - 0.13611 * BHAT * BHAT;
    bool LCONE = false;
    if (std::fabs(AHAT) < 0.05) {
        LCONE = true;
        AHAT = (AHAT < 0.0) ? -0.05 : 0.05;
    }
    BHAT = 1.0 - AHAT;
    if (BHAT < 0.0001) BHAT = 0.0001;
    const double VOLT = bhre(0.0, 1.0, AHAT, BHAT);

    // Topkill block present but disabled by TKILL=false (as in FORTRAN)
    double HTRUNC = 0.0, PHT = 0.0, DTRUNC = 0.0; // unused unless TKILL
    if (TKILL) {
        HTRUNC = ITHT / 100.0;
        PHT = 1.0 - (HTRUNC / HTTOT);
        DTRUNC = PHT / (AHAT * PHT + BHAT);
        // If needed, VOLTK etc. could be computed here (inactive).
    }

    // Merchantable cubic volume (correct for stump height & top diameter)
    const double STUMP = 1.0 - (CFSTMP / HTTOT);

    if (DBHOB < DBHMIN || DBHOB < TOPD) {
        CUVOL = 0.0;
        out.totalCubicFoot = TCVOL;
        out.grossCubicFootPrimary = CUVOL;
        return out;
    }

    const double DMRCH = TOPD / DBHOB;
    const double HTMRCH = (BHAT * DMRCH) / (1.0 - (AHAT * DMRCH));

    if (!LCONE) {
        const double VOLM = bhre(HTMRCH, STUMP, AHAT, BHAT);
        CUVOL = VMAX * (VOLM / VOLT);

        if (TKILL && DTRUNC > DMRCH) {
            // VOLTK = bhre(PHT, STUMP, AHAT, BHAT);
            // VM = VMAX * VOLTK / VOLT; // inactive
        }
    }
    else {
        // Conical case
        const double S3 = STUMP * STUMP * STUMP;
        const double VOLM = S3 - HTMRCH * HTMRCH * HTMRCH;
        CUVOL = VMAX * VOLM;

        if (TKILL && DTRUNC > DMRCH) {
            const double VOLTK = S3 - PHT * PHT * PHT;
            CUVOL = CUVOL * (VOLTK / VOLM);
        }
    }
    out.totalCubicFoot = TCVOL;
    out.grossCubicFootPrimary = CUVOL;
    return out;
}

// =========================
// R1ALLENB: board foot volume (merchantable)
// =========================
double R1ALLENB(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{   // out
    // Initialize
    double DBHOB = tree.dbh;
    double HTTOT = tree.totalHeight;
    double TOPD = merchRules.minTopDibSaw;
    if (tree.minTopDibSawOverride > 0.0) TOPD = tree.minTopDibSawOverride;
    //reset TOPD for nonSaw prod
    if (vco.primaryProduct != 1)
    {
        TOPD = merchRules.minTopDibNonSaw;
        if (tree.minTopDibNonSawOverride > 0.0) TOPD = tree.minTopDibNonSawOverride;
    }
    double BTR = merchRules.barkThicknessRatio;
    double BFSTMP = merchRules.stumpHeight;
    if (tree.stumpHeightOverride > 0.0) BFSTMP = tree.stumpHeightOverride;

    int ITHT = 0;
    bool TKILL = false;
    double BFVOL = 0.0;
    int ERRFLAG = 0;

    int ISPC = 0;
    bool early_zero = false;
    if (!map_ispc_R1ALLENC(VOLEQ, ISPC)) {
        ERRFLAG = 1;
        BFVOL = 0.0;
        return BFVOL;
    }
    if (ISPC == 12 || ISPC == 13) {
        BFVOL = 0.0;
        return BFVOL;
    }

    const double D2H = DBHOB * DBHOB * HTTOT;
    const double BARK = (BTR > 0.0) ? BTR : BRATIO11[ISPC - 1];

    // Transition size variable
    double TSIZE = DBHOB;
    if (IBTRAN[ISPC - 1] > 0) TSIZE = D2H;

    // Top volume VT; bypass Allen equations if TOPD == 8"
    double VT = 0.0;
    if (TOPD != 8.0) {
        const double HDRATA = HTTOT / DBHOB;
        int ID = static_cast<int>(DBHOB - 0.5);
        if (ID < 1) ID = 1;
        if (ID > 100) ID = 100;

        if (TOPD < 1.0) TOPD = 6.0; // avoid ITD=0
        int ITD = static_cast<int>(TOPD - 0.5);
        if (ITD < 1) ITD = 1;
        if (ITD > 100) ITD = 100;

        if (D2H <= D2HBRK[ISPC - 1]) {
            // Allen-Adams-Prausa for D2H below breakpoint
            VT = -COFBVS[ISPC - 1] * (std::pow(TOPD, 3) * HDRATM[ITD - 1] - 80.0)
                * std::sqrt(HDRATA / HDRATM[ID - 1])
                - (TOPD * TOPD - 4.0) * 0.12153;
            BFVOL = COFBVS[ISPC - 1] * D2H + VT;
        }
        else {
            // D2H above breakpoint: Allen for top, Kemp for stem
            double DTOPK = 0.4 * DBHOB;
            DTOPK = std::max(4.0, std::min(DTOPK, 8.0));
            int IVTD = static_cast<int>(DTOPK - 0.5);
            if (IVTD < 1) IVTD = 1;
            if (IVTD > 100) IVTD = 100;

            VT = COFBVS[ISPC - 1] * std::sqrt(HDRATA / HDRATM[ID - 1]) *
                (std::pow(DTOPK, 3) * HDRATM[IVTD - 1] - std::pow(TOPD, 3) * HDRATM[ITD - 1])
                - (DTOPK * DTOPK - TOPD * TOPD) * 0.12153;

            // Stem via BFVEQS/BFVEQL below (fallthrough)
            BFVOL = VT; // start from VT, then add stem
        }
    }

    // Stem volume via KEMP's equations: small vs large transition
    const int si = ISPC - 1;
    if (TSIZE < BTRAN[si]) {
        BFVOL += BFVEQS[si][0]
            + BFVEQS[si][1] * DBHOB
            + BFVEQS[si][2] * DBHOB * HTTOT
            + BFVEQS[si][3] * D2H
            + BFVEQS[si][4] * std::pow(DBHOB, BFVEQS[si][5]) * std::pow(HTTOT, BFVEQS[si][6]);
    }
    else {
        BFVOL += BFVEQL[si][0]
            + BFVEQL[si][1] * DBHOB
            + BFVEQL[si][2] * DBHOB * HTTOT
            + BFVEQL[si][3] * D2H
            + BFVEQL[si][4] * std::pow(DBHOB, BFVEQL[si][5]) * std::pow(HTTOT, BFVEQL[si][6]);
    }

    // Minimum 10 BF
    if (BFVOL < 10.0) BFVOL = 10.0;

    // Topkill correction block (inactive unless TKILL=true)
    if (TKILL) {
        const double VMAX = TOTVOL(ISPC, DBHOB, HTTOT, D2H);
        double BHAT = VMAX / (0.00545415 * DBHOB * DBHOB * BARK * BARK * HTTOT);
        if (BHAT > 0.95) BHAT = 0.95;

        double AHAT = 0.44277 - 0.99167 / BHAT - 1.43237 * std::log(BHAT)
            + 1.68581 * std::sqrt(BHAT) - 0.13611 * BHAT * BHAT;
        bool LCONE = false;
        if (std::fabs(AHAT) < 0.05) {
            LCONE = true;
            AHAT = (AHAT < 0.0) ? -0.05 : 0.05;
        }
        BHAT = 1.0 - AHAT;
        if (BHAT < 0.0001) BHAT = 0.0001;

        const double HTRUNC = ITHT / 100.0;
        const double PHT = 1.0 - (HTRUNC / HTTOT);
        const double DTRUNC = PHT / (AHAT * PHT + BHAT);

        if (DTRUNC > TOPD / DBHOB) {
            double VOLTK = bhre(PHT, 1.0, AHAT, BHAT);
            const double HTMRCH = ((BHAT * TOPD) / DBHOB) / (1.0 - (AHAT * TOPD / DBHOB));
            const double STUMP = 1.0 - BFSTMP / HTTOT;
            if (LCONE) {
                double VOLM = std::pow(STUMP, 3) - std::pow(HTMRCH, 3);
                VOLTK = std::pow(STUMP, 3) - std::pow(PHT, 3);
                BFVOL = BFVOL * (VOLTK / VOLM);
            }
            else {
                BFVOL = (BFVOL * VOLTK) / bhre(HTMRCH, STUMP, AHAT, BHAT);
            }
        }
    }
    return BFVOL;
}
