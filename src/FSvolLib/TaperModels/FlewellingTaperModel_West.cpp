#include "FlewellingTaperModel_West.h"
#include <string_view>
#include <array>
#include <cmath>
#include<algorithm>

// given Diameter at breat height, outside bark
// calculate double bark thickness at breast height.

//     DBHOB        INPUT    Diameter(o.b.) at Breast height(inches)
//     HTTOT        INPUT    Total tree height(ft)
//     FDBT_C1   RETURNED    Double bark thickness

double FDBT_C1(int JSP, std::string_view GEOSUB, double DBHOB, double HTTOT)
{
    static constexpr std::array<double, 8> ROFF =
    { 0.0, 0.117, 0.121, 0.133, 0.025, 0.028, 0.088, 0.028 };

    static constexpr std::array<double, 8> ROFF1 =
    { 0.0, 0.118, 0.204, 0.105, 0.058, 0.061, 0.118, 0.071 };

    double ratio = 0.0;
    int id = find_geo_index(GEOSUB);
    double logd = std::log(DBHOB); // reused

    switch (JSP)
    {
    case 3:
    {
        double DMEDIAN =
            0.566 * std::pow(HTTOT - 4.5, 0.634 + 0.00074 * HTTOT);

        double DFORM = DBHOB / DMEDIAN - 1.0;

        if (id < 0)   // GEOSUB = "00"
        {
            ratio = std::exp(
                -2.4641 +
                0.04393 * logd -
                0.2922 * DFORM +
                0.05964 * DFORM * logd
            );
        }
        else
        {
            ratio = std::exp(
                -2.5087 +
                0.03600 * logd -
                0.4086 * DFORM +
                0.10120 * DFORM * logd +
                ROFF[id]
            );
        }
        return ratio * DBHOB;
    }

    case 4:
    {
        if (id < 0)
        {
            ratio = 0.04504 * (1.0 + 0.8307 * std::exp(-0.2048 * DBHOB));
        }
        else
        {
            ratio =
                0.04221 * (1.0 + 0.8836 * std::exp(-0.2145 * DBHOB))
                * (1.0 + ROFF1[id]);
        }
        return ratio * DBHOB;
    }

    case 5:
    {
        double D_USE = std::max(DBHOB, 3.8);
        ratio = 0.01949 *
            (1.0 + 15.599 / D_USE - 29.212 / (D_USE * D_USE));

        return ratio * DBHOB;
    }
    }

    // Unknown species
    return 0.0;
}


//Douglas-fir
//Stem form  SHAPE paraemeters for Doug Fir (English units)
FlewellingShapeParams SHP_W3(double DBHOB, double HTTOT, std::string_view geoSub)
{
    // Region modifiers
    static const std::array<double, 8> R25 = {
        -2.0262, -1.7945, -2.0366, -2.0811,
        -1.9868, -2.0151, -1.9475, -2.0151
    };
    static const std::array<double, 8> R34 = {
         5.2132,  5.1417,  5.1768,  5.1156,
         5.1654,  5.2413,  5.1427,  5.2413
    };

    // Internal coefficient array F(50)
    static const double F[47] = {
        /* 1–12 unused */ 0,0,0,0,0,0,0,0,0,0,0,0,

        // F(13)–F(15)
        -0.07799267, 0.8096211, -0.9247244,
        /* 16 unused */ 0,

        // F(17)–F(19)
        0.166749, -9.20884, 0.1212094,
        /* 20 unused */ 0,

        // F(21)–F(24)
        5.0719922, -0.12555733, -1.6408313, -0.3005388,

        // F(25)–F(26)
        -2.000088, -0.410677,
        /* 27–28 unused */ 0,0,

        // F(29)–F(31)
        -1.428671, -0.8660924, 0.0543758,
        /* 32 unused */ 0,

        // F(33)–F(37)
        0.80981083, 5.1785820, 1.8935219, -2.420031, -0.009232401,

        // F(38)–F(40)
        -11.49305, 2.037438, -0.0114178,

        // F(41 unused)
        0,

        // F(42)–F(44)
        2.246288, -1.1410822, 1.7293166,

        // F(45)–F(47)
        -3.18221, -2.119838, 1.3756086

    };

    //------------------------------------------------------------------
    // Determine region‑specific overrides
    //------------------------------------------------------------------
    double FR25 = F[24];  // F(25)
    double FR34 = F[33];  // F(34)

    if (geoSub != "00") {
        int i = find_geo_index(geoSub);
        FR25 = R25[i];
        FR34 = R34[i];
    }

    //------------------------------------------------------------------
    // Main calculation
    //------------------------------------------------------------------
    const double BH = 4.5;

    const double m1 = 0.566;
    const double m2 = 0.634;
    const double m3 = 0.00074;

    const double DMEDIAN =
        m1 * std::pow(HTTOT - BH, m2 + m3 * HTTOT);

    const double DFORM = DBHOB / DMEDIAN - 1.0;

    //------------------------------------------------------------------
    // Compute U parameters
    //------------------------------------------------------------------
    auto clamp = [](double x, double lo, double hi) {
        return std::max(lo, std::min(x, hi));
        };

    double U7 =
        F[12] + F[13] * std::log(DBHOB + 1.0) + F[14] * std::log(HTTOT);
    U7 = clamp(U7, -7.0, 7.0);

    double U9A = F[17] + F[18] * HTTOT;
    U9A = clamp(U9A, -7.0, 7.0);

    const double U9 =
        F[16] * std::exp(U9A) / (1.0 + std::exp(U9A));

    double U8 =
        F[20] + F[21] * std::log(HTTOT) +
        F[22] * DFORM +
        F[23] * std::pow(DBHOB / 10.0, 1.5);
    U8 = clamp(U8, -7.0, 7.0);

    double U1 = FR25 + F[25] * std::log(HTTOT);
    double U2 = F[28] + F[29] * std::log(DBHOB) + F[30] * DBHOB;
    double U3 =
        FR34 +
        F[34] * std::log(DBHOB + 1.0) +
        F[35] * std::log(HTTOT) +
        F[36] * DFORM * HTTOT +
        F[32] * DFORM;
    double U4 = F[37] + F[38] * DBHOB + F[39] * HTTOT * DBHOB;
    double U5 = F[41] + F[42] * HTTOT + F[43] * DFORM;

    U1 = clamp(U1, -7.0, 7.0);
    U2 = clamp(U2, -7.0, 7.0);
    U3 = clamp(U3, -7.0, 7.0);
    U4 = clamp(U4, -7.0, 7.0);
    U5 = clamp(U5, -7.0, 7.0);

    double U6 =
        F[44] +
        F[45] * std::log(DBHOB + 1.0) +
        F[46] * std::log(HTTOT);
    U6 = clamp(U6, -6.0, 6.0);
    U6 = 1.0 + std::exp(U6);
    U6 = clamp(U6, 1.005, 100.0);

    //------------------------------------------------------------------
    // Geometric parameters
    //------------------------------------------------------------------
    auto logistic = [](double u) {
        return std::exp(u) / (1.0 + std::exp(u));
        };

    const double R1 = logistic(U1);
    const double R2 = logistic(U2);
    const double R3 = logistic(U3);
    const double R4 = logistic(U4);
    const double R5 = 0.5 + 0.5 * logistic(U5);

    const double RHI1 = std::min(0.5, logistic(U7));
    const double RHLONGI = U9;
    const double RHI2 = RHI1 + RHLONGI;

    const double RHC =
        RHI2 + (1.0 - RHI2) * logistic(U8);

    //------------------------------------------------------------------
    // Fill outputs
    //------------------------------------------------------------------
    FlewellingShapeParams out = { R1, R2, R3, R4, R5, U6, RHI1, RHI2, RHC, RHLONGI };

    return out;
}

//Western hemlock
//Stem form  SHAPE paraemeters for Western Hemlock (English units)
FlewellingShapeParams SHP_W4(double DBHOB, double HTTOT, std::string_view geoSub)
{
    static const std::array<double, 8> R25 = {
        -7.511,  -7.687,  -7.224,  -7.355,
        -7.632,  -7.646,  -7.687,  -7.911
    };
    static const std::array<double, 8> R34 = {
        -1.215, -1.355, -1.177, -1.373,
        -1.398, -1.188, -1.355, -1.449
    };

    //------------------------------------------------------------------
    // FORTRAN F-coefficients mapped directly into a C++ array.
    // Only elements actually referenced in SHP_W4 are included here.
    //------------------------------------------------------------------
    static const double F[48] = {
        //*1-12 unused*/
        0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,

        // F(13)-F(15)
        -3.1137977e+00, 1.1996084e+00, -1.1959010e-02,
        //*F(16) unused*/
        0.0,

        // F(17)-F(19)
        -6.6829984e-02, 2.6990398e-02, 2.4661021e-01,
        //*F(20) unused*/
        0.0,

        // F(21)-F(24)
        -8.3415555e+00,  2.4274384e+00, -5.6918023e+00, 5.6487213e-01,
        // F(25)-F(28)
        -7.6464554e+00,  5.1709049e+00, -2.7133381e+00, 3.8918349e-01,
        // F(29)
        -7.0,

        //*F(30-33 unused*/
        0.0,0.0,0.0,0.0,

        // F(34)-F(37)
        -1.2882571e+00,  3.5688410e+01,  1.7995769e-01,  1.5565605e+00,
        // F(38)-F(40)
        6.4397446e+00, -1.3439736e+00,  6.3442558e+00,
        // F(41 unused*/
        0.0,

        // F(42)-F(44)
        1.1898092e+01, -3.6789851e+00,  1.5168209e-01,
        // F(45)-F(47)
        -1.3248733e+00, -1.1788962e-01, -1.5909154e-02,
        0.0
    };

    //------------------------------------------------------------------
    // Find region modifiers FR25 and FR34
    //------------------------------------------------------------------
    double FR25 = F[24]; // F(25)
    double FR34 = F[33]; // F(34)

    if (geoSub != "00") {
        int i = find_geo_index(geoSub);
        FR25 = R25[i];
        FR34 = R34[i];
    }

    //------------------------------------------------------------------
    // Median D and DFORM
    //------------------------------------------------------------------
    const double BH = 4.5;

    const double DMEDIAN =
        0.2855 * std::pow(HTTOT - BH,
            0.307
            - 0.00505 * HTTOT
            + 0.00001745 * HTTOT * HTTOT
            + 0.19 * std::log(HTTOT));

    const double DFORM = DBHOB / DMEDIAN - 1.0;

    //------------------------------------------------------------------
    // Utility lambdas
    //------------------------------------------------------------------
    auto clamp = [](double x, double lo, double hi) {
        return std::max(lo, std::min(x, hi));
        };
    auto logistic = [](double u) {
        return std::exp(u) / (1.0 + std::exp(u));
        };

    //------------------------------------------------------------------
    // Compute U7 (RHI1)
    //------------------------------------------------------------------
    double U7 = F[12] + F[13] * (1.0 - std::exp(F[14] * HTTOT));
    U7 = clamp(U7, -7.0, 7.0);

    double RHI1 = logistic(U7);
    if (RHI1 > 0.5) RHI1 = 0.5;

    //------------------------------------------------------------------
    // Compute U9 (RHLONGI)
    //------------------------------------------------------------------
    double U9A = F[16] + F[17] * std::log(HTTOT) + F[18] * DFORM;

    // Enforce upper bound on RHI1 + U9A
    if (RHI1 + U9A > 0.75)
        U9A = 0.75 - RHI1;

    double RHLONGI = std::max(0.0, U9A);

    //------------------------------------------------------------------
    // Compute U8 (RHC core)
    //------------------------------------------------------------------
    double U8 =
        F[20]
        + F[21] * std::log(HTTOT)
        + F[22] * DFORM
        + F[23] * std::log(HTTOT) * DFORM;

    U8 = clamp(U8, -7.0, 7.0);

    //------------------------------------------------------------------
    // Compute U1-U5
    //------------------------------------------------------------------
    const double H100 = HTTOT / 100.0;

    double U1 =
        FR25 +
        F[25] / H100 +
        F[26] / (H100 * H100) +
        F[27] / (H100 * H100 * H100);

    double U2 = F[28];

    double U3 =
        FR34 +
        F[34] / HTTOT +
        F[35] * HTTOT * DBHOB / 1000.0 +
        F[36] * DFORM;

    double U4 =
        F[37] +
        F[38] * std::log(HTTOT) +
        F[39] * DFORM;

    double U5 =
        F[41] +
        F[42] * std::log(HTTOT) +
        F[43] * DBHOB;

    U1 = clamp(U1, -7.0, 7.0);
    U2 = clamp(U2, -7.0, 7.0);
    U3 = clamp(U3, -7.0, 7.0);
    U4 = clamp(U4, -7.0, 7.0);
    U5 = clamp(U5, -7.0, 7.0);

    //------------------------------------------------------------------
    // Compute U6 → A3 (bounds 1.005 - 100)
    //------------------------------------------------------------------
    double U6 =
        F[44] +
        F[45] * std::log(DBHOB) +
        F[46] * HTTOT +
        F[47] * std::log(HTTOT);

    U6 = clamp(U6, -6.0, 6.0);

    U6 = 1.0 + std::exp(U6);
    U6 = clamp(U6, 1.005, 100.0);

    //------------------------------------------------------------------
    // Generate R1–R5, A3
    //------------------------------------------------------------------
    const double R1 = logistic(U1);
    const double R2 = logistic(U2);
    const double R3 = logistic(U3);
    const double R4 = logistic(U4);
    const double R5 = 0.5 + 0.5 * logistic(U5);

    const double A3 = U6;

    //------------------------------------------------------------------
    // Combine height-fraction parameters
    //------------------------------------------------------------------
    const double RHI2 = RHI1 + RHLONGI;

    const double RHC =
        RHI2 + (1.0 - RHI2) * logistic(U8);

    //------------------------------------------------------------------
    // Fill output arrays
    //------------------------------------------------------------------
    FlewellingShapeParams out = { R1, R2, R3, R4, R5, U6, RHI1, RHI2, RHC, RHLONGI };

    return out;
}

//Red cedar
//Stem form  SHAPE paraemeters for Red Cedar (English units)
FlewellingShapeParams SHP_W5(double DBHOB, double HTTOT, std::string_view geoSub)
{
    //------------------------------------------------------------------
    // FORTRAN coefficients: mapped to F(50) 1-based → C++ 0-based
    //------------------------------------------------------------------
    static const double F[47] = {
        /* 1–12 unused */0,0,0,0,0,0,0,0,0,0,0,0,

        // F(13), F(15)
        -1.1917515e+00, 0.0, 1.5632952e-01,
        //F(16)
        0.0,
        // F(17)–F(18)
        4.4805624e-01, -1.0269221e-01,
        //* F19–F20 unused */
        0.0,0.0,

        // F(21)–F(22)
        8.2814200e-01, 1.7750205e+01,
        //* F23–F24 unused */
        0,0,

        // F(25)–F(27)
        -1.7959260e+01, 3.8308966e+00, 4.5163788e+01,
        //* F28 unused */
        0,

        // F(29)–F(31)
        -1.7546090e+00, -7.9230813e+00, -1.1253761e+02,
        //* F32–F33 unused */
        0,0,

        // F(34)–F(36)
        -1.7943249e-01, 1.5534485e-01, -3.2777026e-02,
        //* F37 unused */
        0,

        // F(38)–F(40)
        8.5305448e+00, -8.3350599e-01, 1.2100013e+01,

        // F(41 unused*/
        0.0,

        // F(42)–F(43)
        8.3021283e+00, -1.50e+02,
        //F(44)
        0.0,
        // F(45)–F(47)
        -7.1594841e+00, 1.1263465e-01, 2.2396603e+01
    };

    //------------------------------------------------------------------
    // Utility lambdas
    //------------------------------------------------------------------
    auto clamp = [](double x, double lo, double hi) {
        return std::max(lo, std::min(x, hi));
        };

    auto logistic = [](double u) {
        return std::exp(u) / (1.0 + std::exp(u));
        };

    //------------------------------------------------------------------
    // Main variables
    //------------------------------------------------------------------
    const double BH = 4.5;

    // DMEDIAN uses Red Cedar formula
    double DMEDIAN =
        0.11 * std::pow(HTTOT - BH, 1.08 + 0.0006 * HTTOT);

    double DFORM = DBHOB / DMEDIAN - 1.0;

    //------------------------------------------------------------------
    // === RHI1 (U7) ===
    //------------------------------------------------------------------
    double U7 = F[12] + F[14] * DFORM;  // F(13) + F(15)*DFORM

    U7 = clamp(U7, -7.0, 1.0);   // note: original limits slightly asymmetric

    double RHI1 = logistic(U7);

    if (RHI1 > 0.5)
        RHI1 = 0.5;

    //------------------------------------------------------------------
    // === RHLONGI (U9) ===
    //------------------------------------------------------------------
    double U9A =
        F[16] + F[17] * std::log(HTTOT);  // F(17) + F(18)*log(H)

    // Enforce consistency: RHI1 + U9A <= 0.75
    if (RHI1 + U9A > 0.75)
        U9A = 0.75 - RHI1;

    double RHLONGI = std::max(0.0, U9A);

    //------------------------------------------------------------------
    // === RHC core (U8) ===
    //------------------------------------------------------------------
    double U8 =
        F[20] + F[21] / DBHOB;  // f(21) + f(22)/DBHOB

    U8 = clamp(U8, -7.0, 7.0);

    //------------------------------------------------------------------
    // === U1–U5 ===
    //------------------------------------------------------------------
    double U1 =
        F[24] + F[25] * std::log(DBHOB) + F[26] / DBHOB;

    double U2 =
        F[28] + F[29] / DBHOB + F[30] / (DBHOB * DBHOB);

    double U3 =
        F[33] + F[34] * DBHOB + F[35] * HTTOT;

    double U4 =
        F[37] + F[38] * DBHOB + F[39] * DFORM;

    double U5 =
        F[41] + F[42] / DBHOB;

    U1 = clamp(U1, -7.0, 7.0);
    U2 = clamp(U2, -7.0, 7.0);
    U3 = clamp(U3, -7.0, 7.0);
    U4 = clamp(U4, -7.0, 7.0);
    U5 = clamp(U5, -7.0, 7.0);

    //------------------------------------------------------------------
    // === U6 → A3 ===
    //------------------------------------------------------------------
    double U6 =
        F[44] + F[45] * DBHOB + F[46] / DBHOB;

    U6 = clamp(U6, -6.0, 6.0);
    U6 = 1.0 + std::exp(U6);
    U6 = clamp(U6, 1.005, 100.0);

    double A3 = U6;

    //------------------------------------------------------------------
    // === Geometric logistic parameters R1–R5 ===
    //------------------------------------------------------------------
    double R1 = logistic(U1);
    double R2 = logistic(U2);
    double R3 = logistic(U3);
    double R4 = logistic(U4);
    double R5 = 0.5 + 0.5 * logistic(U5);

    //------------------------------------------------------------------
    // === Height‑fraction parameters ===
    //------------------------------------------------------------------
    double RHI2 = RHI1 + RHLONGI;

    double RHC =
        RHI2 + (1.0 - RHI2) * logistic(U8);

    //------------------------------------------------------------------
    // Fill output
    //------------------------------------------------------------------
    FlewellingShapeParams out = { R1, R2, R3, R4, R5, U6, RHI1, RHI2, RHC, RHLONGI };

    return out;
}

// given 2 heights(hi and hj), estimate the correlation of the
// Z errors in the corresponding dib's                                      

double COR_WS(int JSP, double HTTOT, double HI, double HJ)
{
    // QH(3,5) in FORTRAN → QH[3][5] in C++
    static const double QH[3][5] = {
        // Douglas-fir
        { -4.9012, 14.0602, -14.98,  -1.813,   0.249   },

        // Western hemlock
        { -4.1915, 10.026,  -11.536, -1.3865, -0.338   },

        // Western red cedar
        { -5.6689, 22.896,  -28.335, -1.9173,  0.242   }
    };

    // FORTRAN uses 1-based: JSPR = JSP - 2
    // QH index: QH(JSP,1) → QH[JSPR][0]
    int JSPR = JSP - 2 - 1;  //-1 to get 0-based index

    // Extract Q-coefficients
    double Q1 = QH[JSPR][0];
    double Q2 = QH[JSPR][1];
    double Q3 = QH[JSPR][2];
    double Q4 = QH[JSPR][3];
    double Q5 = QH[JSPR][4];

    const double BH = 4.5;

    // Case 1: same height → perfectly correlated
    if (HI == HJ)
        return 1.0;

    // Case 2: one height at breast height → irrelevant correlation
    if (HI == BH || HJ == BH)
        return 0.5;

    // Order heights
    double h1 = std::min(HI, HJ);
    double h2 = std::max(HI, HJ);

    double CORR = 0.0;

    // Case 3: both above BH
    if (h1 > BH)
    {
        double t3 = (h1 - BH) / (HTTOT - BH);
        double t4 = (h2 - BH) / (HTTOT - BH);

        CORR = std::exp(
            Q1 * (t4 - t3)
            + Q2 * (t4 * t4 - t3 * t3) / 2.0
            + Q3 * (t4 * t4 * t4 - t3 * t3 * t3) / 3.0
        );

        return CORR;
    }

    // Case 4: spans BH (h1 < BH < h2)
    if (h2 > BH)
    {
        double t3 = (h2 - BH) / (HTTOT - BH);
        double t2 = (BH - h1) / BH;

        CORR =
            Q5 * std::exp(
                Q4 * t2
                + Q1 * t3
                + Q2 * (t3 * t3) / 2.0
                + Q3 * (t3 * t3 * t3) / 3.0
            );

        return CORR;
    }

    // Case 5: both below BH
    double t2 = (BH - h2) / BH;
    double t1 = (BH - h1) / BH;

    CORR = std::exp(Q4 * (t1 - t2));
    return CORR;
}

//struct ZResult {
//    double Z;       // Standardized offset (forward transform)
//    double dibAct;  // Actual dib (inverse transform)
//};


//      H       In    REAL * 4    Section HTTOT(0 <= h < HTTOT, h not= BH)
//      DIBmod  In    Real * 4    Base model prediction of dib(expectation)
//      DIBact  In    Real * 4    Actual dib
//      Z       out   Real * 4    Standardized offset

double VAR_C1(int JSP, double DBHOB, double HTTOT, double H, double dibMod, double dibAct)
{
    switch (JSP)
    {
    case 3:
        return Z_from_sf3(DBHOB, HTTOT, H, dibMod, dibAct);

    case 4:
        return Z_from_sf4(DBHOB, HTTOT, H, dibMod, dibAct);

    case 5:
        return Z_from_sf5(DBHOB, HTTOT, H, dibMod, dibAct);

    default:
        return 0.0;
    }
}

//Douglas-fir
double Z_from_sf3(double DBHOB, double HTTOT, double H, double dibMod, double dibAct)
{
    const double BH = 4.5;

    // Constants from DATA statements
    const double BL1 = 9.2484016;
    const double BL4 = 19.051579;
    const double MU = 0.01;

    const double S11 = 1.18456, S12 = -0.02638802, S13 = 0.4293105;
    const double S21 = -3.638135, S22 = 0.44202834;
    const double S3 = -1.7685049, S4 = -0.066272536;
    const double S5 = -11.061295, S6 = -0.010578261;
    const double S7 = -0.29628457, S8 = -0.43808609, S9 = 0.74422178;
    const double SU5 = -0.25196078, SU6 = 1.7192804;

    //------------------------------------------------------------------
    // Step 1: compute X
    //------------------------------------------------------------------
    double X = dibAct / dibMod;
    if (X <= MU + 0.0005)
        X = MU + 0.0005;

    //------------------------------------------------------------------
    // Step 2: compute T
    //------------------------------------------------------------------
    double T;
    if (H > BH)
        T = (H - BH) / (HTTOT - BH);
    else
        T = (BH - H) / BH;

    //------------------------------------------------------------------
    // Step 3: compute LAMDA and TR2
    //------------------------------------------------------------------
    double ALHT = std::log(HTTOT);
    double ALAMDA, LAMDA, TR2;

    if (H > BH)
    {
        ALAMDA = SU5 + SU6 * T * T;
        ALAMDA = std::clamp(ALAMDA, -12.0, 12.0);
        LAMDA = 1.0 - MU + std::exp(ALAMDA);

        TR2 = (S11 + S12 * DBHOB + S13 * ALHT)
            + (S21 + S22 * ALHT) * T
            + S3 * std::pow(T, 4)
            + S4 * std::log(T);
    }
    else
    {
        ALAMDA = BL1 + BL4 * T * T;
        ALAMDA = std::clamp(ALAMDA, 0.0, 12.0);
        LAMDA = std::exp(ALAMDA);

        TR2 = S5 + S6 * HTTOT + (S7 + S9 * std::pow(HTTOT, 0.3)) * std::pow(T, S8);
    }

    TR2 = std::clamp(TR2, -25.0, 12.0);

    double DZ_DX = std::exp(TR2) * LAMDA;

    //------------------------------------------------------------------
    // Step 4: compute DELTA and GAMMA
    //------------------------------------------------------------------
    double YM = (1.0 - MU) / LAMDA;

    double DELTA =
        DZ_DX * LAMDA / 4.0 *
        (1.0 - std::pow(2.0 * YM - 1.0, 2.0));

    double GAMMA = -DELTA * std::log(YM / (1.0 - YM));

    //------------------------------------------------------------------
    // Step 5: compute Y, Z
    //------------------------------------------------------------------
    double Y = (X - MU) / LAMDA;
    Y = std::clamp(Y, 0.0, 0.999999);

    double Z = GAMMA + DELTA * std::log(Y / (1.0 - Y));
    return Z;
}

// ----------------------------------------------------------------------
// Compute actual dib (DIBact) from Z for Douglas-fir (NW Taper Coop CD17)
// This is the modern C++ equivalent of the FORTRAN SF_DFZ3 ENTRY block.
// ----------------------------------------------------------------------
double dib_from_Z_sf3(double DBHOB, double HTTOT, double H, double dibMod, double Z)
{
    const double BH = 4.5;

    // Special case: exactly breast height → no error allowed
    if (std::fabs(H - BH) < 1e-6)
        return dibMod;

    // ------------------ Constants from DATA statements -----------------
    const double BL1 = 9.2484016;
    const double BL4 = 19.051579;

    const double MU = 0.01;

    const double S11 = 1.18456, S12 = -0.02638802, S13 = 0.4293105;
    const double S21 = -3.638135, S22 = 0.44202834;
    const double S3 = -1.7685049, S4 = -0.066272536;
    const double S5 = -11.061295, S6 = -0.010578261;
    const double S7 = -0.29628457, S8 = -0.43808609, S9 = 0.74422178;

    const double SU5 = -0.25196078;
    const double SU6 = 1.7192804;

    // ------------------------------------------------------------------
    // STEP 1: Compute T (height scaling)
    // ------------------------------------------------------------------
    double T;
    if (H > BH)
        T = (H - BH) / (HTTOT - BH);
    else
        T = (BH - H) / BH;

    double ALHT = std::log(HTTOT);

    // ------------------------------------------------------------------
    // STEP 2: Compute LAMDA and TR2
    // ------------------------------------------------------------------
    double ALAMDA, LAMDA, TR2;

    if (H > BH)
    {
        ALAMDA = SU5 + SU6 * (T * T);
        ALAMDA = std::clamp(ALAMDA, -12.0, 12.0);

        LAMDA = 1.0 - MU + std::exp(ALAMDA);

        TR2 =
            (S11 + S12 * DBHOB + S13 * ALHT) +
            (S21 + S22 * ALHT) * T +
            S3 * std::pow(T, 4.0) +
            S4 * std::log(T);
    }
    else
    {
        ALAMDA = BL1 + BL4 * (T * T);
        ALAMDA = std::clamp(ALAMDA, 0.0, 12.0);

        LAMDA = std::exp(ALAMDA);

        TR2 =
            S5 + S6 * HTTOT +
            (S7 + S9 * std::pow(HTTOT, 0.3)) * std::pow(T, S8);

        TR2 = std::clamp(TR2, -25.0, 12.0);
    }

    double DZ_DX = std::exp(TR2) * LAMDA;

    // ------------------------------------------------------------------
    // STEP 3: Compute DELTA and GAMMA
    // ------------------------------------------------------------------
    double YM =
        (1.0 - MU) / LAMDA;

    double DELTA =
        DZ_DX * LAMDA / 4.0 *
        (1.0 - std::pow((2.0 * YM - 1.0), 2.0));

    double GAMMA =
        -DELTA * std::log(YM / (1.0 - YM));

    // ------------------------------------------------------------------
    // STEP 4: Invert Johnson Sb transform (Given Z → get Y)
    // ------------------------------------------------------------------
    double YLOGIT = (Z - GAMMA) / DELTA;

    double Y = std::exp(YLOGIT) / (1.0 + std::exp(YLOGIT));

    // ------------------------------------------------------------------
    // STEP 5: Compute final normalized dib ratio X
    // ------------------------------------------------------------------
    double X = MU + LAMDA * Y;

    // ------------------------------------------------------------------
    // STEP 6: Return DIBact = X * DIBmod
    // ------------------------------------------------------------------
    return X * dibMod;
}

//Western hemlock
double Z_from_sf4(double DBHOB, double HTTOT, double H, double dibMod, double dibAct)
{
    const double BH = 4.5;

    // DATA constants
    const double A11 = 3.7244760, A12 = -0.64373853, A13 = -2.5045615;
    const double A21 = -7.6871598, A22 = 0.030248851;
    const double A31 = 1.2762845, A32 = 0.44828840, A33 = -0.0050630122;
    const double A40 = 0.032150116, A41 = 4.0216438, A42 = -4.8014107;

    const double S1 = 0.01, S3 = 0.69481696;
    const double S5 = 0.70394499, S6 = 5.4510380;
    const double S7 = 2.8470272;
    const double U11 = 3.4018968, U12 = -0.88734933, U2 = -2.334093;

    //------------------------------------------------------------------
    // Step 1: T
    //------------------------------------------------------------------
    double T =
        (H > BH) ? (H - BH) / (HTTOT - BH) : (BH - H) / BH;

    //------------------------------------------------------------------
    // Step 2: compute LAMDA and TR2
    //------------------------------------------------------------------
    double MU, LAMDA, TR2;

    if (H > BH)
    {
        MU = S1;
        LAMDA = 1.0 - MU + std::exp(S5 + S6 * T * T);

        double A1 = A11 + A12 * std::log(DBHOB + 1.0) + A13 / DBHOB;
        double A2 = std::min(A21 + A22 * DBHOB, 0.0);
        double A3 = A31
            + A32 * std::log(DBHOB + 0.5)
            + A33 * std::pow(std::log(DBHOB + 0.5), 2);

        double A4 = A40 + A41 * std::exp(A42 * DBHOB);

        TR2 = A1 + A2 * std::pow(T, A3) + A4 / T;
    }
    else
    {
        MU = S3;
        LAMDA = S7;

        double U1 = U11 * (1.0 - std::exp(U12 * DBHOB));
        TR2 = U1 + U2 * T;
    }

    TR2 = std::clamp(TR2, -12.0, 12.0);

    double DZ_DX = std::exp(TR2) * LAMDA;

    //------------------------------------------------------------------
    // Step 3: DELTA and GAMMA
    //------------------------------------------------------------------
    double YM = (1.0 - MU) / LAMDA;

    double DELTA =
        DZ_DX * LAMDA / 4.0 *
        (1.0 - std::pow(2.0 * YM - 1.0, 2.0));

    double GAMMA = -DELTA * std::log(YM / (1.0 - YM));

    //------------------------------------------------------------------
    // Step 4: X, Y, Z
    //------------------------------------------------------------------
    double X = dibAct / dibMod;
    if (X <= MU + 0.0005)
        X = MU + 0.0005;

    double Y = (X - MU) / LAMDA;
    Y = std::clamp(Y, 0.0, 0.999999);

    double Z = GAMMA + DELTA * std::log(Y / (1.0 - Y));

    return Z;
}

double dib_from_Z_sf4(double DBHOB, double HTTOT, double h, double DIBMOD, double Z)
{
    const double A11 = 3.7244760, A12 = -0.64373853, A13 = -2.5045615;
    const double A21 = -7.6871598, A22 = 0.030248851;
    const double A31 = 1.2762845, A32 = 0.44828840, A33 = -0.0050630122;
    const double A40 = 0.032150116, A41 = 4.0216438, A42 = -4.8014107;

    const double S1 = 0.01, S3 = 0.69481696;
    const double S5 = 0.70394499, S6 = 5.4510380;
    const double S7 = 2.8470272;
    const double U11 = 3.4018968, U12 = -0.88734933, U2 = -2.334093;

    double BH = 4.5;
    double T;

    if (h > BH)
        T = (h - BH) / (HTTOT - BH);
    else
        T = (BH - h) / BH;

    if (std::abs(h - BH) < 1e-6)
        return DIBMOD;

    double MU, LAMDA, TR2;

    if (h > BH) {
        MU = S1;
        LAMDA = 1.0 - MU + std::exp(S5 + S6 * T * T);

        double A1 = A11 + A12 * std::log(DBHOB + 1.0) + A13 / DBHOB;
        double A2 = std::min(A21 + A22 * DBHOB, 0.0);
        double A3 = A31 + A32 * std::log(DBHOB + 0.5)
            + A33 * std::pow(std::log(DBHOB + 0.5), 2);
        double A4 = A40 + A41 * std::exp(A42 * DBHOB);

        TR2 = A1 + A2 * std::pow(T, A3) + A4 / T;
    }
    else {
        double U1 = U11 * (1.0 - std::exp(U12 * DBHOB));
        MU = S3;
        LAMDA = S7;
        TR2 = U1 + U2 * T;
    }

    TR2 = std::clamp(TR2, -12.0, 12.0);

    double DZ_DX = std::exp(TR2) * LAMDA;
    double YM = (1.0 - MU) / LAMDA;

    double DELTA = DZ_DX * LAMDA / 4.0 *
        (1.0 - std::pow((2.0 * YM - 1.0), 2.0));

    double GAMMA = -DELTA * std::log(YM / (1.0 - YM));

    double YLOGIT = (Z - GAMMA) / DELTA;
    double Y = std::exp(YLOGIT) / (1.0 + std::exp(YLOGIT));

    double X = MU + LAMDA * Y;

    return X * DIBMOD;
}

//Red cedar
double Z_from_sf5(double DBHOB, double HTTOT, double H, double dibMod, double dibAct)
{
    const double BH = 4.5;

    // DATA constants
    const double A11 = 4.3050830, A12 = -0.85232440, A13 = -2.3355492;
    const double A21 = -7.5573025, A22 = 0.011360349;
    const double A31 = -1.9172059, A32 = 3.1382338, A33 = -0.53258164;
    const double A40 = 0.019259274, A41 = 4.0216439, A42 = -4.7142447;

    const double S1 = 0.01, S3 = 0.78308913;
    const double S5 = 0.21140396, S6 = 6.4129714;
    const double S7 = 2.03990921;
    const double U11 = 3.3372438, U12 = -4.0010883, U2 = -2.2145198;

    //------------------------------------------------------------------
    // Step 1: T
    //------------------------------------------------------------------
    double T =
        (H > BH) ? (H - BH) / (HTTOT - BH) : (BH - H) / BH;

    //------------------------------------------------------------------
    // Step 2: compute LAMDA and TR2
    //------------------------------------------------------------------
    double MU, LAMDA, TR2;

    if (H > BH)
    {
        MU = S1;
        LAMDA = 1.0 - MU + std::exp(S5 + S6 * T * T);

        double A1 = A11 + A12 * std::log(DBHOB + 1.0) + A13 / DBHOB;
        double A2 = std::min(A21 + A22 * DBHOB, 0.0);
        double logd = std::log(DBHOB + 0.5);
        double A3 = A31 + A32 * logd + A33 * logd * logd;

        double A4 = A40 + A41 * std::exp(A42 * DBHOB);

        TR2 = A1 + A2 * std::pow(T, A3) + A4 / T;
    }
    else
    {
        MU = S3;
        LAMDA = S7;

        double U1 = U11 * (1.0 - std::exp(U12 * DBHOB));
        TR2 = U1 + U2 * T;
    }

    TR2 = std::clamp(TR2, -12.0, 12.0);

    double DZ_DX = std::exp(TR2) * LAMDA;

    //------------------------------------------------------------------
    // Step 3: DELTA / GAMMA
    //------------------------------------------------------------------
    double YM = (1.0 - MU) / LAMDA;

    double DELTA =
        DZ_DX * LAMDA / 4.0 *
        (1.0 - std::pow(2.0 * YM - 1.0, 2.0));

    double GAMMA = -DELTA * std::log(YM / (1.0 - YM));

    //------------------------------------------------------------------
    // Step 4: X, Y, Z
    //------------------------------------------------------------------
    double X = dibAct / dibMod;
    if (X <= MU + 0.0005)
        X = MU + 0.0005;

    double Y = (X - MU) / LAMDA;
    Y = std::clamp(Y, 0.0, 0.999999);

    double Z = GAMMA + DELTA * std::log(Y / (1.0 - Y));
    return Z;
}

double dib_from_Z_sf5(double DBHOB, double HTTOT, double h, double DIBMOD, double Z)
{
    // --- Coefficients ---
    const double A11 = 4.3050830, A12 = -0.85232440, A13 = -2.3355492;
    const double A21 = -7.5573025, A22 = 0.011360349;
    const double A31 = -1.9172059, A32 = 3.1382338, A33 = -0.53258164;
    const double A40 = 0.019259274, A41 = 4.0216439, A42 = -4.7142447;

    const double S1 = 0.01, S3 = 0.78308913;
    const double S5 = 0.21140396, S6 = 6.4129714;
    const double S7 = 2.03990921;
    const double U11 = 3.3372438, U12 = -4.0010883, U2 = -2.2145198;

    const double BH = 4.5;

    // --------------------------------------------------------------
    // Compute T
    // --------------------------------------------------------------
    double T;
    if (h > BH)
        T = (h - BH) / (HTTOT - BH);
    else
        T = (BH - h) / BH;

    // Special case: exactly at BH
    if (std::abs(h - BH) < 1e-6)
        return DIBMOD;

    // --------------------------------------------------------------
    // Compute MU, LAMDA, TR2 exactly as FORTRAN does
    // --------------------------------------------------------------
    double MU, LAMDA, TR2;

    if (h > BH) {
        MU = S1;
        LAMDA = 1.0 - MU + std::exp(S5 + S6 * T * T);

        double logterm = std::log(DBHOB + 0.5);
        double A1 = A11 + A12 * std::log(DBHOB + 1.0) + A13 / DBHOB;
        double A2 = std::min(A21 + A22 * DBHOB, 0.0);
        double A3 = A31 + A32 * logterm + A33 * logterm * logterm;
        double A4 = A40 + A41 * std::exp(A42 * DBHOB);

        TR2 = A1 + A2 * std::pow(T, A3) + A4 / T;
    }
    else {
        // Below breast height
        double U1 = U11 * (1.0 - std::exp(U12 * DBHOB));
        MU = S3;
        LAMDA = S7;
        TR2 = U1 + U2 * T;
    }

    // Clamp TR2 to [-12, 12]
    TR2 = std::max(-12.0, std::min(12.0, TR2));

    // --------------------------------------------------------------
    // Compute DELTA, GAMMA
    // --------------------------------------------------------------
    double DZ_DX = std::exp(TR2) * LAMDA;

    double YM = (1.0 - MU) / LAMDA;

    double DELTA =
        DZ_DX * LAMDA / 4.0 *
        (1.0 - std::pow((2.0 * YM - 1.0), 2));

    double GAMMA = -DELTA * std::log(YM / (1.0 - YM));

    // --------------------------------------------------------------
    // Reverse Sb transform: from Z -> Y -> X -> DIBact
    // --------------------------------------------------------------
    double YLOGIT = (Z - GAMMA) / DELTA;
    double Y = std::exp(YLOGIT) / (1.0 + std::exp(YLOGIT));

    double X = MU + LAMDA * Y;

    return X * DIBMOD;
}

// given a section height(h), calculate the ratio :
//  DBT / DOB
// for Red Cedar.NW Taper Coop  4 / 15 / 94

double BRK_WS(int JSP, double DBHOB, double HTTOT, double DBTBH, double h)
{
    // Constants from FORTRAN DATA blocks
    const double a1 = 1.8773;
    const double a2 = -3.15524;
    const double a3 = -0.06725;
    const double a4 = 0.68229;
    const double b2 = 0.2553;

    const double C1 = 1.1309;
    const double C2 = -2.021;
    const double C3 = -0.03323;
    const double C4 = 0.8167;

    const double BH = 4.5;

    //------------------------------------------------------------------
    // CASE 1 — JSP = 3 (Western Red Cedar, complex DBT/DOB model)
    //------------------------------------------------------------------
    if (JSP == 3)
    {
        // Compute E2, T1, TBH, R1
        double E2 = 0.51705 + 0.483 * std::exp(-0.251 * DBHOB);
        double T1 = 0.6406 + 0.0512 * (1.0 - std::exp(-0.0201 * DBHOB));
        double TBH = std::pow(BH / HTTOT, E2);

        double R1 = 0.6294 + 0.7901 * std::exp(-0.3 * DBHOB);
        if (R1 > 0.99) R1 = 0.99;

        // Solve for E1, E0
        double numerator = (R1 - 1.0);
        double denom = -2.0 * T1 * (T1 - TBH)
            + (T1 * T1 - TBH * TBH);

        double E1 = numerator / denom;
        double E0 = -2.0 * T1 * E1;

        // Compute T
        double T = (h > 0.0)
            ? std::pow(h / HTTOT, E2)
            : 0.0;

        // 75% height breakpoint
        double H75 = std::max(0.75 * HTTOT,
            BH + 0.5 * (HTTOT - BH));

        // Base R
        double R = 1.0
            + E0 * (T - TBH)
            + E1 * (T * T - TBH * TBH);

        // Upper‑stem adjustment
        if (h > H75)
        {
            double ratio = (h - H75) / (HTTOT - H75);
            R += 0.5983 * (ratio * ratio);
        }

        return (DBTBH / DBHOB) * R;
    }

    //------------------------------------------------------------------
    // CASE 2 — JSP = 4 (Western Hemlock)
    //------------------------------------------------------------------
    if (JSP == 4)
    {
        double x = h / HTTOT;
        double xbh = BH / HTTOT;

        double factor =
            1.0
            + (C1 + C2 * std::exp(C3 * DBHOB)) * (x - xbh)
            + C4 * (x * x - xbh * xbh);

        return (DBTBH / DBHOB) * factor;
    }

    //------------------------------------------------------------------
    // CASE 3 — JSP = 5 (Red Cedar simple DBT/DOB formula)
    //------------------------------------------------------------------
    if (JSP == 5)
    {
        double x = (h > 0.0)
            ? std::pow(h / HTTOT, b2)
            : 0.0;

        double xbh = std::pow(BH / HTTOT, b2);

        double factor =
            1.0
            + (a1 + a2 * std::exp(a3 * DBHOB)) * (x - xbh)
            + a4 * (x * x - xbh * xbh);

        return (DBTBH / DBHOB) * factor;
    }

    //------------------------------------------------------------------
    // Default (undefined species code)
    //------------------------------------------------------------------
    return 0.0;
}