#include "FlewellingTaperModel_Other.h"
#include <cmath>
#include <string>
#include <algorithm>

//***********************************************************************
//     Calculates DIB and double bark thickness for PP on the BHNF, PP on the SJNF, ES on the DNF
//     Variables passed :
//         DBHOB     real    in  diameter breash HTTOT
//         HT2     real    in  height of measurement point
//         DOB     real    in  diameter at measurement point
//         DIB     real    out calculated diameter inside bark
//         DBTBH     real    bark thickness ratio(optional input)

double BRK_OT(int JSP, const std::string_view geosub, double DBHOB, double DOB, double HT2, double DBTBH_in,   // optional input (≤0 means calculate)
    double& dbt) 
{
    // Copy DBTBH so we can modify it
    double DBTBH = DBTBH_in;
    double DBHIB = DBHOB;
    double DIB = 0.0;

    // BK(9,8) in Fortran is BK[1..9][1..8]
    // Store as BK[9][8] zero‑indexed as BK[0..8][0..7].
    static const double BK[8][9] = {
        //BLACK HILLS PP MODEL
        {-0.310745, -5.267465,  4.056924,   1.1159037603, -0.082066096, 0.424652164, 0.699678,  0.067394, 0.0},
        //SAN JUAN PP MODEL
        {12.88990159, 17.90876955, 0.05237532,  0.0145082565, 0.0027058753, 0.1022613683, 0.702268, 0.050063, 0.0},
        //DIXIE ES MODEL
        {-0.8527459114, 0.9336248438, 0.2809059946, 0.0029765304, 0.0535125376, 0.3088136745, 0.7630215, 0.05957, 0.0},
        //R2 LODGEPOLE MODEL
        {-4.053133738, -3.891047743, 0.380866177, 0.7617392463, 0.2071264529, 0.6491861037, 0.8793398, 0.03020202, 0.0},
        //R2 DOUGLAS FIR MODEL
        {2.0518, 0.5569, 0.2009, 1.1163, 0.2194, 0.2525, -0.121321, 0.861817, 0.0},
        //R2 WHITE FIR MODEL
        {-0.3114, -0.5979, -0.00029, 0.4661, 0.2448, 0.1619, -0.155160, 0.875930, 0.0},
        //R2 ASPEN
        {20.1848, 111.927, 0.0808502, 2.24620, 0.336067, 0.486677, 0.0142862, 0.919056, 0.0},
        //R3 PONDEROSA PINE
        {2.67998, 3.49916, 0.111418, 2.28510, 0.374458, 0.450724, -0.0113180, 0.801889, 0.00308737}
    };

    int JSPR = JSP - 21; 

    //-1 to get zero based index
    double B2 = BK[JSPR - 1][0];
    double B3 = BK[JSPR - 1][1];
    double B5 = BK[JSPR - 1][2];
    double C1 = BK[JSPR - 1][3];
    double C2 = BK[JSPR - 1][4];
    double C3 = BK[JSPR - 1][5];
    double D0 = BK[JSPR - 1][6];
    double D1 = BK[JSPR - 1][7];
    double D2 = BK[JSPR - 1][8];

    //double D2 = (JSPR == 7 ? 0.00308737 : 0.0);  // last column in FORTRAN row 9 (only for model 8)

    // DR = DOB/DBHOB
    double DR = (DOB > 0.0 ? DOB / DBHOB : 0.0);

    // Compute DBTBH if missing
    if (DBTBH <= 0.0) {
        if (JSPR <= 1) {
            // Black Hills
            DBHIB = DBHOB * (D0 + D1 * std::log(DBHOB));

        }
        else if (JSPR == 2) {
            // San Juan
            if (geosub == "07") {
                DBHIB = 0.4065547 + 0.7794452 * DBHOB + 0.0035815 * DBHOB * DBHOB;
            }
            else if (geosub == "13") {
                DBHIB = DBHOB * (D0 + D1 * std::log(DBHOB));
            }
            else if (geosub == "01") {
                DBHIB = -0.649171195 + 0.925582344 * DBHOB;
            }
            else {
                DBHIB = -1.024742 + 0.933772 * DBHOB;
            }

        }
        else if (JSPR == 3) {
            DBHIB = DBHOB * (D0 + D1 * std::log(DBHOB));

        }
        else if (JSPR == 4) {
            if (geosub == "02") {
                DBHIB = DBHOB * (0.925915 + 0.0153361 * std::log(DBHOB));
            }
            else {
                DBHIB = DBHOB * (D0 + D1 * std::log(DBHOB));
            }

        }
        else if (JSPR == 5 || JSPR == 6 || JSPR == 7) {
            DBHIB = D0 + D1 * DBHOB;

        }
        else if (JSPR == 8) {
            DBHIB = D0 + D1 * DBHOB + D2 * DBHOB * DBHOB;
        }

        DBTBH = DBHOB - DBHIB;

    }
    else {
        DBHIB = DBHOB - DBTBH;
    }

    double PY = 1.0;

    // Height-based Wenzel model logic
    if (HT2 > 4.5) {
        if (DR > 0.01) {
            PY = (DR * ((B2 - 1) / (B2 - std::pow(DR, B3)))) -
                ((std::pow(DR, B5) - 1) / DBTBH);
        }
        else {
            PY = 0.0;
        }

    }
    else if (HT2 == 4.5) {
        PY = 1.0;

    }
    else {
        double CLX = C1 * (DR - 1);
        if (CLX >= 0.0) {
            PY = 1 + std::pow(CLX, (C2 + C3 * DBTBH));
        }
        else {
            PY = 1.0;
        }
    }

    dbt = PY * DBTBH;

    DIB = DOB - dbt;
    if (DIB < 0.0) DIB = 0.0;

    return DIB;
}

// Stem form SHAPE parameters for San Juan PP, Dixie ES
FlewellingShapeParams SHP_OT(int JSP, double DBHOB, double HTTOT) 
{
    using std::log;
    using std::exp;
    using std::min;

    FlewellingShapeParams out;

    int JRSP = JSP - 22;

    // Retrieve parameters (adjusting Fortran 1-based indexing)
    //auto f = int i, int j -> double{
    //    return F[i - 1][j - 1];
    //};
    auto f = F_shapeOther[JRSP - 1];

    double DMEDIAN =
        f[10] * pow(HTTOT - 4.5, f[11] + f[12] * HTTOT);

    double DFORM = DBHOB / DMEDIAN - 1.0;

    // U7
    double U7 =
        f[13] + f[14] * log(HTTOT) + f[15] * DFORM;

    // U9T
    double U9T =
        f[18] + f[19] * log(HTTOT) + f[20] * DFORM;

    U9T = std::clamp(U9T, -7.0, 7.0);

    double U9 =
        f[16] * exp(U9T) / (1.0 + exp(U9T));

    // U8
    double U8 =
        f[21] + f[22] * HTTOT + f[23] * log(HTTOT)
        + f[24] * DFORM;

    // U1–U5
    double U1 =
        f[25] + f[26] * log(HTTOT) + f[27] * DFORM +
        f[28] * DFORM * log(HTTOT);

    double U2 =
        f[29] + f[30] * DFORM + f[31] * log(HTTOT)
        + f[32] * DFORM * log(HTTOT)
        + f[33] * DBHOB;

    double U3;
    if (JRSP == 3) {
        U3 = f[34] + f[35] * DFORM +
            f[36] * (1.0 - exp(f[37] * HTTOT));
    }
    else {
        U3 = f[34] + f[35] * DFORM +
            f[36] * log(HTTOT) +
            f[37] * log(HTTOT) * DFORM;
    }

    double U4 =
        f[38] + f[39] * DFORM +
        f[40] * log(HTTOT) + f[41] * DBHOB;

    double U5 =
        f[42] + f[43] * log(HTTOT);

    double U6 =
        f[45] + f[46] * DFORM + f[47] * log(HTTOT);

    // Clamp limits
    auto clamp7 = [](double& x) {
        if (x < -7.0) x = -7.0;
        if (x > 7.0) x = 7.0;
        };


    clamp7(U1);
    clamp7(U2);
    clamp7(U3);
    clamp7(U4);
    clamp7(U5);
    clamp7(U7);

    if (U5 > 7.1) U5 = 7.1;
    //U6 = std::clamp(U6, 1.005, 10.0);
    if (U6 < 1.005) U6 = 1.005;
    if (U6 > 10.0) U6 = 10.0;

    if (U8 > 0.99) U8 = 0.99;
    //U9 = std::clamp(U9, 0.0, 0.3);
    if (U9 < 0.0) U9 = 0.0;
    if (U9 > 0.3) U9 = 0.3;

    // Geometric parameter outputs
    double R1 = exp(U1) / (1.0 + exp(U1));
    double R2 = exp(U2) / (1.0 + exp(U2));
    double R3 = exp(U3) / (1.0 + exp(U3));
    double R4 = exp(U4) / (1.0 + exp(U4));

    double R5;
    if (U5 <= 7.0)
        R5 = 0.5 + 0.5 * (exp(U5) / (1.0 + exp(U5)));
    else
        R5 = 1.0;

    double A3 = U6;

    double RHI1 = exp(U7) / (1.0 + exp(U7));
    if (RHI1 > 0.5) RHI1 = 0.5;

    double RHLONGI = U9;
    double RHI2 = RHI1 + RHLONGI;

    double RHC = U8;
    if (RHC < RHI2 + 0.01f)
        RHC = std::min<double>(RHI2 + 0.01, (RHI2 + 1.0) / 2.0);

    // Fill outputs
    out = { R1,R2,R3,R4,R5,A3,RHI1,RHI2,RHC,RHLONGI };

    return out;
}

//     given 2 heights(hi and hj), estimate the correlation of the
//       Z errors in the corresponding dib's

double COR_OT(int JSP, double HTTOT, double HI, double HJ)
{
    // Each species has 5 coefficients
    static const double V[7][5] = {
        // 1. Ponderosa Pine (San Juan NF)
        {-0.37047662e+01,  0.36383106e+01, -0.24995390e+01,
         -0.20054612e+01, -0.34460970e+00},

        // 2. Engelmann Spruce (Dixie NF)
        {-0.54787570e+01,  0.96890017e+01, -0.53401299e+01,
         -0.13433096e+01, -0.20405779e+00},

        // 3. Region 2 Lodgepole Pine
        {-0.47777690e+01,  0.12360133e+02, -0.76787940e+01,
         -0.15295707e+01, -0.16751541e+00},

        // 4. Region 2 Douglas Fir
        {-0.24676508e+01, -0.18606299e+01, -0.45496728e+01,
         -0.12369208e+01,  0.19863544e+00},

        // 5. Region 2 White Fir
        {-0.12659920e+02,  0.41776970e+02, -0.12349876e+02,
         -0.13485158e+01, -0.97359467e+00},

        // 6. Region 2 Aspen
        {-0.24901327e+01,  0.18981262e+01, -0.34884538e+01,
         -0.13900552e+01,  0.14103043e-02},

        // 7. Region 3 Ponderosa Pine
        {-0.81723321e+01,  0.19235015e+02, -0.63296283e+01,
         -0.23078864e+01,  0.20674878e+00}
    };

    double BH = 4.5;
    int JSPR = JSP - 22 - 1;  // -1 to get zero based index

    double Q1 = V[JSPR][0];
    double Q2 = V[JSPR][1];
    double QS = V[JSPR][2];
    double Q4 = V[JSPR][3];
    double Q5 = V[JSPR][4];

    double Q3 = QS - (Q1 + Q2);

    // Same point → correlation = 1
    if (HI == HJ)
        return 1.0;

    // If either point is at breast height → correlation=0.5
    if (HI == BH || HJ == BH)
        return 0.5;

    // label common variables
    double h1 = std::min(HI, HJ);
    double h2 = std::max(HI, HJ);

    double CORR = 0.0;

    if (h1 > BH)
    {
        // Both heights above BH
        double t3 = (h1 - BH) / (HTTOT - BH);
        double t4 = (h2 - BH) / (HTTOT - BH);

        CORR = std::exp(
            Q1 * (t4 - t3)
            + Q2 * (t4 * t4 - t3 * t3) / 2.0
            + Q3 * (t4 * t4 * t4 - t3 * t3 * t3) / 3.0);
    }
    else if (h2 > BH)
    {
        // h1 < BH < h2
        double t3 = (h2 - BH) / (HTTOT - BH);
        double T2 = (BH - h1) / BH;

        CORR = Q5 *
            std::exp(
                Q4 * T2
                + Q1 * t3
                + Q2 * (t3 * t3) / 2.0
                + Q3 * (t3 * t3 * t3) / 3.0);
    }
    else
    {
        // Both heights below BH
        double T2 = (BH - h2) / BH;
        double T1 = (BH - h1) / BH;

        CORR = std::exp(Q4 * (T1 - T2));
    }

    return CORR;
}

// Variance of dib  for species assuming lognormal errors.
// H      input   R * 4   Section height, for h NE breast height.
// SE_LNX output  R * 4   SQRT{ VAR[ln(dib)] }

double VAR_OT(int JSP, double DBHOB, double HTTOT, double H)
{
    double SE_LNX;
    // F(13,7) and V(20,7) -> C++ arrays [13][7], [20][7]
    // Only elements 10–12 of F() and 1–16 of V() are used.

    static const double F[7][3] = {
        // SAN JUAN PONDEROSA PINE
        {0.66384514e+01, 0.10000000e-01, 0.29774591e-03},

        // DIXIE ENGELMANN SPRUCE
        {0.21020000e+00, 0.91540000e+00, 0.37900000e-03},

        // Species 3
        {0.12277881e+00, 1.00000000e+01, 0.13175957e-03},

        // Species 4 (Region 2 Douglas Fir)
        {0.35453368e+01, 0.11656118e+00, 0.12768626e-02},

        // Species 5 (Region 2 White Pine)
        {0.73716080e+00, 0.68027288e+00, 0.00000000e+00},

        // Species 6 (Region 2 Aspen)
        {0.36691430e+01, 0.18992789e+00, 0.33176964e-04},

        // Species 7 (Region 3 Ponderosa Pine)
        {0.92005972e+01, 0.39365313e-01, 0.29661933e-02}
    };

    static const double V[7][16] = {
        // SAN JUAN PONDEROSA PINE
        {-0.11491009e+02,  0.95333994e+00,  0.16501845e+01,  0.11286695e+02,
         -0.17267989e+01,  0.00000000e+00,  0.21717935e+00,  0.79497708e-01,
          0.10000000e+01, -0.36608798e+00, -0.66158695e+01, -0.36269972e+00,
         -0.14169144e+00,  0.35434282e+01, -0.22939660e+00,  0.13578551e+01},

        // Species 2 (Dixie Engelmann Spruce)
        {-0.11631559e+02,  0.10438290e+01,  0.21132184e+01,  0.29726674e+01,
          0.38794708e+00,  0.84852726e+00,  0.25705963e-01,  0.84286599e-01,
         -1.00000000e+01, -0.41862718e+00, -0.61674503e+01, -0.25117268e+00,
         -0.10512011e-01,  0.51224598e+01, -0.76280178e+00,  0.14363734e+01},

        // Species 3
        {-0.15550680e+02,  0.18607266e+01,  0.53949081e+01,  0.34883785e+01,
          0.25891538e+00,  0.00000000e+00,  0.18218388e+00,  0.77716059e-01,
         -1.00000000e+01, -0.12490343e+01, -0.17214291e+01, -0.14819004e+01,
         -0.14800926e+00, -0.37137885e+01,  0.16299145e+01,  0.17250455e+01},

        // Species 4 (Region 2 Douglas Fir)
        {-0.24105465e+01, -0.12642787e+01, -0.27874506e+01,  0.11686963e+02,
         -0.18509747e+01,  0.17764737e-01,  0.27313143e+00,  0.25755302e-01,
          0.10000000e+01,  0.80356609e+00, -0.12781743e+02,  0.15577218e+01,
         -0.35755969e+00,  0.11889490e+02, -0.24101283e+01,  0.24070251e+01},

        // Species 5 (Region 2 White Pine)
         { 0.64719863e+01, -0.35842944e+01, -0.81824294e+01, -0.41853414e+01,
           0.21790612e+01,  0.15211185e+01, -0.10000000e+00,  0.66609003e-01,
          -1.00000000e+01,  0.22112781e+01, -0.34405197e+01, -0.11414631e+01,
           0.12167945e+00, -0.40868589e+01,  0.24337754e+01,  0.58663676e+01},

         // Species 6 (Region 2 Aspen)
          {-0.16511297e+01, -0.13388430e+01, -0.21100633e+01, -0.71713056e+00,
            0.13137397e+01,  0.70666496e+00,  0.33437187e+00,  0.46455296e-01,
           -1.00000000e+01,  0.64999079e+00,  0.15524976e+03, -0.77626650e+02,
            0.58213345e+00, -0.15840278e+03,  0.76819165e+02,  0.13116911e-01},

          // Species 7 (Region 3 Ponderosa Pine)
          {-0.79890640e+01, -0.59433730e-01,  0.41539599e+01,  0.76398811e+01,
           -0.96260263e+00,  0.35888329e+00,  0.13432608e+00,  0.63964977e-01,
            0.47319344e+00, -0.60513918e+00,  0.11725801e+01, -0.21999221e+01,
           -0.12620490e+01, -0.10542030e+02,  0.40156677e+01,  0.17008028e+01}
    };

    int JSPR = JSP - 22 -1;  //-1 to get zero based index

    // Assign coefficients
    double VA00 = V[JSPR][0];
    double VA01 = V[JSPR][1];
    double VA02 = V[JSPR][2];
    double VB00 = V[JSPR][3];
    double VB01 = V[JSPR][4];
    double VC0 = V[JSPR][5];

    double VX1 = V[JSPR][6];
    double VX2 = V[JSPR][7];
    double VX3 = V[JSPR][8];

    double VA03 = V[JSPR][9];
    double VE00 = V[JSPR][10];
    double VE01 = V[JSPR][11];
    double VE02 = V[JSPR][12];
    double VF00 = V[JSPR][13];
    double VF01 = V[JSPR][14];
    double VG0 = V[JSPR][15];

    double BH = 4.5;

    // === DMEDIAN, DRATIO ===
    double DMEDIAN =
        F[JSPR][0] *
        std::pow(HTTOT - BH, F[JSPR][1] + F[JSPR][2] * HTTOT);

    double DRATIO = DBHOB / DMEDIAN;
    double LOGHT = std::log(HTTOT);

    // Compute intermediate variables
    double VA0 = VA00 + VA01 * LOGHT + VA02 * DRATIO + VA03 * LOGHT * DRATIO;
    double VB0 = VB00 + VB01 * LOGHT;
    double VE0 = VE00 + VE01 * LOGHT + VE02 * DRATIO;
    double VF0 = VF00 + VF01 * LOGHT;
    double VC = VC0 + VX1 * LOGHT;

    double LVARHAT = 0.0;
    double VARHAT = 0.0;
    double X = 0.0;

    // === CASE 1: H < BH (below breast height) ===
    if (H < BH)
    {
        X = (BH - H) / BH;
        LVARHAT = VE0 + VF0 * std::pow(X, VG0);

        if (LVARHAT > 15.0)  LVARHAT = 15.0;
        if (LVARHAT < -15.0) LVARHAT = -15.0;

        VARHAT = std::exp(LVARHAT);
        SE_LNX = std::sqrt(VARHAT);
    }
    else if (H == BH)
    {
        // === CASE 2: at BH ===
        SE_LNX = 0.0;
    }
    else if (H < HTTOT)
    {
        // === CASE 3: above BH ===
        X = (H - BH) / (HTTOT - BH);

        LVARHAT =
            VA0
            + VB0 * std::pow(X, VC)
            + VX2 * std::pow((HTTOT / 50.0), VX3) / (1.0 - X);

        if (LVARHAT > 15.0)  LVARHAT = 15.0;
        if (LVARHAT < -15.0) LVARHAT = -15.0;

        VARHAT = std::exp(LVARHAT);
        SE_LNX = std::sqrt(VARHAT);
    }
    else
    {
        // === CASE 4: H ≥ HTTOT (meaningless) ===
        VARHAT = 1.0;
        SE_LNX = std::sqrt(VARHAT);
    }

    return SE_LNX;
}

//Black Hill Ponderosa pine shape parameters
FlewellingShapeParams SHP_BH(double DBHOB, double HTTOT)
{
    double BH = 4.5;
    double H = HTTOT;

    // === Compute DMEDIAN & DFORM ===
    double DMEDIAN =
        1.6802000e+00 *
        std::pow(H - BH, 0.40850000 + 0.00169 * H);

    double DFORM = DBHOB / DMEDIAN - 1.0;

    // === U7, U8, U9 ===
    double U7 = -1.2726446e+00 - 4.8259438e-03 * H;
    double U9 = 1.8219470e-01;
    double U8 = 9.9000000e-01;

    // === U1–U5 ===
    double U1 = -1.5505171e+00 - 1.7174522e-02 * H;

    double U2 =
        2.7722769e-01
        - 0.0 * DFORM
        - 0.0 * std::log(H)
        + 0.0 * DFORM * H
        - 2.1540189e-01 * DBHOB;

    double U3 =
        2.0426515e+00
        + 0.0 * DFORM
        - 8.3434213e-01 * std::log(H);

    double U4 =
        -7.0000000e+00;

    double U5 = 7.7448837e+00;

    // === U6 ===
    double U6 =
        1.3766370e+00 - 4.7598661e-01 * DFORM;

    // === Clamping helper ===
    auto clamp = [](double& x, double lo, double hi) {
        if (x < lo) x = lo;
        if (x > hi) x = hi;
        };

    clamp(U1, -7.0, 7.0);
    clamp(U2, -7.0, 7.0);
    clamp(U3, -7.0, 7.0);
    clamp(U4, -7.0, 7.0);
    clamp(U5, -7.0, 7.1);

    if (U6 < 1.005)   U6 = 1.005;
    if (U6 > 100.0)   U6 = 100.0;

    clamp(U7, -7.0, 7.0);

    if (U8 > 0.99) U8 = 0.99;
    if (U9 > 0.3)  U9 = 0.3;
    if (U9 < 0.0)  U9 = 0.0;

    // === R1–R5 ===
    double R1 = std::exp(U1) / (1.0 + std::exp(U1));
    double R2 = std::exp(U2) / (1.0 + std::exp(U2));
    double R3 = std::exp(U3) / (1.0 + std::exp(U3));
    double R4 = std::exp(U4) / (1.0 + std::exp(U4));
    double R5 = 0.5 + 0.5 * (std::exp(U5) / (1.0 + std::exp(U5)));

    if (U5 > 7.0) R5 = 1.0;

    double A3 = U6;

    // === Height Ratio Parameters ===
    double RHI1 = std::exp(U7) / (1.0 + std::exp(U7));
    if (RHI1 > 0.5) RHI1 = 0.5;

    double RHLONGI = U9;
    double RHI2 = RHI1 + RHLONGI;

    double RHC = U8;
    if (RHC < RHI2 + 0.01)
        RHC = std::min(RHI2 + 0.01, (RHI2 + 1.0) / 2.0);

    // === Output Arrays ===
    FlewellingShapeParams out = { R1,R2,R3,R4,R5,A3,RHI1,RHI2,RHC,RHLONGI };

    return out;
}

// given 2 heights(hi and hj), estimate the correlation of the
// errors in the corresponding dib's

double COR_BH(double HTTOT, double H30, double HT2)
{
    // Coefficients from FORTRAN
    const double Q1 = -4.2141136e+00;
    const double Q2 = 3.6157646e+00;
    const double QS = 0.0;
    const double Q4 = -1.5164459e+00;
    const double Q5 = 2.8261064e-01;

    const double Q3 = QS - (Q1 + Q2);  // same as FORTRAN

    // If heights equal → perfect correlation
    if (H30 == HT2)
        return 1.0;

    // If either height = BH = 4.5
    if (H30 == 4.5 || HT2 == 4.5)
        return 0.5;

    // Order heights so h1 < h2
    double h1 = std::min(H30, HT2);
    double h2 = std::max(H30, HT2);

    double CORR = 0.0;

    // Case 1: both above BH
    if (h1 > 4.5)
    {
        double t3 = (h1 - 4.5) / (HTTOT - 4.5);
        double t4 = (h2 - 4.5) / (HTTOT - 4.5);

        double expr =
            Q1 * (t4 - t3)
            + Q2 * (t4 * t4 - t3 * t3) / 2.0
            + Q3 * (t4 * t4 * t4 - t3 * t3 * t3) / 3.0;

        double corr = std::exp(expr);

        if (corr > 0.999) corr = 0.999;

        return corr;
    }

    // Case 2: spans BH
    if (h2 > 4.5)
    {
        double t3 = (h2 - 4.5) / (HTTOT - 4.5);
        double t2 = (4.5 - h1) / 4.5;

        double expr =
            Q4 * t2 +
            Q1 * t3 +
            Q2 * (t3 * t3) / 2.0 +
            Q3 * (t3 * t3 * t3) / 3.0;

        double corr = Q5 * std::exp(expr);

        if (corr > 0.999) corr = 0.999;
        if (corr < -0.999) corr = -0.999;

        return corr;
    }

    // Case 3: both below BH
    double t2 = (4.5 - h2) / 4.5;
    double t1 = (4.5 - h1) / 4.5;

    CORR = std::exp(Q4 * (t1 - t2));
    return CORR;
}

// calculates standard error for 2 - point stem - form system
//       H     input    r * 8    section height(ft or meters)
//       SE    output   r * 8    standard error
//
//         Note : this follows Flewelling and Raynes(Part I), except :
//               special interpolation in 3 segments :
//                  1.  3.0 ft to 4.5 ft(linear on SE)
//                  2.  4.5 ft to BH + 0.1 * (Total height - BH) (linear on SE)
//                  3.  BH + 0.9 * (HTTOT - BH) to Totalht(quadradic on SE)

double VAR_BH(double DBHOB, double HTTOT, double HUP)
{
    double LVARHAT = 0.0;
    double VARHAT = 0.0;
    double SE;

    // Case 1: below BH
    if (HUP < 4.5)
    {
        LVARHAT =
            -1.5512542e+00
            - 0.82366251 * HUP
            + 0.10190777 * DBHOB;

        VARHAT = std::exp(LVARHAT);
    }
    else if (HUP > 4.5) // Case 2: above BH
    {
        double RH = (HUP - 4.5) / (HTTOT - 4.5);

        LVARHAT =
            (-7.2335700e+00 + 2.2333875e+00 * std::log(DBHOB)) +
            (2.5870429e+00 - 4.3950365e-02 * std::log(DBHOB)) *
            std::pow(RH, -5.7411597e-01 + 6.7914946e-01 * std::log(DBHOB));

        VARHAT = std::exp(LVARHAT);
    }
    else
    {
        // At breast height exactly
        SE = 0.0;
        return SE;
    }

    // Final SE = sqrt(VARHAT), unless VARHAT <= 0 (numerical guard)
    if (VARHAT > 0.0)
        SE = std::sqrt(VARHAT);
    else
        SE = 0.0;

    return SE;
}
