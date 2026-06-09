#include "FlewellingTaperModel_Ingy.h"
#include <string>
#include <string_view>
#include <algorithm>
#include <cmath>
#include <functional>
#include <stdexcept>
#include <limits>

FlewellingShapeParams shapeIngy(int JSP, const std::string_view GEOSUB,
    double DBH, double HT)
{
    FlewellingShapeParams out;

    int JRSP = JSP - 10;
    std::string_view geotemp = GEOSUB;

    auto F = F_shapeIngy[JRSP - 1];

    // -------- SPECIES / REGION LOGIC -----------
    if (GEOSUB != "00") {

        // Doug fir
        if (JSP == 11 && (GEOSUB == "15" || GEOSUB == "03")) {
            geotemp = "15";
            for (int IP = 0; IP <= 2; IP++)
                if (DFSUB[IP] == geotemp)
                    F[DFSUBF[IP]] = DFSUBC[IP];
        }

        // Engelmann spruce
        else if (JSP == 19 && GEOSUB == "15") {

            for (int IP = 0; IP <= 2; IP++)
                if (ESSUB[IP] == GEOSUB)
                    F[ESSUBF[IP]] = ESSUBC[IP];
        }

        // Grand fir / White fir
        else if (JSP == 13) {

            if (GEOSUB == "15" || GEOSUB == "03") {
                geotemp = "15";
            }
            else {
                geotemp = GEOSUB;

                for (int IP = 0; IP <= 13; IP++)
                    if (GFSUB[IP] == geotemp)
                        F[GFSUBF[IP]] = GFSUBC[IP];
            }
        }

        // Ponderosa pine
        else if (JSP == 14) {

            for (int IP = 0; IP <= 15; IP++)
                if (PPSUB[IP] == GEOSUB)
                    F[PPSUBF[IP]] = PPSUBC[IP];
        }

        // Western larch
        else if (JSP == 12) {

            for (int IP = 0; IP <= 20; IP++)
                if (WLSUB[IP] == GEOSUB)
                    F[WLSUBF[IP]] = WLSUBC[IP];
        }
    }

    // ------------- EQUATIONS ----------------
    double DMEDIAN = F[10] * pow(HT - 4.5, (F[11] + F[12] * HT));
    double DFORM = DBH / DMEDIAN - 1.0;

    double U7 = F[13] + F[14] * log(HT) + F[15] * DFORM;

    double U9T = F[18] + F[19] * log(HT) + F[20] * DFORM;
    //U9T = max(-7.0, min(U9T, 7.0));
    U9T = std::clamp(U9T, -7.0, 7.0);
    double U9 = F[16] * exp(U9T) / (1.0 + exp(U9T));

    double U8 = F[21] + F[22] * HT
        + F[23] * log(HT) + F[24] * DFORM;

    double U1 = F[25] + F[26] * log(HT)
        + F[27] * DFORM + F[28] * DFORM * log(HT);

    double U2 = F[29] + F[30] * DFORM + F[31] * log(HT)
        + F[32] * DFORM * log(HT) + F[33] * DBH;

    double U3;
    if (JSP == 15) {
        U3 = F[34] + F[35] * DFORM
            + F[36] * (1.0 - exp(F[37] * HT));
    }
    else {
        U3 = F[34] + F[35] * DFORM
            + F[36] * log(HT) + F[37] * log(HT) * DFORM;
    }

    double U4 = F[38] + F[39] * DFORM + F[40] * log(HT)
        + F[41] * DBH;

    double U5 = F[42] + F[43] * log(HT);
    double U6 = F[45] + F[46] * DFORM + F[47] * log(HT);

    // limits
    auto clip = [](double& x, double lo, double hi) {
        if (x < lo) x = lo;
        if (x > hi) x = hi;
        };

    clip(U1, -7.0, 7.0);
    clip(U2, -7.0, 7.0);
    clip(U3, -7.0, 7.0);
    clip(U4, -7.0, 7.0);
    clip(U5, -7.0, 7.1);

    if (U6 < 1.005) U6 = 1.005;
    if (U6 > 10.0)  U6 = 10.0;

    clip(U7, -7.0, 7.0);

    if (U8 > 0.99) U8 = 0.99;
    U9 = std::max(0.0, std::min(U9, 0.3));

    // geometric parameters
    double R1 = exp(U1) / (1.0 + exp(U1));
    double R2 = exp(U2) / (1.0 + exp(U2));
    double R3 = exp(U3) / (1.0 + exp(U3));
    double R4 = exp(U4) / (1.0 + exp(U4));

    double R5;
    if (U5 <= 7.0)
        R5 = 0.5 + 0.5 * exp(U5) / (1.0 + exp(U5));
    else
        R5 = 1.0;

    double A3 = U6;

    double RHI1 = exp(U7) / (1.0 + exp(U7));
    if (RHI1 > 0.5) RHI1 = 0.5;

    double RHLONGI = U9;
    double RHI2 = RHI1 + RHLONGI;

    double RHC = U8;
    if (RHC < RHI2 + 0.01)
        RHC = std::min(RHI2 + 0.01, (RHI2 + 1.0) / 2.0);

    // pack outputs
    out = { R1,R2,R3,R4,R5,A3,RHI1,RHI2,RHC,RHLONGI };

    return out;
}

//***********************************************************************
//     given 2 heights(hi and hj), estimate the correlation of the
//       Z errors in the corresponding dib's
//               NW Taper coop #2, (INGY, east - side 1995 - 1996)
//       coefficients 81 - 85

double COR_C2(int JSP, const std::string_view geosub, double TOTALH, double HI, double HJ)
{
    // Coefficient table V(5,13)
    static const double V[5][13] = {
        { -4.8680838e+00, -4.0872513e+00, -5.3258004e+00, -6.4087039e+00, -6.3095389e+00,
          -3.7292867e+00, -5.5386062e+00, -4.8456403e+00, -4.1694935e+00, -5.5353327e+00,
          -4.6723264e+00, -5.2706199e+00, -6.3303605e+00 },
        {  1.4663798e+01,  9.8400990e+00,  1.8207797e+01,  1.8426727e+01,  1.6848451e+01,
           9.5204603e+00,  1.3890065e+01,  1.6415000e+01,  1.3158633e+01,  2.0954955e+01,
           1.5282969e+01,  1.8147772e+01,  1.8704116e+01 },
        { -4.8216320e+00, -3.8399599e+00, -4.6314647e+00, -5.0623505e+00, -4.1147697e+00,
          -3.1268123e+00, -3.6169043e+00, -5.0265184e+00, -4.6716770e+00, -6.6258696e+00,
          -5.2893666e+00, -5.0616937e+00, -5.8215704e+00 },
        { -1.7800924e+00, -2.4301984e+00, -1.2177523e+00, -2.1710945e+00, -1.8701241e+00,
          -1.2917494e+00, -1.0135346e+00, -1.0653677e+00, -1.0569956e+00, -9.8999018e-01,
          -1.4847301e+00, -1.3304567e+00, -1.7448762e+00 },
        { -2.0276812e-01,  3.5024803e-01, -1.1309926e-01,  2.9814748e-01,  6.5264446e-01,
          -2.6280112e-01,  4.0538989e-02, -4.2501392e-03, -7.7545519e-01,  2.1262294e-01,
          -3.7828956e-01,  6.7322419e-01,  3.4012396e-01 }
    };

    const double BH = 4.5;

    int JSPR = JSP - 10;

    if (JSP == 3 && (geosub == "15" || geosub == "03")) JSPR = 12;
    if (JSP == 4 && geosub == "15") JSPR = 13;

    // Convert to 0‑based index
    JSPR -= 1;

    double Q1 = V[0][JSPR];
    double Q2 = V[1][JSPR];
    double QS = V[2][JSPR];
    double Q4 = V[3][JSPR];
    double Q5 = V[4][JSPR];

    double Q3 = QS - (Q1 + Q2);

    if (HI == HJ)
        return 1.0;

    if (HI == BH || HJ == BH)
        return 0.5;

    double h1 = std::min(HI, HJ);
    double h2 = std::max(HI, HJ);

    double CORR = 0.0;

    if (h1 > BH)
    {
        double t3 = (h1 - BH) / (TOTALH - BH);
        double t4 = (h2 - BH) / (TOTALH - BH);

        CORR = std::exp(
            Q1 * (t4 - t3)
            + Q2 * (t4 * t4 - t3 * t3) / 2.0
            + Q3 * (t4 * t4 * t4 - t3 * t3 * t3) / 3.0
        );
    }
    else if (h2 > BH)
    {
        double t3 = (h2 - BH) / (TOTALH - BH);
        double t2 = (BH - h1) / BH;

        CORR = Q5 * std::exp(
            Q4 * t2
            + Q1 * t3
            + Q2 * t3 * t3 / 2.0
            + Q3 * t3 * t3 * t3 / 3.0
        );
    }
    else
    {
        double t2 = (BH - h2) / BH;
        double t1 = (BH - h1) / BH;

        CORR = std::exp(Q4 * (t1 - t2));
    }

    return CORR;
}

//***********************************************************************
//                Variance of dib  for species assuming lognormal errors.
//                NW Coop #2, INGY
//
//                         J.W.Flewelling, July, 1996
//
//      H      input   R * 4   Section height, for h NE breast height.
//      SE_LNX output  R * 4   SQRT{ VAR[ln(dib)] }

double VAR_C2(int JSP, const std::string_view geosub, double DBH, double HT, double H)
{
    // F(12,13) but only rows 10-12 are populated
    static const double F[12][13] = {
        {0.0}, {0.0}, {0.0}, {0.0}, {0.0}, {0.0}, {0.0}, {0.0}, {0.0},
        // Row 10
        {0.1975,   0.05580,  0.61910,  0.60780,  0.27880,
         1.20030,  0.83820,  0.49340,  0.21020,  0.51943,
         0.61910,  0.46115714, 0.0},
         // Row 11
         {0.9663, 12.6350, 0.6200, 0.6853, 0.8057,
          0.3837, 0.5130, 0.6704, 0.9154, 0.7109,
          0.6200, 0.76559415, 0.0},
          // Row 12
          {0.000367, -0.000316, 0.001106, 0.000896, 0.000603,
           0.002361, 0.001643, 0.000723, 0.000379, 0.000503,
           0.001106, 0.00034060552, 0.0}
    };

    // V(16,13)
    static const double V[16][13] = {
        {-8.7861916e+01,-5.2217635e+01,-7.7600152e+01,-5.7324396e+01,-8.3307969e+01,
         -7.5750015e+01,-5.3661720e+01,-7.4781469e+01,-9.7862646e+01,-1.6236989e+02,
         -1.0424358e+02,-8.7229255e+01,0.0},
        {3.8942378e+00,-2.3364372e+00,-6.8002365e-01,-2.5574556e+00, 1.4499630e+00,
         -7.4534057e-01,-6.3960656e+00,-5.6696621e-01, 5.5558106e+00, 1.5325659e+01,
          5.1847185e+00,-4.9823584e-01,0.0},
        {2.4151069e+01,1.0581464e+00,3.0281870e+01,2.0750963e+01,2.2757330e+01,
         2.8421868e+01,2.3393943e+01,2.5621584e+01,2.4780360e+01,2.8717816e+01,
         2.3862124e+01,3.5238539e+01,0.0},
        {8.6871264e+01,7.6117971e+01,8.5370762e+01,3.7746335e+01,9.6926943e+01,
         5.7478382e+01,4.6136800e+01,3.0300072e+01,1.0225780e+02,1.4230027e+02,
         9.5984104e+01,9.3969507e+01,0.0},
        {-1.2932895e+01,-1.0588704e+01,-1.0788092e+01,-1.1127807e+00,-1.3015806e+01,
         -3.6066482e+00,4.5601491e-01,2.4739882e+00,-1.6303736e+01,-2.2483676e+01,
         -1.1852491e+01,-1.1480943e+01,0.0},
        {1.0448179e+01,1.1469169e+01,9.4569668e+00,1.3005455e+01,1.1174823e+01,
         1.3819886e+01,1.0292839e+01,1.0209356e+01,1.0660142e+01,2.8714841e+00,
         7.7096585e+00,1.1385876e+01,0.0},
        {0.0,1.6914337e+00,0.0,0.0,0.0,
         0.0,0.0,0.0,0.0,0.0,
         0.0,-1.0,0.0},
        {0.096526647,0.35117997,0.023088981,0.056372266,0.028164929,
         0.028223671,0.056691981,0.051477606,0.046270096,0.082144971,
         0.013623505,0.046648531,0.0},
        {0.13685097,10.0,8.7032708, -1.5253253,10.0,
         -10.0,-2.7453440,-4.1482008,-0.93496678,0.54360298,
          10.0,6.3740406,0.0},
        {-4.4306740,0.39438121,-4.1523979,-4.0439166,-3.9492882,
         -3.6858293,-3.9145604,-3.8330247,-4.2025655,-4.5567161,
         -3.7634939,-5.0851998,0.0},
        {-6.3677273e+01,-6.8717202e+01,-5.3889325e+01,-4.6449612e+01,-1.9496333e+01,
         -6.3488088e+01,-5.0792677e+01,-9.2572316e+01,-4.2221193e+01,-4.2759971e+01,
         -3.5299059e+01,-3.5671833e+01,0.0},
        {-0.28346734,-0.40369684,-7.7824215,-6.2894842,-14.500534,
         -2.2907055,-4.4971305,2.5930838,-7.2661816,-7.0136279,
         -21.046362,-8.5252043,0.0},
        {0.30403741,5.7719712,9.3680884,-0.85690977,-2.6604864,
         6.2337257,-1.2039165,3.9072598,-5.2578751,-2.0865044,
         12.369575,-3.4854118,0.0},
        {23.458454,-4.8188520,12.692083,-0.57316015,-18.586386,
         32.002232,-21.584487,26.542139,-45.541436,8.0246718,
         -49.61,-8.87825360,0.0},
        {1.5233441,7.7458363,5.2691142,5.6201552,12.454997,
         1.4868195,13.472605,1.1447332,20.352324,4.6365564,
         28.054351,8.3369209,0.0},
        {15.82119,29.09715,14.583838,10.348040,12.241344,
         28.488670,35.584299,10.846468,13.481250,20.164054,
         6.8522936,14.868045,0.0}
    };

    double SE_LNX;
    double BH = 4.5;

    int JSPR = JSP - 10;
    if (JSP == 3 && (geosub == "15" || geosub == "03"))
        JSPR = 12;

    // Convert to 0-based index
    JSPR -= 1;

    // Extract coefficients
    double VA00 = V[0][JSPR];
    double VA01 = V[1][JSPR];
    double VA02 = V[2][JSPR];
    double VB00 = V[3][JSPR];
    double VB01 = V[4][JSPR];
    double VC0 = V[5][JSPR];

    double VX1 = V[6][JSPR];
    double VX2 = V[7][JSPR];
    double VX3 = V[8][JSPR];

    double VA03 = V[9][JSPR];
    double VE00 = V[10][JSPR];
    double VE01 = V[11][JSPR];
    double VE02 = V[12][JSPR];
    double VF00 = V[13][JSPR];
    double VF01 = V[14][JSPR];
    double VG0 = V[15][JSPR];

    double DMEDIAN =
        F[9][JSPR] * std::pow(HT - BH, F[10][JSPR] + F[11][JSPR] * HT);

    double DRATIO = DBH / DMEDIAN;
    double LOGHT = std::log(HT);

    double VA0 = VA00 + VA01 * LOGHT + VA02 * DRATIO + VA03 * LOGHT * DRATIO;
    double VB0 = VB00 + VB01 * LOGHT;

    double VE0 = VE00 + VE01 * LOGHT + VE02 * DRATIO;
    double VF0 = VF00 + VF01 * LOGHT;
    double VC = VC0 + VX1 * LOGHT;

    bool VMOD1 = false;
    bool VMOD2 = false;
    double XU = 0.0, X = 0.0, STD_FRAC = 1.0;

    double LVARHAT = 0.0;
    double VARHAT = 0.0;

    if (H < BH)
    {
        XU = (BH - H) / BH;

        if (XU < 0.111) {
            VMOD1 = true;
            X = XU;
            XU = 0.111;
        }

        LVARHAT = VE0 + VF0 * std::pow(XU, VG0);

        LVARHAT = std::clamp(LVARHAT, -15.0, 15.0);

        VARHAT = std::exp(LVARHAT);
        SE_LNX = std::sqrt(VARHAT);

        if (VMOD1) SE_LNX *= (X / XU);
    }
    else if (H == BH)
    {
        SE_LNX = 0.0;
    }
    else if (H < HT)
    {
        XU = (H - BH) / (HT - BH);

        if (XU < 0.02) {
            VMOD2 = true;
            X = XU;
            XU = 0.02;
            STD_FRAC = X / XU;
        }
        else if (XU > 0.96) {
            VMOD2 = true;
            X = XU;
            XU = 0.96;
            STD_FRAC = 1.0;
        }

        LVARHAT = VA0 + VB0 * std::pow(XU, VC)
            + VX2 * std::pow(HT / 50.0, VX3) / (1.0 - XU);

        LVARHAT = std::clamp(LVARHAT, -15.0, 15.0);

        VARHAT = std::exp(LVARHAT);
        SE_LNX = std::sqrt(VARHAT);

        if (VMOD2) SE_LNX *= STD_FRAC;
    }
    else
    {
        VARHAT = 1.0;
        SE_LNX = 1.0;
    }

    return SE_LNX;
}

//***********************************************************************
//                   given Diameter at breat height, outside bark
//                   calculate double bark thickness at breast height.
//                   NW Taper coop 2, INGY(east - side)
//              COEFFICIENT FILE - BARKBHC2.COE

//     DBH        INPUT     R * 4  Diameter(o.b.) at Breast height(inches)
//     TOTHT      INPUT     R * 4  Total tree height(ft)
//     FDBT_BC2   RETURNED  R * 4  Double bark thickness

double FDBT_C2(int JSP, const std::string_view GEOSUB, double DBH, double TOTHT)
{
    // -------------------------
    // Coefficient arrays A(6,11)
    // -------------------------
    static const double A[6][11] = {
        {-2.002013, -1.757421, -2.619824, -1.587349, -1.826117,
         -2.200198, -2.127891, -3.018987, -2.240592, -2.340455,
         -2.520019},
        {0.049128, -0.001908, 0.030658, 0.010579, -0.033403,
         -0.070784, 0.127621, 0.062254, -0.030430, -0.035228,
         -0.024743},
        {-0.000358, -0.000108, 0.0, -0.000289, -0.005038,
         0.0, -0.002670, -0.001539, -0.000011, 0.0,
         0.0},
        {-0.003961, -0.000348, -0.001651, -0.001853, -0.020658,
         0.000136, -0.021932, -0.007301, -0.006039, -0.007820,
         -0.005114},
        {-0.0001318, 0.0000732, -0.0000951, -0.0000888, 0.0017612,
         0.0003750, 0.0002920, 0.0001524, 0.0002215, 0.0004648,
         0.0003716},
        {0.0, 0.0, 0.0, 1.0, -1.0,
         1.0, 1.0, 0.0, 1.0, 1.0,
         0.0}
    };

    // --------------------------------
    // AR(23,11) regional replacement
    // Only rows 11–23 have coefficients
    // --------------------------------
    static const double AR[23][11] = {
        {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, {0}, // rows 1–10 unused
        {-2.136, 0.0,   -2.825, 0.0,   0.0,   0.0, -2.189, 0.0,  -2.400, -2.275, 0.0},
        {-2.136, 0.0,   -2.741, 0.0,   0.0,   0.0, -2.220, 0.0,  -2.210, -2.664, 0.0},
        {-2.136, 0.0,   -2.384, 0.0,   0.0,   0.0,  0.0,   0.0,  -2.199, -2.368, 0.0},
        {-2.136, 0.0,   -2.654, 0.0,   0.0,   0.0, -2.073, 0.0,  -2.328, -2.232, 0.0},
        {-2.136, 0.0,    0.000, 0.0,   0.0,   0.0,  0.0,   0.0,   0.000,  0.000, 0.0},
        {0}, {0}, {0}, {0}, {0},    // rows 16–20
        {-1.813, 0.0,    0.000, 0.0,   0.0,   0.0,  0.00,  0.0,  -2.278, 0.000, 0.0},
        {-1.977, 0.0,    0.000, 0.0,   0.0,   0.0, -1.952, 0.0,  -2.214, 0.000, 0.0},
        {-1.969, 0.0,    0.000, 0.0,   0.0,   0.0, -2.290, 0.0,  -2.198, 0.000, 0.0}
    };

    int JSPR = JSP - 10;  // FORTRAN → C++ species index
    // Index conversion for 0-based C++ arrays
    int s = JSPR - 1;

    // ---------------------------------------------
    // Special regional override formulas
    // ---------------------------------------------
    if (s == 0 && GEOSUB == "15") {
        return DBH - (-0.06933188 + 0.8981755 * DBH);
    }
    if (JSP == 1 && GEOSUB == "03") {
        return DBH - (-0.008020069 + 0.853465707 * DBH);
    }
    if (s == 2 && GEOSUB == "15") {
        return DBH - (0.35929539 + 0.9213101 * DBH - 0.2543261 * std::log(DBH));
    }
    if (JSP == 3 && GEOSUB == "03") {
        return DBH - (-0.2137925258 + 0.880382786 * DBH);
    }
    if (s == 8 && GEOSUB == "15") {
        return DBH - (-0.2110537 + 0.9682267 * DBH - 0.0053090 * std::log(DBH));
    }

    // ------------------------------------------------
    // Regional replacement of A00
    // ------------------------------------------------
    double A00;
    if (GEOSUB == "00" ) {
        A00 = A[0][s];
    }
    else {
        int IREGION = std::stoi(std::string(GEOSUB));
        double val = AR[IREGION - 1][s];
        A00 = (val != 0.0 ? val : A[0][s]);
    }

    // ----------------------------------------------------
    // Extrapolation control on DBH
    // ----------------------------------------------------
    double DBHuse = DBH;

    if (A[2][s] < 0.0 && A[5][s] == 0.0)
    {
        double DBHmax = -(A[1][s] + A[4][s] * TOTHT) / (2.0 * A[2][s]);
        DBHuse = std::min(DBH, DBHmax);
    }
    else if (A[2][s] < 0.0 && A[5][s] == -1.0)
    {
        double DBHmin = -(A[1][s] + A[4][s] * TOTHT) / (2.0 * A[2][s]);
        DBHuse = std::max(DBH, DBHmin);
    }

    // ----------------------------------------------------
    // Compute logit(Y2)
    // ----------------------------------------------------
    double Y2 =
        A00 +
        A[1][s] * DBHuse +
        A[2][s] * DBHuse * DBHuse +
        A[3][s] * TOTHT +
        A[4][s] * TOTHT * DBHuse;

    // Clamp Y2 to [-8, 8]
    Y2 = std::clamp(Y2, -8.0, 8.0);

    // Convert logit → ratio
    double ex = std::exp(Y2);
    double RATIO = ex / (1.0 + ex);

    // Final double bark thickness
    return RATIO * DBH;
}

// ------------------------------------------------------
// Modern Brent's Method Root Solver (clean replacement)
// ------------------------------------------------------
double brent_root(double x1, double x2,
    std::function<double(double)> f,
    double tol)
{
    const double EPS = std::numeric_limits<double>::epsilon();
    double a = x1, b = x2, c = x2;
    double fa = f(a), fb = f(b), fc = fb;

    if (fa * fb > 0.0)
        throw std::runtime_error("Brent: root not bracketed");

    for (int iter = 0; iter < 100; ++iter) {
        if ((fb > 0 && fc > 0) || (fb < 0 && fc < 0)) {
            c = a;
            fc = fa;
        }

        if (std::abs(fc) < std::abs(fb)) {
            a = b;  b = c;  c = a;
            fa = fb; fb = fc; fc = fa;
        }

        double tol1 = 2 * EPS * std::abs(b) + 0.5 * tol;
        double xm = 0.5 * (c - b);

        if (std::abs(xm) <= tol1 || fb == 0.0)
            return b;

        static double d = 0.0;
        static double e = 0.0;

        if (std::abs(e) >= tol1 && std::abs(fa) > std::abs(fb)) {
            double s = fb / fa;
            double p, q;

            if (a == c) {
                p = 2 * xm * s;
                q = 1 - s;
            }
            else {
                double q1 = fa / fc;
                double r = fb / fc;
                p = s * (2 * xm * q1 * (q1 - r) - (b - a) * (r - 1));
                q = (q1 - 1) * (r - 1) * (s - 1);
            }

            if (p > 0) q = -q;
            p = std::abs(p);

            if (2 * p < std::min(3 * xm * q - std::abs(tol1 * q), std::abs(e * q))) {
                e = d;
                d = p / q;
            }
            else {
                d = xm;
                e = xm;
            }
        }
        else {
            d = xm;
            e = xm;
        }

        a = b;
        fa = fb;

        if (std::abs(d) > tol1)
            b += d;
        else
            b += (xm >= 0 ? tol1 : -tol1);

        fb = f(b);
    }

    return b;
}

// =========================================================
//         B R K   C 2   -   I N G Y   C O O P 2
// =========================================================

// =========================================================
// BRK_UPA2  (no GOTOs, Brent-based root solving)
// =========================================================
double BRK_UPA2(int JSP, double DBH, double TOTALH,
    double DBT_BH, double h, double dib)
{
    // PART 1 — initial guess from height-only model
    double R0 = BRK_UPH2(JSP, DBH, TOTALH, DBT_BH, h);

    auto f = [&](double R)
        {
            double DOB = dib / (1.0 - R);
            double RATIO = BRK_UPD2(JSP, DBH, DBT_BH, h, DOB);
            return (1.0 - RATIO) * DOB - dib;
        };

    double R1 = R0;
    double f1 = f(R1);

    // BRACKETING SECTION — EXACT FORTRAN LOGIC
    double R2 = R0;
    double f2;

    // Case 1: f1 > 0 → search downward by 0.7
    if (f1 > 0.0) {
        while (true) {
            R2 = 0.7 * R2;
            if (R2 <= 0.0001) break; // safety cap
            f2 = f(R2);
            if (f2 <= 0.0) break;
        }
    }
    else {
        // Case 2: upward search by +0.05 until f changes sign
        int npass = 0;
        while (true) {
            npass++;
            if (npass > 4 && R2 > 0.5) {
                // fallback to using the starting estimate
                return BRK_UPC2(JSP, h, R0, R0);
            }
            R2 += 0.05;
            if (R2 >= 0.9999) break;
            f2 = f(R2);
            if (f2 >= 0.0) break;
        }
    }

    // If bracketing failed (still same sign)
    if (f1 * f2 > 0.0) {
        return BRK_UPC2(JSP, h, R0, R0);
    }

    // USE BRENT to refine root between R1 and R2
    double Rroot = brent_root(R1, R2, f, 0.0001 * dib);

    // Final combination step
    return BRK_UPC2(JSP, h, Rroot, R0);
}


// =========================================================
// BRK_UPB2  (Combined DOB + H models)
// =========================================================
double BRK_UPB2(int JSP, double DBH, double TOTALH,
    double DBT_BH, double h, double dob)
{
    double r1 = BRK_UPD2(JSP, DBH, DBT_BH, h, dob);
    double r2 = BRK_UPH2(JSP, DBH, TOTALH, DBT_BH, h);
    return BRK_UPC2(JSP, h, r1, r2);
}


// =========================================================
// BRK_UPC2  (combine ratio_1 and ratio_2)
// =========================================================
double BRK_UPC2(int JSP, double h, double r1, double r2)
{
    static const double WW[11] =
    { 0.76,0.46,0.68,0.58,1.00,0.93,0.84,0.80,0.67,0.68,0.90 };

    int s = JSP - 10;

    if (h > 4.5) {
        double v1_1 = 1.0 - (1.0 - r1) * (1.0 - r1);
        double v1_2 = 1.0 - (1.0 - r2) * (1.0 - r2);
        double v1 = WW[s] * v1_1 + (1 - WW[s]) * v1_2;
        return 1.0 - std::sqrt(1.0 - v1);
    }
    return r1;
}


// =========================================================
// BRK_UPD2  (DOB-dependent model)
// =========================================================
double BRK_UPD2(int JSP, double DBH, double DBT_BH,
    double h, double DOB)
{
    static const double BK[11][8] = {
        {-0.28511,0.006013,0.00650,0, 2.7986,6.6873,0.04244,0},
        {-0.22906,-0.000569,-0.03472,0,3.2514,7.2262,0.03676,0},
        {-0.11651,0.000499,0.04889,0,4.1898,26.5988,0.05871,0},
        {-0.19942,-0.000449,0.03997,0,2.8495,4.2885,0.09823,0},
        {-0.31811,0.025982,-0.21811,0,3.4490,10.9047,0.06326,0},
        { 0.01581,-0.002937,0.09659,1,0.0462,-0.0002,0.23015,-5.79308},
        {-0.07488,-0.014725,0.26396,1,0.0311,-0.0001,0.18066,-3.32865},
        {-0.04445,-0.006854,0.17022,0,22.7604,22.9213,0.05486,0},
        {-0.05520,-0.000681,0.07176,0,119.8049,30.0000,0.08307,0},
        {-0.04229,-0.005308,0.16093,0,1000.0,0.0,0.08730,0},
        {-0.12405,-0.001084,0.17016,0,41.4671,30.0000,0.07944,0}
    };

    int s = JSP - 10;
    double CP1 = BK[s][0], CP2 = BK[s][1], CP3 = BK[s][2];
    double B1 = BK[s][3], B2 = BK[s][4], B3 = BK[s][5];
    double B4 = BK[s][6], B5 = BK[s][7];

    double DBH_IB = DBH - DBT_BH;

    if (h < 4.5) {
        double P = CP1 + CP2 * DBH + CP3 * DBT_BH;
        P = std::max(P, -0.9);

        double DIB =
            DOB * (DBH_IB / DBH) * std::pow(DOB / DBH, P);

        DIB = std::max(DIB, 0.5 * DOB);
        double DIBmax = std::max(0.99, DBH_IB / DBH) * DOB;
        DIB = std::min(DIB, DIBmax);

        return (DOB - DIB) / DOB;
    }

    // Above BH
    if (B1 == 0.0) {
        double DR = std::min(DOB / DBH, 1.0);
        if (DR < 0.07) DR = 0.07;

        double DBT =
            DBT_BH * (
                (DR * (B2 - 1) / (B2 - std::pow(DR, B3))) -
                ((std::pow(DR, B4) - 1) / DBT_BH)
                );

        DBT = std::min(DBT, 0.5 * DOB);
        double DBTmin = std::min(0.01, DBT_BH / DBH) * DOB;
        DBT = std::max(DBT, DBTmin);

        return DBT / DOB;
    }

    // Alternative above-BH model
    double ratioBH = DBT_BH / DBH;
    double P = B2 + B3 * DBH + B4 * ratioBH + B5 * ratioBH * ratioBH;
    P = std::max(P, -0.9);

    double DIB = DOB * (DBH_IB / DBH) * std::pow(DOB / DBH, P);
    DIB = std::max(DIB, 0.5 * DOB);
    double DIBmax = std::max(0.99, DBH_IB / DBH) * DOB;
    DIB = std::min(DIB, DIBmax);

    return (DOB - DIB) / DOB;
}


// =========================================================
// BRK_UPH2  (height-dependent model)
// =========================================================
double BRK_UPH2(int JSP, double DBH, double TOTALH,
    double DBT_BH, double h)
{
    static const double BK[11][5] = {
        {-2.1311,-0.3407,-0.043016,1.99440,0.65442},
        {-9.9977, 7.4501, 0.0,     1.96902,0.45589},
        { 5.8731,-8.1187,-0.000535,2.70954,0.96775},
        {20.0,  -22.0641,-0.000667,1.85483,0.86883},
        {-2.1180,0.0,     0.0,     2.90737,1.09057},
        {20.0,  -20.7223,-0.002407,1.53573,1.46826},
        {-0.2539,0.0,     0.0,     1.08666,3.0},
        {-0.5571,0.0,     0.0,     3.54313,3.0},
        { 0.3963,-0.4524,-0.093202,2.14604,3.0},
        { 0.7086,0.0,     0.0,     2.20383,2.99991},
        { 0.3425,0.0,     0.0,     2.00807,3.0}
    };

    int s = JSP - 10;

    double RJ1 = BK[s][0], RJ2 = BK[s][1], RJ3 = BK[s][2];
    double RJ4 = BK[s][3], RJ5 = BK[s][4];

    double X = (h > 0.0) ? std::pow(h / TOTALH, RJ5) : 0.0;
    double XBH = std::pow(4.5 / TOTALH, RJ5);

    double ratio =
        (DBT_BH / DBH) *
        (1.0 +
            (RJ1 + RJ2 * std::exp(RJ3 * DBH)) * (X - XBH) +
            RJ4 * (X * X - XBH * XBH));

    ratio = std::min(ratio, 0.5);
    double rmin = std::min(0.01, DBT_BH / DBH);
    ratio = std::max(ratio, rmin);

    return ratio;
}
