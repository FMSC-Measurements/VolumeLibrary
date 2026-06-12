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
    static const double F[12][3] = {
        // 1. Douglas Fir
        {0.19750000e+00, 0.96630000e+00, 0.36700000e-03},

        // 2. Western Larch
        {0.55800000e-01, 0.12635000e+01, -0.31600000e-03},

        // 3. Grand Fir
        {0.61910000e+00, 0.62000000e+00, 0.11060000e-02},

        // 4. Ponderosa Pine
        {0.60780000e+00, 0.68530000e+00, 0.89600000e-03},

        // 5. Lodgepole Pine
        {0.27880000e+00, 0.80570000e+00, 0.60300000e-03},

        // 6. Western Red Cedar
        {0.12003000e+01, 0.38370000e+00, 0.23610000e-02},

        // 7. Mountain Hemlock
        {0.83820000e+00, 0.51300000e+00, 0.16430000e-02},

        // 8. White Pine
        {0.49340000e+00, 0.67040000e+00, 0.72300000e-03},

        // 9. Engelmann Spruce
        {0.21020000e+00, 0.91540000e+00, 0.37900000e-03},

        // 10. Subalpine Fir
        {0.51943000e+00, 0.71090000e+00, 0.50300000e-03},

        // 11. Balsam (BC)
        {0.61910000e+00, 0.62000000e+00, 0.11060000e-02},

        // 12. Grand Fir (Central Idaho Taper)
        {0.46115714e+00, 0.76559415e+00, 0.34060552e-03}
    };

    static const double V[12][16] = {
        // 1. Douglas Fir
        {-0.87861916e+01, 0.38942378e+00, 0.24151069e+01, 0.86871264e+01,
         -0.12932895e+01, 0.10448179e+01, 0.00000000e+00, 0.96526647e-01,
          0.13685097e+00, -0.44306740e+00, -0.63677273e+01, -0.28346734e+00,
          0.30403741e+00, 0.23458454e+01, 0.15233441e+00, 0.15821190e+01},

          // 2. Western Larch
          {-0.52217635e+01, -0.23364372e+00, 0.10581464e+00, 0.76117971e+01,
           -0.10588704e+01, 0.11469169e+01, 0.16914337e+00, 0.35117997e-01,
            0.10000000e+01, 0.39438121e-01, -0.68717202e+01, -0.40369684e-01,
            0.57719712e+00, -0.48188520e+00, 0.77458363e+00, 0.29097150e+01},

            // 3. Grand Fir
            {-0.77600152e+01, -0.68002365e-01, 0.30281870e+01, 0.85370762e+01,
             -0.10788092e+01, 0.94569668e+00, 0.00000000e+00, 0.23088981e-01,
              0.87032708e+00, -0.41523979e+00, -0.53889325e+01, -0.77824215e+00,
              0.93680884e+00, 0.12692083e+01, 0.52691142e+00, 0.14583838e+01},

              // 4. Ponderosa Pine
              {-0.57324396e+01, -0.25574556e+00, 0.20750963e+01, 0.37746335e+01,
               -0.11127807e+00, 0.13005455e+01, 0.00000000e+00, 0.56372266e-01,
               -0.15253253e+00, -0.40439166e+00, -0.46449612e+01, -0.62894842e+00,
               -0.85690977e-01, -0.57316015e-01, 0.56201552e+00, 0.10348040e+01},

               // 5. Lodgepole Pine
               {-0.83307969e+01, 0.14499630e+00, 0.22757330e+01, 0.96926943e+01,
                -0.13015806e+01, 0.11174823e+01, 0.00000000e+00, 0.28164929e-01,
                 0.10000000e+01, -0.39492882e+00, -0.19496333e+01, -0.14500534e+01,
                -0.26604864e+00, -0.18586386e+01, 0.12454997e+01, 0.12241344e+01},

                // 6. Western Red Cedar
                {-0.75750015e+01, -0.74534057e-01, 0.28421868e+01, 0.57478382e+01,
                 -0.36066482e+00, 0.13819886e+01, 0.00000000e+00, 0.28223671e-01,
                 -0.10000000e+01, -0.36858293e+00, -0.63488088e+01, -0.22907055e+00,
                  0.62337257e+00, 0.32002232e+01, 0.14868195e+00, 0.28488670e+01},

                  // 7. Mountain Hemlock
                  {-0.53661720e+01, -0.63960656e+00, 0.23393943e+01, 0.46136800e+01,
                    0.45601491e-01, 0.10292839e+01, 0.00000000e+00, 0.56691981e-01,
                   -0.27453440e+00, -0.39145604e+00, -0.50792677e+01, -0.44971305e+00,
                   -0.12039165e+00, -0.21584487e+01, 0.13472605e+01, 0.35584299e+01},

                   // 8. White Pine
                   {-0.74781469e+01, -0.56696621e-01, 0.25621584e+01, 0.30300072e+01,
                     0.24739882e+00, 0.10209356e+01, 0.00000000e+00, 0.51477606e-01,
                    -0.41482008e+00, -0.38330247e+00, -0.92572316e+01, 0.25930838e+00,
                     0.39072598e+00, 0.26542139e+01, 0.11447332e+00, 0.10846468e+01},

                     // 9. Engelmann Spruce
                     {-0.97862646e+01, 0.55558106e+00, 0.24780360e+01, 0.10225780e+02,
                      -0.16303736e+01, 0.10660142e+01, 0.00000000e+00, 0.46270096e-01,
                       0.93496678e+00, -0.42025655e+00, -0.42221193e+01, -0.72661816e+00,
                      -0.52578751e+00, -0.45541436e+01, 0.20352324e+01, 0.13481250e+01},

                      // 10. Subalpine Fir
                      {-0.16236989e+02, 0.15325659e+01, 0.28717816e+01, 0.14230027e+02,
                       -0.22483676e+01, 0.28714841e+00, 0.00000000e+00, 0.82144971e-01,
                        0.54360298e+00, -0.45567161e+00, -0.42759971e+01, -0.70136279e+00,
                       -0.20865044e+00, 0.80246718e+00, 0.46365564e+00, 0.20164054e+01},

                       // 11. Balsam (BC)
                       {-0.10424358e+02, 0.51847185e+00, 0.23862124e+01, 0.95984104e+01,
                        -0.11852491e+01, 0.77096585e+00, 0.00000000e+00, 0.13623505e-01,
                         0.10000000e+01, -0.37634939e+00, -0.35299059e+01, -0.21046362e+01,
                         0.12369575e+01, -0.49610000e+01, 0.28054351e+01, 0.68522936e+00},

                         // 12. Grand Fir (Central Idaho Taper)
                         {-0.87229255e+01, -0.49823584e-01, 0.35238539e+01, 0.93969507e+01,
                          -0.11480943e+01, 0.11385876e+01, -0.10000000e+00, 0.46648531e-01,
                           0.63740406e+00, -0.50851998e+00, -0.35671833e+01, -0.85252043e+00,
                          -0.34854118e+00, -0.87825360e+00, 0.83369209e+00, 0.14868045e+01}
    };

    double SE_LNX;
    double BH = 4.5;

    int JSPR = JSP - 10;
    if (JSP == 3 && (geosub == "15" || geosub == "03"))
        JSPR = 12;

    // Convert to 0-based index
    JSPR -= 1;

    // Extract coefficients
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

    double DMEDIAN =
        F[JSPR][0] * std::pow(HT - BH, F[JSPR][1] + F[JSPR][2] * HT);

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
