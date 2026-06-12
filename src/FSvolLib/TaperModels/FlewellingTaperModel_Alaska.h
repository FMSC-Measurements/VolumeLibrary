#pragma once
#include <array>
#include <cmath>
#include <string>
#include <algorithm>
#include "FlewellingShapeCoef.h"

// ------------------------------------------------------------
// Coefficient Tables (constexpr)
// ------------------------------------------------------------

static constexpr std::array<std::array<double, 48>, 4> F_shape = { {
    /* Alaska Cedar (JSP=31 -> JRSP=1) */
    {
      0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
      0.36073443e+01, 0.28808170e+00,
      0.36880717e-03,  -0.59261770e+00,  -0.26176422e+00,
      0.21473459e+00,   0.10424453e-04,   0.00000000e+00,
      0.12753659e+05,  -0.29418473e+04,  -0.59464947e-01,
     -0.52356508e+00,   0.31885182e+01,   0.44300416e+00,
     -0.11222337e+00,   0.62259640e+01,  -0.24733297e+01,
     -0.80698148e+01,   0.23730057e+01,   0.69999943e+01,
     -0.97392805e+01,  -0.17557578e+01,   0.30946244e+01,
     -0.88493982e-01,   0.23735651e+01,   0.12393680e+01,
     -0.74687251e+00,  -0.11540908e+00,   0.41429549e+01,
      0.18799039e+01,  -0.70710595e+00,  -0.59270766e-01,
     -0.17168611e+04,   0.35569956e+03,   0.00000000e+00,
      0.10969542e+01,  -0.28489405e-01,  -0.87696686e-01
    },

    /* Western Redcedar */
    {
      0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
      0.98477853e+00, 0.58511441e+00,
      0.30909028e-03,  -0.89204162e+00,  -0.23390452e+00,
     -0.49998897e-02,   0.54604625e-01,   0.00000000e+00,
      0.12769089e+05,  -0.28750172e+04,   0.23208522e+01,
      0.97290535e+00,   0.21836353e+01,   0.96352866e-01,
     -0.60131935e-01,   0.46427702e+00,  -0.78654704e+00,
      0.30346464e+01,  -0.50430550e+00,  -0.25389446e+01,
     -0.21025226e+02,   0.28035137e+01,   0.87909814e+01,
     -0.86687109e+00,   0.73773176e+00,  -0.13553797e+01,
     -0.24498232e+00,   0.50321072e+00,   0.17521623e+01,
      0.35018471e+01,   0.14592921e+00,  -0.12726548e+00,
     -0.17269922e+04,   0.35994895e+03,   0.00000000e+00,
      0.88681845e+01,   0.58686630e+02,   0.51018047e+00
    },

    /* Spruce–Hemlock (Old Growth) */
    {
      0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
      0.49336868e+01,   0.40232019e-01,
      0.83224395e-03,   0.90107598e+00,  -0.90527505e+00,
      0.89268065e+00,   0.14535135e+00,   0.00000000e+00,
      0.78000000e+01,   0.00000000e+00,   0.00000000e+00,
      0.99000000e+00,   0.50129548e+01,  -0.32927757e+00,
     -0.82895182e+00,   0.46568444e+01,  -0.16673502e+01,
     -0.54307696e+01,   0.12439151e+01,   0.13910769e+02,
     -0.56519913e+01,   0.11187845e+02,   0.69281311e+01,
      0.23890379e+02,   0.96523635e+01,  -0.95228393e+01,
     -0.20216598e+01,   0.19022197e+01,   0.63832200e+01,
      0.46545050e+01,   0.81554530e+01,   0.14744247e+02,
      0.00000000e+00,   0.00000000e+00,   0.00000000e+00,
      0.14356636e+01,  -0.50828256e+01,  -0.21749222e+02
    },

    /* Spruce–Hemlock Second Growth (JSP=35,36) */
    {
      0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
      0.57392551e+00,  0.52192881e+00,
      0.72540826e-03, -0.36763113e+01,  0.27363686e+00,
     -0.27523803e+00,  0.00000000e+00,  0.00000000e+00,
      0.64635211e+03, -0.27845408e+03,  0.80008578e+03,
      0.61787943e+00,  0.19381343e+01,  0.83437003e-01,
      0.16607331e-01,  0.39548950e+01, -0.23516392e+01,
      0.81537829e+01, -0.13385701e+01,  0.70000000e+01,
      0.11598864e+02, -0.26234962e+01, -0.18689695e+01,
      0.10535631e-01,  0.16926399e+01, -0.22896149e+00,
     -0.61712415e+00,  0.14818512e+00,  0.64572326e+03,
     -0.25009909e+03,  0.13805138e+03, -0.13979034e+03,
      0.46307044e+00,  0.22407584e+01,  0.00000000e+00,
      0.10050000e+01, -0.36363974e+00, -0.38757500e+01
    }
} };

// Sub-model adjustment (SUBF)
static constexpr std::array<double, 3> SUBF = {
    45.284010, 93.038602, 70.000492
};


// ------------------------------------------------------------
// Modern C++ SHP_AK rewrite
// ------------------------------------------------------------
//SHP_AK
FlewellingShapeParams shapeAlaska(int JSP, const std::string_view geocode,
    double DBHIB, double HT)
{
    FlewellingShapeParams out{};
    const double BH = 4.5;

    int JRSP = JSP - 30;
    if (JRSP == 4) JRSP = 3;
    if (JRSP == 5 || JRSP == 6) JRSP = 4;

    auto F = F_shape[JRSP - 1];

    // second‑growth submodel, 
    // the original Fortran uses geoCode 01 and JRSP = 3. I think it should be JRSP=4 and geoCode = "02"!!!
    if (JRSP == 3 && geocode == "01") {
        F[25] = SUBF[0];
        F[34] = SUBF[1];
        F[42] = SUBF[2];
    }

    double DMEDIAN = F[10] * std::pow(HT - BH, F[11] + F[12] * HT);
    double DFORM = DBHIB / DMEDIAN - 1.0;

    auto logistic = [](double x) {
        x = std::clamp(x, -7.0, 7.0);
        return std::exp(x) / (1.0 + std::exp(x));
        };

    // Compute transformed U-values
    double U7 = F[13] + F[14] * std::log(HT) + F[15] * DFORM;

    double U9T = F[18] + F[19] * std::log(HT) + F[20] * DFORM;
    U9T = std::clamp(U9T, -7.0, 7.0);
    double U9 = F[16] * (std::exp(U9T) / (1.0 + std::exp(U9T)));

    double U8 = F[21] + F[22] * HT + F[23] * std::log(HT) + F[24] * DFORM;

    double U1 = F[25] + F[26] * std::log(HT) + F[27] * DFORM +
        F[28] * DFORM * std::log(HT);

    double U2 = F[29] + F[30] * DFORM + F[31] * std::log(HT) +
        F[32] * DFORM * std::log(HT) + F[33] * DBHIB;

    double U3;
    if (JRSP == 15) {
        U3 = F[34] + F[35] * DFORM +
            F[36] * (1.0 - std::exp(F[37] * HT));
    }
    else {
        U3 = F[34] + F[35] * DFORM + F[36] * std::log(HT) +
            F[37] * DFORM * std::log(HT);
    }

    double U4 = F[38] + F[39] * DFORM + F[40] * std::log(HT) +
        F[41] * DBHIB;

    double U5 = F[42] + F[43] * std::log(HT);

    double U6 = F[45] + F[46] * DFORM + F[47] * std::log(HT);

    //U6 = std::clamp(U6, 1.005, 10.0);
    if (U5 < -7.0) U5 = -7.0;
    else if (U5 > 7.1) U5 = 7.1;
    if (U6 < 1.005) U6 = 1.005;
    else if (U6 > 10.0) U6 = 10.0;
    U7 = std::clamp(U7, -7.0, 7.0);
    if (U8 > 0.99) U8 = 0.99;
    if (U9 > 0.3) U9 = 0.3;
    else if (U9 < 0.0) U9 = 0.0;

    // Geometric quantities
    double R1 = logistic(U1);
    double R2 = logistic(U2);
    double R3 = logistic(U3);
    double R4 = logistic(U4);
    double R5 = (U5 <= 7.0 ? 0.5 + 0.5 * logistic(U5) : 1.0);
    double A3 = U6;

    double RHI1 = logistic(U7);
    RHI1 = std::min(RHI1, 0.5);

    double RHLONGI = U9;
    double RHI2 = RHI1 + RHLONGI;

    double RHC = U8;
    if (RHC < RHI2 + 0.01)
        RHC = std::min(RHI2 + 0.01, (RHI2 + 1.0) / 2.0);

    // Fill outputs
    out = { R1, R2, R3, R4, R5, A3,  RHI1, RHI2, RHC, RHLONGI };
    return out;
}


// ------------------------------------------------------------
// Modern C++ COR_AK rewrite
// ------------------------------------------------------------
double COR_AK(int JSP, double totalH, double h1, double h2)
{
    static constexpr std::array<std::array<double, 5>, 4> V_corr = { {
        //ALASKA CEDAR
        { -0.48680838e+01, 0.14663798e+02,
          -0.48216320e+01,-0.17800924e+01,-0.20276812e+00},
        //WESTERN RED CEDAR
        {-0.40872513e+01, 0.98400990e+01,
         -0.38399599e+01,-0.24301984e+01, 0.35024803e+00},
        //SPRUCE HEMLOCK
        {-0.38994532e+01, 0.10094975e+02,
         -0.29557863e+01,-0.10967578e+01,-0.34281732e+00},
        //SPRUCE HEMLOCK SECOND GROWTH
        {-0.63750351e+01, 0.15306425e+02,
         -0.26876808e+01,-0.10538661e+01,-0.41907036e+00},
    } };

    const double BH = 4.5;

    int JSPR = JSP - 30;
    if (JSPR == 4) JSPR = 3;
    if (JSPR == 5 || JSPR == 6) JSPR = 4;

    const auto& V = V_corr[JSPR - 1];
    double Q1 = V[0], Q2 = V[1], QS = V[2], Q4 = V[3], Q5 = V[4];
    double Q3 = QS - (Q1 + Q2);

    if (h1 == h2)
        return 1.0;
    if (h1 == BH || h2 == BH)
        return 0.5;

    double Hlow = std::min(h1, h2);
    double Hhigh = std::max(h1, h2);

    double corr;

    if (Hlow > BH) {
        double t3 = (Hlow - BH) / (totalH - BH);
        double t4 = (Hhigh - BH) / (totalH - BH);
        corr = std::exp(Q1 * (t4 - t3) +
            Q2 * (t4 * t4 - t3 * t3) / 2.0 +
            Q3 * (t4 * t4 * t4 - t3 * t3 * t3) / 3.0);
    }
    else if (Hhigh > BH) {
        double t3 = (Hhigh - BH) / (totalH - BH);
        double T2 = (BH - Hlow) / BH;
        corr = Q5 * std::exp(Q4 * T2 +
            Q1 * t3 +
            Q2 * t3 * t3 / 2.0 +
            Q3 * t3 * t3 * t3 / 3.0);
    }
    else {
        double T1 = (BH - Hlow) / BH;
        double T2 = (BH - Hhigh) / BH;
        corr = std::exp(Q4 * (T1 - T2));
    }

    return corr;
}


// ------------------------------------------------------------
// Modern C++ VAR_AK rewrite
// ------------------------------------------------------------
double VAR_AK(int JSP, double DBH, double HT, double H)
{
    const double BH = 4.5;

    // F(10..12) and V(1..16) compacted
    static constexpr std::array<std::array<double, 3>, 4> Fv = { {
        {0.1975, 0.9663, 0.000367},     // AC
        {0.0558, 1.2635, -0.000316},    // WRC
        {4.9336868, 0.040232019, 0.00083224395}, // SH
        {0.51542754, 0.52156599, 0.00071621730}  // SH2
    } };

    static constexpr std::array<std::array<double, 16>, 4> Vv = { {
        //AC
        {-0.87861916e+01,
          0.38942378e+00, 0.24151069e+01, 0.86871264e+01,
         -0.12932895e+01, 0.10448179e+01, 0.00000000e+00,
          0.96526647e-01, 0.13685097e+00,-0.44306740e+00,
         -0.63677273e+01,-0.28346734e+00, 0.30403741e+00,
          0.23458454e+01, 0.15233441e+00, 0.15821190e+01},
        //WRC
        {-0.52217635e+01,-0.23364372e+00,
          0.10581464e+00, 0.76117971e+01,-0.10588704e+01,
          0.11469169e+01, 0.16914337e+00, 0.35117997e-01,
          0.10000000e+01, 0.39438121e-01,-0.68717202e+01,
         -0.40369684e-01, 0.57719712e+00,-0.48188520e+00,
          0.77458363e+00, 0.29097150e+01},
        //SH
        {-0.65056179e+01,  -0.27270953e+00,
          0.21140193e+01,   0.75614779e+01,  -0.71651812e+00,
          0.74179167e+00,   0.34846513e-01,   0.35421062e-01,
         -0.18935952e+00,  -0.36782454e+00,  -0.33555096e+02,
         -0.36226359e+02,   0.49485299e+00,   0.30907042e+02,
          0.35490428e+02,   0.12091440e-01},
        //SH2
        {-0.59805600e+01,  -0.24733469e+00,
          0.26188946e+00,   0.14285228e+02,  -0.21896412e+01,
          0.67083289e+00,   0.16760600e+00,   0.48980591e-01,
          0.64890061e+00,  -0.49655428e-01,  -0.10135682e+02,
          0.35558601e-01,   0.32424523e+00,   0.42749494e+01,
         -0.71915171e-01,   0.66192729e+00}
    } };

    int JSPR = JSP - 30;
    if (JSPR == 4) JSPR = 3;
    if (JSPR == 5 || JSPR == 6) JSPR = 4;

    const auto& F = Fv[JSPR - 1];
    const auto& V = Vv[JSPR - 1];

    double DMED = F[0] * std::pow(HT - BH, F[1] + F[2] * HT);
    double DRATIO = DBH / DMED;
    double logHT = std::log(HT);

    double VA0 = V[0] + V[1] * logHT + V[2] * DRATIO + V[9] * logHT * DRATIO;
    double VB0 = V[3] + V[4] * logHT;
    double VE0 = V[10] + V[11] * logHT + V[12] * DRATIO;
    double VF0 = V[13] + V[14] * logHT;
    double VC = V[5] + V[6] * logHT;
    double VX2 = V[7], VX3 = V[8], VG0 = V[15];

    double XU, LVAR, VAR;

    if (H < BH) {
        XU = (BH - H) / BH;
        bool mod1 = false;
        double origX = XU;

        if (XU < 0.111) {
            mod1 = true;
            XU = 0.111;
        }

        LVAR = VE0 + VF0 * std::pow(XU, VG0);
        LVAR = std::clamp(LVAR, -15.0, 15.0);

        VAR = std::exp(LVAR);
        if (mod1) VAR *= (origX / XU);
    }
    else if (H == BH) {
        return 0.0;
    }
    else if (H < HT) {
        XU = (H - BH) / (HT - BH);

        bool mod2 = false;
        double origX = XU;
        double factor = 1.0;

        if (XU < 0.02) {
            mod2 = true;
            XU = 0.02;
            factor = origX / XU;
        }
        else if (XU > 0.96) {
            mod2 = true;
            XU = 0.96;
            factor = 1.0;
        }

        LVAR = VA0 + VB0 * std::pow(XU, VC) +
            VX2 * std::pow(HT / 50.0, VX3) / (1.0 - XU);

        LVAR = std::clamp(LVAR, -15.0, 15.0);
        VAR = std::exp(LVAR);
        if (mod2) VAR *= factor;
    }
    else {
        VAR = 1.0;
    }

    return std::sqrt(VAR);
}


// ------------------------------------------------------------
// Modern C++ FDBT_AK rewrite
// ------------------------------------------------------------
double FDBT_AK(int JSP, double DBHOB, double HTTOT)
{
    int JSPR = JSP - 30;
    double DBHIB;

    switch (JSPR) {
    case 1: DBHIB = -0.307528 + 0.982927 * DBHOB; break;
    case 2: DBHIB = -0.451778 + 0.975611 * DBHOB; break;
    case 3: DBHIB = -0.0171289 + 0.9511897 * DBHOB +
        0.000507 * DBHOB * DBHOB; break;
    case 4: DBHIB = -0.054306 + 0.916145 * DBHOB +
        0.004821 * HTTOT; break;
    case 5: DBHIB = -0.212445 + 0.983230 * DBHOB; break;
    case 6: DBHIB = -0.088908 + 0.963193 * DBHOB; break;
    default: DBHIB = DBHOB; break;
    }

    return DBHOB - DBHIB;  // double bark thickness
}

