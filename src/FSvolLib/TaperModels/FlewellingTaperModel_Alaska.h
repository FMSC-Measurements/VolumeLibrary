#pragma once
#include <array>
#include <cmath>
#include <string>
#include <algorithm>
#include "FlewellingShapeCoef.h"
//namespace TaperAK {

// ------------------------------------------------------------
// Coefficient Tables (constexpr)
// ------------------------------------------------------------

static constexpr std::array<std::array<double, 48>, 4> F_shape = { {
        /* Alaska Cedar (JSP=31 -> JRSP=1) */
        {
            0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
            3.6073443, 0.28808170, 0.00036880717, -5.9261770,
            -2.6176422, 2.1473459, 0.000010424453, 0.0,
            127536.59, -29418.473, -0.059464947, -5.2356508,
            31.885182, 4.4300416, -1.1222337, 62.25964,
            -24.733297, -80.698148, 23.730057, 69.999943,
            -97.392805, -17.557578, 30.946244, -0.88493982,
            23.735651, 12.39368, -7.4687251, -1.1540908,
            41.429549, 18.799039, -7.0710595, -0.059270766,
            -1716.8611, 355.69956, 0.0, 1.0969542, -0.028489405,
            -0.87696686
        },

    /* Western Redcedar */
    {
        0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
        0.98477853, 0.58511441, 0.00030909028, -8.9204162,
        -2.3390452, -0.049998897, 0.054604625, 0.0,
        127690.89, -28750.172, 23.208522, 9.7290535,
        21.836353, 0.96352866, -0.60131935, 4.6427702,
        -7.8654704, 30.346464, -5.0430550, -25.389446,
        -210.25226, 28.035137, 87.909814, -8.6687109,
        7.3773176, -13.553797, -2.4498232, 5.0321072,
        17.521623, 35.018471, 1.4592921, -1.2726548,
        -1726.9922, 359.94895, 0.0, 88.681845, 586.86630,
        5.1018047
    },

    /* Spruce–Hemlock (Old Growth) */
    {
        0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
        4.9336868, 0.040232019, 0.00083224395, 9.0107598,
        -9.0527505, 8.9268065, 1.4535135, 0.0,
        78.0, 0.0, 0.0, 9.9,
        50.129548, -3.2927757, -8.2895182, 46.568444,
        -16.673502, -54.307696, 12.439151, 139.10769,
        -56.519913, 111.87845, 69.281311, 238.90379,
        96.523635, -95.228393, -20.216598, 19.022197,
        63.832200, 46.545050, 81.554530, 147.44247,
        0.0,0.0,0.0, 14.356636, -50.828256, -217.49222
    },

    /* Spruce–Hemlock Second Growth (JSP=35,36) */
    {
        0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,
        0.57392551, 0.52192881, 0.00072540826, -36.763113,
        2.7363686, -2.7523803, 0.0,0.0,
        6463.5211, -2784.5408, 8000.8578, 6.1787943,
        19.381343, 0.83437003, 0.016607331, 39.548950,
        -23.516392, 81.537829, -13.385701, 70.0,
        115.98864, -26.234962, -18.689695, 0.010535631,
        16.926399, -2.2896149, -6.1712415, 1.4818512,
        6457.2326, -2500.9909, 1380.5138, -1397.9034,
        4.6307044, 22.407584, 0.0, 1.005, -3.6363974,
        -38.7575
    }
} };

// Sub-model adjustment (SUBF)
static constexpr std::array<double, 3> SUBF = {
    45.284010, 93.038602, 70.000492
};


// ------------------------------------------------------------
// Modern C++ SHP_AK rewrite
// ------------------------------------------------------------
struct ShapeParams {
    std::array<double, 6> RFLW;
    std::array<double, 4> RHFW;
};

FlewellingShapeParams SHP_AK(int JSP, const std::string_view geocode,
    double DBHIB, double HT)
{
    FlewellingShapeParams out{};
    const double BH = 4.5;

    int JRSP = JSP - 30;
    if (JRSP == 4) JRSP = 3;
    if (JRSP == 5 || JRSP == 6) JRSP = 4;

    auto F = F_shape[JRSP - 1];

    // second‑growth submodel
    if (JRSP == 3 && geocode == "01") {
        F[24] = SUBF[0];
        F[33] = SUBF[1];
        F[41] = SUBF[2];
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

    U6 = std::clamp(U6, 1.005, 10.0);

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
        {-48.680838, 146.63798, -48.216320, -17.800924, -2.0276812},
        {-40.872513, 98.400990, -38.399599, -24.301984, 3.5024803},
        {-38.994532, 100.94975, -29.557863, -10.967578, -3.4281732},
        {-63.750351, 153.06425, -26.876808, -10.538661, -4.1907036}
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
        {49.336868, 0.040232019, 0.00083224395}, // SH
        {0.51542754, 0.52156599, 0.00071621730}  // SH2
    } };

    static constexpr std::array<std::array<double, 16>, 4> Vv = { {
        {-87.861916,3.8942378,24.151069,86.871264,-12.932895,10.448179,
         0.0,0.096526647,0.13685097,-4.4306740,-63.677273,-0.28346734,
         0.30403741,23.458454,1.5233441,15.821190},

        {-52.217635,-2.3364372,1.0581464,76.117971,-10.588704,11.469169,
         1.6914337,0.035117997,10.0,0.39438121,-68.717202,-0.40369684,
         5.7719712,-4.8188520,7.7458363,29.097150},

        {-65.056179,-2.7270953,21.140193,75.614779,-7.1651812,7.4179167,
         0.034846513,0.035421062,-1.8935952,-3.6782454,-335.55096,-362.26359,
         4.9485299,309.07042,354.90428,0.12091440},

        {-59.805600,-2.4733469,2.6188946,142.85228,-21.896412,6.7083289,
         1.6760600,0.048980591,6.4890061,-0.49655428,-101.35682,0.35558601,
         3.2424523,42.749494,-0.71915171,6.6192729}
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

//} // namespace TaperAK