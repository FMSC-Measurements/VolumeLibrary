#include "ClarkTaperModel.h"
#include "..\SmalianScribnerIntl14.h"
#include <cmath>
#include <algorithm>

int ClarkTaperModel::findSpeciesIndex(int spcd)
{
    int idx = -1;
    int sppGrp;
    if (volumeEquation_.geoCode == VolumeEquation::GeoCode::R9) {
        idx = R9SpIndex(spcd);
        if (idx == -1)
        {
            if (spcd < 300) {
                // conifers
                sppGrp = 1000;

                if (spcd >= 90 && spcd <= 99) {
                    // spruces
                    sppGrp = 1090;
                }
                else if (spcd >= 100 && spcd <= 199) {
                    // pines
                    sppGrp = 1100;
                }

            }
            else {
                // hardwoods
                sppGrp = 1300;

                if (spcd >= 310 && spcd <= 329) {
                    // maples
                    sppGrp = 1310;
                }
                else if (spcd >= 370 && spcd <= 379) {
                    // birches
                    sppGrp = 1370;
                }
                else if (spcd >= 400 && spcd <= 410) {
                    // hickories
                    sppGrp = 1400;
                }
                else if (spcd >= 540 && spcd <= 549) {
                    // ashes
                    sppGrp = 1540;
                }
                else if (spcd == 740 || spcd == 742 || spcd == 744 ||
                    spcd == 745 || spcd == 753) {
                    // cottonwoods
                    sppGrp = 1740;
                }
                else if (spcd == 741 || spcd == 743 || spcd == 746 ||
                    spcd == 752) {
                    // poplars
                    sppGrp = 1750;
                }
                else if (spcd >= 760 && spcd <= 769) {
                    // cherries / plums
                    sppGrp = 1760;
                }
                else if (spcd >= 800 && spcd <= 899) {
                    // oaks
                    sppGrp = 1800;
                }
                else if (spcd >= 950 && spcd <= 954) {
                    // basswoods
                    sppGrp = 1950;
                }
                else if (spcd >= 970 && spcd <= 979) {
                    // oaks
                    sppGrp = 1970;
                }
            }
            idx = R9SpIndex(sppGrp);
        }
    }
    //else //R8 species
    else
    {
        sppGrp = spcd;
        if (spcd == 123 || spcd == 197) {
            sppGrp = 100;
        }
        else if (spcd == 268) {
            sppGrp = 261;
        }
        else if (spcd == 313 || spcd == 314 || spcd == 317 ||
            spcd == 650 || spcd == 651 ||
            spcd == 691 || spcd == 711 || spcd == 742 ||
            spcd == 762 || spcd == 920 || spcd == 930 ||
            spcd == 545 || spcd == 546) {
            sppGrp = 300;
        }
        else if (spcd == 521 || spcd == 550 || spcd == 580 ||
            spcd == 601 || spcd == 602 || spcd == 318) {
            sppGrp = 500;
        }
        else if (spcd == 804 || spcd == 817 || spcd == 820 ||
            spcd == 823 || spcd == 825 ||
            spcd == 826 || spcd == 830 || spcd == 834) {
            sppGrp = 800;
        }

        idx = r8SpeciesIndex(sppGrp);
    }
    return idx;
}

void ClarkTaperModel::shortTreeReset(double upperHt) {
    if (upperHt < 17.4) {
        shortHeight = true;
        topHt = 17.4;
        shrtHt = upperHt;
    }
}

//set the coef for the species
void ClarkTaperModel::setClarkCoef(int spcd)
{
    int idx = findSpeciesIndex(spcd);
    sppIdx = idx;
    volSp = spcd;
    if (volumeEquation_.geoCode == VolumeEquation::GeoCode::R9)
    {
        clarkCoef.a4  = coefA[idx][2];
        clarkCoef.b4  = coefA[idx][3];
        clarkCoef.a17 = coef0[idx][1];
        clarkCoef.b17 = coef0[idx][2];
        clarkCoef.r   = coef0[idx][3];
        clarkCoef.c   = coef0[idx][4];
        clarkCoef.e   = coef0[idx][5];
        clarkCoef.p   = coef0[idx][6];
        clarkCoef.a   = coef0[idx][7];
        clarkCoef.b   = coef0[idx][8];
        //need to reset a17 and b17 based on if total height is not measured, but measured ht1prd or ht2prd
    }
    else if (volumeEquation_.geoCode == VolumeEquation::GeoCode::R8)
    {
        volSp = R8Species[idx];
        geoSppIdx = findR8GeoSppIndex(subRegion, volSp);

        clarkCoef.a4 = R8CF[geoSppIdx][3];
        clarkCoef.b4 = R8CF[geoSppIdx][4];
        clarkCoef.afi = R8CF[geoSppIdx][5];
        clarkCoef.bfi = R8CF[geoSppIdx][6];

        if (eqHeightType == 0 || eqHeightType == 1 || eqHeightType == 8) {
            
            clarkCoef.r = coefH0[idx][1];
            clarkCoef.c = coefH0[idx][2];
            clarkCoef.e = coefH0[idx][3];
            clarkCoef.p = coefH0[idx][4];
            clarkCoef.b = coefH0[idx][5];
            clarkCoef.a = coefH0[idx][6];
            clarkCoef.a17 = R8CF[geoSppIdx][13];
            clarkCoef.b17 = R8CF[geoSppIdx][14];

            clarkCoefOb.r = coefH0Ob[idx][1];
            clarkCoefOb.c = coefH0Ob[idx][2];
            clarkCoefOb.e = coefH0Ob[idx][3];
            clarkCoefOb.p = coefH0Ob[idx][4];
            clarkCoefOb.b = coefH0Ob[idx][5];
            clarkCoefOb.a = coefH0Ob[idx][6];
            clarkCoefOb.a17 = obR8CF[geoSppIdx][3];
            clarkCoefOb.b17 = obR8CF[geoSppIdx][4];

            clarkCoef.fixDi = DIB479[idx][1];
            clarkCoef.spgrp = R8CF[geoSppIdx][2];
        }
        else if (eqHeightType == 4) {
            clarkCoef.r = coefH4[idx][1];
            clarkCoef.c = coefH4[idx][2];
            clarkCoef.e = coefH4[idx][3];
            clarkCoef.p = coefH4[idx][4];
            clarkCoef.q = coefH4[idx][5];
            clarkCoef.a17 = R8CF[geoSppIdx][7];
            clarkCoef.b17 = R8CF[geoSppIdx][8];
            clarkCoefOb.a17 = obR8CF[geoSppIdx][5];
            clarkCoefOb.b17 = obR8CF[geoSppIdx][6];
            clarkCoef.fixDi = DIB479[idx][1];
        }
        else if (eqHeightType == 7 || eqHeightType == 9) {
            clarkCoef.r = coefH79[idx][1];
            clarkCoef.c = coefH79[idx][2];
            clarkCoef.e = coefH79[idx][3];
            clarkCoef.p = coefH79[idx][4];
            clarkCoef.q = coefH79[idx][5];
            clarkCoefOb.a17 = obR8CF[geoSppIdx][7];
            clarkCoefOb.b17 = obR8CF[geoSppIdx][8];
            if (volSp < 300) {
                clarkCoef.a17 = R8CF[geoSppIdx][9];
                clarkCoef.b17 = R8CF[geoSppIdx][10];
            }
            else {
                clarkCoef.a17 = R8CF[geoSppIdx][11];
                clarkCoef.b17 = R8CF[geoSppIdx][12];
            }
            clarkCoef.tr1 = R8CF[geoSppIdx][15];
            clarkCoef.tr2 = R8CF[geoSppIdx][16];
            clarkCoef.tr3 = R8CF[geoSppIdx][17];
            clarkCoef.fixDi = DIB479[idx][2];
        }
    }
}

//  Calculates total tree height from the specified inside - bark DBH
//  (dbhIb), top height(topHt) and inside - bark top diameter(topDib).
//  a and b are coefficients for inside - bark calculations.

double GetTotalHeight(double htTot, double dbhIb, double dib17, double topHt, double topDib, double a, double b)
{
    double totHt = 0.0;

    // --------------------------------------------------------------------
    // Case 1: Measured total height present
    // --------------------------------------------------------------------
    if (htTot > 0.0)
    {
        if (htTot > 17.4)
        {
            totHt = htTot;
        }
        else
        {
            // Short tree: use 17.4 topHT (Fortran comment)
            totHt = topHt;     // topHt should already be 17.4
        }
    }

    // --------------------------------------------------------------------
    // Case 2: No measured total height, but topHt > 17.3
    // --------------------------------------------------------------------
    else if (topHt > 17.3)
    {
        double Im = 0.0;

        // topDib**2 .gt. b*(a-1)**2 * dib17**2
        if ((topDib * topDib) > b * std::pow(a - 1.0, 2) * (dib17 * dib17))
        {
            Im = 1.0;
        }

        double Qa = b + Im * (1.0 - b) / std::pow(a, 2);
        double Qb = -2.0 * b - Im * 2.0 * (1.0 - b) / a;
        double Qc = b + (1.0 - b) * Im - std::pow(topDib / dib17, 2);

        // Total height formula:
        //  totHt = 17.3 + (topHt - 17.3)*(2*Qa) / (-Qb - sqrt(Qb^2 - 4*Qa*Qc))
        double discriminant = Qb * Qb - 4.0 * Qa * Qc;
        if (discriminant < 0.0)
        {
            // Physically invalid parabola → set error?
            // Fortran does NOT set errFlg here, so we mimic behavior.
            discriminant = 0.0;
        }

        totHt = 17.3 +
            (topHt - 17.3) * (2.0 * Qa) /
            (-Qb - std::sqrt(discriminant));

        // Limiters from Fortran:
        totHt = std::max(totHt, topHt + topDib * 2.0);
        totHt = std::min(totHt, topHt + topDib * 8.0);
    }

    // --------------------------------------------------------------------
    // Case 3: Short measured height, fixed taper rule
    // --------------------------------------------------------------------
    else
    {
        totHt = 17.3 + dib17 * 3.0;
    }

    return totHt;
}

//  Calculates cubic foot volume(cfVol) from the specified lower height
//  or stump(lowrHt) to the specified upper height(upprHt), given
//  inside - bark dbh(dbhIb), inside - bark diameter at 17.3' (dib17) and 
//  total height(totHt).r, c, e, p, b, and a are the coefficients
//  for inside - bark calculations.

// ---------------------------------------------------------------------
// C++ version of: SUBROUTINE R9CUFT(cfVol, COEFFS, lowrHt, upprHt, errFlg)
// ---------------------------------------------------------------------
double ClarkTaperModel::ClarkCubicFootVol(double lowrHt, double upprHt)
{
    double r = clarkCoef.r;
    double c = clarkCoef.c;
    double e = clarkCoef.e;
    double p = clarkCoef.p;
    double b = clarkCoef.b;
    double a = clarkCoef.a;
    double dib17 = clarkCoef.dib17;

    // Final volume
    double cfVol = 0.0;

    // If upper height <= 0, nothing to compute
    if (upprHt <= 0.0)
        return cfVol;

    // If equation use height to 4, 7, 9 top
    if (eqHeightType == 4 || eqHeightType == 7 || eqHeightType == 9) {
        return ClarkCubicVolH479(lowrHt, upprHt);
    }

    // ------------------------------------------------------------------
    // Combined variables
    // ------------------------------------------------------------------
    double G = std::pow(1.0 - 4.5 / totHt, r);
    double W = (c + e / std::pow(dbhIb, 3.0)) / (1.0 - G);

    double X = std::pow(1.0 - 4.5 / totHt, p);

    double Y;
    // Avoid extremely small Y (Fortran protection)
    if ((1.0 - 17.3 / totHt) < 0.005748 && p > 14.0) {
        Y = 0.0;
    }
    else {
        Y = std::pow(1.0 - 17.3 / totHt, p);
    }

    double Z = (dbhIb * dbhIb - dib17 * dib17) / (X - Y);
    double T = dbhIb * dbhIb - Z * X;

    // Height interval partitioning
    double L1 = std::max(lowrHt, 0.0);
    double U1 = std::min(upprHt, 4.5);

    double L2 = std::max(lowrHt, 4.5);
    double U2 = std::min(upprHt, 17.3);

    double L3 = std::max(lowrHt, 17.3);
    double U3 = std::min(totHt, upprHt);

    // ------------------------------------------------------------------
    // Indicator variables (I1..I6)
    // ------------------------------------------------------------------
    double I1 = (lowrHt < 4.5) ? 1.0 : 0.0;
    double I2 = (lowrHt < 17.3) ? 1.0 : 0.0;
    double I3 = (upprHt > 4.5) ? 1.0 : 0.0;
    double I4 = (upprHt > 17.3) ? 1.0 : 0.0;
    double I5 = ((L3 - 17.3) < a * (totHt - 17.3)) ? 1.0 : 0.0;
    double I6 = ((U3 - 17.3) < a * (totHt - 17.3)) ? 1.0 : 0.0;

    // ------------------------------------------------------------------
    // Volume components V1, V2, V3
    // ------------------------------------------------------------------
    double V1 = 0.0;
    double V2 = 0.0;
    double V3 = 0.0;

    // PART 1: stump to 4.5 ft section
    if (I1 > 0.0) {
        V1 =
            I1 * dbhIb * dbhIb *
            (
                (1.0 - G * W) * (U1 - L1)
                + W * (
                    std::pow(1.0 - L1 / totHt, r) * (totHt - L1)
                    - std::pow(1.0 - U1 / totHt, r) * (totHt - U1)
                    ) / (r + 1.0)
                );
    }

    // PART 2: 4.5 to 17.3 ft section
    if (I2 > 0.0 && I3 > 0.0)
    {
        // Fortran: prevent underflow for (1 - U2/totHt)**p
        if ((1.0 - U2 / totHt) < 0.005748 && p > 14.0) {
            V2 =
                T * (U2 - L2)
                + Z * (
                    std::pow(1.0 - L2 / totHt, p) * (totHt - L2)
                    ) / (p + 1.0);
        }
        else {
            V2 =
                T * (U2 - L2)
                + Z * (
                    std::pow(1.0 - L2 / totHt, p) * (totHt - L2)
                    - std::pow(1.0 - U2 / totHt, p) * (totHt - U2)
                    ) / (p + 1.0);
        }
    }

    // PART 3: 17.3 ft to top section (parabolic section)
    if (I4 > 0.0)
    {
        V3 =
            dib17 * dib17 *
            (
                b * (U3 - L3)
                - b * (((U3 - 17.3) * (U3 - 17.3))
                    - ((L3 - 17.3) * (L3 - 17.3))) / (totHt - 17.3)
                + (b / 3.0) * (
                    std::pow(U3 - 17.3, 3) - std::pow(L3 - 17.3, 3)
                    ) / std::pow(totHt - 17.3, 2)
                + I5 * (1.0 / 3.0) * ((1.0 - b) / std::pow(a, 2)) *
                std::pow(a * (totHt - 17.3) - (L3 - 17.3), 3)
                / std::pow(totHt - 17.3, 2)
                - I6 * (1.0 / 3.0) * ((1.0 - b) / std::pow(a, 2)) *
                std::pow(a * (totHt - 17.3) - (U3 - 17.3), 3)
                / std::pow(totHt - 17.3, 2)
                );
    }

    // ------------------------------------------------------------------
    // Final cubic foot volume
    // ------------------------------------------------------------------
    cfVol = 0.005454154 * (V1 + V2 + V3);

    if (shortHeight) cfVol *= shrtHt / 17.3;

    if (cfVol < 0.0)
        cfVol = 0.0;

    return cfVol;
}

//  Calculates cubic foot volume(cfVol) from the specified lower height
//  or stump(lowrHt) to the specified upper height(upprHt), given
//  inside - bark dbh(dbhIb), inside - bark diameter at 17.3' (dib17) and 
//  top height(topHt), i.e. height to 4, 7 or 9" top dob. The r, c, e, p, q are the coefficients
//  for inside - bark calculations.
 
double ClarkTaperModel::ClarkCubicVolH479(double lowrHt, double upHt)
{
    double DIB, FCLSS, FIXDI, HT2;
    double TERML1, TERML2, TERML3;
    double TERMU1, TERMU2, TERMU3;
    double TL1, TL2, TL3, TU1, TU2, TU3;
    double DIB2, FCLSS2, DIB3, LOWER, UPPER, L1;
    double L2, L3, U1, U2, U3, V, W, X, Y, Z, T, G;
    double V1, X1, Y1, G1;
    double CAPR, N;
    int I1, I2, I3, I4;
    double R, Cc, E, P, Q;
    double cfVol = 0.0;

    //set the coefficient variable
    R = clarkCoef.r;
    Cc = clarkCoef.c;
    E = clarkCoef.e;
    P = clarkCoef.p;
    Q = clarkCoef.q;

    DIB = dbhIb;
    FCLSS = clarkCoef.dib17;
    DIB2 = DIB * DIB;
    FCLSS2 = FCLSS * FCLSS;
    DIB3 = DIB * DIB * DIB;
    FIXDI = clarkCoef.fixDi;
    HT2 = topHt;

    V1 = (1.0 - 4.5 / HT2);
    V = (V1 < 0.01 ? 0.0 : pow(V1, R));

    W = (Cc + E / (DIB3)) / (1.0 - V);

    X1 = (1.0 - 4.5 / HT2);
    X = (X1 < 0.01 ? 0.0 : pow(X1, P));

    Y1 = (1.0 - 17.3 / HT2);
    Y = (Y1 < 0.01 ? 0.0 : pow(Y1, P));

    Z = (DIB2 - FCLSS2) / (X - Y);
    T = DIB2 - Z * X;

    G1 = (1.0 - 17.3 / HT2);
    G = (G1 < 0.01 ? 0.0 : pow(G1, Q));

    if (G == 0.0)
        CAPR = 0.0;
    else
        CAPR = (FCLSS2 - FIXDI * FIXDI) / G;

    N = FCLSS2 - CAPR * G;

    LOWER = lowrHt;
    UPPER = upHt;

    L1 = std::max(LOWER, 0.0);
    U1 = std::min(UPPER, 4.5);
    L2 = std::max(LOWER, 4.5);
    U2 = std::min(UPPER, 17.3);
    L3 = std::max(LOWER, 17.3);
    U3 = std::min(UPPER, HT2);

    I1 = 1;
    I2 = 1;
    I3 = (UPPER > 4.5 ? 1 : 0);
    I4 = (UPPER > 17.3 ? 1 : 0);

    TL1 = 1.0 - L1 / HT2;
    TERML1 = (TL1 < 0.01 ? 0.0 : pow(TL1, R) * (HT2 - L1));

    TU1 = 1.0 - U1 / HT2;
    TERMU1 = (TU1 < 0.01 ? 0.0 : pow(TU1, R) * (HT2 - U1));

    TL2 = 1.0 - L2 / HT2;
    TERML2 = (TL2 < 0.01 ? 0.0 : pow(TL2, P) * (HT2 - L2));

    TU2 = 1.0 - U2 / HT2;
    TERMU2 = (TU2 < 0.01 ? 0.0 : pow(TU2, P) * (HT2 - U2));

    TL3 = 1.0 - L3 / HT2;
    TERML3 = (TL3 < 0.01 ? 0.0 : pow(TL3, Q) * (HT2 - L3));

    TU3 = 1.0 - U3 / HT2;
    TERMU3 = (TU3 < 0.01 ? 0.0 : pow(TU3, Q) * (HT2 - U3));

    cfVol = 0.005454 * (
        I1 * DIB2 * ((1.0 - V * W) * (U1 - L1) +
            W * (TERML1 - TERMU1) / (R + 1.0))
        + I2 * I3 * (T * (U2 - L2) +
            Z * (TERML2 - TERMU2) / (P + 1.0))
        + I4 * (N * (U3 - L3) +
            CAPR * (TERML3 - TERMU3) / (Q + 1.0))
        );

    if (shortHeight) cfVol *= shrtHt / 17.3;
    return cfVol;
}

//  Calculates inside - bark diameter(stmDib) at specified height(stemHt)
//  given inside - bark dbh(dbhIb), inside - bark diameter at 17.3' (dib17) 
//  and total height(totHt).r, c, e, p, b, and a, are the coefficients
//  for inside - bark calculations.

// ---------------------------------------------------------------
// C++ version of: SUBROUTINE R9DIB(stmDib, stemHt, COEFFS)
// ---------------------------------------------------------------
double ClarkTaperModel::ClarkDib(double stemHt)
{
    double r = clarkCoef.r;
    double c = clarkCoef.c;
    double e = clarkCoef.e;
    double p = clarkCoef.p;
    double b = clarkCoef.b;
    double a = clarkCoef.a;
    double dib17 = clarkCoef.dib17;

    double stmDib = 0.0;

    // If equation use height to 4, 7, 9 top
    if (eqHeightType == 4 || eqHeightType == 7 || eqHeightType == 9) {
        return ClarkDibH479(stemHt);
    }

    // -----------------------------------------------------------
    // Fix potential problem when r < 0 and stemHt == totHt
    // -----------------------------------------------------------
    if (r < 0.0 && std::abs(stemHt - totHt) < 0.00001)
        stemHt -= 0.1;

    // -----------------------------------------------------------
    // Height indicator variables
    // -----------------------------------------------------------
    double Is = (stemHt < 4.5) ? 1.0 : 0.0;
    double Ib = (stemHt >= 4.5 && stemHt <= 17.3) ? 1.0 : 0.0;
    double It = (stemHt > 17.3) ? 1.0 : 0.0;
    double Im = (stemHt < (17.3 + a * (totHt - 17.3))) ? 1.0 : 0.0;

    // -----------------------------------------------------------
    // Protect (1 - stemHt/totHt)^r from underflow
    // -----------------------------------------------------------
    double StTot = stemHt / totHt;
    if (std::log(1.0 - StTot) < (-20.0 / std::abs(r)))
        StTot = 1.0;

    // -----------------------------------------------------------
    // Compute Ds, Db, Dt separately (Fortran: replaced due to errors)
    // -----------------------------------------------------------
    double Ds = 0.0;
    double Db = 0.0;
    double Dt = 0.0;

    // -------- stump section: 0–4.5 ft
    if (Is == 1.0) {
        Ds =
            dbhIb * dbhIb *
            (
                1.0
                + (c + e / std::pow(dbhIb, 3.0)) *
                (
                    std::pow(1.0 - StTot, r)
                    - std::pow(1.0 - 4.5 / totHt, r)
                    )
                / (1.0 - std::pow(1.0 - 4.5 / totHt, r))
                );
    }

    // -------- breast-height to 17.3 ft
    if (Ib == 1.0) {
        Db =
            dbhIb * dbhIb
            - (dbhIb * dbhIb - dib17 * dib17) *
            (
                std::pow(1.0 - 4.5 / totHt, p)
                - std::pow(1.0 - stemHt / totHt, p)
                )
            /
            (
                std::pow(1.0 - 4.5 / totHt, p)
                - std::pow(1.0 - 17.3 / totHt, p)
                );
    }

    // -------- above 17.3 ft: parabolic taper + mode adjustment
    if (It == 1.0) {
        double term1 = (stemHt - 17.3) / (totHt - 17.3);
        Dt =
            dib17 * dib17 *
            (
                b * std::pow(term1 - 1.0, 2)
                + Im * ((1.0 - b) / std::pow(a, 2.0)) *
                std::pow(a - term1, 2)
                );
    }

    // -----------------------------------------------------------
    // Final diameter = sqrt(Ds + Db + Dt)
    // -----------------------------------------------------------
    double sum = Ds + Db + Dt;
    if (sum > 0.0)
        stmDib = std::sqrt(sum);

    if (stmDib < 0.0)
        stmDib = 0.0;

    return stmDib;
}

//  Calculates inside - bark diameter(stmDib) at specified height(stemHt)
//  given inside - bark dbh(dbhIb), inside - bark diameter at 17.3' (dib17) 
//  and topl height(topHt) to 4, 7/9" top. r, c, e, p, and q, are the coefficients
//  for inside - bark calculations.

double ClarkTaperModel::ClarkDibH479(double stemHt)
{
    double r = clarkCoef.r;
    double c = clarkCoef.c;
    double e = clarkCoef.e;
    double p = clarkCoef.p;
    double q = clarkCoef.q;
    double dib17 = clarkCoef.dib17;

    double stmDib = 0.0;
    double D = dbhIb;
    double Dx = topDib;
    double F = dib17;
    double Hx = topHt;
    double h = stemHt;

    if (h >= Hx) return clarkCoef.fixDi;

    double D2 = D * D;
    double D3 = D2 * D;
    double F2 = F * F;
    double Dx2 = Dx * Dx;

    //Indicator virables
    double Is = (h < 4.5 ? 1.0 : 0.0);
    double Ib = ((h >= 4.5 && h <= 17.3) ? 1.0 : 0.0);
    double It = (h > 17.3 ? 1.0 : 0.0);

    // -----------------------------------------------------------
    // Compute Ds, Db, Dt separately (Fortran: replaced due to errors)
    // -----------------------------------------------------------
    double Ds = 0.0;
    double Db = 0.0;
    double Dt = 0.0;

    // Combined variables
    double G = std::pow((1.0 - 4.5 / Hx), r);
    double W = std::pow((1.0 - h / Hx), r);
    double X = std::pow((1.0 - 4.5 / Hx), p);
    double Y = std::pow((1.0 - h / Hx), p);
    double Z = std::pow((1.0 - 17.3 / Hx), p);
    double T = std::pow((Hx - h) / (Hx - 17.3), q);

    // -------- stump section: 0–4.5 ft
    if (Is == 1.0) {
        Ds = D2 * (1.0 + (c + e / D3) * (G - W) / (1.0 - G));
    }

    // -------- breast-height to 17.3 ft
    if (Ib == 1.0) {
        Db = D2 - (D2 - F2) * (X - Y) / (X - Z);
    }

    // -------- above 17.3 ft: parabolic taper + mode adjustment
    if (It == 1.0) {
        Dt = F2 - (F2 - Dx2) * (1.0 - T);
    }

    // -----------------------------------------------------------
    // Final diameter = sqrt(Ds + Db + Dt)
    // -----------------------------------------------------------
    double sum = Ds + Db + Dt;
    if (sum > 0.0)
        stmDib = std::sqrt(sum);

    if (stmDib < 0.0)
        stmDib = 0.0;

    return stmDib;
}

//  Calculates height(stemHt) at which the specified inside - bark
//  diameter(stmDib) occurs, given inside - bark dbh(dbhIb), inside - bark
//  diameter at 17.3' (dib17) and total height (totHt).  r, c, e, p, b,
//  and a are the coefficients for inside - bark calculations.

//struct CLKCOEF {
//    double R, C, E, P, B, A;
//    double TOTHT, DBHIB, DIB17;
//};

// ---------------------------------------------------------------
// C++ version of: SUBROUTINE R9HT(stemHt, COEFFS, stmDib, errFlg)
// ---------------------------------------------------------------
double ClarkTaperModel::ClarkHt(double stmDib, bool useDob)
{
    double r = clarkCoef.r;
    double c = clarkCoef.c;
    double e = clarkCoef.e;
    double p = clarkCoef.p;
    double b = clarkCoef.b;
    double a = clarkCoef.a;
    double dib17 = clarkCoef.dib17;
    double dbhIb = clarkCoef.dbhIb;

    if (useDob) {
        r = clarkCoefOb.r;
        c = clarkCoefOb.c;
        e = clarkCoefOb.e;
        p = clarkCoefOb.p;
        b = clarkCoefOb.b;
        a = clarkCoefOb.a;
        dib17 = clarkCoefOb.dib17;
        dbhIb = dbhOb;
    }
    double xxx = 0.0;

    double stemHt = 0.0;

    // If equation use height to 4, 7, 9 top
    if (eqHeightType == 4 || eqHeightType == 7 || eqHeightType == 9) {
        return ClarkHtH479(stmDib, useDob);
    }
    // -----------------------------------------------------------
    // Combined variables
    // -----------------------------------------------------------
    double G = std::pow(1.0 - 4.5 / totHt, r);
    double W = (c + e / std::pow(dbhIb, 3.0)) / (1.0 - G);

    double X = std::pow(1.0 - 4.5 / totHt, p);
    double Y = std::pow(1.0 - 17.3 / totHt, p);

    double Z = (dbhIb * dbhIb - dib17 * dib17) / (X - Y);

    // -----------------------------------------------------------
    // Height indicator variables (based on stmDib)
    // -----------------------------------------------------------

    double Is = (stmDib >= dbhIb) ? 1.0 : 0.0;
    double Ib = (stmDib < dbhIb && stmDib >= dib17) ? 1.0 : 0.0;
    double It = (stmDib < dib17) ? 1.0 : 0.0;

    // Mode indicator Im
    double Im = (stmDib * stmDib >
        b * std::pow(a - 1.0, 2) * dib17 * dib17) ?
        1.0 : 0.0;

    double Qa = b + Im * (1.0 - b) / std::pow(a, 2);
    double Qb = -2.0 * b - Im * 2.0 * (1.0 - b) / a;
    double Qc = b + (1.0 - b) * Im - (stmDib * stmDib) / (dib17 * dib17);

    // -----------------------------------------------------------
    // Compute height based on diameter
    // (NCrookston 2017 corrected logic)
    // -----------------------------------------------------------

    // --- Case 1: stump section (near dbh)
    if (Is == 1.0)
    {
        xxx = ((stmDib * stmDib) / (dbhIb * dbhIb) - 1.0) / W + G;

        if (xxx > 0.0)
        {
            stemHt = totHt * (1.0 - std::pow(xxx, 1.0 / r));
        }
    }

    // --- Case 2: 4.5–17.3 ft region
    else if (Ib == 1.0)
    {
        xxx = X - ((dbhIb * dbhIb - stmDib * stmDib) / Z);

        if (xxx > 0.0)
        {
            stemHt = totHt * (1.0 - std::pow(xxx, 1.0 / p));
        }
    }

    // --- Case 3: above 17.3 ft (parabolic region)
    else
    {
        xxx = Qb * Qb - 4.0 * Qa * Qc;

        if (xxx > 0.0)
        {
            stemHt =
                17.3 +
                (totHt - 17.3) *
                ((-Qb - std::sqrt(xxx)) / (2.0 * Qa));
        }
    }

    return stemHt;
}

//  Calculates height(stemHt) at which the specified inside - bark
//  diameter(stmDib) occurs, given inside - bark dbh(dbhIb), inside - bark
//  diameter at 17.3' (dib17) and top height (topHt).  r, c, e, p,
//  and q are the coefficients for inside - bark calculations.

double ClarkTaperModel::ClarkHtH479(double stmDib, bool useDob) {
    double r = clarkCoef.r;
    double c = clarkCoef.c;
    double e = clarkCoef.e;
    double p = clarkCoef.p;
    double q = clarkCoef.q;
    double dib17 = clarkCoef.dib17;
    double dbhIb = clarkCoef.dbhIb;

    if (useDob) {
        r = clarkCoefOb.r;
        c = clarkCoefOb.c;
        e = clarkCoefOb.e;
        p = clarkCoefOb.p;
        q = clarkCoefOb.q;
        dib17 = clarkCoefOb.dib17;
        dbhIb = dbhOb;
    }
   
    double stmHt = 0.0;
    double D = dbhIb;
    double Dx = topDib;
    double F = dib17;
    double Hx = topHt;
    double d = stmDib;

    if (d <= Dx) return Hx;

    double D2 = D * D;
    double D3 = D2 * D;
    double F2 = F * F;
    double Dx2 = Dx * Dx;
    double d2 = d * d;

    //Indicator virables
    double Is = (d2 >= D2 ? 1.0 : 0.0);
    double Ib = ((d2 < D2 && d2 >= F2) ? 1.0 : 0.0);
    double It = (d2 < F2 ? 1.0 : 0.0);

    //Combined variables
    double G = std::pow((1.0 - 4.5 / Hx), r);
    double W = (c + e / D3) / (1 - G);
    double X = std::pow((1.0 - 4.5 / Hx), p);
    double Y = std::pow((1.0 - 17.3 / Hx), p);
    double Z = (D2 - F2) / (X - Y);
    double T = D2 - Z * X;
    double J = std::pow((1.0 - 17.3 / Hx), q);
    double R = (F2 - Dx2) / J;
    double N = F2 - R * J;

    double Hs = 0.0;
    double Hb = 0.0;
    double Ht = 0.0;

    // --- Case 1: stump section (near dbh)
    if (Is == 1.0) {
        Hs = 1.0 - std::pow(((d2 / D2 - 1.0) / W + G), 1.0 / r);
    }

    // --- Case 2: 4.5–17.3 ft region
    if (Ib == 1.0) {
        Hb = 1.0 - std::pow((X - (D2 - d2) / Z), 1.0 / p);
    }

    // --- Case 3: above 17.3 ft (parabolic region)
    if (It == 1.0) {
        Ht = 1.0 - std::pow((J - (F2 - d2) / R), 1.0 / q);
    }

    stmHt = Hx * (Hs + Hb + Ht);

    return stmHt;
}
void ClarkTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    int errFlg = 0;
    dbhOb = tree.dbh;
    if (volumeEquation_.geoCode == VolumeEquation::GeoCode::R9)
    {
        sawDib = merchRules.minTopDibSaw;
        plpDib = merchRules.minTopDibNonSaw;
        clarkCoef.dbhIb = clarkCoef.a4 + clarkCoef.b4 * tree.dbh;
        dbhIb = clarkCoef.dbhIb;

        if (tree.totalHeight > 0.0) {
            topDib = 0.0;
            topHt = tree.totalHeight;
            shortTreeReset(topHt);
        }
        else if (tree.merchHeightNonsaw > 0.0) {

            topDib = 4.0;
            topHt = tree.merchHeightNonsaw;
            shortTreeReset(topHt);
        }
        else {

            if (volumeEquation_.fiaCode < 300) {
                topDib = 7.0;
            }
            else {
                topDib = 9.0;
            }

            if (tree.referenceHeight > 0.0) {
                topHt = tree.referenceHeight;
                shortTreeReset(topHt);
            }
            else {

                if (tree.merchHeightSaw >= 17.4) {

                    // Use linear extrapolation from sawDib to topDib
                    if ((tree.dbh - sawDib) > 0.0) {
                        topHt = 4.5 + (tree.merchHeightSaw - 4.5) * (tree.dbh - topDib) / (tree.dbh - sawDib);
                    }
                    else {
                        errFlg = 13;
                        return;    // Same behavior as Fortran RETURN
                    }
                }
                else {
                    shortTreeReset(tree.merchHeightSaw);
                }
            }
        }
        // reset a17 and b17
        if (topDib == 0.0) {
            clarkCoef.a17 = coef0[sppIdx][1];
            clarkCoef.b17 = coef0[sppIdx][2];
        }
        else if (topDib == 4.0) {
            clarkCoef.a17 = coef4[sppIdx][1];
            clarkCoef.b17 = coef4[sppIdx][2];
        }
        else if (topDib == 7.0 || topDib == 9.0) {
            clarkCoef.a17 = coef79[sppIdx][1];
            clarkCoef.b17 = coef79[sppIdx][2];
        }

        //Calculate DIB at 4.5' from DBH (eqn 7)
        clarkCoef.dbhIb = clarkCoef.a4 + clarkCoef.b4 *tree.dbh;
        dbhIb = clarkCoef.dbhIb;

        if (dbhIb >= tree.dbh || dbhIb <= 0.0) dbhIb = std::max(tree.dbh - 0.1, 0.1);
        
        if (((topDib > dbhIb) && (topHt > 4.5)) ||
            ((topDib < dbhIb) && (topHt < 4.5)))
        {
            errFlg = 11;
            return;   // Ends the function exactly like Fortran RETURN
        }

        //Calculate DIB at 17.3' from top height and DBH (eqn 9)

        if (std::abs(tree.merchHeightNonsaw - 17.3) < 0.00001) {

            clarkCoef.dib17 = plpDib;

        }
        else if ((std::abs(tree.merchHeightSaw - 17.3) < 0.00001) && (tree.referenceHeight < 0.01)) {

            clarkCoef.dib17 = sawDib;

        }
        else if (topHt > 17.3) {

            clarkCoef.dib17 = dbhIb * (clarkCoef.a17 + clarkCoef.b17 * std::pow(17.3 / topHt, 2));
            clarkCoef.dib17 = std::max(clarkCoef.dib17, topDib + 0.1);

        }
        else {

            clarkCoef.dib17 = topDib - 0.1;
        }

        // ----------------------------------------------------------------------
        // Ensure DIB at 17.3 is large enough for product top diameters
        // ----------------------------------------------------------------------

        if ((tree.referenceHeight < 0.01) && (tree.merchHeightSaw > 17.3) && (clarkCoef.dib17 < sawDib)) {

            clarkCoef.dib17 = sawDib +
                (dbhIb - sawDib) * (tree.merchHeightSaw - 17.3) / (tree.merchHeightSaw - 4.5);

        }
        else if ((tree.referenceHeight > 17.3) && (clarkCoef.dib17 < sawDib)) {

            clarkCoef.dib17 = sawDib +
                (dbhIb - sawDib) * (tree.referenceHeight - 17.3) / (tree.referenceHeight - 4.5);

        }
        else if ((tree.merchHeightNonsaw > 17.3) && (clarkCoef.dib17 < plpDib)) {

            clarkCoef.dib17 = plpDib +
                (dbhIb - plpDib) * (tree.merchHeightNonsaw - 17.3) / (tree.merchHeightNonsaw - 4.5);
        }

        // Enforce minimum dib
        if (clarkCoef.dib17 < 0.1)
            clarkCoef.dib17 = 0.1;

        //get total height
        totHt = GetTotalHeight(tree.totalHeight, dbhIb, clarkCoef.dib17, topHt, topDib, clarkCoef.a, clarkCoef.b);

        //set the volume correction factor
        if (volumeEquation_.fiaCode < 300) r9VolCorFactor = 1.04;
        else if ((volumeEquation_.fiaCode >= 741 && volumeEquation_.fiaCode <= 746) || volumeEquation_.fiaCode == 621)
            r9VolCorFactor = 1.0;
        else r9VolCorFactor = 1.1;

    } //end geoCode R9
    else if (volumeEquation_.geoCode == VolumeEquation::GeoCode::R8)
    {
        if (eqHeightType == 4) {
            if (tree.referenceDiameter != 4.0 && tree.referenceHeight == 0.0 && tree.merchHeightNonsaw == 0.0) {
                throw std::invalid_argument("Reference height to 4 inch top is needed for this equation");
            }
            topDib = 4.0;
        }

        if (eqHeightType == 7  || eqHeightType == 9) {
            if ((tree.referenceDiameter != 7.0 && tree.referenceDiameter != 9.0) && tree.referenceHeight == 0.0 && tree.merchHeightSaw == 0.0) {
                throw std::invalid_argument("Reference height to 7 or 9 inch top is needed for this equation");
            }
            topDib = (volumeEquation_.fiaCode < 300) ? 7.0 : 9.0;
        }

        if (tree.totalHeight > 0.0 && topDib == 0.0) {
            topDib = 0.0;
            topHt = tree.totalHeight;
            shortTreeReset(topHt);
            
        }
        else if (tree.referenceHeight > 0.0) {
            if (tree.referenceDiameter == 4.0) {
                topDib = 4.0;
                topHt = tree.referenceHeight;
                shortTreeReset(topHt);
            }
            else if (tree.referenceDiameter == 7.0 || tree.referenceDiameter == 9.0) {
                topDib = (volumeEquation_.fiaCode < 300) ? 7.0 : 9.0;
                topHt = tree.referenceHeight;
                shortTreeReset(topHt);
                
            }
        }
        else if (tree.merchHeightNonsaw > 0.0) {
            topDib = 4.0;
            topHt = tree.merchHeightNonsaw;
            shortTreeReset(topHt);
            
        }
        else if (tree.merchHeightSaw > 0.0) {

            topDib = (volumeEquation_.fiaCode < 300) ? 7.0 : 9.0;
            topHt = tree.merchHeightSaw;
            shortTreeReset(topHt);
            
        }

        if (topHt == 0.0) {
            throw std::invalid_argument("Upper stem height measurements required");
        }

        double topDob = topDib; //R8 uses top DOB

        //calculate dbhIb
        clarkCoef.dbhIb = clarkCoef.a4 + clarkCoef.b4 * tree.dbh;
        dbhIb = clarkCoef.dbhIb;
        if (dbhIb < clarkCoef.fixDi) dbhIb = clarkCoef.fixDi;

        //calculate dib and dob at 17.3 (form class)
        if (topDob == 0.0) {
            clarkCoef.dib17 = tree.dbh * (clarkCoef.a17 + clarkCoef.b17 * std::pow(17.3 / topHt, 2));
            if (clarkCoef.dib17 < 0.0) clarkCoef.dib17 = 0.1;
            double FCLSS = clarkCoef.dib17;
            double DBH = tree.dbh;
            double THT = tree.totalHeight;

            if (volSp != 221 && volSp != 222 && volSp != 544) {

                double FCMIN = 0.0;

                if (spgrp == 100) {
                    if (THT < 32.5)                    FCMIN = 56;
                    else if (THT < 37.5)               FCMIN = 64;
                    else if (THT < 42.5)               FCMIN = 66;
                    else                               FCMIN = 67;
                }
                else if (spgrp == 300) {
                    if (THT < 32.5)                    FCMIN = 57;
                    else if (THT < 37.5)               FCMIN = 60;
                    else if (THT < 42.5)               FCMIN = 64;
                    else                               FCMIN = 67;
                }
                else {
                    if (THT < 32.5)                    FCMIN = 58;
                    else if (THT < 37.5)               FCMIN = 65;
                    else if (THT < 42.5)               FCMIN = 67;
                    else                               FCMIN = 69;
                }

                double FCDIB = DBH * FCMIN * 0.01;

                if (THT < 47.5 && FCLSS < FCDIB)
                    FCLSS = FCDIB;

                clarkCoef.dib17 = FCLSS;
                clarkCoefOb.dib17 = (FCLSS - clarkCoef.afi) / clarkCoef.bfi;
            }
        }
        else if (topDob == 4.0) {
            clarkCoef.a17 = R8CF[geoSppIdx][7];
            clarkCoef.b17 = R8CF[geoSppIdx][8];
            clarkCoefOb.a17 = obR8CF[geoSppIdx][5];
            clarkCoefOb.b17 = obR8CF[geoSppIdx][6];
            clarkCoef.dib17 = tree.dbh * (clarkCoef.a17 + clarkCoef.b17 * std::pow(17.3 / topHt, 2));

            // Assign initial values
            double DBH = tree.dbh;
            double HT2 = topHt;
            double FCLSS = clarkCoef.dib17;

            if (FCLSS < clarkCoef.fixDi)
                FCLSS = clarkCoef.fixDi;

            double FCMIN = 0.0;

            // Condition: SPEC ≠ 221,222,544
            if (volSp != 221 && volSp != 222 && volSp != 544) {

                if (spgrp == 100) {

                    if (DBH < 5.5) {
                        if (HT2 < 22.5)                     FCMIN = 70;
                        else if (HT2 < 27.5)                FCMIN = 75;
                        else                                FCMIN = 80;
                    }
                    else if (DBH < 6.5) {
                        if (HT2 < 22.5)                     FCMIN = 66;
                        else if (HT2 < 27.5)                FCMIN = 70;
                        else                                FCMIN = 74;
                    }
                    else if (DBH < 7.5) {
                        if (HT2 < 22.5)                     FCMIN = 64;
                        else if (HT2 < 27.5)                FCMIN = 67;
                        else                                FCMIN = 72;
                    }
                    else if (DBH < 8.5) {
                        if (HT2 < 22.5)                     FCMIN = 60;
                        else if (HT2 < 27.5)                FCMIN = 67;
                        else                                FCMIN = 69;
                    }
                    else {
                        if (HT2 < 22.5)                     FCMIN = 59;
                        else if (HT2 < 27.5)                FCMIN = 67;
                        else                                FCMIN = 69;
                    }
                }

                else if (spgrp == 300) {

                    if (DBH < 5.5) {
                        if (HT2 < 22.5)                     FCMIN = 74;
                        else if (HT2 < 27.5)                FCMIN = 76;
                        else                                FCMIN = 76;
                    }
                    else if (DBH < 6.5) {
                        if (HT2 < 22.5)                     FCMIN = 65;
                        else if (HT2 < 27.5)                FCMIN = 69;
                        else                                FCMIN = 74;
                    }
                    else if (DBH < 7.5) {
                        if (HT2 < 22.5)                     FCMIN = 61;
                        else if (HT2 < 27.5)                FCMIN = 65;
                        else                                FCMIN = 68;
                    }
                    else {
                        if (HT2 < 22.5)                     FCMIN = 60;
                        else if (HT2 < 27.5)                FCMIN = 62;
                        else                                FCMIN = 68;
                    }
                }

                else {  // all other SPGRP

                    if (DBH < 5.5) {
                        if (HT2 < 22.5)                     FCMIN = 71;
                        else if (HT2 < 27.5)                FCMIN = 72;
                        else                                FCMIN = 76;
                    }
                    else if (DBH < 6.5) {
                        if (HT2 < 22.5)                     FCMIN = 68;
                        else if (HT2 < 27.5)                FCMIN = 71;
                        else                                FCMIN = 74;
                    }
                    else {
                        if (HT2 < 22.5)                     FCMIN = 63;
                        else if (HT2 < 27.5)                FCMIN = 67;
                        else                                FCMIN = 70;
                    }
                }

                // Compute DIB
                double FCDIB = DBH * FCMIN * 0.01;

                if (HT2 < 32.5 && FCLSS < FCDIB)
                    FCLSS = FCDIB;
            }

            clarkCoef.dib17 = FCLSS;
            clarkCoefOb.dib17 = tree.dbh * (clarkCoefOb.a17 + clarkCoefOb.b17 * std::pow(17.3 / topHt, 2));
        }
        else {
            clarkCoefOb.a17 = obR8CF[geoSppIdx][7];
            clarkCoefOb.b17 = obR8CF[geoSppIdx][8];
            if (volSp < 300) {
                clarkCoef.a17 = R8CF[geoSppIdx][9];
                clarkCoef.b17 = R8CF[geoSppIdx][10];
            }
            else {
                clarkCoef.a17 = R8CF[geoSppIdx][11];
                clarkCoef.b17 = R8CF[geoSppIdx][12];
            }
            clarkCoef.dib17 = tree.dbh * (clarkCoef.a17 + clarkCoef.b17 * std::pow(17.3 / topHt, 2));

            if (clarkCoef.dib17 < DIB479[sppIdx][2]) clarkCoef.dib17 = DIB479[sppIdx][2];
            if (clarkCoef.dib17 < 0.0) clarkCoef.dib17 = 0.1;

            clarkCoefOb.dib17 = tree.dbh * (clarkCoefOb.a17 + clarkCoefOb.b17 * std::pow(17.3 / topHt, 2));
            clarkCoef.fixDi = DIB479[sppIdx][2];
        }

        //Get total height
        totHt = GetTotalHeight(tree.totalHeight, tree.dbh, clarkCoefOb.dib17, topHt, topDob, clarkCoefOb.a, clarkCoefOb.b);

    }
}

//calculate topwood ratio for 7/9 equation
double ClarkTaperModel::ClarkTopwoodRatio(double dib17, double ht, double r1, double r2, double r3)
{
    return std::exp(r1 * std::pow(dib17, r2) * std::pow(ht, r3)) - 1.0;
}

double ClarkTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    return ClarkDib(height);
}

double ClarkTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob)
{
    return ClarkHt(diameter, useDob);
}

//get stem volume for stump, promary prod, topwood, and tip
StemVolume ClarkTaperModel::GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    StemVolume result = { 0.0,0.0,0.0,0.0, false, false };
    double tr1 = clarkCoef.tr1;
    double tr2 = clarkCoef.tr2;
    double tr3 = clarkCoef.tr3;
    double lowHt = 0.0;
    double upHt = merchRules.stumpHeight;
    result.stumpVol = ClarkCubicFootVol(lowHt, upHt);
    //double stumpvol = ClarkCubicVolH479(lowHt, upHt);
    lowHt = merchRules.stumpHeight;
    bool useDob = false;
    if (vco.region == 8) useDob = true;
    if (vco.primaryProduct == 1) {
        upHt = (tree.merchHeightSaw > 0.0 ? tree.merchHeightSaw : ClarkHt(merchRules.minTopDibSaw, useDob));
        if (eqHeightType == 4 || eqHeightType == 7 || eqHeightType == 9) {
            result.primaryVol = ClarkCubicVolH479(lowHt, upHt);
            if (eqHeightType == 7 || eqHeightType == 9) {
                double yr = ClarkTopwoodRatio(clarkCoef.dib17, topHt, tr1, tr2, tr3);
                result.topwoodVol = result.primaryVol * yr;
                if (result.topwoodVol < 0.0) result.topwoodVol = 0.0;
            }
        }
        else result.primaryVol = ClarkCubicFootVol(lowHt, upHt);

        upHt = (tree.merchHeightNonsaw > 0.0 ? tree.merchHeightNonsaw : ClarkHt(merchRules.minTopDibNonSaw, useDob));
        double vol4 = 0.0;
        if (eqHeightType != 4 && eqHeightType != 7 && eqHeightType != 9) {
            vol4 = ClarkCubicFootVol(lowHt, upHt);
            result.topwoodVol = vol4 - result.primaryVol;
            if (result.topwoodVol < 0.0) result.topwoodVol = 0.0;
        }
    }
    else
    {
        upHt = (tree.merchHeightNonsaw > 0.0 ? tree.merchHeightNonsaw : ClarkHt(merchRules.minTopDibNonSaw, useDob));
        
        if (eqHeightType == 4 || eqHeightType == 7 || eqHeightType == 9) {
            result.primaryVol = ClarkCubicVolH479(lowHt, upHt);
            if (eqHeightType == 7 || eqHeightType == 9) {
                double yr = ClarkTopwoodRatio(clarkCoef.dib17, topHt, tr1, tr2, tr3);
                result.topwoodVol = result.primaryVol * yr;
                if (result.topwoodVol < 0.0) result.topwoodVol = 0.0;
            }
        }
        else result.primaryVol = ClarkCubicFootVol(lowHt, upHt);

        result.topwoodVol = 0.0;
    }

    result.tipVol = ClarkCubicFootVol(lowHt, totHt) - result.primaryVol - result.topwoodVol;
    if (result.tipVol < 0.0) result.tipVol = 0.0;
    
    result.volCalculated = true;

    if (vco.region == 9) {

        result.stumpVol *= r9VolCorFactor;    // multiply AND store back into 'result'
        result.primaryVol *= r9VolCorFactor;
        result.topwoodVol *= r9VolCorFactor;
        result.tipVol *= r9VolCorFactor;
    }
    return result;
}