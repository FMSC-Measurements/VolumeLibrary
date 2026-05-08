#include "ClarkTaperModel.h"
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
        return idx;
    }
    //else //R8 species
}

//set the coef for the species
void ClarkTaperModel::setClarkCoef(int spcd)
{
    if (volumeEquation_.geoCode == VolumeEquation::GeoCode::R9)
    {
        int idx = findSpeciesIndex(spcd);
        sppIdx = idx;
        a4  = coefA[idx][2];
        b4  = coefA[idx][3];
        a17 = coef0[idx][1];
        b17 = coef0[idx][2];
        r   = coef0[idx][3];
        c   = coef0[idx][4];
        e   = coef0[idx][5];
        p   = coef0[idx][6];
        a   = coef0[idx][7];
        b   = coef0[idx][8];
        //need to reset a17 and b17 based on if total height is not measured, but measured ht1prd or ht2prd
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

    // Final volume
    double cfVol = 0.0;

    // If upper height <= 0, nothing to compute
    if (upprHt <= 0.0)
        return cfVol;

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

    if (cfVol < 0.0)
        cfVol = 0.0;

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

    double stmDib = 0.0;

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

//  Calculates height(stemHt) at which the specified inside - bark
//  diameter(stmDib) occurs, given inside - bark dbh(dbhIb), inside - bark
//  diameter at 17.3' (dib17) and total height (totHt).  r, c, e, p, b,
//  and a are the coefficients for inside - bark calculations.
#include <cmath>
#include <algorithm>

struct CLKCOEF {
    double R, C, E, P, B, A;
    double TOTHT, DBHIB, DIB17;
};

// ---------------------------------------------------------------
// C++ version of: SUBROUTINE R9HT(stemHt, COEFFS, stmDib, errFlg)
// ---------------------------------------------------------------
double ClarkTaperModel::ClarkHt(double stmDib)
{
    double xxx = 0.0;

    double stemHt = 0.0;

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

void ClarkTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    int errFlg = 0;
    if (volumeEquation_.geoCode == VolumeEquation::GeoCode::R9)
    {
        sawDib = merchRules.minTopDibSaw;
        plpDib = merchRules.minTopDibNonSaw;
        dbhIb = a4 + b4 * tree.dbh;
        if (tree.totalHeight > 0.0) {
            topDib = 0.0;
            if (tree.totalHeight >= 17.4) {
                topHt = tree.totalHeight;
            }
            else {

                shortHeight = true;
                topHt = 17.4;
                shrtHt = tree.totalHeight;
            }
        }
        else if (tree.merchHeightNonsaw > 0.0) {

            topDib = 4.0;

            if (tree.merchHeightNonsaw >= 17.4) {
                topHt = tree.merchHeightNonsaw;
            }
            else {
                shortHeight = true;
                topHt = 17.4;
                shrtHt = tree.merchHeightNonsaw;
            }
        }
        else {

            if (volumeEquation_.fiaCode < 300) {
                topDib = 7.0;
            }
            else {
                topDib = 9.0;
            }


            if (tree.referenceHeight > 0.0) {

                if (tree.referenceHeight >= 17.4) {
                    topHt = tree.referenceHeight;
                }
                else {
                    shortHeight = true;
                    topHt = 17.4;
                    shrtHt = tree.referenceHeight;
                }
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

                    shortHeight = true;
                    topHt = 17.4;
                    shrtHt = tree.merchHeightSaw;

                }
            }
        }
        // reset a17 and b17
        if (topDib == 0.0) {
            a17 = coef0[sppIdx][1];
            b17 = coef0[sppIdx][2];
        }
        else if (topDib == 4.0) {
            a17 = coef4[sppIdx][1];
            b17 = coef4[sppIdx][2];
        }
        else if (topDib == 7.0 || topDib == 9.0) {
            a17 = coef79[sppIdx][1];
            b17 = coef79[sppIdx][2];
        }

        //Calculate DIB at 4.5' from DBH (eqn 7)
        dbhIb = a4 + b4 *tree.dbh;
        
        if (dbhIb >= tree.dbh || dbhIb <= 0.0) dbhIb = std::max(tree.dbh - 0.1, 0.1);
        
        if (((topDib > dbhIb) && (topHt > 4.5)) ||
            ((topDib < dbhIb) && (topHt < 4.5)))
        {
            errFlg = 11;
            return;   // Ends the function exactly like Fortran RETURN
        }

        //Calculate DIB at 17.3' from top height and DBH (eqn 9)

        if (std::abs(tree.merchHeightNonsaw - 17.3) < 0.00001) {

            dib17 = plpDib;

        }
        else if ((std::abs(tree.merchHeightSaw - 17.3) < 0.00001) && (tree.referenceHeight < 0.01)) {

            dib17 = sawDib;

        }
        else if (topHt > 17.3) {

            // volEq(1:1) == '9'  →  volEq[0] == '9'
            //if (volEq.size() > 0 && volEq[0] == '9') {
                // Regression for R9 uses dbhIb
                dib17 = dbhIb * (a17 + b17 * std::pow(17.3 / topHt, 2));
            //}
            //else {
            //    // Regression for R8 uses dbhOb
            //    dib17 = dbhOb * (a17 + b17 * std::pow(17.3 / topHt, 2));
            //}

            dib17 = std::max(dib17, topDib + 0.1);

        }
        else {

            dib17 = topDib - 0.1;
        }

        // ----------------------------------------------------------------------
        // Ensure DIB at 17.3 is large enough for product top diameters
        // ----------------------------------------------------------------------

        if ((tree.referenceHeight < 0.01) && (tree.merchHeightSaw > 17.3) && (dib17 < sawDib)) {

            dib17 = sawDib +
                (dbhIb - sawDib) * (tree.merchHeightSaw - 17.3) / (tree.merchHeightSaw - 4.5);

        }
        else if ((tree.referenceHeight > 17.3) && (dib17 < sawDib)) {

            dib17 = sawDib +
                (dbhIb - sawDib) * (tree.referenceHeight - 17.3) / (tree.referenceHeight - 4.5);

        }
        else if ((tree.merchHeightNonsaw > 17.3) && (dib17 < plpDib)) {

            dib17 = plpDib +
                (dbhIb - plpDib) * (tree.merchHeightNonsaw - 17.3) / (tree.merchHeightNonsaw - 4.5);
        }

        // Enforce minimum dib
        if (dib17 < 0.1)
            dib17 = 0.1;

        //get total height
        totHt = GetTotalHeight(tree.totalHeight, dbhIb, dib17, topHt, topDib, a, b);

        //set the volume correction factor
        if (volumeEquation_.fiaCode < 300) r9VolCorFactor = 1.04;
        else if ((volumeEquation_.fiaCode >= 741 && volumeEquation_.fiaCode <= 746) || volumeEquation_.fiaCode == 621)
            r9VolCorFactor = 1.0;
        else r9VolCorFactor = 1.1;

    } //end geoCode R9

}

double ClarkTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    return ClarkDib(height);
}

double ClarkTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter)
{
    return ClarkHt(diameter);
}

//get stem volume for stump, promary prod, topwood, and tip
std::array<double, 4> ClarkTaperModel::GetStemVolumes(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    std::array<double, 4> result = { 0.0,0.0,0.0,0.0 };
    double lowHt = 0.0;
    double upHt = merchRules.stumpHeight;
    result[0] = ClarkCubicFootVol(lowHt, upHt);
    lowHt = merchRules.stumpHeight;
    if (vco.primaryProduct == 1) {
        upHt = GetHeightAtDiameter(tree, merchRules.minTopDibSaw);
        result[1] = ClarkCubicFootVol(lowHt, upHt);
        upHt = GetHeightAtDiameter(tree, merchRules.minTopDibNonSaw);
        result[2] = ClarkCubicFootVol(lowHt, upHt) - result[1];
    }
    else
    {
        upHt = GetHeightAtDiameter(tree, merchRules.minTopDibNonSaw);
        result[1] = ClarkCubicFootVol(lowHt, upHt);
        result[2] = 0.0;
    }
    result[3] = ClarkCubicFootVol(lowHt, tree.totalHeight) - result[1] - result[2];

    if (vco.region == 9) {

        for (double& v : result) {
            v *= r9VolCorFactor;    // multiply AND store back into 'result'
        }
    }
    return result;
}