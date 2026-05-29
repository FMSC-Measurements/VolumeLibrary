#include "BehreHyperbolaTaperModel.h"
#include "../DefaultFormClassForFVS.h"
#include "../Volumecalculators/JenkinsBiomass.h"

BehreHyperbolaTaperModel::BehreHyperbolaTaperModel(VolumeEquation volumeEquation)
    : TaperModel(), volEqStr(volumeEquation.GetVolumeEquationNumber())
{
    
}

// -----------------------------
// BLMTAP: computes D2 (Diameter Inside Bark)
// -----------------------------
double BehreHyperbolaTaperModel::BLMTAP(double DBHOB, double HTTOT, double TLH, double HTUP,
    double D17, double TOP, double XLEN, int Profile)
{
    // Ensure Profile maps to 0..9 for BLMTHT
    if (Profile < 1 || Profile > 10) {
        // Fallback to "ALL OTHER SPECIES"
        Profile = 10;
    }
    const auto& coeff = BLMTHT[Profile - 1];

    double D2 = 0.0;

    // Height given in FEET (TLH == 0.0)
    if (TLH == 0.0) {
        const double HBUTT = HTTOT - (XLEN + stumpHeight); //should use stump height to replace the 1.5
        if (HBUTT <= 0.0) {
            return 0.0;
        }

        const double HTDIB = HTTOT - HTUP;

        const double A = coeff[0]
            + coeff[1] * DBHOB
            + coeff[2] * HTTOT
            + coeff[3] * DBHOB * HTTOT;
        const double B = 1.0 - A;

        const double ratio = HTDIB / HBUTT;
        const double DIBCOR = D17 * (ratio / (A * ratio + B));
        D2 = DIBCOR;

        // (Original comment suggested returning zero for feet mode;
        //  your current code assigns D2 = DIBCOR, so we mirror that.)
    }
    else {
        // Height given in number of LOGS
        if (D17 < 5.0) D17 = 5.0;
        if (D17 < TOP) D17 = TOP;

        if (TLH == 1.0) return D17;
        else if (TLH == 2.0) {
            if (HTUP == 1.0) return D17;
            else return TOP;
        }

        if (TOP == D17)  return D17;

        double RA = 0.0;
        double Tolerance = 1.0;
        const double Tol_Limit = 0.01;

        // Initial estimate
        double A = 0.62;
        double B = 1.0 - A;
        const double C = (TLH - 1.0) * XLEN;
        double H = C / (1.0 - TOP * B / (D17 - A * TOP));
        double HtTot_est = H + XLEN + stumpHeight; // retained though not used in the A-formula

        const int iLimit = 20;
        int iCount = 0;

        // Iteration
        while (Tolerance > Tol_Limit && iCount < iLimit) {
            ++iCount;

            // Note: Retains your Fortran formula using actual HTTOT rather than HtTot_est
            A = coeff[0]
                + coeff[1] * DBHOB
                + coeff[2] * HTTOT
                + coeff[3] * DBHOB * HTTOT;
            B = 1.0 - A;

            H = C / (1.0 - TOP * B / (D17 - A * TOP));
            HtTot_est = H + XLEN + stumpHeight;

            Tolerance = std::fabs(RA - A);
            RA = A;
        }

        const double SMALLH = H - (HTUP - 1.0) * XLEN;
        const double LN = SMALLH / H;
        const double DIBCOR = D17 * LN / (A * LN + B);
        D2 = DIBCOR;

    }

    return D2;
}

// -----------------------------
// BEHTAP: wrapper calculating D17 and calling BLMTAP or simplified flow
// -----------------------------
double BehreHyperbolaTaperModel::BEHtaper(const std::string& VOLEQ_in,
    double DBHOB, double HTTOT, double TLH, double HTUP,
    int FCLASS, double TOP)
{
    const std::string VOLEQ = normalize_voleq(VOLEQ_in);

    double XLEN = 16.3;  //should use max log length + trim to replace the 16.3
    double D2 = 0.0;
    double D17 = (DBHOB * static_cast<double>(FCLASS)) / 100.0;

    if (starts_with_b(VOLEQ)) {
        D17 = std::round(D17); // ANINT in Fortran
        if (slice_1based(VOLEQ, 4, 3) == "B32") XLEN = 32.6;

        int PROFILE = 10;
        int TAPEQU = 56;
        BLMTAPEQ(VOLEQ, PROFILE, TAPEQU);

        if (HTUP == 4.5) D2 = double_bark(TAPEQU, DBHOB);
        else D2 = BLMTAP(DBHOB, HTTOT, TLH, HTUP, D17, TOP, XLEN, PROFILE);
    }
    else {
        // Set XLEN by VOLEQ prefix
        if (slice_1based(VOLEQ, 1, 3) == "632") XLEN = 32.6;
        else XLEN = 16.3;

        double A = 0.62;
        if (slice_1based(VOLEQ, 1, 1) == "I") {  //for BIA Behr equation I16BEH
            A = 0.49;
        }
        double B = 1.0 - A;

        if (HTTOT > 0.0) {
            // Height in FEET
            const double H1 = HTTOT - XLEN - stumpHeight;
            if (H1 <= 0.0) return 0.0;

            const double HX = HTTOT - HTUP;
            const double HR = HX / H1;
            const double DR = HR / (A * HR + B);
            D2 = D17 * DR;
        }
        else {
            // Height in number of LOGS
            const double T = TOP / D17;
            const double AT = A / (1.0 - A * T);
            const double BT = (1.0 / (1.0 - T)) - AT;
            const double H1 = (TLH - 1.0) * XLEN - stumpHeight;
            const double HX = TLH * XLEN - HTUP;
            const double HR = HX / H1;
            const double DR = T + (HR / (AT * HR + BT));
            D2 = D17 * DR;
        }
    }

    return D2;
}

void BehreHyperbolaTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    topDibSaw = merchRules.minTopDibSaw;
    formClass = tree.formClass;
    if (tree.formClass == 0) {
        if (vco.volumeCalculationOptions == VolumeCalculationOptions::VolumeCalculationType::CRUISE) {
            throw std::invalid_argument("Form Class missing");
        }
        else {
            formClass = GetFormClass(volEqStr, vco.forest, tree.dbh);
        }
    }

    if (slice_1based(volEqStr, 4, 3) == "B32" || slice_1based(volEqStr, 1, 3) == "632") formClassHeight = 33.6;

    dbhIb = tree.dbh - merchRules.doubleBarkThicknessAtBrestHeight;

    d17 = (tree.dbh * static_cast<double>(formClass)) / 100.0;

    stumpHeight = merchRules.stumpHeight;
}

double BehreHyperbolaTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    double totalLogHeight = 0.0;
    if (tree.merchHeightUnit != TreeMeasurment::MerchHeightUnit::FEET) totalLogHeight = tree.totalHeight;

    return BEHtaper(volEqStr, tree.dbh, tree.totalHeight, totalLogHeight, height, formClass, topDibSaw);
}

double BehreHyperbolaTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob)
{
    double dbh = tree.dbh;
    double totalHeight = tree.totalHeight;

    // Check if diameter is effectively zero
    if (diameter <= 0.1) {
        return totalHeight;
    }

    // Binary search for the height that gives the target diameter
    double lowHeight = 0.0;
    double highHeight = totalHeight;
    double tolerance = 0.01;  // 0.01 feet tolerance

    while (highHeight - lowHeight > tolerance) {
        double midHeight = (lowHeight + highHeight) / 2.0;
        double calcDiameter = GetDiameterAtHeight(tree, midHeight);

        if (calcDiameter > diameter) {
            lowHeight = midHeight;
        }
        else {
            highHeight = midHeight;
        }
    }

    return (lowHeight + highHeight) / 2.0;

}

StemVolume BehreHyperbolaTaperModel::GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    StemVolume result = { 0.0,0.0,0.0,0.0,false,false };

    //no calculation here for BLM Behre equation
    if (starts_with_b(volEqStr)) return result;

    //Region 6 BEH equation statts here
    StumpVolume stumpVol = raileVol(vco.fiaCode, tree.dbh, merchRules.stumpHeight);
    result.stumpVol = stumpVol.woodVol;

    if (tree.totalHeight < formClassHeight || dbhIb < topDibSaw) {
        result.primaryVol = 0.00272708 * (dbhIb * dbhIb) * tree.totalHeight;
    }
    else if (d17 < topDibSaw) {
        //small tree use d17 for butt log
        double logVol = 0.00272708 * (dbhIb * dbhIb + d17 * d17) * formClassHeight;
        result.primaryVol = logVol;
        //find tip volume
        logVol = 0.00272708 * (d17 * d17) * (tree.totalHeight - formClassHeight);
        result.primaryVol += logVol;
    }
    else {
        // find cubicfoot volume for butt log
        double H17 = formClassHeight;
        double logVol = 0.00272708 * (dbhIb * dbhIb + d17 * d17) * H17;
        result.primaryVol = logVol;

        double HTUP = tree.totalHeight - H17;
        double A = 0.62;
        double B = 1.0 - A;
        double topD = topDibSaw;
        double S = 0.0;
        std::array<double, 21> D;

        D[1] = d17;

        int I = 2;
        bool hitLabel100 = false;
        bool hitLabel130 = false;

        for (I = 2; I <= 20; ++I) {

            double HRATIO = (HTUP - ((I - 1) * 16.3)) / HTUP;
            if (HRATIO <= 0.0) {
                hitLabel100 = true;
                break;
            }

            double DR = HRATIO / (A * HRATIO + B);
            D[I] = DR * D[1];

            if (D[I] < topD) {
                hitLabel100 = true;
                break;
            }

            // FIND VOLUME FOR THE SECTION
            logVol = 0.00272708 * (D[I - 1] * D[I - 1] + D[I] * D[I]) * 16.3;
            result.primaryVol += logVol;

            if (D[I] == topD) {
                S = 16.3;
                hitLabel130 = true;
                break;
            }
        }

        // ----- FORTRAN LABEL 100 LOGIC -----
        if (hitLabel100) {

            double DR = topD / d17;
            double HX = (DR * B * HTUP) / (1.0 - (A * DR));
            double H = (I - 2) * 16.3;
            S = HTUP - HX - H;

            logVol = 0.00272708 * (D[I - 1] * D[I - 1] + topD * topD) * S;
            result.primaryVol += logVol;

            // Falls through to FORTRAN label 130 (no early exit)
            hitLabel130 = true;
        }

        // ----- FORTRAN LABEL 130 LOGIC -----
        if (hitLabel130) {

            HTUP = tree.totalHeight - (16.3 * (I - 2) + H17) - S;

            logVol = 0.00272708 * (topD * topD) * HTUP;
            result.primaryVol += logVol;
        }
    }

    result.volCalculated = true;
    result.isBEH = true;

    return result;
}
