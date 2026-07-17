#include "CzaplewskiTaperModel.h"

double CzaplewskiTaperModel::R2taper(double dbh, double totalHeight, double heightExtra, double diameterExtra, double top6Ht, double dbtbh, double ht2, int& errflg)
{
    std::string VOLEQ = volEqStr_;
    double DBH = dbh;
    double HTTOT = totalHeight;
    double HTUP = std::min(ht2, HTTOT);
    int SP = -1;
    errflg = 0;
    double D2 = 0.0;
    double D30 = diameterExtra;
    double H30 = heightExtra;
    double TOP6 = top6Ht;
    double DBTBH = dbtbh;

    if (VOLEQ.length() < 10) { errflg = 1; return D2; }
    std::string v810 = VOLEQ.substr(7, 3);
    std::string v13 = VOLEQ.substr(0, 3);
    std::string MDL = VOLEQ.substr(3, 3);
    for (auto& c : MDL) c = toupper(c);

    if (v810 == "746") SP = 0;
    else if (v810 == "108") SP = 1;
    else if (v810 == "122" && v13 == "203") SP = 2;
    else if (v810 == "122" && v13 == "200") SP = 3;
    else if (v810 == "015") SP = 4;
    else if (v810 == "019") SP = 5;
    else if (v810 == "093") SP = 6;
    else if (v810 == "202") SP = 7;
    else {
        errflg = 1;
        return D2;
    }

    // ***************** 2-PARAMETER MODEL *****************
    if (MDL == "CZ2") {
        double ratio = HTUP / HTTOT;
        double TERM1 = R2MBB[SP][0] * (ratio - 1.0);
        double TERM2 = R2MBB[SP][1] * (std::pow(ratio, 2) - 1.0);

        double COFI3 = (ratio < R2MBA[SP][0]) ? 1.0 : 0.0;
        double TERM3 = R2MBB[SP][2] * std::pow((R2MBA[SP][0] - ratio), 2) * COFI3;

        double COFI4 = (ratio < R2MBA[SP][1]) ? 1.0 : 0.0;
        double TERM4 = R2MBB[SP][3] * std::pow((R2MBA[SP][1] - ratio), 2) * COFI4;

        double sumTerms = TERM1 + TERM2 + TERM3 + TERM4;
        double DIBUP = (sumTerms < 0) ? 0.0 : DBH * std::sqrt(sumTerms);

        double DIBCOR = DIBUP * (R2CZC[SP][0] + R2CZC[SP][1] * DBH + R2CZC[SP][2] * std::pow(HTUP, 2));
        D2 = DIBCOR;

        // ***************** 3-PARAMETER MODEL *****************
    }
    else if (MDL == "CZ3") {
        double EDBHIB, D30IB;

        if (DBTBH > 0) {
            EDBHIB = DBH - DBTBH;
        }
        else {
            EDBHIB = EDBH[SP][1] * DBH + EDBH[SP][0];
        }

        D30IB = D30 * (1.0 - (1.0 - EDBHIB / DBH) * (1.0 / (2.0 - D30 / DBH)));

        double R1 = HTUP / HTTOT;
        double R2_ratio = 4.5 / HTTOT;
        double R3 = H30 / HTTOT;

        double B1 = std::pow(EDBHIB / DBH, 2) * (std::pow(R3, 2) - 1.0) + std::pow(D30IB / DBH, 2) * (1.0 - std::pow(R2_ratio, 2));

        if (R3 < R23PA[SP][0]) B1 += R23PB[SP][0] * (std::pow(R2_ratio, 2) - 1.0) * std::pow(R23PA[SP][0] - R3, 2);
        if (R2_ratio < R23PA[SP][0]) B1 += R23PB[SP][0] * (1.0 - std::pow(R3, 2)) * std::pow(R23PA[SP][0] - R2_ratio, 2);
        if (R3 < R23PA[SP][1]) B1 += R23PB[SP][1] * (std::pow(R2_ratio, 2) - 1.0) * std::pow(R23PA[SP][1] - R3, 2);
        if (R2_ratio < R23PA[SP][1]) B1 += R23PB[SP][1] * (1.0 - std::pow(R3, 2)) * std::pow(R23PA[SP][1] - R2_ratio, 2);

        B1 = B1 / (((1.0 - R2_ratio) * (1.0 - std::pow(R3, 2))) - ((1.0 - std::pow(R2_ratio, 2)) * (1.0 - R3)));
        double B2 = (B1 * (R3 - 1.0) - std::pow(D30IB / DBH, 2));

        if (R3 < R23PA[SP][0]) B2 += R23PB[SP][0] * std::pow(R23PA[SP][0] - R3, 2);
        if (R3 < R23PA[SP][1]) B2 += R23PB[SP][1] * std::pow(R23PA[SP][1] - R3, 2);

        B2 = B2 / (1.0 - std::pow(R3, 2));

        double DIBCOR = B1 * (R1 - 1.0) + B2 * (std::pow(R1, 2) - 1.0);
        if (R1 < R23PA[SP][0]) DIBCOR += R23PB[SP][0] * std::pow(R23PA[SP][0] - R1, 2);
        if (R1 < R23PA[SP][1]) DIBCOR += R23PB[SP][1] * std::pow(R23PA[SP][1] - R1, 2);

        DIBCOR = DIBCOR * std::pow(DBH, 2);

        if (TOP6 <= 0.0) {
            D2 = (DIBCOR <= 0.0) ? 0.0 : std::sqrt(DIBCOR);
        }
        else {
            if (HTUP >= TOP6) {
                double baseD = (D30IB < 6.0) ? D30IB : 6.0;
                D2 = baseD * ((HTTOT - HTUP) / (HTTOT - TOP6));
            }
            else {
                D2 = std::sqrt(DIBCOR);
            }
        }
    }
    return D2;
}

//
double CzaplewskiTaperModel::top6Height(double dbh, double totalHeight, double heightExtra, double diameterExtra, double stump, double dbtbh, int& errflag)
{
    double TOP = 6.0;
    double TOP6 = 0.0;
    errflag = 0;
    double HTTOT = totalHeight;
    double DBHOB = dbh;
    double UPSD1 = diameterExtra;
    double UPSHT1 = heightExtra;
    double STUMP = stump;
    double DBTBH = dbtbh;

    // DEX[0] in Fortran is now UPSD1, HEX[0] is UPSHT1
    if (UPSD1 < 6.0) {
        TOP6 = UPSHT1;
    }
    else {
        // TOP1 is the target diameter in tenths of an inch
        int TOP1 = static_cast<int>(TOP * 10.0);

        int first = 1;
        int last = static_cast<int>(HTTOT + 0.5) * 10;
        int toplop = last;

        for (int i = 1; i <= toplop; ++i) {
            if (first == last) break;

            int half = (first + last + 1) / 2;
            double HT2_test = static_cast<double>(half) / 10.0;

            double DIB = 0.0;
            // Calling the converted R2TAP function directly
            // Note: TOP6_val is passed as 0.0 inside the loop per Fortran context
            DIB = R2taper(DBHOB, HTTOT,UPSHT1,UPSD1,TOP6, DBTBH, HT2_test, errflag);
            //R2taper(double dbh, double totalHeight, double heightExtra, double diameterExtra, double top6Ht, double dbtbh, double ht2, int& errflg)
            if (errflag != 0) return TOP6;

            // Convert DIB to tenth inch truncated with rounding offset
            int iDIB = static_cast<int>((DIB + 0.005) * 10.0);

            if (TOP1 <= iDIB) {
                first = half; // Move up the stem
            }
            else {
                last = half - 1; // Move down the stem
            }
        }

        TOP6 = (static_cast<double>(first) / 10.0) - STUMP;
    }

    if (TOP6 < 0.0) TOP6 = 0.0;

    return TOP6;
}

void CzaplewskiTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    if (modelType_ == VolumeEquation::ModelType::CZ3)
    {
        if (tree.formClass > 0.0)
        {
            heightExtra_ = 17.3;
            diameterExtra_ = tree.formClass / 100.0 * tree.dbh;
        }
        else if (tree.referenceDiameter > 0.0 && tree.referenceHeight > 0.0)
        {
            heightExtra_ = tree.referenceHeight;
            diameterExtra_ = tree.referenceDiameter;
        }
        else if (tree.merchHeightSaw > 0.0)
        {
            heightExtra_ = tree.merchHeightSaw;
            diameterExtra_ = merchRules.minTopDibSaw;
        }
        if (heightExtra_ <= 0.0 || diameterExtra_ <= 0.0)
        {
            throw std::invalid_argument("Extra height and diameter pair measurement is needed for the 3 points model");
        }
        dbtbh_ = merchRules.doubleBarkThicknessAtBrestHeight;
        double stump = merchRules.stumpHeight;
        int errflag = 0;
        top6Ht_ = top6Height(tree.dbh, tree.totalHeight, heightExtra_, diameterExtra_, stump, dbtbh_, errflag);
        if (errflag > 0) top6Ht_ = 0.0;
    }
}

double CzaplewskiTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    int errflag = 0;
    double D2 = 0.0;
    D2 =  R2taper(tree.dbh, tree.totalHeight, heightExtra_, diameterExtra_, top6Ht_, dbtbh_, height, errflag);
    if (errflag > 0) D2 = 0.0;
    return D2;
}

double CzaplewskiTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob)
{
    // This model doesn't have a direct inverse function
    // Need to iterate to find height at diameter
    double dbh = tree.dbh;
    double totalHeight = tree.totalHeight;

    // Check if diameter is larger than DBH
    if (diameter >= dbh) {
        return 4.5;  // Return breast height
    }

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