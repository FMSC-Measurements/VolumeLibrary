#include "RustagiTaperModel.h"

void RustagiTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) {
	double DBHOB = tree.dbh;
	double HTTOT = tree.totalHeight;
	
    int ERRFLAG = 0;
    double STUMPD = 0.0;
    double BUTTCF = 0.0;
    double CF0 = 0.0;
    //double B = 0.0;

    // Basic input checks (same as Fortran)
    if (DBHOB < 1.0) {
        ERRFLAG = 3;
        throw std::invalid_argument("DBH < 1.0");
    }
    // Proposed modification to prevent div by zero (same as your code)
    if (HTTOT <= 5.0) {
        ERRFLAG = 4;
        throw std::invalid_argument("HTTOT < 5.0");
    }

    if (voleqCoefIdx_ == 0) {
        ERRFLAG = 1;
        throw std::invalid_argument("Invalid VOLEQ");
    }

    int II = voleqCoefIdx_;

    // Mathis anchor at 1 ft
    double THT = HTTOT - 1.0;

    // Compute intermediates
    double HT67 = CF(II, 1) * std::pow(DBHOB, CF(II, 2)) * std::pow(THT, CF(II, 3));
    BUTTCF = CF(II, 5) * DBHOB + CF(II, 4);
    STUMPD = std::sqrt((BUTTCF * BUTTCF * THT) / (THT - 4.0));
    double D67 = CF(II, 7) * DBHOB * (2.0 / 3.0) + CF(II, 6);
    CF0 = 0.002727 * (HT67 * STUMPD * STUMPD + D67 * D67 * THT);
    //if (calcCF0) return CF0;

    double F = CF0 / (0.005454 * STUMPD * STUMPD * THT);
    B_ = (1.0 - F) / (2.0 * F);
    stumpDib_ = STUMPD;
}

double RustagiTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    double HTUP = height;
    double HTTOT = tree.totalHeight;
    double THT = HTTOT - 1.0;
	double dib = 0.0;
	//return R4MAT_Taper(volEqStr, tree.dbh, tree.totalHeight, height, dib);
    if (HTUP > 0.0 && HTUP < HTTOT) {
        if (HTUP <= 1.0) {
            dib = stumpDib_;
        }
        else {
            double PHT = HTUP - 1.0; // height above 1 ft
            dib = stumpDib_ * std::pow((THT - PHT) / THT, B_);
        }
    }
    return dib;
}

double RustagiTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob)
{
    double DIB = diameter;
    double HTTOT = tree.totalHeight;
    double THT = HTTOT - 1.0;
    double STUMPD = stumpDib_;
	double htup = 0.0;
	//return R4MAT_Taper(volEqStr, tree.dbh, tree.totalHeight, htup, diameter);
    if (DIB > 0.0 && DIB < STUMPD) {
        double PHT = THT - THT * std::pow(DIB / STUMPD, 1.0 / B_);
        htup = PHT + 1.0;
    }
    return htup;
}