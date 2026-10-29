#include "BlmTaperAndDbhIb.h"
#include "BehreHyperbolaTaperModel.h"
#include "../DefaultFormClassForFVS.h"
#include "../Volumecalculators/JenkinsBiomass.h"

BehreHyperbolaTaperModel::BehreHyperbolaTaperModel(VolumeEquation volumeEquation)
    : TaperModel(), volEqStr_(volumeEquation.GetVolumeEquationNumber())
{
    volumeEquation_ = volumeEquation;

    //BLM Behr equation
    if (volumeEquation_.geoCode == VolumeEquation::GeoCode::BLM_WO) {
        getBlmTaperEq(volEqStr_, blmProfile, blmTaperEq);
    }
    else if (volumeEquation_.geoCode == VolumeEquation::GeoCode::INGYMODEL) { //this is for BIA Behr equation
        A = 0.49;
        B = 1.0 - A;
    }

}


double BehreHyperbolaTaperModel::BehrTaper(TreeMeasurment tree, double stemUpperHeight) {
    double D2 = 0.0;
    if (tree.merchHeightUnit == TreeMeasurment::MerchHeightUnit::FEET) {
        if (tree.totalHeight > 0.0) {
            const double H1 = tree.totalHeight - formClassHeight_;
            if (H1 <= 0.0) return 0.0;
            const double HX = tree.totalHeight - stemUpperHeight;
            const double HR = HX / H1;
            const double DR = HR / (A * HR + B);
            D2 = d17_ * DR;
        }
    }
    else { //height in number of 16/32 foot logs as 30 for 3 logs
        double heightLogs = 0.0;
        double XLEN = formClassLogLengthWithTrim_;
        double D1 = d17_;
        if (tree.merchHeightSaw > 0.0) {
            heightLogs = tree.merchHeightSaw / 10.0;
            D1 = std::nearbyint(D1);
            const double T = topDibSaw_ / D1;
            const double AT = A / (1.0 - A * T);
            const double BT = (1.0 / (1.0 - T)) - AT;
            const double H1 = (heightLogs - 1.0) * XLEN;
            const double HX = heightLogs * XLEN - (stemUpperHeight - stumpHeight_);
            const double HR = HX / H1;
            const double DR = T + (HR / (AT * HR + BT));
            D2 = D1 * DR;
        }
    }

    return D2;
}
void BehreHyperbolaTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    //setup variable related with tree measurements
    topDibSaw_ = merchRules.minTopDibSaw;
    formClass_ = tree.formClass;
    maxLogLength_ = merchRules.maxLogLength;
    trim_ = merchRules.trim;
    stumpHeight_ = merchRules.stumpHeight;

    if (tree.formClass == 0) {
        if (vco.volumeCalculationOptions == VolumeCalculationOptions::VolumeCalculationType::CRUISE) {
            throw std::invalid_argument("Form Class missing");
        }
        else {
            if (volEqStr_.substr(0, 1) == "I") formClass_ = 70;
            else formClass_ = GetFormClass(volEqStr_, vco.forest, tree.dbh);

            tree.formClass = formClass_;
        }
    }

    d17_ = (tree.dbh * static_cast<double>(formClass_)) / 100.0;

    if (volEqStr_.substr(3, 3) == "B32" || volEqStr_.substr(0, 3) == "632") {
        formClassHeight_ = (16.0 + trim_) * 2.0 + stumpHeight_;
        formClassLogLengthWithTrim_ = (16.0 + trim_) * 2.0;
    }
    else {
        formClassHeight_ = 16.0 + trim_ + stumpHeight_;
        formClassLogLengthWithTrim_ = 16.0 + trim_;
    }

    if (volumeEquation_.geoCode == VolumeEquation::GeoCode::BLM_WO) {
        dbhIb_ = GetBlmDbhIb(blmTaperEq, tree.dbh);
        d17_ = std::nearbyint(d17_);

        //get BLM A and B based on blmProfile
        if (tree.merchHeightUnit == TreeMeasurment::MerchHeightUnit::FEET) {
            const auto& coeff = BLMTHT[blmProfile - 1];
            A = coeff[0]
                + coeff[1] * tree.dbh
                + coeff[2] * tree.totalHeight
                + coeff[3] * tree.dbh * tree.totalHeight;
            B = 1.0 - A;
        }
        else {
            //height in number of 16 foot logs
            double numberOfLogs = tree.merchHeightSaw / 10.0;
            double D17 = d17_;
            double TOP = topDibSaw_;
            double dbh = tree.dbh;

            if (D17 < 5.0) D17 = 5.0;
            if (D17 < TOP) D17 = TOP;
            if (numberOfLogs > 1.0) {
                double TLH = numberOfLogs;
                double XLEN = formClassLogLengthWithTrim_;
                double RA = 0.0;
                double Tolerance = 1.0;
                const double Tol_Limit = 0.01;

                // Initial estimate
                double A_est = 0.62;
                double B_est = 1.0 - A_est;
                const double C = (TLH - 1.0) * XLEN;
                double H = C / (1.0 - TOP * B / (D17 - A * TOP));
                double HtTot_est = H + XLEN + stumpHeight_;

                const int iLimit = 20;
                int iCount = 0;

                const auto& coeff = BLMTHT[blmProfile - 1];
                // Iteration
                while (Tolerance > Tol_Limit && iCount < iLimit) {
                    ++iCount;

                    A_est = coeff[0]
                        + coeff[1] * dbh
                        + coeff[2] * HtTot_est
                        + coeff[3] * dbh * HtTot_est;
                    B_est = 1.0 - A_est;

                    H = C / (1.0 - TOP * B_est / (D17 - A_est * TOP));
                    HtTot_est = H + XLEN + stumpHeight_;

                    Tolerance = std::abs(RA - A_est);
                    RA = A_est;
                }

                A = A_est;
                B = 1.0 - A_est;
            }
        }
    }
    else dbhIb_ = tree.dbh - merchRules.doubleBarkThicknessAtBrestHeight;


}

double BehreHyperbolaTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    double totalLogHeight = 0.0;
    if (tree.merchHeightUnit != TreeMeasurment::MerchHeightUnit::FEET) totalLogHeight = tree.totalHeight;

    // check if height is less than form class height. 
	// Behr taper does not work for the Butt log. We use parabolic for height above DBH and below form class height.
    if (height < formClassHeight_) {
        double D1 = dbhIb_;
        double D2 = d17_;
        double Dh = 0.0;
        if (height >= 4.5) {
			Dh = std::sqrt(std::pow(D1, 2) - (std::pow(D1, 2) - std::pow(D2, 2)) / (formClassHeight_ - 4.5) * (height - 4.5));
        }
        else {
			Dh = D1 * std::pow((formClassHeight_ - height) / (formClassHeight_ - 4.5), 0.25);   
        }
        return Dh;
    }
    else return BehrTaper(tree, height);

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
    StemVolume result = { 0.0,0.0,0.0,0.0};

    //no calculation here for BLM Behre equation
    if (starts_with_b(volEqStr_)) return result;

    //Region 6 BEH equation statts here
    StumpVolume stumpVol = raileVol(vco.fiaCode, tree.dbh, merchRules.stumpHeight);
    result.stumpVol = stumpVol.woodVol;

    if (tree.totalHeight < formClassHeight_ || dbhIb_ < topDibSaw_) {
        result.primaryVol = 0.00272708 * (dbhIb_ * dbhIb_) * tree.totalHeight;
    }
    else if (d17_ < topDibSaw_) {
        //small tree use d17 for butt log
        double logVol = 0.00272708 * (dbhIb_ * dbhIb_ + d17_ * d17_) * formClassHeight_;
        result.primaryVol = logVol;
        //find tip volume
        logVol = 0.00272708 * (d17_ * d17_) * (tree.totalHeight - formClassHeight_);
        result.primaryVol += logVol;
    }
    else {
        // find cubicfoot volume for butt log
        double H17 = formClassHeight_;
        double logVol = 0.00272708 * (dbhIb_ * dbhIb_ + d17_ * d17_) * H17;
        result.primaryVol = logVol;

        double HTUP = tree.totalHeight - H17;
        double A = 0.62;
        double B = 1.0 - A;
        double topD = topDibSaw_;
        double S = 0.0;
        std::array<double, 21> D;

        D[1] = d17_;

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

            double DR = topD / d17_;
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

    return result;
}
