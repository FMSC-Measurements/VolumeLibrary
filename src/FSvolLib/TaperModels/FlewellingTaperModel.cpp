#include "FlewellingTaperModel.h"
#include "FlewellingTaperModel_Alaska.h"
#include "FlewellingTaperModel_Ingy.h"
#include "FlewellingTaperModel_Other.h"
#include "FlewellingTaperModel_West.h"
#include <array>
#include <string_view>
#include <cmath>

FlewellingTaperModel::FlewellingTaperModel(VolumeEquation volumeEquation)
    : TaperModel(), volumeEquation_(volumeEquation)
{
    //Set Flewelling JSP, geoCode, and geoSubregion 
    // JSP VALUES
    //    WESTSIDE : 3 - 5
    //    INGY : 11 - 21
    //      Douglas fir            11
    //      Western Larch          12
    //      Grand fir              13
    //      Ponderosa pine         14
    //      Lodgepole pine         15
    //      Western Red Cedar      16
    //      Mountain Hemlock       17
    //      White pine             18
    //      Engelmann Spruce       19
    //      Subalpine fir          20
    //      Balsam fir             21
    //    BLACK HILLS PP           22
    //    SAN JUAN & R2 PP         23
    //    R4 DIXIE ES              24
    //    R2 LODGEPOLE             25
    //    R2 DOUG FIR              26
    //    R2 WHITE FIR             27
    //    R2 Aspen                 28
    //    R3 PP                    29
    //    R10 ALASKA CEDAR         31
    //    R10 WESTERN RED CEDAR    32
    //    R10 Spruce               33
    //    R10 Hemlock              34
    //    R10 Spruce (second growth)  35
    //    R10 Hemlock (second growth) 36

    int fiaCode = volumeEquation_.fiaCode;
    geoCode_ = volumeEquation_.geoCode;
    geoSubregion_ = std::string(volumeEquation_.subregionalCode, std::size(volumeEquation_.subregionalCode));
    if (volumeEquation_.volEqStr.substr(5, 1) == "3") threePoint_ = true;

    // ---------------- REGION I ----------------
    if (geoCode_ == VolumeEquation::GeoCode::INGYMODEL)      
    {
        if (fiaCode == 202 || fiaCode == 205 || fiaCode == 204)
            JSP_ = 11; // Douglas fir
        else if (fiaCode == 73 || fiaCode == 70)
            JSP_ = 12; // Western Larch
        else if (fiaCode == 17)
            JSP_ = 13; // Grand fir
        else if (fiaCode == 122)
            JSP_ = 14; // Ponderosa pine
        else if (fiaCode == 108)
            JSP_ = 15; // Lodgepole pine
        else if (fiaCode == 242 || fiaCode == 240)
            JSP_ = 16; // Western Red Cedar
        else if (fiaCode == 260 || fiaCode == 263 || fiaCode == 264)
            JSP_ = 17; // Mountain Hemlock
        else if (fiaCode == 119)
            JSP_ = 18; // White pine
        else if (fiaCode == 93 || fiaCode == 90)
            JSP_ = 19; // Engelmann Spruce
        else if (fiaCode == 19)
            JSP_ = 20; // Subalpine fir
        else if (fiaCode == 12)
            JSP_ = 21; // Balsam fir
    }

    // ---------------- WESTSIDE (F) ----------------
    else if (geoCode_ == VolumeEquation::GeoCode::FLWELLINGMODEL_WEST)
    {
        if (fiaCode == 202 || fiaCode == 205 || fiaCode == 204)
            JSP_ = 3;   // Douglas fir
        else if (fiaCode == 263)
            JSP_ = 4;   // Western Hemlock
        else if (fiaCode == 242)
            JSP_ = 5;   // Western Red Cedar
    }

    // ---------------- REGION 2 ----------------
    else if (geoCode_ == VolumeEquation::GeoCode::R2)
    {
        if (fiaCode == 122)
        {
            if (geoSubregion_ == "03")
                JSP_ = 22;   // Black Hills model
            else
                JSP_ = 23;   // region-wide
        }
        else if (fiaCode == 108)
            JSP_ = 25;       // Lodgepole
        else if (fiaCode == 202)
            JSP_ = 26;       // Douglas fir
        else if (fiaCode == 15)
            JSP_ = 27;       // White fir
        else if (fiaCode == 746)
            JSP_ = 28;       // Aspen
    }

    // ---------------- REGION 4 ----------------
    else if (geoCode_ == VolumeEquation::GeoCode::R4)
    {
        if (geoSubregion_ == "07")
        {
            if (fiaCode == 93)
                JSP_ = 24;       // Engelmann spruce
            else if (fiaCode == 122)
                JSP_ = 23;       // R2 Ponderosa with R4 bark
        }
    }

    // ---------------- REGION 3 ----------------
    else if (geoCode_ == VolumeEquation::GeoCode::R3)
    {
        if (geoSubregion_ == "00")
        {
            if (fiaCode == 122)
                JSP_ = 29;
            else if (fiaCode == 202)
            {
                JSP_ = 26;
                dibDobPercent_ = 88.85;
            }
        }

        else if (geoSubregion_ == "01")
        {
            if (fiaCode == 122)
            {
                JSP_ = 29;
                dibDobPercent_ = 89.12;
            }
            else if (fiaCode == 108)
            {
                JSP_ = 25;
                dibDobPercent_ = 93.26;
            }
            else if (fiaCode == 202)
            {
                JSP_ = 26;
                dibDobPercent_ = 89.72;
            }
            else if (fiaCode == 15)
            {
                JSP_ = 27;
                dibDobPercent_ = 91.16;
            }
        }
    }

    // ---------------- REGION 10 (A) ----------------
    else if (geoCode_ == VolumeEquation::GeoCode::R10)
    {
        if (fiaCode == 42)
            JSP_ = 31;        // Alaska yellow cedar
        else if (fiaCode == 242)
            JSP_ = 32;        // Western Red Cedar
        else if (fiaCode == 98)
        {
            if (geoSubregion_ == "02")
                JSP_ = 35;    // spruce (variant)
            else
                JSP_ = 33;
        }
        else if (fiaCode == 263 || fiaCode == 260 || fiaCode == 264)
        {
            if (geoSubregion_ == "02")
                JSP_ = 36;    // hemlock (variant)
            else
                JSP_ = 34;
        }
    }

    if (JSP_ == 0) {
        throw std::invalid_argument("Invalid species code in volume equation: " + volumeEquation_.volEqStr);
    }

}


//get dbhIb
double FlewellingTaperModel::getDbhIb(int JSP, std::string_view geoSub, double dbh, double totalHt)
{
    double dbtBH = 0.0;
    if (doubleBarkThicknessBH_ > 0.0) dbtBH = doubleBarkThicknessBH_;
    if (dbtBH == 0.0 && dibDobPercent_ > 0.0) dbtBH = dbh * (1.0 - dibDobPercent_ / 100.0);

    if(dbtBH ==0.0) {
        //get doublebarkThicknessAtBreastHeight from bark model
        if (JSP >= 3 && JSP <= 5) {
            //West coast bark model
            dbtBH = FDBT_C1(JSP, geoSub, dbh, totalHt);
        }
        else if (JSP >= 11 && JSP <= 21) {
            //Ingy
            dbtBH = FDBT_C2(JSP, geoSub, dbh, totalHt);
        }
        else if (JSP >= 31 && JSP <= 36) {
            //Alaska
            dbtBH = FDBT_AK(JSP, dbh, totalHt);
        }
    }

    //f_other use DBHOB for taper
    if (JSP >= 22 && JSP <= 30) {
        //other use dbhOb
        dbtBH = 0.0;
    }

    double dbhIb = dbh - dbtBH;
    return dbhIb;
}

//get shape parameters
FlewellingShapeParams FlewellingTaperModel::getShapeParameter(int JSP, std::string_view geoSub, double dbh, double totalHt)
{
    //sf_shp.f
    FlewellingShapeParams shapeParameter;
    if (JSP == 3) {
        //shp_w3
        shapeParameter = SHP_W3(dbh, totalHt, geoSub);
    }
    else if (JSP == 4) {
        //shp_w4
        shapeParameter = SHP_W4(dbh, totalHt, geoSub);
    }
    else if (JSP == 5) {
        //shp_w5
        shapeParameter = SHP_W5(dbh, totalHt, geoSub);
    }
    else if (JSP >= 11 && JSP <= 21) {
        //shp_c2
        shapeParameter = shapeIngy(JSP, geoSub, dbh, totalHt);
    }
    else if (JSP == 22) {
        //shp_bh
        shapeParameter = SHP_BH(dbh, totalHt);
    }
    else if (JSP >= 23 && JSP <= 30) {
        //shp_ot
        shapeParameter = SHP_OT(JSP, dbh, totalHt);
    }
    else if (JSP >= 31 && JSP <= 36) {
        shapeParameter = shapeAlaska(JSP, geoSub, dbh, totalHt);
    }

    return shapeParameter;
}

//get taper coef
FlewellingTaperCoef FlewellingTaperModel::getTaperCoef(FlewellingShapeParams shapeParemeter)
{
    //sf_taper.f
    FlewellingTaperCoef taperCoef;

    // Extract inputs into double precision (as in original Fortran)
    const double R1 = shapeParemeter.r1;
    const double R2 = shapeParemeter.r2;
    const double R3 = shapeParemeter.r3;
    const double R4 = shapeParemeter.r4;
    const double R5 = shapeParemeter.r5;
    const double A3 = shapeParemeter.a3;

    const double RHI1 = shapeParemeter.rhi1;
    const double RHI2 = shapeParemeter.rhi2;
    const double RHC = shapeParemeter.rhc;
    const double RHLONGI = shapeParemeter.rhlongi;

    constexpr double K = 1.0;

    // Upper segment
    const double YC = K * (1.0 - RHC);
    const double C2 = R5 * YC;
    const double C1 = 3.0 * (YC - C2);
    const double SLOPE = -(3.0 - R5) * K / 2.0;

    // Middle segment
    const double S1 = SLOPE * (RHC - RHI2);
    const double YI_MIN = YC - S1 * (1.0 + 2.0 * R3) / 3.0;
    const double YI_MAX = YC - S1 * (5.0 + 4.0 * R3) / 9.0;
    const double YI2 = YI_MIN + R4 * (YI_MAX - YI_MIN);
    const double S0 = R3 * S1;

    const double B1 = (6.0 * YC - 6.0 * YI2 - 2.0 * S0 - 4.0 * S1)
        / (-3.0 * YC + 3.0 * YI2 + 2.0 * S0 + S1);

    const double B2 = S1 * (1.0 - R3) / (0.5 - 1.0 / (B1 + 1.0));
    const double B4 = S0;
    const double B0 = YI2;

    const double slope_RHI = R3 * S1 / (RHC - RHI2);

    // Straight segment
    const double YI1 = YI2 - slope_RHI * RHLONGI;

    double E1, E2;
    if (RHLONGI > 0.0) {
        E2 = (YI2 - YI1) / RHLONGI;
        E1 = YI1 - E2 * RHI1;
    }
    else {
        E1 = YI2;
        E2 = 0.0;
    }

    // Lower segment
    const double S3 = -slope_RHI * RHI1;
    const double K2 = S3 / R1;

    const double F_A3 =
        1.0 / (6.0 * A3 * A3)
        + std::log(1.0 - 1.0 / A3)
        + 1.0 / (3.0 * (A3 - 1.0))
        + 2.0 / (3.0 * A3);

    const double G_A3 =
        (1.0 / (A3 - 1.0)
            - 1.0 / A3
            - 1.0 / (A3 * A3)
            - 1.0 / std::pow(A3 - 1.0, 3))
        / F_A3;

    const double denom =
        (1.0 / (A3 - 1.0)
            - 1.0 / A3
            - 1.0 / (A3 * A3)
            - 1.0 / (A3 * A3 * A3));

    const double YB_MIN =
        YI1 + (2.0 * S3 + K2) / 3.0
        + (S3 - K2) * F_A3 / denom;

    const double YB_MAX =
        YI1 + (2.0 * S3 + K2) / 3.0
        + (S3 - K2) / G_A3;

    const double YB = YB_MIN + R2 * (YB_MAX - YB_MIN);

    // Polynomial coefficients
    const double A0 = YI1;
    const double A2 = (YB - YI1 - (2.0 * S3 + K2) / 3.0) / F_A3;
    const double A1 = (K2 - S3
        + A2 * (1.0 / (A3 - 1.0)
            - 1.0 / A3
            - 1.0 / (A3 * A3))) / 3.0;
    const double A4 = S3;

    taperCoef.a0 = A0;
    taperCoef.a1 = A1;
    taperCoef.a2 = A2;
    taperCoef.a4 = A4;
    taperCoef.b0 = B0;
    taperCoef.b1 = B1;
    taperCoef.b2 = B2;
    taperCoef.b4 = B4;
    taperCoef.c1 = C1;
    taperCoef.c2 = C2;
    taperCoef.e1 = E1;
    taperCoef.e2 = E2;

    return taperCoef;
}

//predicate dib at given relative height using two point model (sf_yhat.f)
double FlewellingTaperModel::getDibFrom2PointModel(int jsp, double relativeHeight, double totalHt,
    FlewellingShapeParams shapeParemeter, FlewellingTaperCoef taperCoef, 
    double& slope, double scalingFactor, bool needSlope)
{
    double dib = 0.0;

    // Extract inputs into double precision
    const double R1 = shapeParemeter.r1;
    const double R2 = shapeParemeter.r2;
    const double R3 = shapeParemeter.r3;
    const double R4 = shapeParemeter.r4;
    const double R5 = shapeParemeter.r5;
    const double A3 = shapeParemeter.a3;

    const double RHI1 = shapeParemeter.rhi1;
    const double RHI2 = shapeParemeter.rhi2;
    const double RHC = shapeParemeter.rhc;
    const double RHLONGI = shapeParemeter.rhlongi;

    const double A0 = taperCoef.a0;
    const double A1 = taperCoef.a1;
    const double A2 = taperCoef.a2;
    const double A4 = taperCoef.a4;
    const double B0 = taperCoef.b0;
    const double B1 = taperCoef.b1;
    const double B2 = taperCoef.b2;
    const double B4 = taperCoef.b4;
    const double C1 = taperCoef.c1;
    const double C2 = taperCoef.c2;
    const double E1 = taperCoef.e1;
    const double E2 = taperCoef.e2;

    double RH = relativeHeight;
    double F = scalingFactor;

    double y = 0.0;
    double x = 0.0;
    slope = 0.0;
    double RH_LENGTH = 0.0;
    double dy_dx = 0.0;
    int I_SEG = 0;

    // Invalid input
    if (RH > 1.0 || RH < 0.0) {
        return 0.0; 
    }

    // --- Upper segment ---
    if (RH >= RHC) {
        x = (1.0 - RH) / (1.0 - RHC);

        if (jsp == 22) {
            y = C2 * x + (C1 / 2.0) * x * x - (C1 / 6.0) * x * x * x;
        }
        else {
            y = x * (C2 + x * ((C1 / 2.0) - (C1 / 6.0) * x));
        }
        
        RH_LENGTH = 1.0 - RHC;
        I_SEG = 1;

        if (needSlope) {
            dy_dx = C2 + x * (C1 - (C1 / 2.0) * x);
        }
            
    }

    // --- Middle segment ---
    else if (RH >= RHI2) {
        x = (RH - RHI2) / (RHC - RHI2);

        if (x > 0.0) {
            if (jsp == 22) {
                double susExpVal = (B1 * std::log10(x) <= -20.0)
                    ? 0.0
                    : std::pow(x, B1 + 2.0);

                y = B0 + B4 * x
                    - B2 / ((B1 + 1.0) * (B1 + 2.0)) * susExpVal
                    + B2 / 6.0 * x * x * x;
            }
            else {
                double susExpVal = (B1 * std::log10(x) <= -20.0)
                    ? 0.0
                    : std::pow(x, B1);

                y = B0 + x * (B4 + x *
                    (-B2 / ((B1 + 1.0) * (B1 + 2.0)) * susExpVal
                        + B2 / 6.0 * x));
            }
        }
        else {
            y = B0;
        }

        if (needSlope) {
            RH_LENGTH = RHC - RHI2;
            I_SEG = 2;

            if (x > 0.0) {
                double SUS3;
                if (B1 * log10(x) <= -20.0)
                    SUS3 = 0.0;
                else
                    SUS3 = pow(x, B1 + 1.0);

                dy_dx = B4 - B2 / (B1 + 1.0) * SUS3 + B2 / 2.0 * x * x;
            }
            else {
                dy_dx = B4;
            }
        }
    }

    // --- Straight segment ---
    else if (RHLONGI > 0.0 && RH > RHI1) {
        y = E1 + E2 * RH;

        if (needSlope) {
            I_SEG = 3;
            dy_dx = E2;
            RH_LENGTH = 1.0;
        }
    }

    // --- Lower segment ---
    else {
        x = (RHI1 - RH) / RHI1;

        if (jsp == 22) {
            y = A0 + (A4 + A2 / A3) * x
                + A2 / (2.0 * A3 * A3) * x * x
                + A1 * x * x * x
                + A2 * std::log(1.0 - x / A3);
        }
        else {
            y = A0
                + x * ((A4 + A2 / A3)
                    + x * (A2 / (2.0 * A3 * A3) + A1 * x))
                + A2 * std::log(1.0 - x / A3);
        }

        if (needSlope) {
            RH_LENGTH = RHI1;
            I_SEG = 4;
            dy_dx = A4 + A2 / A3 + A2 / (A3 * A3) * x + 3 * A1 * x * x - A2 / (A3 - x);
        }
    }

    // Final yhat
    double yhat = F * y;
    dib = yhat;

    if (needSlope) {
        double dd_dH = dy_dx * F / (RH_LENGTH * totalHt);
        if (I_SEG != 2 && I_SEG != 3)
            dd_dH = -dd_dH;
        slope = dd_dH;
    }

    return dib;
}

//get scaling factor F
double FlewellingTaperModel::getScalingFactor(int JSP, double dbhIb, double totalHeight, FlewellingShapeParams shapeParameter, FlewellingTaperCoef taperCoef)
{
    double RH = 4.5 / totalHeight;
    double slope;
    double dbhIbPred = getDibFrom2PointModel(JSP, RH, totalHeight, shapeParameter, taperCoef, slope);
    
    return dbhIb / dbhIbPred;
}

//get 3-point extra parameter
//sf_3pt.f

Flewelling3PointExtra FlewellingTaperModel::getThreePointExtra(int jsp, std::string_view geoSub, double dbh, double totalHt, double dbtBH,
    double heightExtra, double diameterExtra, double scalingFactor,
    FlewellingShapeParams shapeParameter, FlewellingTaperCoef taperCoef)
{
    Flewelling3PointExtra out;

    double F = scalingFactor;
    double HEX = heightExtra;
    double DEX = diameterExtra;
    double TOTALH = totalHt;
    double slope;

    const double BH = 4.5;
    double FMODMAX = 0.15;
    double HFIRSTUP = 0.0;
    double FMODMAXU = 0.0;

    // local arrays
    //double PINV_Z = 0.0;
    double Z = 0.0;
    double hextra;
    double dhatex;
    double bark_r;
    double dobex;

    double h = hextra = HEX;
    double x = h / TOTALH;

    //double slope_dummy = 0.0;
    dhatex = getDibFrom2PointModel(jsp, x, TOTALH, shapeParameter, taperCoef, slope, F);

    // actual diameter processing
    double dibmod = dhatex;
    double dibact = 0.0;

    if (jsp >= 11 && jsp <= 21) {
        //double dob = DEX;
        //dobex = dob;
        //bark_r = BRK_UPB2(jsp, dbh, TOTALH, dbtBH, h, dob);

        //double dbt = bark_r * dob;
        //dibact = dob - dbt;
        dibact = DEX;
    }
    else if (jsp >= 3 && jsp <= 5) {
        double dob = DEX;
        dobex = dob;
        bark_r = BRK_WS(jsp, dbh, TOTALH, dbtBH, h);

        double dbt = bark_r * dob;
        dibact = dob - dbt;
    }
    else {
        // outside bark species
        dibact = DEX;
    }

    // compute standardized error Z(j)
    double se_lnx = 0.0;

    if (jsp >= 11 && jsp <= 21) {
        se_lnx = VAR_C2(jsp, geoSub, dbh, TOTALH, h);
        Z = std::log(dibact / dibmod) / se_lnx;
    }
    else if (jsp >= 3 && jsp <= 5) {
        Z = VAR_C1(jsp, dbh, TOTALH, h, dibmod, dibact);
    }
    else if (jsp >= 23 && jsp <= 30) {
        se_lnx = VAR_OT(jsp, dbh, TOTALH, h);
        Z = std::log(dibact / dibmod) / se_lnx;
    }
    else if (jsp >= 31 && jsp <= 36) {
        se_lnx = VAR_AK(jsp, dbh, totalHt, heightExtra);
        Z = std::log(dibact / dibmod) / se_lnx;
    }
    else if (jsp == 22) {
        se_lnx = VAR_BH(dbh, TOTALH, h);
        Z = (dibact - dibmod) / se_lnx;
    }

    out.zValueExtra = Z;

    double abs_change = std::abs(dibact - dibmod);
    FMODMAX = std::max(FMODMAX, abs_change / dibmod);

    if (h > BH) {
        if (HFIRSTUP == 0.0 || h < HFIRSTUP) {
            FMODMAXU = std::min(dibmod, 2.0 * abs_change) / dibmod;
            HFIRSTUP = h;
        }
    }

    out.fModMax = FMODMAX;
    out.fModMaxU = FMODMAXU;
    out.hFirstUp = HFIRSTUP;

    return out;
}

//get dib from 3-point model
double FlewellingTaperModel::getDibFrom3PointModel(int jsp, std::string_view geoSub, double dbh, double totalHt, double H, double DIB, double HEX, double DEX, Flewelling3PointExtra threePointExtra)
{
    const double BH = 4.5;
    double THT = totalHt;

    double FMODMAX = threePointExtra.fModMax;
    double FMODMAXU = threePointExtra.fModMaxU;
    double HFIRSTUP = threePointExtra.hFirstUp;
    double ZEX = threePointExtra.zValueExtra;

    // Quick exits
    if (DIB <= 0.0 || H >= THT) {
        return 0.0;
    }

    // ------------------------------------------------------------
    // Because NEXTRA = 1 always:
    // EZ = p12 * ZEX(1)
    // ------------------------------------------------------------
    double p12 = computeDibErrorCorrelationAtHeights(jsp, geoSub, THT, HEX, H);
    double EZ = p12 * ZEX;

    // ------------------------------------------------------------
    // Compute adjusted diameter using SF_DFZ
    // DIBact = f(DIB, EZ)
    // ------------------------------------------------------------
    double DIBact = 0.0;
    DIBact = adjustDibAtHeight(jsp, geoSub, dbh, THT, H, DIB, EZ);

    double CHANGE = DIBact - DIB;

    // overall limit
    if (std::abs(CHANGE) / DIB > FMODMAX) {
        CHANGE = (CHANGE >= 0.0 ? 1.0 : -1.0) * FMODMAX * DIB;
    }

    // upper-stem limit
    if (H > BH && H < HFIRSTUP) {
        double FULIMIT = (H - BH) / (HFIRSTUP - BH) * FMODMAXU;
        if (std::abs(CHANGE) / DIB > FULIMIT) {
            CHANGE = (CHANGE >= 0.0 ? 1.0 : -1.0) * FULIMIT * DIB;
        }
    }

    // ------------------------------------------------------------
    // Return expected diameter inside bark
    // ------------------------------------------------------------
    double EDIB = DIB + CHANGE;
    return EDIB;
}

//Predicted dib at h1 minus measured diameter there
// ------------------------------------------------------------
// Helper: compute diameter error at H1 for HT trial
// ------------------------------------------------------------
double FlewellingTaperModel::computeDiameterError(int jsp, std::string_view geoSub,
    double DBH,
    double HT_try,
    double DBTBH,
    double H1,
    double D1)
{
    double dbhIb;
    if (DBTBH == 0.0) {
        dbhIb = getDbhIb(jsp, geoSub, DBH, HT_try);
        DBTBH = DBH - dbhIb;
    }

    FlewellingShapeParams shapeParameter = getShapeParameter(jsp, geoSub, DBH, HT_try);

    FlewellingTaperCoef taperCoef = getTaperCoef(shapeParameter);

    dbhIb = DBH - DBTBH;
    double F = getScalingFactor(jsp, dbhIb, HT_try, shapeParameter, taperCoef);

    double relativeHeight = H1 / HT_try;
    double slope;
    double D2 = getDibFrom2PointModel(jsp, relativeHeight, HT_try, shapeParameter, taperCoef, slope, F);

    if (jsp >= 22 && jsp <= 30) {
        double DOB, DBT;
        DOB = D2;
        //the D2 calculated is DOB, need to get DIB
        D2 = BRK_OT(jsp, geoSub, DBH, DOB, H1, DBTBH, DBT);
    }

    return D2 - D1;  // inside-bark difference
}

// solve total height from merch height and top diameter
double FlewellingTaperModel::solveTotalHeight(int JSP, std::string_view geoSub, double DBH, double dbtBH, double H1, double D1)
{
    //Fortran sf_2pth1
    const double BH = 4.5;

    if (H1 <= 1.33 * BH || D1 >= DBH)
        return 0.0;

    double dH = H1 * D1 / (DBH - D1);
    dH = std::clamp(dH, 0.05 * H1, 0.25 * H1);

    double HT_low = H1;
    double eval_low = -D1;
    double HT_try = H1 + dH;

    bool bracketed = false;
    double HT_high = 0.0, eval_high = 0.0;

    for (int i = 0; i < 25; i++) {

        double eval = computeDiameterError(JSP, geoSub, DBH, HT_try, dbtBH, H1, D1);

        if (eval > 0.0 || bracketed) {

            if (std::abs(eval) < 0.01)
                return HT_try;

            bracketed = true;

            if (eval < 0.0) {
                HT_low = HT_try;
                eval_low = eval;
            }
            else {
                HT_high = HT_try;
                eval_high = eval;
            }

            double A = std::abs(eval_low);
            HT_try = (A / (A + eval_high)) * (HT_high - HT_low) + HT_low;

        }
        else {
            HT_low = HT_try;
            eval_low = eval;
            HT_try += dH;
        }
    }

    return 0.0;  // failed to converge in 25 iterations
}

//SF_CORR
// given 2 heights(hi and hj), estimate the correlation of the
// errors in the corresponding dib's
//computeDibErrorCorrelationAtHeights

double FlewellingTaperModel::computeDibErrorCorrelationAtHeights(int JSP, const std::string_view geosub, double TOTALH, double HI, double HJ)
{
    // 11–21 use COR_C2
    if (JSP >= 11 && JSP <= 21)
        return COR_C2(JSP, geosub, TOTALH, HI, HJ);

    // 22 = Black Hills correction
    if (JSP == 22)
        return COR_BH(TOTALH, HI, HJ);

    // 23–30 = Other regions (OT model)
    if (JSP >= 23 && JSP <= 30)
        return COR_OT(JSP, TOTALH, HI, HJ);

    // 31–36 = Alaska taper model
    if (JSP >= 31 && JSP <= 36)
        return COR_AK(JSP, TOTALH, HI, HJ);

    // 1 or 2 → historic placeholder routines (unknown)
    if (JSP == 1 || JSP == 2)
        return 0.0;  // original FORTRAN had no implementation

    // 3–5 = Western species (Douglas‑fir, Hemlock, Cedar)
    if (JSP >= 3 && JSP <= 5)
        return COR_WS(JSP, TOTALH, HI, HJ);

    // default (invalid species)
    return 0.0;
}


// -------------------------------------------------------------------
// Modern C++ version of:
// SUBROUTINE SF_DFZ(JSP,GEOSUB,DBH,THT, H, DIBmod, Z, DIBact)
// adjustDibAtHeight
// -------------------------------------------------------------------
double FlewellingTaperModel::adjustDibAtHeight(int JSP, const std::string_view geoSub, double DBH, double THT, double H, double DIBmod, double Z)
{
    // Output variable
    double DIBact = DIBmod;

    // ---------------------------------------------------------------
    // GROUP 1: LOGNORMAL species
    // ---------------------------------------------------------------
    if (JSP >= 11)
    {
        double SE_LNX = 0.0;

        if (JSP >= 11 && JSP <= 21)
        {
            SE_LNX = VAR_C2(JSP, geoSub, DBH, THT, H);
        }
        else if (JSP >= 23 && JSP <= 30)
        {
            SE_LNX = VAR_OT(JSP, DBH, THT, H);
        }
        else if (JSP == 22)
        {
            SE_LNX = VAR_BH(DBH, THT, H);
        }
        else if (JSP >= 31 && JSP <= 36)
        {
            SE_LNX = VAR_AK(JSP, DBH, THT, H);
        }

        // Clamp Z*SE to ±5
        double prod = Z * SE_LNX;
        if (std::fabs(prod) > 5.0)
            prod = (prod < 0.0 ? -5.0 : 5.0);

        // Compute actual dib
        DIBact = DIBmod * std::exp(prod);
        return DIBact;
    }

    // ---------------------------------------------------------------
    // GROUP 2: NORMAL species (JSP = 1, 2)
    // (Original FORTRAN had no VAR_STD implemented)
    // ---------------------------------------------------------------
    if (JSP == 1 || JSP == 2)
    {
        // No SE available; FORTRAN left these unimplemented
        // Equivalent behavior: return base prediction
        return DIBmod;
    }

    // ---------------------------------------------------------------
    // GROUP 3: Johnson Sb species (JSP = 3,4,5)
    // ---------------------------------------------------------------
    if (JSP == 3)
        return dib_from_Z_sf3(DBH, THT, H, DIBmod, Z);

    if (JSP == 4)
        return dib_from_Z_sf4(DBH, THT, H, DIBmod, Z);

    if (JSP == 5)
        return dib_from_Z_sf5(DBH, THT, H, DIBmod, Z);

    // ---------------------------------------------------------------
    // Default: unprogrammed species
    // ---------------------------------------------------------------
    return DIBmod;
}

//sf_ds.f
//Estimate Dib at any height  (any species, any method)
//Optionally estimate slope   d(DIB) / d(H)
//estimateDibAtHeight
//double FlewellingTaperModel::SF_DS(
//    int JSP,
//    const std::string_view GEOSUB,
//    double DBH,
//    double TOTALH,
//    double HTUP,
//    double Hex,
//    double Dex,
//    FlewellingShapeParams shapeParameter,
//    FlewellingTaperCoef taperCoef,
//    Flewelling3PointExtra threePointExtra,
//    double scalingFactor,
//    double& slope,
//    bool needSlope)
double FlewellingTaperModel::estimateDibAtHeight(double upperHeight, double& slope, bool needSlope)
{
    double DIB;
    //------------------------------------------------------
    // Invalid input: height above total height
    //------------------------------------------------------
    if (upperHeight > totalHeight_) {
        DIB = 0.0;
        if (needSlope)
            slope = -1.0;
        return DIB;
    }

    //------------------------------------------------------
    // Compute base taper function
    //------------------------------------------------------
    double RH = upperHeight / totalHeight_;

    // This call computes:
    // D1 = predicted dib
    // slope = dD/dH (if ineedsl=1)
    double D1 = getDibFrom2PointModel(JSP_, RH, totalHeight_, shapeParameter_, taperCoef_, slope, scalingFactor_, needSlope);
    double SL1 = slope;  // base derivative

    DIB = D1;
    //------------------------------------------------------
    // If no extra taper modifiers → return immediately
    //------------------------------------------------------
    if (threePoint_ && heightExtra_ > 0.0 && diameterExtra_ > 0.0) {

        //------------------------------------------------------
        // WITH EXTRA MODIFIERS
        // Call sf_yhat3 to get modified dib and offset
        //------------------------------------------------------
        double D1rev = getDibFrom3PointModel(JSP_, geoSubregion_, dbh_, totalHeight_, upperHeight, D1, heightExtra_, diameterExtra_, threePointExtra_);

        // Modified diameter
        DIB = D1rev;

        double off1 = D1rev - D1;

        //------------------------------------------------------
        // If slope needed: compute derivative of the offset as well
        //------------------------------------------------------
        if (needSlope) {

            // pick a nearby height HH2 for numerical derivative
            double HH1 = upperHeight;
            double HH2;

            if (HH1 < 0.99 * totalHeight_)
                HH2 = HH1 + totalHeight_ / 800.0;
            else
                HH2 = HH1 - totalHeight_ / 800.0;

            // Compute base taper at HH2
            double slope2 = 0.0;
            double RH2 = HH2 / totalHeight_;
            double D2 = getDibFrom2PointModel(JSP_, RH2, totalHeight_, shapeParameter_, taperCoef_, slope2, scalingFactor_, needSlope);

            // Compute modifier taper at HH2
            double D2rev = getDibFrom3PointModel(JSP_, geoSubregion_, dbh_, totalHeight_, HH2, D2, heightExtra_, diameterExtra_, threePointExtra_);

            double off2 = D2rev - D2;

            // Numerical derivative of offset
            double SL2 = (off2 - off1) / (HH2 - HH1);

            // Combined slope = base taper slope + modifier slope
            slope = SL1 + SL2;
        }
    }

    //For JSP 22 - 30, the profile use DOB, need to convert the DOB to DIB
    if (JSP_ >= 22 && JSP_ <= 30) {
        double dbt = 0;
        double DOB = DIB;
        DIB = BRK_OT(JSP_, geoSubregion_, dbh_, DOB, upperHeight, doubleBarkThicknessBH_, dbt);
    }
    return DIB;
}

void FlewellingTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    //fwinit.f
    dbh_ = tree.dbh;
    if (merchRules.doubleBarkThicknessAtBrestHeight > 0.0) {
        doubleBarkThicknessBH_ = merchRules.doubleBarkThicknessAtBrestHeight;
    }
    else if (merchRules.barkThicknessRatio > 0.0) {
        dibDobPercent_ = merchRules.barkThicknessRatio;
    }

    //R3 could set BTR from Equation default
    if (doubleBarkThicknessBH_ == 0.0 && dibDobPercent_ > 0.0) {
        doubleBarkThicknessBH_ = dbh_ * (1.0 - dibDobPercent_ / 100.0);
    }

    //If totalheight is not measured, get the estimated from merchHeight and merchTopDiameter
    if (tree.totalHeight <= 0.0) {
        //Fortran subroutine sf_2pth
        if (tree.merchHeightNonsaw > 0.0) {
            merchHeight_ = tree.merchHeightNonsaw;
            merchTopDib_ = merchRules.minTopDibNonSaw;
        }
        else if (tree.merchHeightSaw > 0.0) {
            merchHeight_ = tree.merchHeightSaw;
            merchTopDib_ = merchRules.minTopDibSaw;
        }
        else if (tree.referenceDiameter > 0.0 && tree.referenceHeight > 0.0) {
            merchHeight_ = tree.referenceHeight;
            merchTopDib_ = tree.referenceDiameter;
        }
        else if (tree.heightToTopBroken > 0.0 && tree.topBrokenDiameter > 0.0) {
            merchHeight_ = tree.heightToTopBroken;
            merchTopDib_ = tree.topBrokenDiameter;
        }

        if (merchHeight_ == 0.0) {
            throw std::invalid_argument("Total height or merch height is needed for Flewelling profile model.");
        }

        totalHeight_ = solveTotalHeight(JSP_, geoSubregion_, dbh_, doubleBarkThicknessBH_, merchHeight_, merchTopDib_);
        
        if (totalHeight_ > 0.0) threePoint_ = true;
        else {
            //cannot fit total height, use dbhIb and Topd and merch height to calculate log volume

        }
    }
    else totalHeight_ = tree.totalHeight;
    
    if (totalHeight_ == 0.0) {
        throw std::invalid_argument("Total height or merch height is needed for Flewelling profile model.");
    }

    if (volumeEquation_.modelType == VolumeEquation::ModelType::FW3 || volumeEquation_.modelType == VolumeEquation::ModelType::F33) {
        threePoint_ = true;
    }

    //sf_2pt.f
    //Get dbhIb for JSP other than 21 - 30 (f_other.f)
    dbhIb_ = getDbhIb(JSP_, geoSubregion_, dbh_, totalHeight_);

    //double dbtBH = dbh_ - dbhIb_;
    if(doubleBarkThicknessBH_ == 0.0) doubleBarkThicknessBH_ = dbh_ - dbhIb_;

    //Get shape parameters: r1, r2, r3, r4, r5, a3, rhi1, rhi2, rhc, rhlongi
    shapeParameter_ = getShapeParameter(JSP_, geoSubregion_, dbh_, totalHeight_);

    //Get Flewelling coefficients: a0, a1, a2, a4, b0, b1, b2, b4, c1, c2, e1, e2, F
    taperCoef_ = getTaperCoef(shapeParameter_);

    //Get scaling factor
    //if (JSP >= 22 && JSP < 30) dbhIb = dbh;
    scalingFactor_ = getScalingFactor(JSP_, dbhIb_, totalHeight_, shapeParameter_, taperCoef_);

    //add 3-point model here. Only work with one pair of heightExtra/diameterExtra
    if (threePoint_) {
        if (merchHeight_ > 0.0 && merchTopDib_ > 0.0) {
            heightExtra_ = merchHeight_;
            diameterExtra_ = merchTopDib_;
        }
        else if (tree.referenceHeight > 0.0 && tree.referenceDiameter > 0.0) {
            heightExtra_ = tree.referenceHeight;
            diameterExtra_ = tree.referenceDiameter;
        }
        else if (tree.formClass > 0.0) {
            diameterExtra_ = dbh_ * tree.formClass / 100.0;
            double maxLogLength = 16.0;
            if (volumeEquation_.modelType == VolumeEquation::ModelType::F32 || volumeEquation_.modelType == VolumeEquation::ModelType::F33) {
                heightExtra_ = merchRules.stumpHeight + (maxLogLength + merchRules.trim) * 2.0; //33.6;
            }
            else heightExtra_ = merchRules.stumpHeight + (maxLogLength + merchRules.trim); //17.3;
        }
        else if (tree.merchHeightSaw > 0.0) {
            heightExtra_ = tree.merchHeightSaw;
            diameterExtra_ = merchRules.minTopDibSaw;
        }
        else if (tree.merchHeightNonsaw > 0.0) {
            heightExtra_ = tree.merchHeightNonsaw;
            diameterExtra_ = merchRules.minTopDibNonSaw;
        }
        else {
            throw std::invalid_argument("Upper stem measurement required for Flewelling 3-point profile model.");
        }


        //sf_3pt.f
        threePointExtra_ = getThreePointExtra(JSP_, geoSubregion_, dbh_, totalHeight_, doubleBarkThicknessBH_, heightExtra_, diameterExtra_, scalingFactor_, shapeParameter_, taperCoef_);
        zValueExtra_ = threePointExtra_.zValueExtra;
        
    }
}

double FlewellingTaperModel::GetDiameterAtHeight(TreeMeasurment tree, double height)
{
    double relativeHeight = height / totalHeight_;
    double slope;

    //Small tree stump diameter 
    if (totalHeight_ <= 15.0 && height < 2.0) {
        double H1 = 1.0;
        double H5 = 5.0;
        double H15 = 15.0;

        double HR_5 = 0.18;
        double HR_15LO = 0.11;
        double HR_15HI = 0.25;

        double DIB = 0.0;

        // Species-specific adjustment (Ponderosa Pine)
        if (JSP_ == 14) {
            HR_5 = 0.25;
            HR_15LO = 0.21;
            HR_15HI = 0.27;
        }

        //DIB = SF_DS(JSP_, geoSubregion_, dbh_, totalHeight_, H1, heightExtra_, diameterExtra_, shapeParameter_, taperCoef_, threePointExtra_, scalingFactor_, slope);
        DIB = estimateDibAtHeight(H1, slope);
        // Compute and possibly modify ratio DIB(1 ft)/(TOTALH - 1)
        double HRATIO = DIB / (totalHeight_ - H1);

        double HR_MIN = HR_5 + (totalHeight_ - H5) / (H15 - H5) * (HR_15LO - HR_5);
        double HR_MAX = HR_5 + (totalHeight_ - H5) / (H15 - H5) * (HR_15HI - HR_5);

        if (HRATIO < HR_MIN) HRATIO = HR_MIN;
        if (HRATIO > HR_MAX) HRATIO = HR_MAX;

        double DIB1 = HRATIO * (totalHeight_ - H1);

        // Impose a limit of DOB(1 ft)/DBHob > DR_MIN
        double DR_MIN = 1.0;

        if (totalHeight_ < H15)
            DR_MIN += 0.3 * (H15 - totalHeight_) / (H15 - H5);

        double DR = DIB1 / (dbh_ - doubleBarkThicknessBH_);

        if (DR < DR_MIN)
            DIB1 = DR_MIN * (dbh_ - doubleBarkThicknessBH_);

        return DIB1;
    }
    else {

        double dia = getDibFrom2PointModel(JSP_, relativeHeight, totalHeight_, shapeParameter_, taperCoef_, slope, scalingFactor_);
        if (threePoint_) {
            dia = getDibFrom3PointModel(JSP_, geoSubregion_, dbh_, totalHeight_, height, dia, heightExtra_, diameterExtra_, threePointExtra_);
        }
        double dib = dia;
        if (JSP_ >= 22 && JSP_ <= 30) {
            double dbt = 0;
            dib = BRK_OT(JSP_, geoSubregion_, dbh_, dia, height, doubleBarkThicknessBH_, dbt);
        }
        return dib;
    }
}

double FlewellingTaperModel::GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob)
{
    const double EPSILON = 0.001;
    const double TOL = 0.0005;
    double DIB = diameter;
    double H = 0.0;

    double RHI1 = shapeParameter_.rhi1;
    double RHI2 = shapeParameter_.rhi2;
    double RHC = shapeParameter_.rhc;
    double RHLONGI = shapeParameter_.rhlongi;

    bool needSlope = false;
    double slope = 0.0;

    double HI2 = RHI2 * totalHeight_;
    double TOOHIGH = totalHeight_;
    double TOOLOW = 0.0;

    // Compute diameter at inflection height
    //double DI2 = SF_DS(JSP_, geoSubregion_, dbh_, totalHeight_, HI2, heightExtra_, diameterExtra_, shapeParameter_, taperCoef_, threePointExtra_, scalingFactor_, slope, needSlope);
    double DI2 = estimateDibAtHeight(HI2, slope, needSlope);

    double RH = 0.0;
    // ------------------------------------------------------------
//   INITIAL GUESS FOR H
// ------------------------------------------------------------
    if (DIB > DI2) {
        TOOHIGH = HI2;
        double HI1 = RHI1 * totalHeight_;

        // check straight region if it exists
        //double DI1 = SF_DS(JSP_, geoSubregion_, dbh_, totalHeight_, HI1, heightExtra_, diameterExtra_, shapeParameter_, taperCoef_, threePointExtra_, scalingFactor_, slope, needSlope);
        double DI1 = estimateDibAtHeight(HI1, slope, needSlope);
        if (RHLONGI > 0.0) {
            if (DIB < DI1) {
                TOOLOW = DI1;
                RH = RHI2 - (RHI2 - RHI1) * (DIB - DI2) / (DI1 - DI2);
                H = RH * totalHeight_;
                goto START_NEWTON;
            }
            TOOHIGH = DI1;
        }
        else {
            DI1 = DI2;
        }

        // Base diameter
        //double DBASE = SF_DS(JSP_, geoSubregion_, dbh_, totalHeight_, 0.0, heightExtra_, diameterExtra_, shapeParameter_, taperCoef_, threePointExtra_, scalingFactor_, slope, needSlope);
        double DBASE = estimateDibAtHeight(0.0, slope, needSlope);

        if (DBASE <= DIB) {
            H = 0.0;
            return H;
        }

        double RZ = pow((DIB - DI1) / (DBASE - DI1), 0.25);
        RH = (1.0 - RZ) * RHI1;
    }
    else {
        // Solution above inflection
        TOOLOW = DI2;
        double RZ = 1.0 - pow(DIB / DI2, 2.0);
        RH = RHI2 + (1.0 - RHI2) * RZ;
    }

    H = RH * totalHeight_;

START_NEWTON:

    // ------------------------------------------------------------
    // NEWTON ITERATION
    // ------------------------------------------------------------
    {
        needSlope = true;
        int IBREAK = 0;
        int ITER = 0;

        while (true) {
            ITER++;
            if (ITER > 30) goto BISECTION;

            //double D = SF_DS(JSP_, geoSubregion_, dbh_, totalHeight_, H, heightExtra_, diameterExtra_, shapeParameter_, taperCoef_, threePointExtra_, scalingFactor_, slope, needSlope);
            double D = estimateDibAtHeight(H, slope, needSlope);

            double ERR = D - DIB;

            if (ERR < 0.0) TOOHIGH = H;

            double ADJUST = -ERR / slope;
            double H_new = H + ADJUST;

            if (H_new > totalHeight_)
                H_new = (H + totalHeight_) / 2.0;
            if (H_new < 0.0)
                H_new = H / 2.0;

            H = H_new;

            if (std::abs(ADJUST) <= TOL * totalHeight_ &&
                std::abs(ERR) <= EPSILON)
            {
                // One more step was taken; now test slope
                if (slope > 0.0) {
                    if (IBREAK < 2) {
                        IBREAK++;
                        if (IBREAK == 1) H = 0.8 * H;
                        if (IBREAK == 2) H = H + 0.25 * (totalHeight_ - H);
                        continue; // restart tightening
                    }
                }
                return H;
            }
        }
    }

    // ------------------------------------------------------------
    // BISECTION METHOD
    // ------------------------------------------------------------
BISECTION:
    {
        needSlope = false;
        int ITER = 0;

        double HHIGH = TOOHIGH;
        double HLOW = TOOLOW;

        double D1, D2;
        //D1 = SF_DS(JSP_, geoSubregion_, dbh_, totalHeight_, HHIGH, heightExtra_, diameterExtra_, shapeParameter_, taperCoef_, threePointExtra_, scalingFactor_, slope, needSlope);
        D1 = estimateDibAtHeight(HHIGH, slope, needSlope);

        //D2 = SF_DS(JSP_, geoSubregion_, dbh_, totalHeight_, HLOW, heightExtra_, diameterExtra_, shapeParameter_, taperCoef_, threePointExtra_, scalingFactor_, slope, needSlope);
        D2 = estimateDibAtHeight(HLOW, slope, needSlope);

        double EHIGH = D1 - DIB;
        double ELOW = D2 - DIB;

        if (EHIGH * ELOW > 0.0) {
            H = 0.0;
            return H; // failed
        }

        double EPS = EPSILON * 2.0;

        while (true) {
            double HTRY = 0.5 * (HHIGH + HLOW);

            double DTRY;
            //DTRY = SF_DS(JSP_, geoSubregion_, dbh_, totalHeight_, HTRY, heightExtra_, diameterExtra_, shapeParameter_, taperCoef_, threePointExtra_, scalingFactor_, slope, needSlope);
            DTRY = estimateDibAtHeight(HTRY, slope, needSlope);

            double ERR = DTRY - DIB;

            if (std::abs(ERR) < EPS) {
                H = HTRY;
                return H;
            }

            if (ITER > 40) {
                H = HTRY;
                return H;
            }

            ITER++;

            if (ERR > 0.0)
                HLOW = HTRY;
            else
                HHIGH = HTRY;
        }
    }

    return H;
}