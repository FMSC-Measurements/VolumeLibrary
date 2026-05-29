#include "FlewellingTaperModel.h"
#include "FlewellingShapeCoef.h"
#include "FlewellingTaperModel_Alaska.h"
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
    geoCode = volumeEquation_.geoCode;
    geoSubregion = { volumeEquation_.subregionalCode };

    // ---------------- REGION I ----------------
    if (geoCode== VolumeEquation::GeoCode::INGYMODEL)      
    {
        if (fiaCode == 202 || fiaCode == 205 || fiaCode == 204)
            JSP = 11; // Douglas fir
        else if (fiaCode == 73 || fiaCode == 70)
            JSP = 12; // Western Larch
        else if (fiaCode == 17)
            JSP = 13; // Grand fir
        else if (fiaCode == 122)
            JSP = 14; // Ponderosa pine
        else if (fiaCode == 108)
            JSP = 15; // Lodgepole pine
        else if (fiaCode == 242 || fiaCode == 240)
            JSP = 16; // Western Red Cedar
        else if (fiaCode == 260 || fiaCode == 263 || fiaCode == 264)
            JSP = 17; // Mountain Hemlock
        else if (fiaCode == 119)
            JSP = 18; // White pine
        else if (fiaCode == 93 || fiaCode == 90)
            JSP = 19; // Engelmann Spruce
        else if (fiaCode == 19)
            JSP = 20; // Subalpine fir
        else if (fiaCode == 12)
            JSP = 21; // Balsam fir
    }

    // ---------------- WESTSIDE (F) ----------------
    else if (geoCode == VolumeEquation::GeoCode::FLWELLINGMODEL_WEST)
    {
        if (fiaCode == 202 || fiaCode == 205 || fiaCode == 204)
            JSP = 3;   // Douglas fir
        else if (fiaCode == 263)
            JSP = 4;   // Western Hemlock
        else if (fiaCode == 242)
            JSP = 5;   // Western Red Cedar
    }

    // ---------------- REGION 2 ----------------
    else if (geoCode == VolumeEquation::GeoCode::R2)
    {
        if (fiaCode == 122)
        {
            if (geoSubregion == "03")
                JSP = 22;   // Black Hills model
            else
                JSP = 23;   // region-wide
        }
        else if (fiaCode == 108)
            JSP = 25;       // Lodgepole
        else if (fiaCode == 202)
            JSP = 26;       // Douglas fir
        else if (fiaCode == 15)
            JSP = 27;       // White fir
        else if (fiaCode == 746)
            JSP = 28;       // Aspen
    }

    // ---------------- REGION 4 ----------------
    else if (geoCode == VolumeEquation::GeoCode::R4)
    {
        if (geoSubregion == "07")
        {
            if (fiaCode == 93)
                JSP = 24;       // Engelmann spruce
            else if (fiaCode == 122)
                JSP = 23;       // R2 Ponderosa with R4 bark
        }
    }

    // ---------------- REGION 3 ----------------
    else if (geoCode == VolumeEquation::GeoCode::R3)
    {
        if (geoSubregion == "00")
        {
            if (fiaCode == 122)
                JSP = 29;
            else if (fiaCode == 202)
            {
                JSP = 26;
                BTR = 88.85;
            }
        }

        else if (geoSubregion == "01")
        {
            if (fiaCode == 122)
            {
                JSP = 29;
                BTR = 89.12;
            }
            else if (fiaCode == 108)
            {
                JSP = 25;
                BTR = 93.26;
            }
            else if (fiaCode == 202)
            {
                JSP = 26;
                BTR = 89.72;
            }
            else if (fiaCode == 15)
            {
                JSP = 27;
                BTR = 91.16;
            }
        }
    }

    // ---------------- REGION 10 (A) ----------------
    else if (geoCode == VolumeEquation::GeoCode::R10)
    {
        if (fiaCode == 42)
            JSP = 31;        // Alaska yellow cedar
        else if (fiaCode == 242)
            JSP = 32;        // Western Red Cedar
        else if (fiaCode == 98)
        {
            if (geoSubregion == "02")
                JSP = 35;    // spruce (variant)
            else
                JSP = 33;
        }
        else if (fiaCode == 263 || fiaCode == 260 || fiaCode == 264)
        {
            if (geoSubregion == "02")
                JSP = 36;    // hemlock (variant)
            else
                JSP = 34;
        }
    }

    if (JSP == 0) {
        throw std::invalid_argument("Invalid species code in volume equation: " + volumeEquation_.volEqStr);
    }

}

//double bark thickness in West
inline int find_geo_index(std::string_view code) {
    static constexpr std::array<std::string_view, 8> GCODE =
    { "01","02","03","04","05","06","07","08" };

    for (int i = 0; i < 8; ++i)
        if (GCODE[i] == code)
            return i;

    return -1; // GEOSUB = "00" or not found
}

double FDBT_C1(int JSP, std::string_view GEOSUB, double DBHOB, double HTTOT)
{
    static constexpr std::array<double, 8> ROFF =
    { 0.0, 0.117, 0.121, 0.133, 0.025, 0.028, 0.088, 0.028 };

    static constexpr std::array<double, 8> ROFF1 =
    { 0.0, 0.118, 0.204, 0.105, 0.058, 0.061, 0.118, 0.071 };

    double ratio = 0.0;
    int id = find_geo_index(GEOSUB);
    double logd = std::log(DBHOB); // reused

    switch (JSP)
    {
    case 3:
    {
        double DMEDIAN =
            0.566 * std::pow(HTTOT - 4.5, 0.634 + 0.00074 * HTTOT);

        double DFORM = DBHOB / DMEDIAN - 1.0;

        if (id < 0)   // GEOSUB = "00"
        {
            ratio = std::exp(
                -2.4641 +
                0.04393 * logd -
                0.2922 * DFORM +
                0.05964 * DFORM * logd
            );
        }
        else
        {
            ratio = std::exp(
                -2.5087 +
                0.03600 * logd -
                0.4086 * DFORM +
                0.10120 * DFORM * logd +
                ROFF[id]
            );
        }
        return ratio * DBHOB;
    }

    case 4:
    {
        if (id < 0)
        {
            ratio = 0.04504 * (1.0 + 0.8307 * std::exp(-0.2048 * DBHOB));
        }
        else
        {
            ratio =
                0.04221 * (1.0 + 0.8836 * std::exp(-0.2145 * DBHOB))
                * (1.0 + ROFF1[id]);
        }
        return ratio * DBHOB;
    }

    case 5:
    {
        double D_USE = std::max(DBHOB, 3.8);
        ratio = 0.01949 *
            (1.0 + 15.599 / D_USE - 29.212 / (D_USE * D_USE));

        return ratio * DBHOB;
    }
    }

    // Unknown species
    return 0.0;
}

//get dbhIb
double getDbhIb(int JSP, std::string_view geoSub, double dbh, double totalHt)
{
    double dbtBH = 0.0;
    if (DBTBH > 0.0) dbtBH = DBTBH;
    if (dbtBH == 0.0 && BTR > 0.0) dbtBH = dbh * BTR / 100.0;

    if(dbtBH ==0.0) {
        //get doublebarkThicknessAtBreastHeight from bark model
        if (JSP >= 3 && JSP <= 5) {
            //West coast bark model
            dbtBH = FDBT_C1(JSP, geoSub, dbh, totalHt);
        }
        else if (JSP >= 11 && JSP <= 21) {
            //Ingy

        }
        else if (JSP >= 31 && JSP <= 36) {
            //Alaska
            dbtBH = FDBT_AK(JSP, dbh, totalHt);
        }
        else if (JSP >= 22 && JSP <= 31) {
            //other use dbhOb
            dbtBH = 0.0;
        }
    }

    double dbhIb = dbh - dbtBH;
    return dbhIb;
}

//get shape parameters
FlewellingShapeParams getShapeParameter(int JSP, std::string_view geoSub, double dbh, double totalHt)
{
    //sf_shp.f
    FlewellingShapeParams shapeParameter;
    if (JSP == 3) {
        //shp_w3

    }
    else if (JSP == 4) {
        //shp_w4

    }
    else if (JSP == 5) {
        //shp_w5

    }
    else if (JSP >= 11 && JSP <= 22) {
        //shp_c2

    }
    else if (JSP == 22) {
        //shp_bh

    }
    else if (JSP >= 23 && JSP <= 30) {
        //shp_ot

    }
    else if (JSP >= 31 && JSP <= 36) {
        shapeParameter = SHP_AK(JSP, geoSub, dbh, totalHt);
    }

    return shapeParameter;
}

//get taper coef
FlewellingTaperCoef getTaperCoef(FlewellingShapeParams shapeParemeter)
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
double getDibFrom2PointModel(int jsp, double relativeHeight, FlewellingShapeParams shapeParemeter, FlewellingTaperCoef taperCoef, double scalingFactor = 1.0)
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

    }

    // --- Straight segment ---
    else if (RHLONGI > 0.0 && RH > RHI1) {
        y = E1 + E2 * RH;
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
    }

    // Final yhat
    double yhat = F * y;
    dib = yhat;

    return dib;
}

//get scaling factor F
double getScalingFactor(int JSP, double dbhIb, double totalHeight, FlewellingShapeParams shapeParameter, FlewellingTaperCoef taperCoef)
{
    double RH = 4.5 / totalHeight;
    double dbhIbPred = getDibFrom2PointModel(JSP, RH, shapeParameter, taperCoef);
    
    return dbhIb / dbhIbPred;
}

//get 3-point extra parameter
Flewelling3PointExtra getThreePointExtra(int jsp, double dbh, double totalHt, double dbtBH, 
    double heightExtra, double diameterExtra, double scalingFactor,
    FlewellingShapeParams shapeParameter, FlewellingTaperCoef taperCoef)
{
    Flewelling3PointExtra out;

    double F = scalingFactor;
    double HEX = heightExtra;
    double DEX = diameterExtra;
    double TOTALH = totalHt;

    const double BH = 4.5;
    double FMODMAX = 0.15;
    double HFIRSTUP = 0.0;
    double FMODMAXU = 0.0;

    // local arrays
    double PINV_Z = 0.0;
    double Z = 0.0;
    double hextra;
    double dhatex;
    double bark_r;
    double dobex;

    double h = hextra = HEX;
    double x = h / TOTALH;

    double slope_dummy = 0.0;
    dhatex = getDibFrom2PointModel(jsp, x, shapeParameter, taperCoef, F);

    // actual diameter processing
    double dibmod = dhatex;
    double dibact = 0.0;

    if (jsp >= 11 && jsp <= 21) {
        double dob = DEX;
        dobex = dob;
        //need add later
        //bark_r = BRK_UPB2(jsp, DBH, TOTALH, DBT_BH, h, dob);

        double dbt = bark_r * dob;
        dibact = dob - dbt;
    }
    else if (jsp >= 3 && jsp <= 5) {
        double dob = DEX;
        dobex = dob;
        //need add later
        //bark_r = BRK_WS(jsp, DBH, TOTALH, DBT_BH, h);

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
        //need add later
        //VAR_C2(jsp, geosub, DBH, TOTALH, h, se_lnx);
        Z = std::log(dibact / dibmod) / se_lnx;
    }
    else if (jsp >= 3 && jsp <= 5) {
        //need add later
        //VAR_C1(jsp, DBH, TOTALH, h, dibmod, dibact, Z);
    }
    else if (jsp >= 23 && jsp <= 30) {
        //need add later
        //VAR_OT(jsp, DBH, TOTALH, h, se_lnx);
        Z = std::log(dibact / dibmod) / se_lnx;
    }
    else if (jsp >= 31 && jsp <= 36) {
        se_lnx = VAR_AK(jsp, dbh, totalHt, heightExtra);
        Z = std::log(dibact / dibmod) / se_lnx;
    }
    else if (jsp == 22) {
        //need add later
        //VAR_BH(DBH, TOTALH, h, se_lnx);
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
double getDibFrom3PointModel(int jsp, std::string_view geoSub, double dbh, double totalHt, double H, double DIB, double HEX, double DEX, Flewelling3PointExtra threePointExtra)
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
    //need to add later
    //double p12 = SF_CORR(jsp, geosub, THT, HEX, H);
    double EZ = p12 * ZEX;

    // ------------------------------------------------------------
    // Compute adjusted diameter using SF_DFZ
    // DIBact = f(DIB, EZ)
    // ------------------------------------------------------------
    double DIBact = 0.0;
    //need to add later
    //SF_DFZ(jsp, geosub, DBH, THT, H, DIB, EZ, DIBact);

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
double computeDiameterError(int jsp, std::string_view geoSub, 
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

    double dbhIb = DBH - DBTBH;
    double F = getScalingFactor(jsp, dbhIb, HT_try, shapeParameter, taperCoef);

    double relativeHeight = H1 / HT_try;
    double D2 = getDibFrom2PointModel(jsp, relativeHeight, shapeParameter, taperCoef, F);

    if (jsp >= 22 && jsp <= 30) {
        double DOB, DBT;
        //add BRK_UP later
        //BRK_UP(S, DBH, HT_try, DBTBH, H1, D2, DOB, DBT);
    }

    return D2 - D1;  // inside-bark difference
}

// solve total height from merch height and top diameter
double solveTotalHeight(int JSP, std::string_view geoSub, double DBH, double dbtBH, double H1, double D1)
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

void FlewellingTaperModel::InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco)
{
    //fwinit.f
    bool threePoint = false;
    dbh = tree.dbh;
    DBTBH = merchRules.doubleBarkThicknessAtBrestHeight;
    if (BTR == 0.0) BTR = merchRules.barkThicknessRatio;

    //If totalheight is not measured, get the estimated from merchHeight and merchTopDiameter
    if (tree.totalHeight <= 0.0) {
        //Fortran subroutine sf_2pth
        if (tree.merchHeightNonsaw > 0.0) {
            merchHeight = tree.merchHeightNonsaw;
            merchTopD = merchRules.minTopDibNonSaw;
        }
        else if (tree.merchHeightSaw > 0.0) {
            merchHeight = tree.merchHeightSaw;
            merchTopD = merchRules.minTopDibSaw;
        }

        if (merchHeight == 0.0) {
            throw std::invalid_argument("Total height or merch height is needed for Flewelling profile model.");
        }

        totalHeight = solveTotalHeight(JSP, geoSubregion, dbh, DBTBH, merchHeight, merchTopD);
        
        if (totalHeight > 0.0) threePoint = true;
        else {
            //cannot fit total height, use dbhIb and Topd and merch height to calculate log volume

        }
    }
    else totalHeight = tree.totalHeight;
    
    if (totalHeight == 0.0) {
        throw std::invalid_argument("Total height or merch height is needed for Flewelling profile model.");
    }

    if (volumeEquation_.modelType == VolumeEquation::ModelType::FW3 || volumeEquation_.modelType == VolumeEquation::ModelType::F33) {
        threePoint = true;
    }

    //sf_2pt.f
    //Get dbhIb
    dbhIb = getDbhIb(JSP, geoSubregion, dbh, totalHeight);

    double dbtBH = dbh - dbhIb;

    //Get shape parameters: r1, r2, r3, r4, r5, a3, rhi1, rhi2, rhc, rhlongi
    FlewellingShapeParams shapeParameter = getShapeParameter(JSP, geoSubregion, dbh, totalHeight);

    //Get Flewelling coefficients: a0, a1, a2, a4, b0, b1, b2, b4, c1, c2, e1, e2, F
    FlewellingTaperCoef taperCoef = getTaperCoef(shapeParameter);

    //Get scaling factor
    F = getScalingFactor(JSP, dbhIb, totalHeight, shapeParameter, taperCoef);

    //add 3-point model here. Only work with one pair of heightExtra/diameterExtra
    if (threePoint) {
        if (merchHeight > 0.0 && merchTopD > 0.0) {
            heightExtra = merchHeight;
            diameterExtra = merchTopD;
        }
        else if (tree.referenceHeight > 0.0 && tree.referenceDiameter > 0.0) {
            heightExtra = tree.referenceHeight;
            diameterExtra = tree.referenceDiameter;
        }
        else if (tree.formClass > 0.0) {
            diameterExtra = dbh * tree.formClass / 100.0;
            double maxLogLength = 16.0;
            if (volumeEquation_.modelType == VolumeEquation::ModelType::F32 || volumeEquation_.modelType == VolumeEquation::ModelType::F33) {
                heightExtra = merchRules.stumpHeight + (maxLogLength + merchRules.trim) * 2.0; //33.6;
            }
            else heightExtra = merchRules.stumpHeight + (maxLogLength + merchRules.trim); //17.3;
        }
        else if (tree.merchHeightSaw > 0.0) {
            heightExtra = tree.merchHeightSaw;
            diameterExtra = merchRules.minTopDibSaw;
        }
        else if (tree.merchHeightNonsaw > 0.0) {
            heightExtra = tree.merchHeightNonsaw;
            diameterExtra = merchRules.minTopDibNonSaw;
        }
        else {
            throw std::invalid_argument("Upper stem measurement required for Flewelling 3-point profile model.");
        }


        //sf_3pt.f
        Flewelling3PointExtra threePointExtra = getThreePointExtra(JSP, dbh, totalHeight, dbtBH, heightExtra, diameterExtra, F, shapeParameter, taperCoef);
        zValueExtra = threePointExtra.zValueExtra;
        
    }
}