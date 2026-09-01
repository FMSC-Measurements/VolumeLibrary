#include "DirectVolumeCalculator_FIA_PacificIsland.h"
#include <cmath>
#include <array>

// Cubic foot volume of main stem outside bark to a variable top and stump
// Developed by Olaf Kuegler for Pacific Island inventory using centroidDia and centroidHeight
inline double centroidVolume(double totalHeight, double centroidDia, double centroidHeight,
    double topDia, double stumpHeight)
{
    double A = 2.0;
    double term1 = (totalHeight - centroidHeight);
    double term2 = (totalHeight - stumpHeight);
    double exponent1 = -2.0 / A;
    double exponent2 = (A + 2.0) / A;

    double part = std::pow(term2, exponent2)
        - std::pow(term1, exponent2) * std::pow((topDia / centroidDia), (A + 2.0));

    return (3.14 / 4.0) * (centroidDia * centroidDia) * (A / (A + 2.0))
        * std::pow(term1, exponent1) * part / 144.0;
}

TreeOutput Centroid_CV(TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;
    double totalHeight = tree.totalHeight;
    double centroidHeight = tree.referenceHeight;
    double centroidDia = tree.referenceDiameter;
    double topDia = 0.0;
    double stumpHeight = 0.0;
    double A = 2.0;

    if (centroidDia <= 0.0 || centroidHeight <= 0.0) {
        out.errflag = 9;
        return out;
    }

    out.totalCubicFoot = centroidVolume(totalHeight, centroidDia, centroidHeight, topDia, stumpHeight);
    if (out.totalCubicFoot < 0.0) out.totalCubicFoot = 0.1;

    topDia = merchRules.minTopDibNonSaw < 0.1? 4.0 : merchRules.minTopDibNonSaw;
    stumpHeight = merchRules.stumpHeight < 0.1? 1.0 : merchRules.stumpHeight;

    out.grossCubicFootPrimary = centroidVolume(totalHeight, centroidDia, centroidHeight, topDia, stumpHeight);
    if (out.grossCubicFootPrimary < 0.0) out.grossCubicFootPrimary = 0.1;

    return out;
}

inline double frustumVolume(double length, double lowerDiameter, double upperDiameter)
{
    const double PI = 3.1415927;
    double r1 = lowerDiameter / 24.0;
    double r2 = upperDiameter / 24.0;

    double volume = PI * length / 3.0 * (r1 * r1 + r1 * r2 + r2 * r2);

    return volume;
}

TreeOutput Frustum_Vol(TreeMeasurment tree, MerchRules merchRules) {
    TreeOutput out;

    double THT = tree.totalHeight;
    double DBHOB = tree.dbh;
    double DRCOB = tree.drc;
    double UPHT = tree.referenceHeight;
    double UPDIA = tree.referenceDiameter;
    double STUMP = merchRules.stumpHeight < 0.1 ? 1.0 : merchRules.stumpHeight;
    double TOPD = merchRules.minTopDibNonSaw < 0.1 ? 4.0 : merchRules.minTopDibNonSaw;
    double HT2BRK = tree.heightToTopBroken;

    double PI = 3.1415927;

    double DBH = 0.0, HTDBH = 0.0;
    if (DBHOB > 0.1) {
        DBH = DBHOB;
        HTDBH = 4.5;
    }
    else if (DRCOB > 0.1) {
        DBH = DRCOB;
        HTDBH = 0.0;
    }
    if (UPHT <= 0.0 || UPDIA <= 0.0) {
        out.errflag = 9;
        return out;
    }

    if (UPHT <= HTDBH) {
        out.errflag = 7;
        return out;
    }

    double TAPER_ANGLE =
        std::atan(((DBH - UPDIA) / 24.0) / (UPHT - HTDBH));

    double DRC = DBH + 24.0 * HTDBH * std::tan(TAPER_ANGLE);
    double STUMPD = DBH + 24.0 * (HTDBH - STUMP) * std::tan(TAPER_ANGLE);

    double LEN = 0.0;
    double VOL_LOWER, VOL_MID, VOL_UPPER;

    LEN = HTDBH;
    VOL_LOWER = frustumVolume(LEN, DRC, DBH);

    LEN = UPHT - HTDBH;
    VOL_MID = frustumVolume(LEN, DBH, UPDIA);

    LEN = THT - UPHT;
    VOL_UPPER = frustumVolume(LEN, UPDIA, 0.0);

    double CVTS = VOL_LOWER + VOL_MID + VOL_UPPER;

    // ---- CV4 calculations ----
    double CV4 = 0.0;
    double TAPER_ANGLE2 = 0.0;

    if (UPDIA <= TOPD) {
        LEN = UPHT - STUMP;
        VOL_LOWER = frustumVolume(LEN, STUMPD, UPDIA);
        VOL_UPPER = 0.0;
    }
    else if (UPHT == THT) {
        LEN = UPHT - STUMP;
        VOL_LOWER = frustumVolume(LEN, STUMPD, UPDIA);
        VOL_UPPER = 0.0;
    }
    else if (UPDIA > TOPD) {
        TAPER_ANGLE2 = std::atan((UPDIA / 24.0) / (THT - UPHT));
        double HTTOP = THT - (TOPD / 24.0) / std::tan(TAPER_ANGLE2);

        LEN = UPHT - STUMP;
        VOL_LOWER = frustumVolume(LEN, STUMPD, UPDIA);

        LEN = HTTOP - UPHT;
        VOL_UPPER = frustumVolume(LEN, UPDIA, TOPD);
    }
    else {
        VOL_LOWER = 0.0;
        VOL_UPPER = 0.0;
    }

    CV4 = VOL_LOWER + VOL_UPPER;

    // ---- Top-break section ----
    if (HT2BRK > 4.5 ) {

        if (UPHT >= HT2BRK) {
            LEN = UPHT;
            VOL_LOWER = frustumVolume(LEN, DRC, UPDIA);
            VOL_UPPER = 0.0;
        }
        else if (UPHT == THT) {
            LEN = UPHT;
            VOL_LOWER = frustumVolume(LEN, DRC, UPDIA);
            VOL_UPPER = 0.0;
        }
        else if (UPHT < HT2BRK) {
            TAPER_ANGLE2 = std::atan((UPDIA / 24.0) / (THT - UPHT));
            double DIA_BREAK = UPDIA - 24.0 * (HT2BRK - UPHT) * std::tan(TAPER_ANGLE2);

            LEN = UPHT;
            VOL_LOWER = frustumVolume(LEN, DRC, UPDIA);

            LEN = HT2BRK - UPHT;
            VOL_UPPER = frustumVolume(LEN, UPDIA, DIA_BREAK);
        }
        else {
            VOL_LOWER = 0.0;
            VOL_UPPER = 0.0;
        }

        CVTS = VOL_LOWER + VOL_UPPER;

        // Second CV4 pass for broken top
        if (THT > UPHT)
            TAPER_ANGLE2 = std::atan((UPDIA / 24.0) / (THT - UPHT));
        else
            TAPER_ANGLE2 = TAPER_ANGLE;

        double DIA_BREAK;
        if (UPHT < HT2BRK)
            DIA_BREAK = UPDIA - 24.0 * (HT2BRK - UPHT) * std::tan(TAPER_ANGLE2);
        else
            DIA_BREAK = DBH - 24.0 * (UPHT - HTDBH) * std::tan(TAPER_ANGLE2);

        double DIA_TOP_BREAK, HT_TOP_BREAK;
        if ((TOPD <= DIA_BREAK) || (THT <= UPHT)) {
            DIA_TOP_BREAK = DIA_BREAK;
            HT_TOP_BREAK = HT2BRK;
        }
        else {
            double HT_TOP;
            if (TAPER_ANGLE2 == 0.0)
                return out;

            HT_TOP = THT - (TOPD / 24.0) / std::tan(TAPER_ANGLE2);
            DIA_TOP_BREAK = TOPD;
            HT_TOP_BREAK = HT_TOP;
        }

        if (UPDIA <= DIA_TOP_BREAK) {
            LEN = UPHT - 1.0;
            VOL_LOWER = frustumVolume(LEN, STUMPD, UPDIA);
            VOL_UPPER = 0.0;
        }
        else if (UPHT == THT) {
            LEN = UPHT - 1.0;
            VOL_LOWER = frustumVolume(LEN, STUMPD, UPDIA);
            VOL_UPPER = 0.0;
        }
        else if (UPDIA > DIA_TOP_BREAK) {
            LEN = UPHT - 1.0;
            VOL_LOWER = frustumVolume(LEN, STUMPD, UPDIA);

            LEN = HT_TOP_BREAK - UPHT;
            VOL_UPPER = frustumVolume(LEN, UPDIA, DIA_TOP_BREAK);
        }
        else {
            VOL_LOWER = 0.0;
            VOL_UPPER = 0.0;
        }

        CV4 = VOL_LOWER + VOL_UPPER;
    }

    // Stump volume
    double STUMPV = 0.002727 * (DRC * DRC + STUMPD * STUMPD);

    out.totalCubicFoot = CVTS;
    out.grossCubicFootPrimary = CV4;
    out.stumpCubicFoot = STUMPV;

    return out;
}

TreeOutput Cone_Vol(TreeMeasurment tree, MerchRules merchRules) {
    TreeOutput out;

    double HT = tree.totalHeight;
    double DBHOB = tree.dbh;
    double DRCOB = tree.drc;
    double HT_BREAK = tree.heightToTopBroken;
    double STUMP = merchRules.stumpHeight < 0.1 ? 1.0 : merchRules.stumpHeight;
    double DIA_TOP = merchRules.minTopDibNonSaw < 0.1 ? 4.0 : merchRules.minTopDibNonSaw;

    const double PI = 3.1415927;

    double DBH = 0.0;
    double HT_DBH = 0.0;

    if (DBHOB > 0.1) {
        DBH = DBHOB;
        HT_DBH = 4.5;
    }
    else if (DRCOB > 0.1) {
        DBH = DRCOB;
        HT_DBH = 0.0;
    }

    double CVTS = 0.0;
    double CV4 = 0.0;
    double HT_STUMP = STUMP;

    if (HT <= HT_DBH) {
        out.errflag = 4;
        return out;
    }

    double TAPER_ANGLE =
        std::atan((DBH / 24.0) / (HT - HT_DBH));

    double DRC = 24.0 * HT * std::tan(TAPER_ANGLE);

    CVTS = 3.14 / 3.0 *
        std::pow(DRC / 24.0, 2.0) * HT;

    double DIA_STUMP =
        DBH + 24.0 * (HT_DBH - STUMP) * std::tan(TAPER_ANGLE);

    double HT_TOP =
        HT - (DIA_TOP / 24.0) / std::tan(TAPER_ANGLE);

    double LEN = HT_TOP - STUMP;
    CV4 = frustumVolume(LEN, DIA_STUMP, DIA_TOP);

    if (HT_BREAK > 4.5) {

        DRC = DBH + 24.0 * HT_DBH * std::tan(TAPER_ANGLE);

        double DIA_BREAK =
            DBH - 24.0 * (HT_BREAK - HT_DBH) *
            std::tan(TAPER_ANGLE);

        if (HT >= HT_BREAK) {
            CVTS = 3.14 / 3.0 *
                std::pow(DRC / 24.0, 2.0) * HT;
        }
        else {
            CVTS = frustumVolume(HT_BREAK, DRC, DIA_BREAK);
        }

        DIA_STUMP =
            24.0 * (HT - HT_STUMP) * std::tan(TAPER_ANGLE);

        DIA_BREAK =
            24.0 * (HT - HT_BREAK) * std::tan(TAPER_ANGLE);

        HT_TOP =
            HT - (DIA_TOP / 24.0) / std::tan(TAPER_ANGLE);

        if (HT_TOP < HT_BREAK) {
            LEN = HT_TOP - HT_STUMP;
            CV4 = frustumVolume(LEN, DIA_STUMP, DIA_TOP);
        }
        else {
            LEN = HT_BREAK - HT_STUMP;
            CV4 = frustumVolume(LEN, DIA_STUMP, DIA_BREAK);
        }
    }

    double STUMPV =
        0.002727 * (DRC * DRC + DIA_STUMP * DIA_STUMP);

    out.totalCubicFoot = CVTS;
    out.grossCubicFootPrimary = CV4;
    out.stumpCubicFoot = STUMPV;

    return out;

}

inline double Cylinder_CVTS(double DIA, double HT)
{
    double r = DIA / 24.0;
    return 3.14 * r * r * HT;
}

TreeOutput PacificIsland_Vol(const std::string& voleq, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;

    int fiaCode = std::stoi(voleq.substr(6, 4));
    double dbh = tree.dbh;
    double drc = tree.drc;
    double dia = 0.0;
    if (dbh > 0.1) dia = dbh;
    else if (drc > 0.1) dia = drc;

    double ht = tree.totalHeight;
    if (merchRules.minTopDibNonSaw < 0.1) merchRules.minTopDibNonSaw = 4.0;
    if (merchRules.stumpHeight < 0.1) merchRules.stumpHeight = 1.0;

    if (fiaCode >= 6545 && fiaCode <= 6549) {
        if (tree.heightToTopBroken > 0.0) ht = tree.heightToTopBroken;
        double cvts = Cylinder_CVTS(dia, ht);
        out.totalCubicFoot = cvts;
        out.grossCubicFootPrimary = cvts;
    }
    else if (!tree.referenceHeightAtCentroid && (tree.referenceDiameter > 0.0 && tree.referenceHeight > 0.0)) {
        out = Frustum_Vol(tree, merchRules);
    }
    else if (tree.referenceHeightAtCentroid && (tree.referenceDiameter > 0.0 && tree.referenceHeight > 0.0)) {
        out = Centroid_CV(tree, merchRules);
    }
    else {
        out = Cone_Vol(tree, merchRules);
    }

    return out;
}