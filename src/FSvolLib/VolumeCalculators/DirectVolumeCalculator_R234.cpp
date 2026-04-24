#include "DirectVolumeCalculator_R234.h"

#include <array>
#include <string>
#include <cmath>
#include <algorithm>

// R2OLDV: Region 2 D*D*H volume equations (English units)
// Mirrors the FORTRAN interface: VOL and ERRFLAG are outputs via reference.
TreeOutput R2OLDV(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
    // Initialize 
    TreeOutput out;
    double HTTOT = tree.totalHeight;
    double DBHOB = tree.dbh;
    double DRC = tree.drc;
    int PROD = vco.primaryProduct;
    double MTOPP = merchRules.minTopDibSaw;
    if (tree.minTopDibSawOverride > 0.0) MTOPP = tree.minTopDibSawOverride > 0.0;
    int ERRFLAG = 0;
    std::array<double, 15> VOL;
    VOL.fill(0.0);

    // Map FCLASS to MSTEM (1=single; else 0) to match FORTRAN change
    int MSTEM = (tree.stems == 1) ? 1 : 0;

    // Early validation (matches FORTRAN)
    if (DBHOB <= 0.0 && DRC <= 0.0) {
        ERRFLAG = 3;
        out.errflag = ERRFLAG;
        return out; // go to 1000
    }
    if (HTTOT <= 0.0) {
        ERRFLAG = 4;
        out.errflag = ERRFLAG;
        return out; // go to 1000
    }


    //auto safe_sub = int start_1based, int end_1based->std::string{
    //    if (start_1based <= 0 || end_1based <= 0 || end_1based < start_1based) return "";
    //    std::size_t start = static_cast<std::size_t>(start_1based - 1);
    //    std::size_t len = static_cast<std::size_t>(end_1based - start_1based + 1);
    //    if (VOLEQ.size() < start + len) return "";
    //    return VOLEQ.substr(start, len);
    //};

    const std::string code_1_3 = VOLEQ.substr(1, 3);
    const std::string code_8_10 = VOLEQ.substr(8, 10);

    // Working variables
    double D2H = (DBHOB * DBHOB) * HTTOT;
    double grsbdt = 0.0; // gross board foot
    double GCUFT = 0.0; // gross cubic (to some top)
    double TCUFT = 0.0; // total cubic

    // Some temporary variables used in specific equations
    double TOPWOOD = 0.0; // sawlog portion separation (topwood)
    double CV6 = 0.0; // cubic at top=6 for sawlog portion
    double SPF = 0.0; // sawlog portion factor
    double DIB = 0.0, X1 = 0.0, X2 = 0.0, X3 = 0.0;

    // -----------------------------
    // Species/equation selection
    // -----------------------------

    // ASPEN - RM232 Total Cubic, TOP=6 board foot, TOP=4 cubic
    if (code_8_10 == "746" && code_1_3 == "200") {
        // Total cubic
        if (D2H <= 12470.0) TCUFT = 0.002219 * D2H;
        else                TCUFT = 0.001896 * D2H + 4.0267;

        // Board foot TOP=6 (if DBHOB > 7)
        if (DBHOB > 7.0) {
            if (D2H <= 2500.0)        grsbdt = 8.0;
            else if (D2H <= 8850.0)   grsbdt = 0.011389 * D2H - 20.5112;
            else                      grsbdt = 0.010344 * D2H - 11.2615;

            // International board foot (VOL(10) -> C++ vol[9])
            if (D2H <= 11460.0) VOL[9] = 0.013472 * D2H - 25.5968;
            else                 VOL[9] = 0.011989 * D2H - 8.6015;
        }

        // Cubic TOP=4
        if (D2H <= 11800.0) GCUFT = 0.002195 * D2H - 0.9076;
        else                GCUFT = 0.001837 * D2H + 3.3075;

        // Sawlog portion cubic (YW 2018/09/05)
        if (PROD == 1 && MTOPP >= 6.0) {
            if (DBHOB < 11.0) SPF = 0.0;
            else if (DBHOB > 42.0) SPF = 0.96;
            else SPF = 0.92 - (0.22 * std::pow((42.0 - DBHOB) / 33.0, 1.4));

            CV6 = SPF * GCUFT;
            if (CV6 > 0.0) {
                TOPWOOD = GCUFT - CV6;
                VOL[6] = TOPWOOD; // VOL(7)
                GCUFT = CV6;
            }
        }
    }
    // ASPEN - Peterson (201) volume to 4" top DIB
    else if (code_8_10 == "746" && code_1_3 == "201") {
        DIB = (0.8954 * DBHOB) + 0.3168;
        X1 = std::log10(std::max(DIB - 4.0, 0.000001)); // guard small
        X2 = std::log10(std::max(HTTOT - 4.5, 0.000001));
        X3 = X1 * X2;
        GCUFT = std::pow(10.0, (0.0827 * X1) + (0.4045 * X2) + (0.6593 * X3) - 0.4721) + 0.3;
    }
    // ASPEN - RM232 total (210)
    else if (code_8_10 == "746" && code_1_3 == "210") {
        if (D2H <= 12470.0) GCUFT = 0.002219 * D2H;
        else                GCUFT = 0.001896 * D2H + 4.0267;
    }

    // LODGEPOLE PINE - RM6 (200)
    else if (code_8_10 == "108" && code_1_3 == "200") {
        // Total cubic
        if (D2H <= 7000.0) TCUFT = 0.002777 * D2H + 0.027967;
        else               TCUFT = 0.002332 * D2H + 3.446454;

        // Board foot TOP=6 (DBHOB > 7)
        if (DBHOB > 7.0) {
            if (D2H <= 22800.0) grsbdt = 0.01202 * D2H - 6.00933;
            else                 grsbdt = 0.01263 * D2H - 19.76641;

            // Myers (1964) RM-6 TOP=8 (Scribner and International)
            if (MTOPP >= 8.0) {
                if (D2H <= 15000.0) {
                    grsbdt = 0.012535 * D2H - 20.19057;
                    VOL[9] = 0.015097 * D2H - 26.28782;
                }
                else {
                    grsbdt = 0.012893 * D2H - 29.24566;
                    VOL[9] = 0.013766 * D2H - 6.30999;
                }
            }
        }

        // Cubic TOP=4
        if (D2H <= 7000.0) GCUFT = 0.002798 * D2H - 1.04578;
        else               GCUFT = 0.002256 * D2H + 2.836222;

        // Sawlog portion cubic
        if (PROD == 1 && MTOPP >= 6.0) {
            if (DBHOB < 9.0)  SPF = 0.0;
            else if (DBHOB > 42.0) SPF = 0.95;
            else SPF = 0.98 - (0.10 * std::pow((42.0 - DBHOB) / 33.0, 4.0));
            CV6 = SPF * GCUFT;
            if (CV6 > 0.0) {
                TOPWOOD = GCUFT - CV6;
                VOL[6] = TOPWOOD;
                GCUFT = CV6;
            }
        }
    }
    // LODGEPOLE PINE - RM6 total (210)
    else if (code_8_10 == "108" && code_1_3 == "210") {
        if (D2H <= 7000.0) GCUFT = 0.002777 * D2H + 0.027967;
        else               GCUFT = 0.002332 * D2H + 3.446454;
    }

    // PONDEROSA PINE - Prosser Black Hills (203)
    else if (code_8_10 == "122" && code_1_3 == "203") {
        // Total cubic
        if (D2H < 6000.0) TCUFT = 0.0024506 * D2H + 0.3470564;
        else              TCUFT = 0.0022325 * D2H + 3.2829984;

        // Board foot TOP=6
        if (DBHOB > 7.0) {
            if (D2H < 16000.0) grsbdt = 0.0132429 * D2H - 26.0553842;
            else               grsbdt = 0.0127305 * D2H - 15.5653183;
        }

        // Cubic TOP=4 (Myers)
        if (D2H <= 6700.0) GCUFT = 0.002297 * D2H - 1.032297;
        else               GCUFT = 0.002407 * D2H - 2.257724;
    }
    // PONDEROSA PINE - Myers (213) total
    else if (code_8_10 == "122" && code_1_3 == "213") {
        if (D2H <= 6000.0) GCUFT = 0.002213 * D2H + 0.030288;
        else               GCUFT = 0.002474 * D2H - 1.557103;
    }

    // PONDEROSA PINE - Front Range RM218 (200)
    else if (code_8_10 == "122" && code_1_3 == "200") {
        // Total cubic
        TCUFT = 0.00226 * D2H;

        // Board foot TOP=6 (DBHOB > 7)
        if (DBHOB > 7.0) {
            if (D2H <= 2830.0) grsbdt = 8.0;
            else               grsbdt = 0.01149 * D2H - 24.5404;
            // International board foot (VOL(10))
            if (D2H <= 2535.0) VOL[9] = 9.0;
            else                VOL[9] = 0.01286 * D2H - 23.5932;
        }

        // Cubic TOP=4
        GCUFT = 0.00216 * D2H - 0.44670;

        // Sawlog portion cubic
        if (PROD == 1 && MTOPP >= 6.0) {
            if (DBHOB < 9.0)  SPF = 0.0;
            else if (DBHOB > 42.0) SPF = 0.93;
            else SPF = 0.95 - (0.26 * std::pow((42.0 - DBHOB) / 33.0, 7.0));
            CV6 = SPF * GCUFT;
            if (CV6 > 0.0) {
                TOPWOOD = GCUFT - CV6;
                VOL[6] = TOPWOOD;
                GCUFT = CV6;
            }
        }
    }
    // PONDEROSA PINE - Front Range total (210)
    else if (code_8_10 == "122" && code_1_3 == "210") {
        GCUFT = 0.00226 * D2H;
    }

    // ENGELMANN SPRUCE - RM95 (200)
    else if (code_8_10 == "093" && code_1_3 == "200") {
        // Total cubic
        if (D2H <= 22500.0) TCUFT = 0.00239 * D2H + 0.06439;
        else                TCUFT = 0.00193 * D2H + 10.41663;

        // Board foot TOP=6 (DBHOB > 7)
        if (DBHOB > 7.0) {
            if (D2H <= 12200.0) grsbdt = 0.01097 * D2H - 15.14466;
            else                grsbdt = 0.01202 * D2H - 27.91343;
            // International board foot
            if (D2H <= 27300.0) VOL[9] = 0.01391 * D2H - 25.61022;
            else                 VOL[9] = 0.01235 * D2H + 17.02079;
        }

        // Cubic TOP=4
        if (D2H <= 27900.0) GCUFT = 0.00232 * D2H - 0.83010;
        else                GCUFT = 0.00182 * D2H + 13.11320;

        // Sawlog portion cubic
        if (PROD == 1 && MTOPP >= 6.0) {
            if (DBHOB < 9.0)  SPF = 0.0;
            else if (DBHOB > 42.0) SPF = 0.98;
            else SPF = 0.96 - (0.15 * std::pow((42.0 - DBHOB) / 33.0, 3.0));
            CV6 = SPF * GCUFT;
            if (CV6 > 0.0) {
                TOPWOOD = GCUFT - CV6;
                VOL[6] = TOPWOOD;
                GCUFT = CV6;
            }
        }
    }
    // ENGELMANN SPRUCE - RM95 total (210)
    else if (code_8_10 == "093" && code_1_3 == "210") {
        if (D2H <= 22500.0) GCUFT = 0.00239 * D2H + 0.06439;
        else                GCUFT = 0.00193 * D2H + 10.41663;
    }

    // PONDEROSA PINE - Myers RM8 total (212)
    else if (code_8_10 == "122" && code_1_3 == "212") {
        if (D2H <= 6000.0) TCUFT = 0.002213 * D2H + 0.030288;
        else               TCUFT = 0.002474 * D2H - 1.557103;

        // Board foot TOP=8 (DBHOB > 7)
        if (DBHOB > 7.0) {
            if (D2H <= 16000.0) grsbdt = 0.012331 * D2H - 34.167170;
            else                grsbdt = 0.016318 * D2H - 99.212720;
            // International BF
            if (D2H <= 13000.0) VOL[9] = 0.015011 * D2H - 44.36046;
            else                 VOL[9] = 0.016991 * D2H - 68.7502;
        }

        // Cubic TOP=4
        if (D2H <= 6700.0) GCUFT = 0.002297 * D2H - 1.032297;
        else               GCUFT = 0.002407 * D2H - 2.257724;

        // Sawlog portion cubic
        if (PROD == 1 && MTOPP >= 6.0) {
            if (DBHOB < 9.0)  SPF = 0.0;
            else if (DBHOB > 42.0) SPF = 0.93;
            else SPF = 0.95 - (0.26 * std::pow((42.0 - DBHOB) / 33.0, 7.0));
            CV6 = SPF * GCUFT;
            if (CV6 > 0.0) {
                TOPWOOD = GCUFT - CV6;
                VOL[6] = TOPWOOD;
                GCUFT = CV6;
            }
        }
    }

    // Oneseed Juniper - INT-339
    else if (code_8_10 == "069") {
        if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
        TCUFT = std::pow(-0.19321 + 0.136101 * std::cbrt(D2H) + 0.038187 * MSTEM, 3.0);
        GCUFT = TCUFT;
    }

    // Rocky Mountain Juniper - INT-339
    else if (code_8_10 == "066") {
        if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
        TCUFT = std::pow(0.02434 + 0.119106 * std::cbrt(D2H), 3.0);
        GCUFT = TCUFT;
    }

    // Utah Juniper - INT-339
    else if (code_8_10 == "065") {
        if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
        TCUFT = std::pow(-0.08728 + 0.135420 * std::cbrt(D2H) - 0.019587 * MSTEM, 3.0);
        GCUFT = TCUFT;
    }

    // Gambel Oak - INT-339
    else if (code_8_10 == "814") {
        if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
        TCUFT = std::pow(-0.13600 + 0.145743 * std::cbrt(D2H), 3.0);
        GCUFT = TCUFT;
    }

    // Bur Oak - INT-339
    else if (code_8_10 == "823") {
        if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
        TCUFT = std::pow(0.12853 + 0.105885 * std::cbrt(D2H), 3.0);
        GCUFT = TCUFT;
    }

    // Pinyon Pine
    else if (code_8_10 == "106") {
        if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
        TCUFT = std::pow(-0.20296 + 0.150283 * std::cbrt(D2H) + 0.054178 * MSTEM, 3.0);
        GCUFT = TCUFT;
    }

    // Mountain Mahogany - INT-339
    else if (code_8_10 == "475") {
        if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
        TCUFT = std::pow(-0.13363 + 0.128222 * std::cbrt(D2H) + 0.080208 * MSTEM, 3.0);
        // Match FIA behavior for small diameters (2024-04-26 note)
        if (DRC < 3.0 && DBHOB < 3.0) TCUFT = 0.1;
        GCUFT = TCUFT;
    }

    // Other Hardwoods - INT-339
    else if (code_8_10 == "998") {
        if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
        TCUFT = std::pow(-0.13822 + 0.121850 * std::cbrt(D2H), 3.0);
        GCUFT = TCUFT;
    }

    // Unknown equation code
    else {
        ERRFLAG = 1;
    }

    // Final clipping and assignments (match FORTRAN behavior)
    if (TCUFT < 0.0) TCUFT = 0.0;
    VOL[0] = TCUFT;  // VOL(1)

    if (grsbdt < 0.0) grsbdt = 0.0;
    VOL[1] = grsbdt; // VOL(2)

    if (GCUFT < 0.0) GCUFT = 0.0;
    VOL[3] = GCUFT;  // VOL(4)

    // VOL(7) (topwood) and VOL(10) (Intl BF) are written earlier as needed.
    out.errflag = ERRFLAG;
    out.totalCubicFoot = VOL[0];
    out.grossBoardFootPrimary = VOL[1];
    out.grossCubicFootPrimary = VOL[3];
    out.grossCubicFootSecondary = VOL[6];
    out.grossInternationalBoardFoot = VOL[9];
    return out;
}

//R3--------------------------------------------
// Safe substring helper: FORTRAN (start..end) 1-based to C++ substr
static inline std::string safe_substr_1based(const std::string& s, int start_1based, int end_1based) {
    if (start_1based <= 0 || end_1based <= 0 || end_1based < start_1based) return "";
    std::size_t start = static_cast<std::size_t>(start_1based - 1);
    std::size_t len = static_cast<std::size_t>(end_1based - start_1based + 1);
    if (start > s.size()) return "";
    if (len > s.size() - start) return "";
    return s.substr(start, len);
}

// Hann & Bare equation subroutine for PP (young vs old growth)
TreeOutput HANN_PP(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;
    double DBHOB = tree.dbh;
    double HTTOT = tree.totalHeight;
    int PROD = vco.primaryProduct;
    double MTOPP = merchRules.minTopDibSaw;
    if (tree.minTopDibSawOverride > 0.0) MTOPP = tree.minTopDibSawOverride;
    if (PROD != 1) {
        MTOPP = merchRules.minTopDibNonSaw;
        if (tree.minTopDibNonSawOverride > 0.0) MTOPP = tree.minTopDibNonSawOverride;
    }
    std::array<double, 15> VOL;
    VOL.fill(0.0);
    int ERRFLAG = 0;

    // Validate species code for PP
    if (safe_substr_1based(VOLEQ, 8, 10) != "122") {
        ERRFLAG = 1;
        out.errflag = ERRFLAG;
        return out;
    }
    if (DBHOB < 1.0) {
        ERRFLAG = 3;
        out.errflag = ERRFLAG;
        return out;
    }
    if (HTTOT < 4.5) {
        ERRFLAG = 4;
        out.errflag = ERRFLAG;
        return out;
    }

    const double D2H = DBHOB * DBHOB * HTTOT;

    double ENTIRE = 0.0;
    double GCUFT6 = 0.0;
    double GCUFT4 = 0.0;
    double INTBDFT = 0.0;
    double SCBDFT = 0.0;
    double UM4 = 0.0;
    double UM6 = 0.0;
    double TWVOL = 0.0;

    // Top diameter handling (valid range [3,8])
    double TOP = MTOPP;
    if (TOP == 0.0) TOP = 4.0;
    TOP = std::clamp(TOP, 3.0, 8.0);

    const std::string code23 = safe_substr_1based(VOLEQ, 2, 3);  // forest group code
    const char young_old = safe_substr_1based(VOLEQ, 7, 7).empty() ? '\0'
        : safe_substr_1based(VOLEQ, 7, 7)[0]; // '0' blackjack (young), '1' yellow (old)

    if (young_old == '0') {
        // Blackjack (young-growth)
        if (code23 == "01") {
            ENTIRE = 0.0810724804 + 0.00198351037 * D2H;
            UM6 = -0.125349396 + 0.00360421889 * ((std::pow(6.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
                + 0.00540634204 * std::pow(DBHOB, 2);
            GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;
            UM4 = -0.125349396 + 0.00360421889 * ((std::pow(TOP, 3) * HTTOT) / std::pow(DBHOB, 1.5))
                + 0.00540634204 * std::pow(DBHOB, 2);
            GCUFT4 = ENTIRE - UM4;
            TWVOL = UM6 - UM4;
            INTBDFT = GCUFT6 * (6.84751736 - (7.69491322 * std::pow(DBHOB, -1))
                - (221.377226 * std::pow(DBHOB, -2)));
            SCBDFT = INTBDFT * (0.96579222 - 0.40579028 * std::pow(DBHOB, -1)
                - 16.93678414 * std::pow(DBHOB, -2));
        }
        else if (code23 == "02") {
            ENTIRE = 0.0483082948 + 0.00204968419 * D2H;
            UM6 = -0.133967845 + 0.00650174839 * ((std::pow(6.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
                + 0.00490223789 * std::pow(DBHOB, 2);
            GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;
            UM4 = -0.133967845 + 0.00650174839 * ((std::pow(TOP, 3) * HTTOT) / std::pow(DBHOB, 1.5))
                + 0.00490223789 * std::pow(DBHOB, 2);
            GCUFT4 = ENTIRE - UM4;
            TWVOL = UM6 - UM4;
            INTBDFT = GCUFT6 * (7.58122078 - (8.51941410 * std::pow(DBHOB, -1))
                - (245.097535 * std::pow(DBHOB, -2)));
            SCBDFT = INTBDFT * (0.993986685 - 1.463486622 * std::pow(DBHOB, -1)
                - 12.40584877 * std::pow(DBHOB, -2));
        }
        else {
            ERRFLAG = 1;
            out.errflag = 1;
            return out;
        }
    }
    else if (young_old == '1') {
        // Yellow pine (old-growth)
        ENTIRE = 0.237204154 + 0.00221122919 * D2H;
        UM6 = 0.0185465259 + 0.000788175798 * ((std::pow(6.0, 3) * HTTOT) / std::pow(DBHOB, 1.0))
            + 0.00505513624 * std::pow(DBHOB, 2);
        GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;
        UM4 = 0.0185465259 + 0.000788175798 * ((std::pow(TOP, 3) * HTTOT) / std::pow(DBHOB, 1.0))
            + 0.00505513624 * std::pow(DBHOB, 2);
        GCUFT4 = ENTIRE - UM4;
        TWVOL = UM6 - UM4;
        INTBDFT = GCUFT6 * (7.10051404 - (7.97921881 * std::pow(DBHOB, -1))
            - (229.556497 * std::pow(DBHOB, -2)));
        SCBDFT = INTBDFT * (0.982101210 - 0.926027395 * std::pow(DBHOB, -1)
            - 14.49443523 * std::pow(DBHOB, -2));
    }
    else {
        ERRFLAG = 1;
        out.errflag = ERRFLAG;
        return out;
    }

    // Populate VOL (FORTRAN mapping)
    VOL[0] = ENTIRE;
    if (PROD == 1) {
        VOL[1] = SCBDFT;
        VOL[9] = INTBDFT;
        VOL[3] = GCUFT6;
        VOL[6] = TWVOL;
    }
    else {
        VOL[3] = GCUFT4;
        VOL[5] = VOL[3] / 79.0; // cords
    }
    VOL[14] = UM4;

    // Clamp negatives to zero
    for (int i : {0, 1, 3, 5, 6, 9, 14}) if (VOL[i] < 0.0) VOL[i] = 0.0;
    out.totalCubicFoot = VOL[0];
    out.grossBoardFootPrimary = VOL[1];
    out.grossCubicFootPrimary = VOL[3];
    out.grossCubicFootSecondary = VOL[6];
    out.grossInternationalBoardFoot = VOL[9];
    out.cordMerchantable = VOL[5];
    out.tipCubicFoot = VOL[14];
    return out;
}

// Main subroutine: Region 3 D*D*H volume equations
TreeOutput R3D2HV(const std::string& VOLEQU, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;
    double DBHOB = tree.dbh;
    double HTTOT = tree.totalHeight;
    //int PROD = vco.primaryProduct;
    int UNT = vco.primaryProduct;
    double MTOPP = merchRules.minTopDibSaw;
    if (tree.minTopDibSawOverride > 0.0) MTOPP = tree.minTopDibSawOverride;
    if (UNT != 1) {
        MTOPP = merchRules.minTopDibNonSaw;
        if (tree.minTopDibNonSawOverride > 0.0) MTOPP = tree.minTopDibNonSawOverride;
    }
    double DRC = tree.drc;
    double HTTFLL = tree.heightToFirstLiveLimb;
    double HT1PRD = tree.merchHeightSaw;
    int FCLASS = tree.stems;

    std::array<double, 15> VOL;
    VOL.fill(0.0);
    int ERRFLAG = 0;

    // If DBHOB and DRC both invalid/small, error 3
    if (DBHOB < 1.0 && DRC < 1.0) {
        ERRFLAG = 3;
        out.errflag = 3;
        return out;
    }

    // Height fallback if HTTOT <= 0 and HT1PRD > 0
    if (HTTOT <= 0.0 && HT1PRD > 0.0) {
        HTTOT = 12.211 + 1.1342 * HT1PRD;
    }
    // The HTTOT<4.5 error was commented out in FORTRAN to match FIA; keep disabled.

    double D2H = DBHOB * DBHOB * HTTOT;
    double ENTIRE = 0.0;
    double GCUFT6 = 0.0;
    double GCUFT4 = 0.0;
    double INTBDFT = 0.0;
    double SCBDFT = 0.0;
    double UM4 = 0.0;
    double UM6 = 0.0;
    double TWVOL = 0.0;
    double TOPD = 4.0;

    std::string PROD = (UNT == 1) ? "01" : "02";
    const std::string code13 = safe_substr_1based(VOLEQU, 1, 3);
    const std::string code810 = safe_substr_1based(VOLEQU, 8, 10);

    // 300DVEW122 – Ponderosa/Arizona/Apache (Eager Mill)
    if (code810 == "122" && code13 == "300") {
        if (D2H <= 31629.91964) {
            SCBDFT = -1.786 + 0.00098814 * D2H;
        }
        else {
            if (HTTFLL <= 0.0) HTTFLL = 25.0;
            SCBDFT = -52.897 + (0.12826 * HTTFLL) + (0.0017678 * D2H)
                + (879120.0 / D2H);
        }
        SCBDFT *= 10.0;

        // Eager cubic to 6"
        if (D2H <= 33590.92207) GCUFT6 = -1.7751 + 0.0018897 * D2H;
        else                    GCUFT6 = -13.542 + 0.00224 * D2H;

        // Hann & Bare cubic (entire, UM4/UM6, topwood between 4 and 6)
        ENTIRE = 0.081072 + (0.001984 * D2H);
        UM4 = -0.125349 + 0.003604 * ((std::pow(4.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.005406 * std::pow(DBHOB, 2);
        GCUFT4 = ENTIRE - UM4;
        UM6 = -0.125349 + 0.003604 * ((std::pow(6.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.005406 * std::pow(DBHOB, 2);
        TWVOL = UM6 - UM4;
    }
    // 301DVEW122 – PP young-growth (<21" use 301HAB0122; else 300HAB1122)
    else if (code810 == "122" && code13 == "301") {
        std::string VOLEQTMP = (DBHOB < 21.0) ? "301HAB0122" : "300HAB1122";
        out = HANN_PP(VOLEQTMP, vco, tree, merchRules);
        return out;
    }
    // 302DVEW122 – PP young-growth (Carson/Santa Fe)
    else if (code810 == "122" && code13 == "302") {
        std::string VOLEQTMP = (DBHOB < 21.0) ? "302HAB0122" : "300HAB1122";
        out = HANN_PP(VOLEQTMP, vco, tree, merchRules);
        return out; 
    }
    // 301DVEW202 – Douglas-fir (Lincoln/Coconino/Tonto)
    else if (code810 == "202" && code13 == "301") {
        ENTIRE = 0.438374 + 0.001756 * D2H;
        UM6 = -0.083149 + 0.001219 * ((std::pow(6.0, 3) * HTTOT) / DBHOB)
            + 0.005417 * std::pow(DBHOB, 2);
        GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;

        INTBDFT = GCUFT6 * (6.587353 - (0.892716 * std::pow(DBHOB, -1))
            - (243.514909 * std::pow(DBHOB, -2)));
        SCBDFT = INTBDFT * (1.000897 - (4.100072 * std::pow(DBHOB, -1.177748)));

        UM4 = -0.083149 + 0.001219 * ((std::pow(4.0, 3) * HTTOT) / DBHOB)
            + 0.005417 * std::pow(DBHOB, 2);
        GCUFT4 = ENTIRE - UM4;
        TWVOL = UM6 - UM4;

        // Adjustment per FY87 validation
        SCBDFT *= 0.932;
    }
    // 302DVEW202 – Douglas-fir (Carson/Santa Fe)
    else if (code810 == "202" && code13 == "302") {
        ENTIRE = 0.341133 + 0.001918 * D2H;
        UM6 = -0.187631 + 0.006719 * ((std::pow(6.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.005364 * std::pow(DBHOB, 2);
        GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;

        INTBDFT = GCUFT6 * (6.597174 - (0.894047 * std::pow(DBHOB, -1))
            - (243.877967 * std::pow(DBHOB, -2)));
        SCBDFT = INTBDFT * (0.870260 - (19.495942 * std::pow(DBHOB, -2)));

        UM4 = -0.187631 + 0.006719 * ((std::pow(4.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.005364 * std::pow(DBHOB, 2);
        GCUFT4 = ENTIRE - UM4;
        TWVOL = UM6 - UM4;
    }
    // 301DVEW015 – White fir (Lincoln/Coconino/Tonto)
    else if (code810 == "015" && code13 == "301") {
        ENTIRE = 0.210904 + 0.001840 * D2H;
        UM6 = -0.182700 + 0.001248 * ((std::pow(6.0, 3) * HTTOT) / DBHOB)
            + 0.006245 * std::pow(DBHOB, 2);
        GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;

        INTBDFT = GCUFT6 * (6.246875 - (7.019940 * std::pow(DBHOB, -1))
            - (201.958728 * std::pow(DBHOB, -2)));
        SCBDFT = INTBDFT * (1.0 - (1.888144 * std::pow(DBHOB, -1))
            - (8.851449 * std::pow(DBHOB, -2)));

        UM4 = -0.182700 + 0.001248 * ((std::pow(4.0, 3) * HTTOT) / std::pow(DBHOB, 1.0))
            + 0.006245 * std::pow(DBHOB, 2);
        GCUFT4 = ENTIRE - UM4;
        TWVOL = UM6 - UM4;
    }
    // 302DVEW015 – White fir (Carson/Santa Fe)
    else if (code810 == "015" && code13 == "302") {
        ENTIRE = 0.157777 + 0.002009 * D2H;
        UM6 = -0.187563 + 0.006326 * ((std::pow(6.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.006041 * std::pow(DBHOB, 2);
        GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;

        INTBDFT = GCUFT6 * (5.736445 - (1.720934 * std::pow(DBHOB, -1))
            - (74.573790 * std::pow(DBHOB, -2)));
        SCBDFT = INTBDFT * (1.017248 - (1.870568 * std::pow(DBHOB, -1))
            - (8.514451 * std::pow(DBHOB, -2)));

        UM4 = -0.187563 + 0.006326 * ((std::pow(4.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.006041 * std::pow(DBHOB, 2);
        GCUFT4 = ENTIRE - UM4;
        TWVOL = UM6 - UM4;
    }
    // 300DVEW093 – Spruces (corkbark/subalpine/engelmann/blue)
    else if (code810 == "093") {
        ENTIRE = 0.225466 + 0.002170 * D2H;
        UM6 = -0.2664752 + 0.006129 * ((std::pow(6.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.007431 * std::pow(DBHOB, 2);
        GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;

        INTBDFT = GCUFT6 * (5.987363 - (9.847918 * std::pow(DBHOB, -1))
            - (-300.812808 * std::pow(DBHOB, -2))
            - (2855.342454 * std::pow(DBHOB, -3)));
        SCBDFT = INTBDFT * (0.878454 - (15.998458 * std::pow(DBHOB, -2)));

        UM4 = -0.2664752 + 0.006129 * ((std::pow(4.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.007431 * std::pow(DBHOB, 2);
        GCUFT4 = ENTIRE - UM4;
        TWVOL = UM6 - UM4;
    }
    // 300DVEW113 – SW white pine / bristlecone / limber / foxtail
    else if (code810 == "113") {
        ENTIRE = 0.160889 + 0.002032 * D2H;
        UM6 = -0.213005 + 0.004912 * ((std::pow(6.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.006061 * std::pow(DBHOB, 2);
        GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;

        INTBDFT = GCUFT6 * (6.691967 - (7.520114 * std::pow(DBHOB, -1))
            - (216.348366 * std::pow(DBHOB, -2)));
        SCBDFT = INTBDFT * (1.006086 - (2.384660 * std::pow(DBHOB, -1)));

        UM4 = -0.213005 + 0.004912 * ((std::pow(4.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.006061 * std::pow(DBHOB, 2);
        GCUFT4 = ENTIRE - UM4;
        TWVOL = UM6 - UM4;
    }
    // 300DVEW746 – Aspen
    else if (code810 == "746") {
        ENTIRE = 0.0327 + 0.002311 * D2H;
        UM6 = -0.236432 + 0.005802 * ((std::pow(6.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.006080 * std::pow(DBHOB, 2);
        GCUFT6 = ENTIRE - UM6; if (GCUFT6 < 0.0) GCUFT6 = 0.0;

        INTBDFT = GCUFT6 * (6.688085 - (-1.276851 * std::pow(DBHOB, -1))
            - (-4.504804 * std::pow(DBHOB, -2))
            - (1423.985244 * std::pow(DBHOB, -3)));
        SCBDFT = INTBDFT * (0.887891 - 17.19374 * std::pow(DBHOB, -2));

        UM4 = -0.236432 + 0.005802 * ((std::pow(4.0, 3) * HTTOT) / std::pow(DBHOB, 1.5))
            + 0.006080 * std::pow(DBHOB, 2);
        GCUFT4 = ENTIRE - UM4;
        TWVOL = UM6 - UM4;
    }
    // 300DVEW060 – Junipers/cypress/misc softwoods (pulpwood only)
    else if (code810 == "060") {
        if (DBHOB > 3.0 || DRC > 3.0) {
            if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
            double D2HA = D2H / 1000.0;
            const std::string code23 = safe_substr_1based(VOLEQU, 2, 3);
            if (code23 == "01") {
                // INT-363
                if (D2HA <= 5.0) GCUFT4 = -0.05 + 2.48 * D2HA + 0.057 * D2HA * D2HA;
                else             GCUFT4 = 4.24 + 2.48 * D2HA - 14.29 / D2HA;
            }
            else if (code23 == "02") {
                // INT-379
                if (FCLASS != 1) {
                    if (D2HA <= 5.0) GCUFT4 = -0.169 + 1.9246 * D2HA + 0.053 * D2HA * D2HA;
                    else             GCUFT4 = 3.805 + 1.9246 * D2HA - 13.249 / D2HA;
                }
                else {
                    if (D2HA <= 5.0) GCUFT4 = -0.002 + 1.7385 * D2HA + 0.181 * D2HA * D2HA;
                    else             GCUFT4 = 13.572 + 1.7385 * D2HA - 45.246 / D2HA;
                }
            }
            else {
                // INT-391
                if (FCLASS != 1) {
                    if (D2HA <= 6.0) GCUFT4 = -0.129 + 2.0255 * D2HA + 0.1011 * D2HA * D2HA;
                    else             GCUFT4 = 10.786 + 2.0255 * D2HA - 43.663 / D2HA;
                }
                else {
                    if (D2HA <= 6.0) GCUFT4 = -0.032 + 2.1076 * D2HA + 0.1454 * D2HA * D2HA;
                    else             GCUFT4 = 15.675 + 2.1076 * D2HA - 62.827 / D2HA;
                }
            }
        }
        else {
            GCUFT4 = 0.0;
        }
        ENTIRE = GCUFT4;
        if (UNT == 1) GCUFT6 = GCUFT4;
    }
    // 300DVEW106 – Pinyon pines (pulpwood only)
    else if (code810 == "106") {
        if (DBHOB > 3.0 || DRC > 3.0) {
            if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
            double D2HA = D2H / 1000.0;
            const std::string code23 = safe_substr_1based(VOLEQU, 2, 3);
            if (code23 == "01") {
                // INT-363
                if (D2HA <= 5.0) GCUFT4 = -0.07 + 2.51 * D2HA + 0.098 * D2HA * D2HA;
                else             GCUFT4 = 7.29 + 2.51 * D2HA - 24.53 / D2HA;
            }
            else if (code23 == "02") {
                // INT-379 (TCVOL equals GCUFT4 here)
                double TCVOL = 0.0;
                if (D2HA <= 5.0) TCVOL = -0.073 + 2.1608 * D2HA + 0.0804 * D2HA * D2HA;
                else             TCVOL = 5.961 + 2.1608 * D2HA - 20.112 / D2HA;
                GCUFT4 = TCVOL;
            }
            else {
                // INT-391
                if (D2HA <= 3.0) GCUFT4 = -0.060 + 2.5139 * D2HA + 0.1466 * D2HA * D2HA;
                else             GCUFT4 = 3.898 + 2.5139 * D2HA - 7.917 / D2HA;
            }
        }
        else {
            GCUFT4 = 0.0;
        }
        ENTIRE = GCUFT4;
        if (UNT == 1) GCUFT6 = GCUFT4;
    }
    // 300DVEW310 – Misc hardwoods/maple (pulpwood only)
    else if (code810 == "310") {
        if (DBHOB > 3.0 || DRC > 3.0) {
            if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
            double B4 = (FCLASS != 1) ? 0.0 : 1.0;
            double B1 = -0.29013;
            double B2 = 0.126114;
            double B3 = 0.14489;
            GCUFT4 = std::pow(B1 + B2 * std::cbrt(D2H) + B3 * B4, 3.0);
        }
        else {
            GCUFT4 = 0.0;
        }
        ENTIRE = GCUFT4;
        if (UNT == 1) GCUFT6 = GCUFT4;
    }
    // 300DVEW800 – Oaks (pulpwood only)
    else if (code810 == "800") {
        if (DBHOB > 3.0 || DRC > 3.0) {
            if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
            double D2HA = D2H / 1000.0;
            const std::string code23 = safe_substr_1based(VOLEQU, 2, 3);
            if (code23 == "01") {
                // INT-379
                if (FCLASS != 1) {
                    if (D2HA <= 5.0) GCUFT4 = -0.181 + 2.1917 * D2HA + 0.0208 * D2HA * D2HA;
                    else             GCUFT4 = -1.742 + 2.1917 * D2HA + 5.205 / D2HA;
                }
                else {
                    if (D2HA <= 5.0) GCUFT4 = -0.02 + 2.1896 * D2HA + 0.127 * D2HA * D2HA;
                    else             GCUFT4 = 9.502 + 2.1896 * D2HA - 31.74 / D2HA;
                }
            }
            else {
                // INT-391
                if (FCLASS != 1) {
                    if (D2HA <= 4.0) GCUFT4 = -0.028 + 1.9545 * D2HA + 0.1400 * D2HA * D2HA;
                    else             GCUFT4 = 6.691 + 1.9545 * D2HA - 17.918 / D2HA;
                }
                else {
                    if (D2HA <= 4.0) GCUFT4 = -0.068 + 2.4048 * D2HA + 0.1383 * D2HA * D2HA;
                    else             GCUFT4 = 6.571 + 2.4048 * D2HA - 17.704 / D2HA;
                }
            }
        }
        else {
            // Match FIA per (2024-04-26) note
            GCUFT4 = 0.1;
        }
        ENTIRE = GCUFT4;
        if (UNT == 1) GCUFT6 = GCUFT4;
    }
    // 300DVEW999 – Mesquite (pulpwood only)
    else if (code810 == "999") {
        if (DBHOB > 3.0 || DRC > 3.0) {
            if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
            double D2HA = D2H / 1000.0;
            if (FCLASS != 1) {
                if (D2HA <= 2.0) GCUFT4 = 0.020 + 1.8972 * D2HA + 0.5756 * D2HA * D2HA;
                else             GCUFT4 = 6.927 + 1.8972 * D2HA - 9.210 / D2HA;
            }
            else {
                if (D2HA <= 2.0) GCUFT4 = -0.043 + 2.3378 * D2HA + 0.8024 * D2HA * D2HA;
                else             GCUFT4 = 9.586 + 2.3378 * D2HA - 12.839 / D2HA;
            }
        }
        else {
            GCUFT4 = 0.0;
        }
        ENTIRE = GCUFT4;
        if (UNT == 1) GCUFT6 = GCUFT4;
    }
    // 300DVEW314 – Hackberry/alderleaf/mtn mahogany (pulpwood only)
    else if (code810 == "314") {
        if (DBHOB > 3.0 || DRC > 3.0) {
            if (DRC > 0.0) D2H = DRC * DRC * HTTOT;
            double B4 = (FCLASS != 1) ? 0.0 : 1.0;
            double B1 = -0.29013;
            double B2 = 0.126114;
            double B3 = 0.14489;
            GCUFT4 = std::pow(B1 + B2 * std::cbrt(D2H) + B3 * B4, 3.0);
        }
        else {
            GCUFT4 = 0.0;
        }
        ENTIRE = GCUFT4;
        if (UNT == 1) GCUFT6 = GCUFT4;
    }
    // Smalian’s formula (fallback, pulpwood only)
    else {
        double B1 = std::pow(DBHOB, 2) * 0.005454;
        double B2 = std::pow(4.0, 2) * 0.005454;
        GCUFT4 = ((B1 + B2) / 2.0) * HT1PRD;
        ENTIRE = GCUFT4;
        if (UNT == 1) GCUFT6 = GCUFT4;
    }

    // DBH < 9 has no board foot volumes
    if (DBHOB < 9.0) {
        SCBDFT = 0.0;
        INTBDFT = 0.0;
    }

    // Write outputs (FORTRAN indices)
    VOL[0] = ENTIRE;
    if (UNT == 1) {
        VOL[1] = SCBDFT;
        VOL[9] = INTBDFT;
        VOL[3] = GCUFT6;
        VOL[6] = TWVOL;
    }
    else if (UNT == 3) {
        VOL[3] = GCUFT4;
        VOL[5] = VOL[3] / 79.0; // cords
    }
    else {
        // For other UNT values, keep ENTIRE only (matching source behavior)
        VOL[3] = GCUFT4;
    }
    VOL[14] = UM4;

    // Clamp negative volumes
    for (int i : {0, 1, 3, 5, 6, 9, 14}) if (VOL[i] < 0.0) VOL[i] = 0.0;
    out.errflag = ERRFLAG;
    out.totalCubicFoot = VOL[0];
    out.grossBoardFootPrimary = VOL[1];
    out.grossCubicFootPrimary = VOL[3];
    out.grossCubicFootSecondary = VOL[6];
    out.cordMerchantable = VOL[5];
    out.grossInternationalBoardFoot = VOL[9];
    out.tipCubicFoot = VOL[14];
    return out;
}

//R4------------------------------------------------------

// Safe substring helper: FORTRAN (start..end) 1-based to C++ substr
//static inline std::string safe_substr_1based(const std::string& s, int start_1based, int end_1based) {
//    if (start_1based <= 0 || end_1based <= 0 || end_1based < start_1based) return "";
//    std::size_t start = static_cast<std::size_t>(start_1based - 1);
//    std::size_t len = static_cast<std::size_t>(end_1based - start_1based + 1);
//    if (start > s.size()) return "";
//    if (len > s.size() - start) return "";
//    return s.substr(start, len);
//}

/**
 * R4D2H — Region 4 cubic volume via D^2*H equations (Chojnacky INT-339)
 *
 * @param VOLEQ   Equation code (10 chars); species at positions 8..10; district at 2..3 (for 065)
 * @param HTTOT   Total tree height (ft)
 * @param DBHOB   Diameter at breast height over bark (in)
 * @param DRC     Diameter at root collar (in) — if > 0, used to compute D2H
 * @param FCLASS  Form/stem class (1 = single-stem; others = multi)
 * @param VOL     Output volumes (size 15). VOL[0] and VOL[3] are set = cubic volume
 * @param ERRFLAG Output error flag (3 = invalid diameters; 4 = invalid height; else 0)
 */
TreeOutput R4D2H(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree)
//void R4D2H(const std::string& VOLEQ,
//    double HTTOT,
//    double DBHOB,
//    double DRC,
//    int FCLASS,
//    std::array<double, 15>& VOL,
//    int& ERRFLAG)
{
    TreeOutput out;
    double HTTOT = tree.totalHeight;
    double DBHOB = tree.dbh;
    double DRC = tree.drc;
    //std::array<double, 15> VOL;
    //VOL.fill(0.0);
    int ERRFLAG = 0;

    // Input validation (matches FORTRAN)
    if (DBHOB <= 0.0 && DRC <= 0.0) {
        ERRFLAG = 3;
        out.errflag = ERRFLAG;
        return out;
    }
    if (HTTOT <= 0.0) {
        ERRFLAG = 4;
        out.errflag = ERRFLAG;
        return out;
    }

    // Compute D2H using DRC if provided, else DBHOB
    double D2H = (DRC > 0.0) ? (DRC * DRC * HTTOT) : (DBHOB * DBHOB * HTTOT);

    // FCLASS to MSTEM: 1 = single-stem -> 1; else 0
    int MSTEM = (tree.stems == 1) ? 1 : 0;

    // Species code
    const std::string spc = safe_substr_1based(VOLEQ, 8, 10);
    const std::string dist = safe_substr_1based(VOLEQ, 2, 3); // used for Utah juniper variants

    double cu = 0.0; // cubic volume (VOL(1) and VOL(4))

    // Western Juniper (064)
    if (spc == "064") {
        cu = std::pow(-0.22048 + 0.125468 * std::cbrt(D2H) + 0.100092 * MSTEM, 3.0);
    }
    // Rocky Mountain Juniper (066)
    else if (spc == "066") {
        cu = std::pow(0.02434 + 0.119106 * std::cbrt(D2H), 3.0);
    }
    // Utah Juniper (065) — multiple district variants
    else if (spc == "065") {
        if (dist == "01") {
            // W. Colorado, E. Utah, Wyoming
            cu = std::pow(-0.08728 + 0.135420 * std::cbrt(D2H) - 0.019587 * MSTEM, 3.0);
        }
        else if (dist == "02") {
            // Ely BLM District Nevada
            cu = std::pow(-0.03655 + 0.135689 * std::cbrt(D2H) - 0.018476 * MSTEM, 3.0);
        }
        else if (dist == "03") {
            // Winnemucca and Susanville BLM Districts in Nevada
            cu = std::pow(0.04829 + 0.114358 * std::cbrt(D2H) - 0.045779 * MSTEM, 3.0);
        }
        else {
            // S. Idaho, parts of Nevada, W. Utah (default)
            cu = std::pow(-0.13386 + 0.133726 * std::cbrt(D2H) + 0.036329 * MSTEM, 3.0);
        }
        // Set to 0.1 for tree DIA < 3 to match FIA (2024-04-26)
        if (DBHOB < 3.0 && DRC < 3.0) cu = 0.1;
    }
    // Single-leaf Pinyon Pine (133)
    else if (spc == "133") {
        cu = std::pow(-0.14240 + 0.148190 * std::cbrt(D2H) - 0.016712 * MSTEM, 3.0);
        // Set to 0.1 for small trees to match FIA (2024-04-26)
        if (DBHOB < 3.0 && DRC < 3.0) cu = 0.1;
    }
    // Pinyon Pine (106)
    else if (spc == "106") {
        cu = std::pow(-0.20296 + 0.150283 * std::cbrt(D2H) + 0.054178 * MSTEM, 3.0);
    }
    // Mountain Mahogany (475)
    else if (spc == "475") {
        cu = std::pow(-0.13363 + 0.128222 * std::cbrt(D2H) + 0.080208 * MSTEM, 3.0);
    }
    // Other Hardwoods (998)
    else if (spc == "998") {
        cu = std::pow(-0.13822 + 0.121850 * std::cbrt(D2H), 3.0);
    }
    // If species not matched, FORTRAN leaves VOL as zeros and ERRFLAG unchanged (0).
    // We preserve that behavior.

    // Write output volumes: VOL(1) and VOL(4)
    //VOL[0] = cu;    // VOL(1)
    //VOL[3] = cu;    // VOL(4)
    out.totalCubicFoot = cu;
    out.grossCubicFootPrimary = cu;
    return out;
}