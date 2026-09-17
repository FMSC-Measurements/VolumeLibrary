#include "DirectVolumeCalculator_FIA_PacificCoast.h"
#include "DirectVolumeCalculator_R5.h"
#include "../SmalianScribnerIntl14.h"
#include <cmath>
#include <string>
#include <vector>
#include <array>
#include <algorithm>

// Brackett, Michael 1977.
// Notes on tarif tree volume computation. Resour. Manage. Rep. 24 (DNR Report # 24)  
// Olympia, WA: State of Washington, Department of Natural Resources;  26 p.
TreeOutput DNR24_Tarif_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules,
    double CV,
    const std::string& CVTYPE,
    double CV4_in)
{
    TreeOutput out;

    double DBH = tree.dbh;
    double HT = tree.totalHeight;
    double MTOPP = merchRules.minTopDibSaw;
    double BFMIND = merchRules.minimumBoardFootDiameter;

    // Species number extracted from VOLEQ(8:10)
    int SPN = std::stoi(VOLEQ.substr(7, 3));

    if (MTOPP < 6.0) MTOPP = 6.0;

    if (BFMIND < 0.1) {
        BFMIND = (SPN < 300 ? 9.0 : 11.0);
    }

    double CVTS = 0.0, CVT = 0.0, CV4 = CV4_in, CV8 = 0.0;
    double TARIF = 0.0, SV616 = 0.0, SV632 = 0.0;
    double SV816 = 0.0, IV6 = 0.0, IV8 = 0.0;

    // --- Assign core CV types ------------------------------------------------

    if (CVTYPE == "CVTS") {
        CVTS = CV;
        TARIF = TRF0(CVTS, DBH);
        if(CV4 == 0.0) CV4 = TARIF * (0.005454154 * DBH * DBH - 0.087266) / 0.912733;
        CVT = CVTS * (0.9679 - 0.1051 * pow(0.5523, DBH - 1.5));

    }
    else if (CVTYPE == "CVT") {
        CVT = CV;
        TARIF = TRF1(CVT, DBH);
        CV4 = TARIF * (0.005454154 * DBH * DBH - 0.087266) / 0.912733;

        CVTS = TARIF *
            ((1.033 * (1.0 + 1.382937 * exp(-4.015292 * DBH / 10.0))) *
                (0.005454154 * DBH * DBH + 0.087266) - 0.174533) / 0.912733;

    }
    else if (CVTYPE == "CV4") {
        CV4 = CV;
        TARIF = TRF4(CV4, DBH);

        CVTS = CV4 * ((1.033 * (1.0 + 1.382937 * exp(-4.015292 * DBH / 10.0))) *
            (0.005454 * DBH * DBH + 0.087266) - 0.174533)
            / (0.005454 * DBH * DBH - 0.087266);

        CVT = TARIF * (0.9679 - 0.1051 * pow(0.5523, DBH - 1.5)) *
            ((1.033 * (1.0 + 1.382937 * exp(-4.015292 * DBH / 10.0))) *
                (0.005454 * DBH * DBH + 0.087266) - 0.174533) / 0.912733;

    }
    else if (CVTYPE == "CV8") {
        CV8 = CV;
        TARIF = TRF8(CV8, DBH);

        CVT = CV8 / (1.03361 - 1.59234 / DBH - 4667.04 / pow(DBH, 4)
            + (0.104498 * HT) / (DBH * DBH)
            + 5322.16 / (pow(DBH, 3) * HT));

        CV4 = CVT * (0.99875 - 43.336 / pow(DBH, 3)
            - 124.717 / pow(DBH, 4)
            + (0.193437 * HT) / pow(DBH, 3)
            + 479.83 / (pow(DBH, 3) * HT));

        CVTS = CV4 * ((1.033 * (1.0 + 1.382937 * exp(-4.015292 * DBH / 10.0))) *
            (0.005454 * DBH * DBH + 0.087266) - 0.174533)
            / (0.005454 * DBH * DBH - 0.087266);
    }

    // --- Basic output fields --------------------------------------------------
    out.totalCubicFoot = CVTS;
    out.grossCubicFootPrimary = CV4;
    out.stumpCubicFoot = CVTS - CVT;

    if (TARIF <= 0.0) return out;

    // --- Sawlog conversion if DBH ≥ BFMIND -----------------------------------
    if (DBH >= BFMIND) {
        double CV6 = CV4 * (0.993 - 0.993 * pow(0.62, DBH - 6.0));

        IV6 = CV6 * (-2.904154 + 3.466328 * log10(DBH * TARIF)
            - 0.02765985 * DBH
            - 0.00008205 * TARIF * TARIF
            + 11.29598 / (DBH * DBH));

        SV616 = pow(10.0,
            0.174439 + 0.117594 * log10(DBH) * log10(TARIF / 0.912733)
            - 8.210585 / (DBH * DBH)
            + 0.236693 * log10(TARIF / 0.912733)
            - 0.00001345 * pow(TARIF / 0.912733, 2)
            - 0.00001937 * DBH * DBH) * CV6;

        out.grossCubicFootPrimary = CV6;
        out.grossCubicFootSecondary = CV4 - CV6;
        out.grossBoardFootPrimary = SV616;
        out.grossInternationalBoardFoot = IV6;

        if (VOLEQ.substr(1, 2) == "32") {
            SV632 = SV616 * (1.001491 - 6.924097 / TARIF + 0.00001351 * DBH * DBH);
            out.grossBoardFootPrimary = SV632;
        }

        if (MTOPP >= 8.0) {
            if (CV8 <= 0.0)
                CV8 = CV4 * (0.983 - 0.983 * pow(0.65, DBH - 8.6));

            SV816 = SV616 * (0.99 - 0.58 * pow(0.484, DBH - 9.5));
            IV8 = IV6 * (0.99 - 0.55 * pow(0.485, DBH - 9.5));

            out.grossCubicFootPrimary = CV8;
            out.grossCubicFootSecondary = CV4 - CV8;
            out.grossBoardFootPrimary = SV816;
            out.grossInternationalBoardFoot = IV8;
        }
    }

    out.tipCubicFoot = CVT - CV4;
    return out;
}


template<size_t N>
bool lookupSpecies(const std::array<std::array<double, 4>, N>& table,
    int SPN, double& A1, double& A2, double& A3)
{
    for (const auto& row : table) {
        if (static_cast<int>(row[0]) == SPN) {
            A1 = row[1];
            A2 = row[2];
            A3 = row[3];
            return true;
        }
    }
    return false;
}

// Browne, J.E. 1962. 
// Standard cubic-foot volume tables for the commercial tree species of British Columbia. 
// British Columbia Forest Service, Forest Surveys and Inventory Division, 1962
double Browne_CVTS(const std::string& VOLEQ, double DBH, double HT)
{

    // -----------------------------------------
    // Extract species number from VOLEQ(8–10)
    // -----------------------------------------
    int SPN = std::stoi(VOLEQ.substr(7, 3));

    // -----------------------------------------
    // Data tables (converted from Fortran)
    // -----------------------------------------
    static const std::array<std::array<double, 4>, 5> COSTAL1 = { {
        {11,  -2.575642, 1.806775, 1.094665},
        {98,  -2.700574, 1.754171, 1.164531},
        {202, -2.712153, 1.659012, 1.195715},
        {242, -2.379642, 1.682300, 1.039712},
        {263, -2.663834, 1.790230, 1.124873}
    } };
    static const std::array<std::array<double, 4>, 5> COSTAL2 = { {
        {11,  -2.575642, 1.806775, 1.094665},
        {98,  -2.550299, 1.835678, 1.042599},
        {202, -2.658025, 1.739925, 1.133187},
        {242, -2.441193, 1.720761, 1.049976},
        {263, -2.702922, 1.842680, 1.123661}
    } };
    static const std::array<std::array<double, 4>, 5> INTERIOR = { {
        {19,  -2.502332, 1.864963, 1.004903},
        {94,  -2.539944, 1.841226, 1.034051},
        {202, -2.734532, 1.739418, 1.166033},
        {242, -2.464614, 1.701993, 1.067038},
        {263, -2.571619, 1.969710, 0.977003}
    } };
    static const std::array<std::array<double, 4>, 10> COEF0 = { {
        {42,  -2.454348, 1.741044, 1.058437},
        {73,  -2.624325, 1.847123, 1.044007},
        {108, -2.615591, 1.847504, 1.085772},
        {119, -2.480145, 1.867286, 0.994351},
        {122, -2.729937, 1.909478, 1.085681},
        {312, -2.770324, 1.885813, 1.119043},
        {351, -2.672775, 1.920617, 1.074024},
        {370, -2.757813, 1.911681, 1.105403},
        {746, -2.635360, 1.946034, 1.024793},
        {747, -2.945047, 1.803973, 1.238853}
    } };

    // -----------------------------------------
    // Find A1, A2, A3 from VOLEQ and species SPN
    // -----------------------------------------
    double A1 = 0, A2 = 0, A3 = 0;
    bool found = false;
    std::string zone = VOLEQ.substr(5, 2);    // characters 6–7

    if (zone == "C1")       found = lookupSpecies(COSTAL1, SPN, A1, A2, A3);
    else if (zone == "C2")  found = lookupSpecies(COSTAL2, SPN, A1, A2, A3);
    else if (VOLEQ[5] == 'I') found = lookupSpecies(INTERIOR, SPN, A1, A2, A3);
    else if (zone == "O0" || zone == "00" || zone == "OO")
        found = lookupSpecies(COEF0, SPN, A1, A2, A3);


    if (!found) {
        return 0.0;
    }

    // -----------------------------------------
    // Compute CVTS using Browne model
    // -----------------------------------------
    double CVTS = std::pow(10.0, A1) * std::pow(DBH, A2) * std::pow(HT, A3);

    return CVTS;
}

// Bell, J.F., Marshall, D.D., and Johnson, G.P. 1981. 
// Tarif Table for Mountain Hemlock. 
// OSU Research Bulletin 35
double Bell_MountainHemlock_CVTS(double DBH, double HT)
{
    return  0.001106485 * std::pow(DBH, 1.8140497) * std::pow(HT, 1.2744923);
}

// King, J.T. and Turnbull, K.J. Weyerhaeuser Douglas fir
double King_DouglasFir_CVTS(double DBH, double HT)
{
    double CVTS =
        std::pow(10.0,
            -3.21809
            + 0.04948 * std::log10(DBH) * std::log10(HT)
            - 0.15664 * std::pow(std::log10(DBH), 2)
            + 2.02132 * std::log10(DBH)
            + 1.63408 * std::log10(HT)
            - 0.16185 * std::pow(std::log10(HT), 2));

    return CVTS;
}

// Summerfield, Edward R. Letter Dated November 7, 1980
double Summerfield_CVTS(int SPN, double DBH, double HT)
{
    double CVTS = 0.0;

    if (SPN == 122) {
        CVTS = std::exp(
            -8.521558
            + 1.977243 * std::log(DBH)
            - 0.105288 * std::pow(std::log(HT), 2)
            + 136.0489 / std::pow(HT, 2)
            + 1.99546 * std::log(HT)
        );
    }
    else if (SPN == 202) {
        CVTS = std::exp(
            -6.110493
            + 1.81306 * std::log(DBH)
            + 1.083884 * std::log(HT)
        );
    }

    return CVTS;
}

// Chambers, Charles J. and Flotz, Bruce W. Washington State DNR Note 27
double Chamber_WesternHemlock_CVTS(double DBH, double HT)
{
    double CVTS =
        std::pow(10.0,
            -2.72170
            + 2.00857 * std::log10(DBH)
            + 1.08620 * std::log10(HT)
            - 0.00568 * DBH
        );
    return CVTS;
}

// Chittester, J.M. and MacLean, C.D. 1984. Cubiv - foot Tree Volume Equations and Tables for Western Juniper.
// USDA Forest Service Pacific Northwest Forest and Range Experiment Station Research Note  PNW - 420.
double Chittester_WesternJuniper_CVTS(double DBH, double HT)
{
    double CVTS = 0.0;
    if (DBH < 5.0 || HT < 10.0) {
        CVTS = 0.00272708 * DBH * DBH * HT;
    }
    else {
        double F = 0.307 + 0.00086 * HT - 0.0037 * DBH * HT / (HT - 4.5);
        double BA = 0.005454154 * DBH * DBH;
        CVTS = BA * F * HT * std::pow(HT / (HT - 4.5), 2.0);
    }
    return CVTS;
}

// Pillsbury, N.H. and Kirkley, M.L. 1984. 
// Equations for Total, Wood, and Saw-Log Volume for Thirteen California Hardwoods. 
// USDA Forest Service Pacific Northwest Forest and Range Experiment Station Research Note  PNW-414.
double Pillsbury_CV8(const std::string& VOLEQ, double DBH, double HT)
{
    TreeMeasurment tree;
    tree.dbh = DBH;
    tree.totalHeight = HT;
    MerchRules merchRules;
    merchRules.minTopDibSaw = 8.0;

    TreeOutput out = r5dve::R5HARV(VOLEQ, tree, merchRules);
    double CV8 = out.grossCubicFootPrimary;

    return CV8;
}

//Curtis, R.O., Bruce, D. and vanCoevering, C. 1968. 
// Volume and Taper Tables for Red Alder. 
// USDA Forest Service Research Paper PNW-56
double Curtis_RedAlder_CVT(double DBH, double THT)
{
    if (THT < 18.0) THT = 18.0;

    double Z = (THT - 0.5 - DBH / 24.0) / (THT - 4.5);
    double Z25 = std::pow(Z, 2.5);

    double F =
        0.3651 * Z25
        - 7.9032 * Z25 * DBH / 1000.0
        + 3.295 * Z25 * THT / 1000.0
        - 1.9856 * Z25 * THT * DBH / 100000.0
        - 2.9668 * Z25 * std::pow(THT, 2) / 1000000.0
        + 1.5092 * Z25 * std::pow(THT, 0.5) / 1000.0
        + 4.9395 * std::pow(Z, 4) * DBH / 1000.0
        - 2.05937 * std::pow(Z, 4) * THT / 1000.0
        + 1.5042 * std::pow(Z, 33) * THT * DBH / 1000000.0
        - 1.1433 * std::pow(Z, 33) * std::pow(THT, 0.5) / 10000.0
        + 1.809 * std::pow(Z, 41) * std::pow(THT, 2) / 10000000.0;

    double CVT = 0.00545415 * std::pow(DBH, 2) * (THT - 4.5) * F;

    return CVT;
}

// Krumland B. E. and L. C. Wensel 1975 
// Preliminary Young Growth Volume Tables for Coastal California Conifers. 
// Reasearch Note No. 1, December 1975. Co-op Redwood Yield Paper
double Krumland_CVTS(int SPN, double DBH, double HT)
{
    double CVTS = 0.0;

    if (SPN == 211) {
        CVTS = exp(-6.2597 + 1.9967 * log(DBH) + 0.9642 * log(HT));
    }
    else if (SPN == 202) {
        CVTS = exp(-6.5193 + 1.7151 * log(DBH) + 1.2274 * log(HT));
    }
    else if (SPN == 17 || SPN == 98 || SPN == 263) {
        CVTS = exp(-6.7013 + 1.7022 * log(DBH) + 1.2979 * log(HT));
    }
    return CVTS;
}

// MacLean, C.D. and Berger, J.M. 1976. 
// Softwood Tree Volume Equations for Major California Species. 
// USDA Forest Service Research Note PNW-266
double Maclean_CV4(int SPN, double DBH, double THT)
{
    double CF4;

    if (SPN == 15) {
        CF4 = 0.299039 + (1.91272 * (1.0 / THT)) + (0.0000367217 * ((THT * THT) / DBH));
    }
    else if (SPN == 20 || SPN == 21) {
        CF4 = 0.231237 + 0.028176 * (THT / DBH);
    }
    else if (SPN == 81) {
        CF4 = 0.225786 + 4.44236 * (1.0 / THT);
        if (CF4 < 0.27) CF4 = 0.27;
    }
    else if (SPN == 108) {
        CF4 = 0.422709 - 0.0000612236 * (THT * THT / DBH);
    }
    else if (SPN == 117 || SPN == 119) {
        CF4 = 0.358550 - 0.488134 * (1.0 / DBH);
    }
    else if (SPN == 122 || SPN == 116) {
        CF4 = 0.40206 - 0.899914 * (1.0 / DBH);
    }
    else if (SPN == 202) {
        CF4 = 0.248569 + (0.0253524 * (THT / DBH)) - (0.0000560175 * ((THT * THT) / DBH));
    }

    // Apply CF4 bounds unless SPN == 81
    if (SPN != 81) {
        if (CF4 > 0.4) CF4 = 0.4;
        if (CF4 < 0.3) CF4 = 0.3;
    }

    double CV4 = 0.005454154 * DBH * DBH * THT * CF4;

    return CV4;
}

// MacLean, Colin D. --Letter Dated January 27, 1983
double Maclean_Otherhardwood_CVTS(double DBH, double HT)
{
    double CVTS = .0016144 * DBH * DBH * HT;
    return CVTS;
}

TreeOutput MacLean_Tarif_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules)  //double DBH, double HT, double sawTopDib, double bfMinDbh)
{
    static std::array<int, 11> volSpList = {15, 20, 21, 81, 108, 116, 117, 119, 122, 202, 998};
    TreeOutput out;
    int SPN = std::stoi(VOLEQ.substr(7, 3));
    double DBH = tree.dbh;
    double HT = tree.totalHeight;

    int idx = array_helper::findIndexInSortedArray(volSpList, SPN);

    if(idx < 0) {
        out.errflag = 6;
        return out;
    }

    if (SPN == 998) {
        double CVTS = Maclean_Otherhardwood_CVTS(DBH, HT);
        out = DNR24_Tarif_Vol(VOLEQ, tree, merchRules, CVTS, "CVTS");
    }
    else {
        double CV4 = Maclean_CV4(SPN, DBH, HT);
        double term = ((1.033 * (1.0 + 1.382937 * exp(-4.015292 * (DBH / 10.0))))
                * (0.005454154 * DBH * DBH + 0.087266) - 0.174533);

        if (tree.dbh >= 6.0) {
            //call pnwtarif for R5
            out = DNR24_Tarif_Vol(VOLEQ, tree, merchRules, CV4, "CV4");
        }
        else {
            double CVTS = 0.0;
            if (tree.dbh < 5.0) {
                double tarifTemp = TRF4(CV4, 6.0);
                if (tarifTemp <= 0.0) tarifTemp = 0.01;

                double tarif =
                    (0.5 * pow(6.0 - DBH, 2))
                    + (1.0 + 0.063 * pow(6.0 - DBH, 2)) * tarifTemp;
                if (tarif <= 0.0) tarif = 0.01;

                CVTS = tarif * term;
            }
            else {
                double CV4Temp = Maclean_CV4(SPN, 6.0, HT);
                double tarifTemp = TRF4(CV4Temp, 6.0);
                if (tarifTemp <= 0.0) tarifTemp = 0.01;
                CVTS = tarifTemp * term;
            }
            out.totalCubicFoot = CVTS;
            out.grossCubicFootPrimary = CV4;
            out.stumpCubicFoot = CVTS * (1.0 - (.9679 - .1051 * pow(.5523, (DBH - 1.5))));
        }
    }

    return out;
}


//PNW Tarif equation with equation number 5##TRFW*** and 6##TRFW***
TreeOutput PNW_Tarif_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules) //double DBH, double HT, double sawTopDib, double bfMinDbh)
{
    static std::array<int, 10> volSpListR5 = { 15, 20, 21, 81, 108, 116, 117, 119, 122, 202};
    static std::array<int, 15> volSpListR6 = {11, 19, 73, 94, 98, 108, 122, 202, 211, 242, 263, 264, 351, 746, 998};

    TreeOutput out;
    int idx = -1;
    int SPN;  
    int region = 0;
    double CVTS = 0.0;
    double CV4 = 0.0;
    std::string voleqTemp = VOLEQ;
    double DBH = tree.dbh;
    double HT = tree.totalHeight;

    if (std::isdigit(VOLEQ[0])) {
        region = VOLEQ[0] - '0';
    }

    if (VOLEQ[0] == 'P') region = 5;

    int logLength = std::stoi(VOLEQ.substr(1, 2));

    SPN = std::stoi(VOLEQ.substr(7, 3));

    if (region == 5) {
        idx = array_helper::findIndexInSortedArray(volSpListR5, SPN);
    }
    else if (region == 6) {
        idx = array_helper::findIndexInSortedArray(volSpListR6, SPN);
    }

    if (idx < 0) {
        out.errflag = 6;
        return out;
    }

    if (region == 6) {
        
        if (SPN == 202) {
            if (logLength == 32) {
                CVTS = King_DouglasFir_CVTS(DBH, HT);
            }
            else {
                CVTS = Summerfield_CVTS(SPN, DBH, HT);
            }
        }
        else if (SPN == 122) {
            CVTS = Summerfield_CVTS(SPN, DBH, HT);
        }
        else if (SPN == 263) {
            CVTS = Chamber_WesternHemlock_CVTS(DBH, HT);
        }
        else if (SPN == 264) {
            CVTS = Bell_MountainHemlock_CVTS(DBH, HT);
        }
        else if (SPN == 211) {
            CVTS = Krumland_CVTS(SPN, DBH, HT);
        }
        else if (SPN == 351) {
            CVTS = Curtis_RedAlder_CVT(DBH, HT);
            out = DNR24_Tarif_Vol(VOLEQ, tree, merchRules, CVTS, "CVT");
            return out;
        }
        else if (SPN == 11 || SPN == 98 || (SPN == 242 && logLength == 32)) {

            size_t pos = voleqTemp.find("TRFW");
            if (pos != std::string::npos) {
                voleqTemp.replace(pos, 4, "BRC1");
            }
            CVTS = Browne_CVTS(voleqTemp, DBH, HT);
        }
        else if (SPN == 19 || SPN == 94 || SPN == 242) {
            size_t pos = voleqTemp.find("TRFW");
            if (pos != std::string::npos) {
                voleqTemp.replace(pos, 4, "BRI0");
            }
            CVTS = Browne_CVTS(voleqTemp, DBH, HT);
        }
        else if (SPN == 73 || SPN == 108 || SPN == 746) {
            size_t pos = voleqTemp.find("TRFW");
            if (pos != std::string::npos) {
                voleqTemp.replace(pos, 4, "BRO0");
            }
            CVTS = Browne_CVTS(voleqTemp, DBH, HT);
        }
        else if (SPN == 998) {
            CVTS = Maclean_Otherhardwood_CVTS(DBH, HT);
        }

        out = DNR24_Tarif_Vol(VOLEQ, tree, merchRules, CVTS, "CVTS");
    }
    else if (region == 5) {
        
        CV4 = Maclean_CV4(SPN, DBH, HT);

        out = DNR24_Tarif_Vol(VOLEQ, tree, merchRules, CV4, "CV4");
    }

    return out;
}

// DeMars volume --
// FIA implement Demars Taper Equation slightly different than R10

// Ratio of inside bark diameter squared to outside bark diameter squared
double BRSQ_DEMARS(int SPN, double DBH, double HT, char GEOSUB)
{
    double BRSQ = 0.0;

    if (SPN == 98 || SPN == 263)
    {
        if (GEOSUB == 'A' && HT <= 110)
        {
            // Spruce-hemlock bark thickness, Afognak Island, HT <= 110 ft
            BRSQ = 0.773 + 0.00134 * DBH + 0.000958 * HT;
        }
        else
        {
            // Spruce-hemlock bark thickness
            BRSQ = 0.8467 + 0.0009144 * DBH + 0.0003568 * HT;
        }
    }
    else if (SPN == 42)
    {
        // AK yellow cedar
        BRSQ = 0.95866817 + 0.00064402 * DBH - 3.1299972 / HT;
    }
    else if (SPN == 242)
    {
        // Western red cedar
        BRSQ = 0.86031485 + 0.00059638 * HT - 0.18335961 / DBH;
    }

    return BRSQ;
}

// Volume ratio
double VOLUME_RATIO(int SPN, double DBH, double HT, double RELHT, char GEOSUB)
{
    double VR = 0.0;

    // Common repeated terms
    auto pow_relht_25 = std::pow(RELHT, 2.5);
    auto pow_relht_4 = std::pow(RELHT, 4.0);
    auto pow_relht_33 = std::pow(RELHT, 33.0);

    if (SPN == 98 || SPN == 263)
    {
        if (GEOSUB == 'A' && HT <= 110)
        {
            VR = 0.4 * pow_relht_25
                + (-0.0269451 * HT + 0.00023794 * HT * HT
                    + 0.148759 * HT / DBH)
                * (0.4 * pow_relht_25 - 0.25 * pow_relht_4)
                + (0.0974044 - 0.0000145706 * HT * HT
                    - 0.0156971 * HT / DBH)
                * (0.4 * pow_relht_25 - (1.0 / 33.0) * pow_relht_33);
        }
        else
        {
            VR = 0.4 * pow_relht_25
                + (-0.0052554 * HT + 0.000034947 * HT * HT
                    + 0.104477 * HT / DBH)
                * (0.4 * pow_relht_25 - 0.25 * pow_relht_4)
                + (7.76807 / (DBH * DBH) - 0.0000094852 * HT * HT
                    - 0.011351 * HT / DBH)
                * (0.4 * pow_relht_25 - (1.0 / 33.0) * pow_relht_33);
        }
    }
    else if (SPN == 42)
    {
        VR = 0.4 * pow_relht_25
            + ((0.4 * pow_relht_25 - 0.25 * pow_relht_4)
                * (-0.02834001 * DBH + 0.00007123 * HT * HT
                    + 0.06709114 * HT / DBH))
            + ((0.4 * pow_relht_25 - (1.0 / 33.0) * pow_relht_33)
                * (0.00282021 * DBH - 0.00002277 * HT * HT
                    + 1.06064717 / (DBH * DBH)
                    - 0.00528349 * HT / DBH));
    }
    else if (SPN == 242)
    {
        VR = 0.4 * pow_relht_25
            + ((0.4 * pow_relht_25 - 0.25 * pow_relht_4)
                * (5.17703194 / (DBH * DBH) - 0.12516819 * DBH
                    + 0.02537037 * HT - 0.00004193 * HT * HT
                    + 0.00155481 * DBH * DBH))
            + ((0.4 * pow_relht_25 - (1.0 / 33.0) * pow_relht_33)
                * (-0.0000207 * HT * HT + 0.24125235 / (DBH * DBH)));
    }

    return VR;
}

// taper equation - yields ratio of squared top diameter inside bark to squared DBH
double RELATIVE_DIA(int SPN, double DBH, double HT, double RELHT, char GEOSUB)
{
    double RELD = 0.0;

    // Common powers of RELHT
    double relht_15 = std::pow(RELHT, 1.5);
    double relht_3 = std::pow(RELHT, 3.0);
    double relht_32 = std::pow(RELHT, 32.0);

    if (SPN == 98 || SPN == 263)
    {
        if (GEOSUB == 'A' && HT <= 110)
        {
            RELD = relht_15
                + (-0.0269451 * HT + 0.00023794 * HT * HT
                    + 0.148759 * HT / DBH)
                * (relht_15 - relht_3)
                + (0.0974044 - 0.0000145706 * HT * HT
                    - 0.0156971 * HT / DBH)
                * (relht_15 - relht_32);
        }
        else
        {
            RELD = relht_15
                + (-0.0052554 * HT + 0.000034947 * HT * HT
                    + 0.104477 * HT / DBH)
                * (relht_15 - relht_3)
                + (7.76807 / (DBH * DBH)
                    - 0.0000094852 * HT * HT
                    - 0.011351 * HT / DBH)
                * (relht_15 - relht_32);
        }
    }
    else if (SPN == 42)
    {
        RELD = relht_15
            + (relht_15 - relht_3)
            * (-0.02834001 * DBH + 0.00007123 * HT * HT
                + 0.06709114 * HT / DBH)
            + (relht_15 - relht_32)
            * (0.00282021 * DBH - 0.00002277 * HT * HT
                + 1.06064717 / (DBH * DBH)
                - 0.00528349 * HT / DBH);
    }
    else if (SPN == 242)
    {
        RELD = relht_15
            + (relht_15 - relht_3)
            * (5.17703194 / (DBH * DBH) - 0.12516819 * DBH
                + 0.02537037 * HT - 0.00004193 * HT * HT
                + 0.00155481 * DBH * DBH)
            + (relht_15 - relht_32)
            * (-0.0000207 * HT * HT + 0.24125235 / (DBH * DBH));
    }

    return RELD;
}

// Computes height to a specified top diameter
double MERCH_HT(int SPN, double DBH, double HT, double TOP,
    double BRSQ, char GEOSUB)
{
    const double RELD_TOLERANCE = 0.0001;
    const double D_TOLERANCE = 0.0001;

    double MHT = 0.0;

    if (SPN == 98 || SPN == 263)
    {
        // Initial estimate of height to merchantable top
        double MHEST = HT * (1.0 - (2.0 / 3.0) * (TOP / DBH));

        // Relative height
        double RELHT = (HT - MHEST) / (HT - 4.5);

        // Relative diameter at RELHT
        double DS = RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB);

        // Target relative diameter (squared ratio)
        double DST = (TOP * TOP) / (BRSQ * DBH * DBH);

        // 90% of RELHT
        double RXL = 0.9 * RELHT;

        // Relative diameter at RXL
        double DXL = RELATIVE_DIA(SPN, DBH, HT, RXL, GEOSUB);

        // Taper per change in relative height
        double TAPER = (DS - DXL) / (0.1 * RELHT);

        // Iterate ≤10 times to converge on top diameter
        for (int K = 1; K <= 10; ++K)
        {
            if (DS <= (DST - RELD_TOLERANCE) ||
                DS >= (DST + RELD_TOLERANCE))
            {
                RXL = RELHT + (DST - DS) / TAPER;
                RELHT = RXL;
                DS = RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB);
            }
            else
            {
                break;
            }
        }

        MHT = HT - (RELHT * (HT - 4.5));
    }
    else if (SPN == 42 || SPN == 242)
    {
        // Binary search for height to merchantable top
        double TOLOW = 4.5;
        double TOHIGH = HT;

        double HTUP = HT - TOP * (HT - 4.5) / DBH;

        double RELHT = (HT - HTUP) / (HT - 4.5);

        double DSX = RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB);

        double DIA = (DSX < 0.0 ? 0.0 : std::sqrt(DSX * BRSQ) * DBH);

        int K = 0;

        while (K < 100 &&
            (DIA < TOP - 0.001 || DIA > TOP + 0.001))
        {
            if (DIA > TOP)
                TOLOW = HTUP;
            else
                TOHIGH = HTUP;

            HTUP = (TOLOW + TOHIGH) / 2.0;

            RELHT = (HT - HTUP) / (HT - 4.5);

            DSX = RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB);

            DIA = (DSX < 0.0 ? 0.0 : std::sqrt(DSX * BRSQ) * DBH);

            ++K;
        }

        MHT = HTUP;
    }
    else if (SPN == 242)
    {
        // This part is not used. The SPN 242 is handled above with SPN 42
        // NOT USED in FIA code — converted faithfully as requested

        int KNOL = 0;
        double STUMP = 0.0; // Fortran has STUMP but does not initialize it here—verify externally

        double HITOP = STUMP + 16.3;
        char SMALLD = 'N';

        double DSX = 0.0;
        double DSI = 0.0;

        int K = 0;

        while (SMALLD == 'N' && HITOP <= HT && K < 100)
        {
            ++K;
            ++KNOL;

            double RELHT = (HT - HITOP) / (HT - 4.5);
            DSX = RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB);

            if (DSX < 0.0) DSI = 0.0;
            else DSI = std::sqrt(DSX * BRSQ) * DBH;

            if (DSI >= TOP)
                SMALLD = 'Y';
            else
                HITOP += 16.3;
        }

        if (DSI < TOP)
            KNOL--;

        HITOP -= 16.3;

        double C = 1.0;
        char ATLIMD = 'N';
        K = 0;

        while (ATLIMD == 'N' && K < 100)
        {
            ++K;

            HITOP += C;

            double RELHT = (HT - HITOP) / (HT - 4.5);
            DSX = RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB);

            DSI = std::sqrt(DSX * BRSQ) * DBH;

            if (((DSI > TOP - D_TOLERANCE) &&
                (DSI < TOP + D_TOLERANCE)) ||
                K > 100)
            {
                MHT = HITOP;
                ATLIMD = 'Y';
            }
            else if (DSI < TOP)
            {
                HITOP -= C;
                // In the original: C = C * 0.1 (commented out)
            }
        }
    }

    return MHT;
}

// DeMars Volume calculation
// NVEL Equation Number :
// P01DMR0042, P01DMR0098, P01DMR0242, P01DMR0263,
// For Afognak Island trees <= 110 ft
// P01DMRA098, P01DMRA263
// THE 7TH CHARACTER IN THE EQUATION NUMBER : 0 = AK WIDE, A = Afognak Island trees <= 110 ft

TreeOutput DeMars_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;
    double DBH = tree.dbh;
    double HT = tree.totalHeight;
    double MTOPP = merchRules.minTopDibSaw;
    double TOP = merchRules.minTopDibNonSaw;
    double STUMP = merchRules.stumpHeight;
    double BFMIND = merchRules.minimumBoardFootDiameter;

    // Extract species number (positions 8–10 in Fortran, 0-based in C++)
    int SPN = std::stoi(VOLEQ.substr(7, 3));

    switch (SPN)
    {
    case 42:
    case 98:
    case 242:
    case 263:
        break;       // valid
    default:
        out.errflag = 6;
        return out;
    }

    char GEOSUB = VOLEQ[6];

    if (STUMP < 0.1) STUMP = 1.0;
    if (MTOPP < 0.1) MTOPP = 6.0;
    if (TOP < 0.1) TOP = 4.0;

    // Compute BRSQ
    double BRSQ = BRSQ_DEMARS(SPN, DBH, HT, GEOSUB);

    // Volume of cylinder inside bark above breast height
    double VOLCYLINDER = 0.005454154 * DBH * DBH * (HT - 4.5) * BRSQ;

    // Height to merchantable top at 4" DIB
    double MHT = MERCH_HT(SPN, DBH, HT, TOP, BRSQ, GEOSUB);

    // Volume stump → tip
    double RELHT = (HT - STUMP) / (HT - 4.5);
    double VR = VOLUME_RATIO(SPN, DBH, HT, RELHT, GEOSUB);
    double CVT = VR * VOLCYLINDER;
    out.totalCubicFoot = CVT;

    // Volume tip → 4”
    RELHT = (HT - MHT) / (HT - 4.5);
    VR = VOLUME_RATIO(SPN, DBH, HT, RELHT, GEOSUB);
    double TIP = VR * VOLCYLINDER;

    double CV4 = CVT - TIP;
    out.grossCubicFootPrimary = CV4;   // VOL(4)
    out.tipCubicFoot = TIP;   // VOL(15)

    // If DBH ≥ board‐foot minimum → compute bf volumes
    if (DBH >= BFMIND)
    {
        TOP = MTOPP;
        MHT = MERCH_HT(SPN, DBH, HT, TOP, BRSQ, GEOSUB);

        RELHT = (HT - MHT) / (HT - 4.5);
        VR = VOLUME_RATIO(SPN, DBH, HT, RELHT, GEOSUB);
        TIP = VR * VOLCYLINDER;
        double CV6 = CVT - TIP;

        out.grossCubicFootSecondary = CV4 - CV6;    // VOL(7)
        out.grossCubicFootPrimary = CV6;          // VOL(4)

        int MLOGS = static_cast<int>((MHT - STUMP) / 16.3);

        std::vector<LogOutput> logResult;
        LogOutput logData;

        double SV6 = 0.0;
        double IV6 = 0.0;
        double HTUP = STUMP;
        double dbhIb = std::sqrt(BRSQ) * DBH;
        double largeDIB = dbhIb;

        for (int I = 1; I <= MLOGS; I++)
        {
            HTUP += 16.3;
            RELHT = (HT - HTUP) / (HT - 4.5);

            double RELD = RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB);
            double DIB = std::sqrt(RELD * BRSQ) * DBH;

            double LOGSV = 0.79 * DIB * DIB - 2.0 * DIB - 4.0;
            SV6 += LOGSV;

            double LOGIV = 0.796 * DIB * DIB - 1.374 * DIB - 1.23;
            IV6 += LOGIV;

            double logCV = smallian(largeDIB, DIB, 16.0);

            logData.logNumber = I;
            logData.grossBoardFoot = LOGSV;
            logData.internationalBoardFoot = LOGIV;
            logData.grossCubicFoot = logCV;
            logData.isSecondary = false;
            logData.length = 16.0;

            logResult.push_back(logData);
            largeDIB = DIB;
        }

        // Last chunk length
        double CL = MHT - 16.3 * MLOGS - STUMP;

        RELHT = (HT - MHT) / (HT - 4.5);
        double RELD = RELATIVE_DIA(SPN, DBH, HT, RELHT, GEOSUB);
        double DIB = std::sqrt(RELD * BRSQ) * DBH;

        double LOGSV = (0.79 * DIB * DIB - 2.0 * DIB - 4.0) * CL / 16.3;
        SV6 += LOGSV;

        double logCV = smallian(largeDIB, DIB, CL);

        out.grossBoardFootPrimary = SV6;   // VOL(2)

        double LOGIV = 0.0;

        if (CL > 0.0 && CL < 4.075)
            LOGIV = CL / 4.075 * (0.199 * DIB * DIB - 0.642 * DIB);
        else if (CL < 8.15)
            LOGIV = CL / 8.15 * (0.398 * DIB * DIB - 1.086 * DIB - 0.271);
        else if (CL < 12.225)
            LOGIV = CL / 12.225 * (0.597 * DIB * DIB - 1.33 * DIB - 0.715);
        else if (CL < 16.3)
            LOGIV = CL / 16.3 * (0.796 * DIB * DIB - 1.375 * DIB - 1.23);

        IV6 += LOGIV;
        out.grossInternationalBoardFoot = IV6;    // VOL(10)

        //add the last chunck
        logData.logNumber = MLOGS + 1;
        logData.grossBoardFoot = LOGSV;
        logData.internationalBoardFoot = LOGIV;
        logData.grossCubicFoot = logCV;
        logData.isSecondary = false;
        logData.length = std::nearbyint(CL);

        logResult.push_back(logData);

        out.logs = logResult;
    }

    return out;
}

//Embry volume calculation

inline double Embry_Cone_CV(double DBH, double THT, double TOP, double CV)
{
    // Height at top diameter point
    double HT_TOP = TOP * (THT - 4.5) / DBH;

    // Cone volume of stem
    double CONE_STEM = (3.14 * std::pow(DBH / 24.0, 2) * (THT - 3.5)) / 3.0;

    // Cone above HT_TOP
    double CONE_TOP = 0.029088 * HT_TOP;

    // Cylinder to DBH (first 3.5 ft)
    double CYLINDER_TO_DBH = 3.14 * std::pow(DBH / 24.0, 2) * 3.5;

    // Total cone approximation
    double CONE = CYLINDER_TO_DBH + CONE_STEM - CONE_TOP;

    // Only replace CV if CONE is larger (same as Fortran assignment semantics)
    if (CONE > CV)
        CV = CONE;

    return CV;
}

// NVEL Equation Number: P01EMB0098, P01EMB0263
TreeOutput Embry_Vol(int SPN, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;
    double MTOPP = merchRules.minTopDibSaw;
    double BFMIND = merchRules.minimumBoardFootDiameter;

    double DBH = tree.dbh;
    double HT = tree.totalHeight;

    if (SPN != 98 && SPN != 263) {
        out.errflag = 6;
        return out;
    }

    if (DBH < 1.0 || HT < 0.0)
        return out;

    if (BFMIND < 0.1) BFMIND = 9.0;
    if (MTOPP < 0.1) MTOPP = 6.0;

    double CV4 = 0.0;
    double SV = 0.0;
    double IV = 0.0;

    if (SPN == 263)
    {
        CV4 = -2.85632 + 0.0633 * HT + 0.00202 * DBH * DBH * HT;
    }
    else if (SPN == 98)
    {
        CV4 = 0.0022 * DBH * DBH * HT - 7.32683 / (DBH * DBH);
    }

    if (CV4 < 0.0)
    {
        double TOP = 4.0;
        CV4 = Embry_Cone_CV(DBH, HT, TOP, CV4);
    }

    out.grossCubicFootPrimary = CV4;       // VOL(4)

    if (DBH >= BFMIND)
    {
        double CV6 = 0.0;

        if (SPN == 263)
        {
            if (DBH < 19.5)
            {
                CV6 = -3.70342 + 0.02856 * HT + 0.00213 * DBH * DBH * HT;
            }
            else
            {
                CV6 = -2.85632 + 0.0633 * HT + 0.00202 * DBH * DBH * HT;
            }

            SV = -0.53887 * DBH * DBH + 0.01614 * DBH * DBH * HT;
            IV = -0.63535 * DBH * DBH + 0.32091 * HT + 0.01741 * DBH * DBH * HT;
        }
        else if (SPN == 98)
        {
            CV6 = 0.00216 * DBH * DBH * HT - 154.15834 / (DBH * DBH);

            SV = -4.60417 * DBH + 0.01427 * DBH * DBH * HT;
            IV = -0.43761 * DBH * DBH + 0.01733 * DBH * DBH * HT;
        }

        if (CV6 < 0.0)
        {
            double TOP = 6.0;
            CV6 = Embry_Cone_CV(DBH, HT, TOP, CV6);
        }

        out.grossCubicFootSecondary = CV4 - CV6;   // VOL(7)
        out.grossCubicFootPrimary = CV6;           // VOL(4)
        out.grossBoardFootPrimary = SV;            // VOL(2)
        out.grossInternationalBoardFoot = IV;      // VOL(10)
    }

    return out;
}

// Switches between Demars and Embry/Haack equations depending on dbh and ht
// Only the Sitka Spruce Embry equation is used by Alaska FIA regardless of species
// Valid species code : 098, 263, 042, 242
// NVEL Equation Number :
// P01DEE0098, P01DEE0263, P01DEE0042, P01DEE0242,
// P01DEEA098, P01DEEA263, P01DEEA042, P01DEEA242,
TreeOutput DeMars_Embry_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;

    // If small DBH or height: use EMBRY equations instead of DEMARS
    if (tree.dbh < 9.0 || tree.totalHeight < 40.0)
    {
        int SPN = 98;
        out = Embry_Vol(SPN, tree, merchRules);
    }
    else
    {
        out = DeMars_Vol(VOLEQ, tree, merchRules);
    }

    return out;
}

// Switches between Browne, Demars and Embry/Haack equations depending on dbh and ht
// Valid species code : 042, 242
// NVEL Equation Number : P01BDE0042, P01BDE0242, P01BDEA042, P01BDEA242
TreeOutput Browne_DeMars_Embry_Vol(std::string VOLEQ, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;
    double DBHOB = tree.dbh;
    double HTTOT = tree.totalHeight;
    double MTOPP = merchRules.minTopDibSaw;
    double STUMP = merchRules.stumpHeight;
    double BFMIND = merchRules.minimumBoardFootDiameter;

    // Extract species code from characters 8-10
    int SPN = std::stoi(VOLEQ.substr(7, 3));

    if (SPN != 42 && SPN != 242)
    {
        out.errflag = 6;
        return out;
    }

    // copy original VOLEQ
    std::string VOLEQTMP = VOLEQ;
    std::string VOLEQTMPI = VOLEQ;

    // Overwrite characters 4–7 (Fortran 4:7 → C++ substr [3..6])
    if (SPN == 42)
        VOLEQTMP.replace(3, 4, "BRO0");
    else if (SPN == 242)
        VOLEQTMP.replace(3, 4, "BRC2");

    VOLEQTMPI = VOLEQTMP;

    // ---------------------------
    // CASE 1: DBH < 5 → use DEMARS
    // ---------------------------
    if (tree.dbh < 5.0)
    {
        out = DeMars_Vol(VOLEQ, tree, merchRules);
        return out;
    }

    // ---------------------------
    // CASE 2: DBH < 9 → Browne DNR24
    // ---------------------------
    if (tree.dbh < 9.0)
    {
        double CVTS = Browne_CVTS(VOLEQTMP, tree.dbh, tree.totalHeight); 
        out = DNR24_Tarif_Vol(VOLEQTMP, tree, merchRules, CVTS, "CVTS");
        // Recalc International BF using Browne (no location/age split)
        VOLEQTMPI.replace(3, 7, "BRO0042");
        CVTS = Browne_CVTS(VOLEQTMPI, tree.dbh, tree.totalHeight);
        TreeOutput out2 = DNR24_Tarif_Vol(VOLEQTMP, tree, merchRules, CVTS, "CVTS");
        out.grossInternationalBoardFoot = out2.grossInternationalBoardFoot;

        return out;
    }

    // ---------------------------
    // CASE 3: mid-range DBH depending on species
    // ---------------------------
    if ((DBHOB < 38.0 && SPN == 42) ||
        (DBHOB < 56.0 && SPN == 242))
    {
        if (HTTOT < 40.0) {
            double CVTS = Browne_CVTS(VOLEQTMP, tree.dbh, tree.totalHeight);
            out = DNR24_Tarif_Vol(VOLEQTMP, tree, merchRules, CVTS, "CVTS");
        }
        else
            out = DeMars_Vol(VOLEQ, tree, merchRules);

        // Recalculate International BF volume
        TreeOutput out2;
        if (HTTOT < 25.0)
        {
            VOLEQTMPI.replace(3, 7, "BRO0042");
            double CVTS = Browne_CVTS(VOLEQTMPI, tree.dbh, tree.totalHeight);
            TreeOutput out2 = DNR24_Tarif_Vol(VOLEQTMP, tree, merchRules, CVTS, "CVTS");
        }
        else
        {
            out2 = DeMars_Vol(VOLEQ, tree, merchRules);
        }

        out.grossInternationalBoardFoot = out2.grossInternationalBoardFoot;
        return out;
    }

    // ---------------------------
    // CASE 4: large DBH trees
    // ---------------------------

    VOLEQ = VOLEQ.substr(0, 7) + "098";  // Fortran VOLEQ(1:7)//'098'
    SPN = 98;

    if (HTTOT < 40.0)
        out = Embry_Vol(SPN, tree, merchRules);
    else
        out = DeMars_Vol(VOLEQ, tree, merchRules);

    return out;
}
