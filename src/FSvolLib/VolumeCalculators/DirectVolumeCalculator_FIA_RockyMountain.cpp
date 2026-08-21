#include "DirectVolumeCalculator_FIA_RockyMountain.h"
#include "..\array_helper.h"
#include <cmath>
#include <string>
#include <array>

TreeOutput ChojnackyWoodlandVol(int fiaCode, double drc, double totalHeight)
{
    TreeOutput out;

    auto V1 = [](double B0, double B1, double B2, double X) {
        return B0 + B1 * X + B2 * X * X;
        };

    auto V2 = [](double B0, double B1, double B2, double X0, double X){
        return B0 + B1 * X + B2 * (3.0 * X0 * X0 - 2.0 * X0 * X0 * X0 / X);
    };

    // Small trees DIA < 3
    if (drc < 3.0) {
        out.totalCubicFoot = 0.1;
        return out;
    }

    double D2H = drc*drc*totalHeight;
    double X = D2H / 1000.0;

    double B0, B1, B2, X0;

    // Species-specific coefficients (1.5-inch equations)
    if (fiaCode == 63 || fiaCode == 66) {
        B0 = 0.0255;  B1 = 1.7479;  B2 = 0.1994;  X0 = 4.0021;
    }
    else if (fiaCode == 65 || fiaCode == 69) {
        B0 = -0.0192; B1 = 2.1297;  B2 = 0.1100;  X0 = 2.5757;
    }
    else if (fiaCode == 106) {
        B0 = -0.0594; B1 = 2.6358;  B2 = 0.3248;  X0 = 2.0773;
    }
    else {
        out.errflag = 6;
        return out;
    }

    double CV15;
    if (X <= X0)
        CV15 = V1(B0, B1, B2, X);
    else
        CV15 = V2(B0, B1, B2, X0, X);

    // Calculate volume to 3" top if needed
    if (fiaCode == 63 || fiaCode == 66) {
        B0 = -0.0601; B1 = 1.3846; B2 = 0.1566; X0 = 5.2101;
    }
    else if (fiaCode == 65 || fiaCode == 69) {
        B0 = -0.1063; B1 = 1.4373; B2 = 0.1324; X0 = 4.0243;
    }
    else if (fiaCode == 106) {
        B0 = -0.1231; B1 = 2.0741; B2 = 0.1831; X0 = 3.5503;
    }

    double CV3;
    if (X <= X0)
        CV3 = V1(B0, B1, B2, X);
    else
        CV3 = V2(B0, B1, B2, X0, X);

    out.totalCubicFoot = CV15;
    out.grossCubicFootPrimary = CV3;

    return out;
}


// --------------------------------------------------------------
// C++ version of SAWLOGFACTOR
// --------------------------------------------------------------
double sawlogFactor(int fiaCode,double dbh)
{
    double SPF = 1.0;

    static const std::array<int, 12> VOLSP = {
        17,19,73,93,108,119,122,202,242,263,740,746
    };

    static const double SAWFAC[12][5] = {
        {17, 0.98, 0.96, 0.16, 4.0},
        {19, 0.98, 0.96, 0.16, 4.0},
        {73, 0.98, 0.96, 0.16, 4.0},
        {93, 0.98, 0.96, 0.15, 3.0},
        {108,0.95, 0.98, 0.10, 4.0},
        {119,0.95, 0.98, 0.13, 5.0},
        {122,0.93, 0.95, 0.26, 7.0},
        {202,0.98, 0.96, 0.19, 5.0},
        {242,0.92, 0.93, 0.21, 2.5},
        {263,0.98, 0.96, 0.21, 2.0},
        {740,0.96, 0.92, 0.22, 1.4},
        {746,0.96, 0.92, 0.22, 1.4}
    };

    int k = array_helper::findIndexInSortedArray(VOLSP, fiaCode);

    if (k < 0)
    {
        return SPF;
    }

    double CSAW = SAWFAC[k][1];
    double ACSP = SAWFAC[k][2];
    double BCSP = SAWFAC[k][3];
    double FACN = SAWFAC[k][4];

    if (dbh <= 42.0)
    {
        SPF = ACSP - (BCSP * std::pow((42.0 - dbh) / 33.0, FACN));
    }
    else
    {
        SPF = CSAW;
    }

    if (SPF < 0.0)
        SPF = 1.0;

    return SPF;
}

TreeOutput Kemp_Vol(int fiaCode, double DBHOB, double HTTOT, double BFMIND)
{
    TreeOutput out;
    // Species tables
    static const std::array<int, 12> VOLSP = {
        17,19,73,93,108,119,122,202,242,263,740,746
    };

    static const double CV4COEF[12][6] = {
        {17, -0.563, 0.00219,  9.969, 0.00197, 47900},
        {19,  1.449, 0.00183, 26.222, 0.00117, 37500},
        {73, -0.056, 0.0017,  19.409, 0.00132, 51200},
        {93,  0.48,  0.00214, 19.041, 0.00174, 46400},
        {108, 1.052, 0.00221, 5.369, 0.00197, 18000},
        {119, 0.166, 0.00206, 4.508, 0.00194, 36200},
        {122, -1.656,0.00203,-9.637,0.00218, 53200},
        {202, 0.437, 0.00178, 7.702, 0.00165, 55900},
        {242, 1.141, 0.00174, 8.931, 0.00146, 27800},
        {263, -0.991,0.00209, 2.544, 0.0021,      0},
        {740, -0.749,0.00204, 4.285, 0.00194, 50300},
        {746, -0.343,0.00224, 1.071, 0.00217, 20200}
    };

    static const double IVCOEF[12][6] = {
        {17,-25.764, 0.01423, 46.951, 0.01299, 58600},
        {19,-1.484,  0.01112,159.286, 0.00738, 43000},
        {73,-5.836,  0.0108, 124.606, 0.0089, 68700},
        {93,-2.363,  0.0128, 48.715,  0.01225,92900},
        {108,3.548,  0.01319,24.579,  0.01205,18400},
        {119,-15.602,0.01302,18.828,  0.01243,58400},
        {122,-46.452,0.0137,-271.093, 0.01691,70000},
        {202,-18.15, 0.01116,25.891,  0.01071,97900},
        {242,-3.099, 0.01014,38.72,   0.00878,30700},
        {263,-31.897,0.01345,-8.618,  0.01379,0},
        {740,-9.24,  0.01157,-24.975, 0.012,  36600},
        {746,-9.547, 0.01309,-12.441, 0.01325,18100}
    };

    static const double SVCOEF[12][6] = {
        {17,-34.127,0.01293,10.603,  0.01218,59600},
        {19,-11.403,0.01011,124.425, 0.00694,42800},
        {73,-29.79, 0.00997,85.15,   0.00841,73700},
        {93,-11.851,0.01149,1.62,    0.01158,0},
        {108,-8.085,0.01208,14.111,  0.01103,21100},
        {119,-26.729,0.01189,-32.516,0.01181,0},
        {122,-50.34, 0.01201,-298.784,0.01595,63100},
        {202,-25.332,0.01003,-9.522, 0.01011,0},
        {242,-10.742,0.00878,-4.064, 0.00799,8500},
        {263,-37.314,0.01203,-50.68, 0.01306,13000},
        {740,-15.966,0.01046,-46.735,0.0114, 32700},
        {746,-18.544,0.01197,-21.309,0.01216,14600}
    };

    static const double CVT2CV4R[12][4] = {
        {17, 0.87614,-1.48268,0.60654},
        {19, 0.97449,-1.42305,0.44856},
        {73, 0.87614,-1.48268,0.60654},
        {93, 0.97449,-1.42305,0.44856},
        {108,0.99471,-1.30771,0.31300},
        {119,0.96272,-1.37551,0.41279},
        {122,0.90178,-1.28594,0.38416},
        {202,0.87614,-1.48268,0.60654},
        {242,1.03508,-2.07016,1.03508},
        {263,0.9806, -1.41272,0.43212},
        {740,0.9615, -1.58271,0.62121},
        {746,0.95806,-1.33682,0.37877}
    };

    int ERRFLG = 0;

    // Extract specie number VOLEQ(8:10) → C++ substr(7,3)
    int SPN = fiaCode;  // std::stoi(VOLEQ.substr(7, 3));

    int IDX = -1;

    //SEARCH_SP(CNT, VOLSP, SPN, IDX, ERRFLG);
    IDX = array_helper::findIndexInSortedArray(VOLSP, SPN);

    if (IDX < 0)
    {
        ERRFLG = 6;
        out.errflag = ERRFLG;
        return out;
    }

    if (DBHOB < 5.0)
        return out;

    double DBH = DBHOB;
    double THT = HTTOT;
    double D2H = DBH * DBH * THT;

    int k = IDX; // convert Fortran index to C++

    // Must match Fortran "IF(SPN .EQ. CV4COEF(IDX,1))"
    if (SPN == (int)CV4COEF[k][0])
    {

        double C1S = CV4COEF[k][1];   // Fortran column 2 (small tree C1)
        double C2S = CV4COEF[k][2];   // Fortran column 3 (small tree C2)

        double C1L = CV4COEF[k][3];
        double C2L = CV4COEF[k][4];
        double CBP = CV4COEF[k][5];

        double R1 = CVT2CV4R[k][1];
        double R2 = CVT2CV4R[k][2];
        double R3 = CVT2CV4R[k][3];

        double CV4 = 0.0;

        if (DBH >= 21.0 && CBP == 0.0)
        {
            CV4 = C1L + C2L * D2H;
        }
        else if (DBH < 21.0 && CBP == 0.0)
        {
            CV4 = C1S + C2S * D2H;
        }
        else if (D2H <= CBP)
        {
            CV4 = C1S + C2S * D2H;
        }
        else
        {
            CV4 = C1L + C2L * D2H;
        }

        if (CV4 < 0.0) CV4 = 0.1;
        out.grossCubicFootPrimary = CV4;

        // Total cubic volume ratio computation
        double inside = (R2 * THT) * (R2 * THT)
            - 4.0 * R3 * (R1 * THT * THT - (16.0 * THT * THT / (DBH * DBH)));

        double TH = (-R2 * THT - std::sqrt(inside)) / (2.0 * R3);

        double TMPCVT =
            0.005454 * DBH * DBH * THT *
            (R1 * (1.0 - (1.0 / THT))
                + (R2 / 2.0) * (1.0 - std::pow((1.0 / THT), 2))
                + (R3 / 3.0) * (1.0 - std::pow((1.0 / THT), 3)));

        double TMPCV4 =
            0.005454 * DBH * DBH * THT *
            (R1 * ((TH / THT) - (1.0 / THT))
                + (R2 / 2.0) * (std::pow((TH / THT), 2) - std::pow((1.0 / THT), 2))
                + (R3 / 3.0) * (std::pow((TH / THT), 3) - std::pow((1.0 / THT), 3)));

        double RATIO = TMPCVT / TMPCV4;
        double CVT = RATIO * CV4;

        if (CVT < 0.0) CVT = 0.0;
        out.totalCubicFoot = CVT;

        // Saw timber portion
        if (DBH >= BFMIND)
        {
            double S1S = SVCOEF[k][1];
            double S2S = SVCOEF[k][2];
            double S1L = SVCOEF[k][3];
            double S2L = SVCOEF[k][4];
            double SBP = SVCOEF[k][5];

            double SV = 0.0;

            if (DBH >= 21.0 && SBP == 0.0)
                SV = S1L + S2L * D2H;
            else if (DBH < 21.0 && SBP == 0.0)
                SV = S1S + S2S * D2H;
            else if (D2H <= SBP)
                SV = S1S + S2S * D2H;
            else
                SV = S1L + S2L * D2H;

            if (SV < 1.0) SV = 1.0;
            out.grossBoardFootPrimary = SV;

            double I1S = IVCOEF[k][1];
            double I2S = IVCOEF[k][2];
            double I1L = IVCOEF[k][3];
            double I2L = IVCOEF[k][4];
            double IBP = IVCOEF[k][5];

            double IV = 0.0;

            if (DBH >= 21.0 && IBP == 0.0)
                IV = I1L + I2L * D2H;
            else if (DBH < 21.0 && IBP == 0.0)
                IV = I1S + I2S * D2H;
            else if (D2H <= IBP)
                IV = I1S + I2S * D2H;
            else
                IV = I1L + I2L * D2H;

            if (IV < 1.0) IV = 1.0;
            out.grossInternationalBoardFoot = IV;

            double SPF = sawlogFactor(SPN,DBH);

            double CV6 = CV4 * SPF;
            out.grossCubicFootPrimary = CV6;          // Overwrite cubic 4 inch top with saw cubic
            out.grossCubicFootSecondary = CV4 - CV6;    // Top portion

            if (out.grossCubicFootSecondary < 0.0)
                out.grossCubicFootSecondary = 0.0;
        }

    }
    return out;
}


inline double CV(double DBH, double THT, double A0, double A1, double A2)
{
    return A0 * std::pow(DBH, A1) * std::pow(THT, A2);
}

inline double TOPRATIO(double DBH, double MTOPP, double B0, double B1, double B2)
{
    return B0 * (std::pow(MTOPP, B1) / std::pow(DBH, B2));
}

TreeOutput Moisen_Vol(const std::string& VOLEQ, double DBHOB, double HTTOT, double MTOPP, double BFMIND)
{
    TreeOutput out;

    static const std::array<int, 6> VOLSP = { 17, 73, 108, 119, 122, 202 };

    static const double COEF[6][8] = {
        {17, 0.001255, 1.662, 1.328, 0.592, 3.595, 3.329, 1.093},
        {73, 0.000964, 1.756, 1.283, 1.133, 3.561, 3.418, 1.175},
        {108,0.002057, 1.862, 1.120, 0.688, 3.580, 3.405, 1.032},
        {119,0.001592, 1.661, 1.256, 0.620, 3.358, 3.137, 1.037},
        {122,0.001720, 1.876, 1.089, 1.047, 3.450, 3.290, 1.128},
        {202,0.001655, 1.703, 1.217, 0.709, 3.475, 3.229, 1.153}
    };

    static const double COEF1[6][4] = {
        {17, 0.001138, 1.666, 1.345},
        {73, 0.000931, 1.756, 1.290},
        {108,0.001948, 1.859, 1.133},
        {119,0.001514, 1.648, 1.274},
        {122,0.001742, 1.882, 1.080},
        {202,0.001633, 1.713, 1.213}
    };

    int ERRFLG = 0;


    // Extract species VOLEQ(8:10) → substr(7,3)
    int SPN = std::stoi(VOLEQ.substr(7, 3));

    if (MTOPP < 0.1) MTOPP = 4.0;
    if (BFMIND < 0.1) BFMIND = 9.0;

    int IDX = array_helper::findIndexInSortedArray(VOLSP, SPN);

    if (IDX < 0)
    {
        ERRFLG = 6;
        out.errflag = ERRFLG;
        return out;
    }

    int k = IDX;   // convert Fortran index → C++

    double DBH = DBHOB;
    double THT = HTTOT;

    // Plantation indicator VOLEQ(7:7) → VOLEQ[6]
    if (VOLEQ[6] == '1')
    {
        if ((int)COEF1[k][0] == SPN)
        {
            double A0 = COEF1[k][1];
            double A1 = COEF1[k][2];
            double A2 = COEF1[k][3];

            double CVT = CV(DBH, THT, A0, A1, A2);
            out.totalCubicFoot = CVT;   // VOL(1)
        }
        return out;
    }

    // Normal Moisen model
    if ((int)COEF[k][0] == SPN)
    {
        double A0 = COEF[k][1];
        double A1 = COEF[k][2];
        double A2 = COEF[k][3];
        double B0 = COEF[k][4];
        double B1 = COEF[k][5];
        double B2 = COEF[k][6];
        double F = COEF[k][7];

        double CVT = CV(DBH, THT, A0, A1, A2);

        double RATIO = TOPRATIO(DBH, 4.0, B0, B1, B2);

        double CV4 = CVT * (1.0 - RATIO);

        out.totalCubicFoot = CVT;   // VOL(1)
        out.grossCubicFootPrimary = CV4;   // VOL(4)

        if (DBH >= BFMIND)
        {
            double SPF = 1.0;
            SPF = sawlogFactor(SPN, DBH);
            double CV6 = CV4 * SPF;

            out.grossCubicFootPrimary = CV6;
            out.grossCubicFootSecondary = CV4 - CV6;

        }
    }
    return out;
}