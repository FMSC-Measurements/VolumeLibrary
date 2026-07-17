#include "SmalianScribnerIntl14.h"
#include <array>
#include <cctype>   // std::toupper
#include <cmath>    // std::trunc, std::floor
#include <stdexcept>
#include <algorithm>


double smallian(double diameterLarge, double diameterSmall, double logLength)
{
	return 0.002727 * (std::pow(diameterLarge, 2.0) + std::pow(diameterSmall, 2.0)) * logLength;
}


// Scribner Decimal C volume (units of 10 board feet if COR == 'Y')
double scribner(double DIA, double LEN, char COR)
{
    // ---- FACTOR table: 1-based indexing (index 1..132) ----
    static const std::array<double, 133> FACTOR = {
        // index 0 unused to mimic Fortran 1-based arrays
        0.0,
        // 1..66
        0.000,0.143,0.390,0.676,
        1.070,1.160,1.400,1.501,2.084,3.126,3.749,4.900,
        6.043,7.140,8.880,10.000,11.528,13.290,14.990,
        17.499,18.990,20.880,23.510,25.218,28.677,31.249,
        34.220,36.376,38.040,41.060,44.376,45.975,48.990,
        50.000,54.688,57.660,64.319,66.730,70.000,75.240,
        79.480,83.910,87.190,92.501,94.990,99.075,103.501,
        107.970,112.292,116.990,121.650,126.525,131.510,
        136.510,141.610,146.912,152.210,157.710,163.288,
        168.990,174.850,180.749,186.623,193.170,199.120,
        205.685,
        // 67..132
        211.810,218.501,225.685,
        232.499,239.317,246.615,254.040,261.525,269.040,
        276.630,284.260,292.501,300.655,308.970,317.360,
        325.790,334.217,343.290,350.785,359.120,368.380,
        376.610,385.135,393.380,402.499,410.834,419.166,
        428.380,437.499,446.565,455.010,464.150,473.430,
        482.490,491.700,501.700,511.700,521.700,531.700,
        541.700,552.499,562.501,573.350,583.350,594.150,
        604.170,615.010,625.890,636.660,648.380,660.000,
        671.700,683.330,695.011,
        // 121..126 (for LEN 16..31 and DIA 6..11)
        1.249,1.608,1.854,2.410,3.542,4.167,
        // 127..132 (for LEN 32..40 and DIA 6..11)
        1.570,1.800,2.200,2.900,3.815,4.499
    };

    // ---- EXCEPT table: 1-based indexing (index 1..149) ----
    static const std::array<double, 150> EXCEPT = {
        0.0, // index 0 unused
        // 1..77
        40420.,40460.,40510.,
        40520.,40530.,40540.,40550.,40600.,40630.,
        40650.,40720.,40730.,40740.,40750.,40760.,
        40800.,40860.,40880.,40930.,40960.,40980.,
        41010.,41020.,41030.,41040.,41050.,41060.,
        41110.,41120.,41150.,41180.,50100.,50191.,
        50211.,50410.,50430.,50460.,50590.,50640.,
        50650.,50740.,50750.,50810.,50840.,50860.,
        50890.,50900.,50930.,50950.,50970.,50990.,
        51090.,51100.,51130.,51190.,51200.,60201.,
        60211.,60821.,60920.,61080.,61120.,61140.,
        70091.,70110.,70611.,70710.,70791.,80241.,
        80291.,80381.,80640.,80660.,80671.,80690.,
        80711.,80771.,
        // 78..149
        80790.,80831.,90291.,
        90431.,90511.,90611.,90741.,100091.,100230.,
        100711.,100740.,100771.,100831.,100960.,
        101071.,110091.,110250.,110581.,110611.,
        110641.,110731.,120100.,130060.,130470.,
        130521.,130611.,130661.,130691.,130770.,
        130990.,140060.,140440.,140800.,150060.,
        150280.,150451.,150511.,150611.,150741.,
        150801.,170461.,170611.,170641.,170751.,
        170801.,180440.,180701.,180710.,180770.,
        180811.,180830.,180911.,180931.,180951.,
        180981.,181001.,181071.,181111.,190080.,
        190090.,190131.,200080.,200090.,200621.,
        200641.,200661.,200670.,200691.,200710.,
        200770.,200791.,
        999990.
    };

    // Normalize COR to uppercase
    COR = static_cast<char>(std::toupper(static_cast<unsigned char>(COR)));

    // Input validation & edge handling (mirrors Fortran behavior)
    if (DIA < 1.0) {
        return 0.0;
    }
    if (DIA > 120.0) {
        DIA = 120.0;
    }

    // Choose factor index (Q9), mimicking Fortran integer cast & rules
    int Q9 = static_cast<int>(DIA);  // truncation toward zero (DIA is positive)
    if (DIA > 5.0 && DIA <= 11.0) {
        if (LEN > 15.0 && LEN < 32.0) {
            Q9 += 115;   // 121..126
        }
        else if (LEN > 31.0 && LEN < 41.0) {
            Q9 += 121;   // 127..132
        }
    }

    if (Q9 < 1 || Q9 > 132) {
        // Defensive check: should not happen with inputs and logic above
        return 0.0;
    }

    const double VOLFAC = FACTOR[Q9];

    // Compute factor volume with AINT semantics
    double VOL = 0.0;
    if (COR == 'Y') {
        // AINT((LEN*VOLFAC+5)/10): truncate toward zero
        VOL = std::trunc((LEN * VOLFAC + 5.0) / 10.0);
    }
    else if (COR == 'N') {
        // AINT(LEN*VOLFAC + 0.5)
        VOL = std::trunc(LEN * VOLFAC + 0.5);
    }
    else {
        // Fortran just printed an error and left VOL possibly uninitialized.
        // Be safer here and return 0.0. If you want exact Fortran behavior, remove this.
        return 0.0;
    }

    // Apply exceptions only if COR == 'Y'
    if (COR == 'Y') {
        int ILOW = 1;
        int IHIGH = 149;

        // ANUM = LEN*1000 + DIA (Fortran REAL); compare against integer COMPAR
        const double ANUM = LEN * 1000.0 + DIA;

        // Binary search like the Fortran loop with GOTO 100
        while (true) {
            const int ISCRPT = (IHIGH + ILOW) / 2;

            // COMPAR = AINT(EXCEPT(ISCRPT)/10) -> truncate -> integer
            const int COMPAR = static_cast<int>(std::trunc(EXCEPT[ISCRPT] / 10.0));

            if (ANUM == static_cast<double>(COMPAR)) {
                // Determine odd/even: XXX = (EXCEPT/2) - INT(EXCEPT/2)
                const double half = EXCEPT[ISCRPT] / 2.0;
                const double XXX = half - std::floor(half); // fractional part
                if (XXX > 0.0) {
                    // odd -> add 1
                    VOL += 1.0;
                }
                else {
                    // even -> subtract 1
                    VOL -= 1.0;
                }
                break;
            }
            else if (ILOW != IHIGH - 1) {
                if (ANUM > static_cast<double>(COMPAR)) {
                    ILOW = ISCRPT;
                }
                else {
                    IHIGH = ISCRPT;
                }
                continue;
            }
            // No match or narrowed to 1-apart -> done
            break;
        }
    }
    if (COR == 'Y') VOL *= 10.0;
    return VOL;
}


// Compute International 1/4 board foot volume for a log.
// DIB: small-end diameter inside bark (inches)
// LENGTH: log length (feet)
// Returns BFINT (board feet)
double intl14(double DIB, double LENGTH)
{
    // If diameter inside bark is less than 4, no volume
    if (DIB < 4.0) {
        return 0.0;
    }

    double LOGVOL = 0.0;

    // Number of full 4' segments (INT -> trunc toward zero)
    int LOGSEG = static_cast<int>(LENGTH / 4.0);

    // Leftover fraction of a 4' segment (0.0, 0.25, 0.5, 0.75 typically)
    double FF = LENGTH / 4.0 - static_cast<double>(LOGSEG);

    // For each 4' segment, compute small end diameter assuming 1/2" taper per segment
    // Fortran: SEDIAM = DIB + (LOGSEG - J)/2.0
    for (int J = 1; J <= LOGSEG; ++J) {
        double SEDIAM = DIB + (static_cast<double>(LOGSEG - J) / 2.0);
        double SEGVOL = (0.22 * SEDIAM * SEDIAM - 0.71 * SEDIAM) * 0.905;
        LOGVOL += SEGVOL;
    }

    // If there's a leftover fraction, take the top 4' piece volume and scale by fraction FF
    if (FF > 0.0) {
        double SEGVOL = FF * (0.22 * DIB * DIB - 0.71 * DIB) * 0.905;
        LOGVOL += SEGVOL;
    }

    // Rounding logic to nearest 5 board feet per original rules
    if (LOGVOL < 7.5) {
        LOGVOL = 5.0;
    }
    else {
        // Largest multiple of 10 not exceeding LOGVOL (INT -> trunc toward zero)
        int IRNDVOL = static_cast<int>(LOGVOL / 10.0);

        // Remaining (ones & tenths) as integer hundredths out of 10 BF unit
        int JJJ = static_cast<int>(((LOGVOL / 10.0) - IRNDVOL) * 100.0);

        if (JJJ < 25) {
            // < 2.5 BF remainder -> round down to 10
            LOGVOL = static_cast<double>(IRNDVOL * 10);
        }
        else if (JJJ >= 75) {
            // >= 7.5 BF remainder -> round up to next 10
            LOGVOL = static_cast<double>((IRNDVOL + 1) * 10);
        }
        else {
            // Else round to 5
            LOGVOL = static_cast<double>(IRNDVOL * 10 + 5);
        }
    }

    return LOGVOL;
}

//Gross Volume of Trees, Doyle Log Scale Form Class 78. Merchantable height in number of 16-foot logs
double treeFormClass78BoardFootTable(double dbh, double numberOf16FootLogs, std::string_view boardFootTable)
{
    static const int DOYAL[31][11] = {
        {14,17,20,21,22,0,0,0,0,0,0},
        {22,27,32,35,38,0,0,0,0,0,0},
        {29,36,43,48,53,54,56,0,0,0,0},
        {38,48,59,66,73,76,80,0,0,0,0},
        {48,62,75,84,93,98,103,0,0,0,0},
        {60,78,96,108,121,128,136,0,0,0,0},
        {72,94,116,132,149,160,170,0,0,0,0},
        {86,113,140,161,182,196,209,0,0,0,0},
        {100,132,164,190,215,232,248,0,0,0,0},
        {118,156,194,225,256,276,297,0,0,0,0},
        {135,180,225,261,297,322,346,364,383,0,0},
        {154,207,260,302,344,374,404,428,452,0,0},
        {174,234,295,344,392,427,462,492,521,0,0},
        {195,264,332,388,444,483,522,558,594,0,0},
        {216,293,370,433,496,539,582,625,668,0,0},
        {241,328,414,486,558,609,660,709,758,0,0},
        {266,362,459,539,619,678,737,793,849,0,0},
        {292,398,505,594,684,749,814,877,940,0,0},
        {317,434,551,650,750,820,890,961,1032,1096,1161},
        {346,475,604,714,824,902,980,1061,1142,1218,1294},
        {376,517,658,778,898,984,1069,1160,1251,1339,1427},
        {408,562,717,850,983,1080,1176,1273,1370,1470,1570},
        {441,608,776,922,1068,1176,1283,1386,1488,1600,1712},
        {474,654,835,994,1152,1268,1385,1497,1609,1734,1858},
        {506,700,894,1064,1235,1361,1487,1608,1730,1866,2003},
        {544,754,964,1149,1334,1472,1610,1743,1876,2020,2163},
        {581,808,1035,1234,1434,1583,1732,1878,2023,2173,2323},
        {618,860,1102,1318,1534,1694,1854,2013,2172,2332,2492},
        {655,912,1170,1402,1635,1805,1975,2148,2322,2491,2660},
        {698,974,1250,1498,1746,1932,2118,2298,2479,2662,2844},
        {740,1035,1330,1594,1858,2059,2260,2448,2636,2832,3027}
    };

    static const int INTL[31][11] = {
        {36,48,59,66,73,0,0,0,0,0,0},
        {46,61,76,86,96,0,0,0,0,0,0},
        {56,74,92,106,120,128,137,0,0,0,0},
        {67,90,112,130,147,158,168,0,0,0,0},
        {78,105,132,153,174,187,200,0,0,0,0},
        {92,124,156,182,208,225,242,0,0,0,0},
        {106,143,180,210,241,263,285,0,0,0,0},
        {121,164,206,242,278,304,330,0,0,0,0},
        {136,184,233,274,314,344,374,0,0,0,0},
        {154,209,264,311,358,392,427,0,0,0,0},
        {171,234,296,348,401,440,480,511,542,0,0},
        {191,262,332,391,450,496,542,579,616,0,0},
        {211,290,368,434,500,552,603,647,691,0,0},
        {231,318,404,478,552,608,663,714,766,0,0},
        {251,346,441,523,605,664,723,782,840,0,0},
        {275,380,484,574,665,732,800,865,930,0,0},
        {299,414,528,626,725,801,877,949,1021,0,0},
        {323,448,572,680,788,870,952,1032,1111,0,0},
        {347,482,616,733,850,938,1027,1114,1201,1280,1358},
        {375,521,667,794,920,1016,1112,1210,1308,1398,1488},
        {403,560,718,854,991,1094,1198,1306,1415,1517,1619},
        {432,602,772,921,1070,1184,1299,1412,1526,1640,1754},
        {462,644,826,988,1149,1274,1400,1518,1637,1762,1888},
        {492,686,880,1053,1226,1360,1495,1622,1750,1888,2026},
        {521,728,934,1119,1304,1447,1590,1727,1864,2014,2163},
        {555,776,998,1196,1394,1548,1702,1851,2000,2156,2312},
        {589,826,1063,1274,1485,1650,1814,1974,2135,2298,2461},
        {622,873,1124,1351,1578,1752,1926,2099,2272,2444,2616},
        {656,921,1186,1428,1670,1854,2038,2224,2410,2590,2771},
        {694,976,1258,1514,1769,1968,2166,2359,2552,2744,2937},
        {731,1030,1329,1598,1868,2081,2294,2494,2693,2898,3103}
    };

    static const int SCRIBNER[31][11] = {
        {28,36,44,48,52,0,0,0,0,0,0},
        {38,49,60,67,74,0,0,0,0,0,0},
        {47,61,75,85,95,100,106,0,0,0,0},
        {58,76,94,107,120,128,136,0,0,0,0},
        {69,92,114,130,146,156,166,0,0,0,0},
        {82,109,136,157,178,192,206,0,0,0,0},
        {95,127,159,185,211,229,247,0,0,0,0},
        {109,146,184,215,246,268,289,0,0,0,0},
        {123,166,209,244,280,306,331,0,0,0,0},
        {140,190,240,281,322,352,382,0,0,0,0},
        {157,214,270,317,364,298,432,459,486,0,0},
        {176,240,304,358,411,450,490,523,556,0,0},
        {194,266,338,398,458,504,549,588,626,0,0},
        {214,294,374,441,508,558,607,652,698,0,0},
        {234,322,409,484,558,611,665,718,770,0,0},
        {258,355,452,534,617,678,740,799,858,0,0},
        {281,388,494,585,676,745,814,880,945,0,0},
        {304,420,536,636,736,811,886,959,1032,0,0},
        {327,452,578,686,795,877,959,1040,1120,1190,1261},
        {354,491,628,746,864,953,1042,1132,1222,1306,1389},
        {382,530,678,806,933,1028,1124,1224,1325,1421,1517},
        {411,571,731,871,1011,1117,1223,1328,1434,1541,1648},
        {440,612,784,936,1089,1206,1322,1432,1543,1661,1779},
        {469,654,838,1001,1164,1280,1414,1534,1654,1783,1912},
        {487,695,892,1066,1239,1373,1507,1636,1766,1906,2046},
        {530,742,954,1141,1328,1473,1618,1757,1896,2044,2192},
        {563,789,1015,1216,1416,1572,1728,1877,2026,2182,2338},
        {596,836,1075,1290,1506,1670,1835,1998,2160,2324,2488},
        {629,882,1135,1366,1596,1769,1942,2118,2295,2466,2637},
        {666,935,1204,1449,1694,1881,2068,2251,2434,2616,2799},
        {703,988,1274,1532,1791,1993,2195,2384,2574,2768,2961}
    };

    double boardFootVol = 0.0;

    if (dbh <= 1.0) {
        return boardFootVol;
    }
    if (numberOf16FootLogs <= 1.0) {
        return boardFootVol;
    }

    int INDEX = static_cast<int>(std::round(dbh)) - 9;
    double numberOfLogs = 0.0;
    if(numberOf16FootLogs > 10) numberOfLogs = numberOf16FootLogs / 10.0;

    if (INDEX < 1 || INDEX > 31) {
        boardFootVol = 0.0;
    }
    else {
        int col = -1;
        if (numberOf16FootLogs == 1.0) col = 0;
        else if (numberOf16FootLogs == 1.5) col = 1;
        else if (numberOf16FootLogs == 2.0) col = 2;
        else if (numberOf16FootLogs == 2.5) col = 3;
        else if (numberOf16FootLogs == 3.0) col = 4;
        else if (numberOf16FootLogs == 3.5) col = 5;
        else if (numberOf16FootLogs == 4.0) col = 6;
        else if (numberOf16FootLogs == 4.5) col = 7;
        else if (numberOf16FootLogs == 5.0) col = 8;
        else if (numberOf16FootLogs == 5.5) col = 9;
        else if (numberOf16FootLogs == 6.0) col = 10;

        if (col >= 0) {
            if (boardFootTable == "D") boardFootVol = DOYAL[INDEX - 1][col];
            else if (boardFootTable == "I") boardFootVol = INTL[INDEX - 1][col];
            else if (boardFootTable == "S") boardFootVol = SCRIBNER[INDEX - 1][col];
        }
            
    }

    return boardFootVol;
}

//Gross Volume of Trees, Doyal, Scribner and International 1/4 inch Log Scale Form Class 78.
//Merchantable height in number of 16-foot logs
double treeFormClass78BoardFootFormula(double dbh, double numberOf16FootLogs, std::string_view boardFootTable) {
    double D = dbh;
    double L = numberOf16FootLogs;
    double D2 = D * D;
    double L2 = L * L;
    double boardFootVol = 0.0;

    //formula from Wiant 1986 for the form class 78 tables
    if (boardFootTable == "D") {
        boardFootVol = 
            (0.55743 * L2 + 41.51275 * L - 29.37337)
            + (2.78043 - 0.04516 * L2 - 8.77272 * L) * D
            + (0.04177 - 0.01578 * L2 + 0.59042 * L) * D2;
    }
    else if (boardFootTable == "S") {
        boardFootVol = 
            (17.53508 * L - 0.59242 * L2 - 22.50365)
            + (3.02988 - 0.02302 * L2 - 4.34381 * L) * D
            + (0.51593 * L - 0.02035 * L2 - 0.01969) * D2;
    }
    else if (boardFootTable == "I") {
        boardFootVol = 
            (1.52968 * L2 + 9.58615 * L - 13.35212)
            + (1.79620 - 0.27465 * L2 - 2.59995 * L) * D
            + (0.04482 - 0.00961 * L2 + 0.45997 * L) * D2;
    }
    return boardFootVol;
}

//BIA Behr equation boardfoot volume
double biaBehBoardfoot(double dia, double loglength) {
    double bf = 0.0;
    if (dia < 8.0) {
        bf = (-0.083714 + 0.018569 * dia + 0.059009
            * std::pow(dia, 2) - 0.003894 * std::pow(dia, 3)) * loglength;
    }
    else {
        bf = (-0.26875 - 0.12375 * dia + 0.049375 *
            std::pow(dia, 2)) * loglength;
    }
    return bf;
}