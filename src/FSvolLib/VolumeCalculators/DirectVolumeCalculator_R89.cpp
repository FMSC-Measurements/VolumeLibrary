#include "DirectVolumeCalculator_R89.h"
#include "MerchHeightCalculator_R89.h"

#include <cmath>
#include <string>

double VEQ(double DBHOB, double HT, double MODEL,
    double B0, double B1, double B2, double B3, double B4,
    double B5, double B6, double B7, double B8,
    int& ERRFLAG)
{
    ERRFLAG = 0;

    double VEQ = 0.0;
    double D2H = DBHOB * DBHOB * HT;

    if (D2H <= 0.0) {
        ERRFLAG = 5;
        return 0.0;
    }

    double D = DBHOB;
    double VTEMP = 0.0;

    if (MODEL == 1.0) {
        VTEMP = B0 + B1 * D2H;
    }
    else if (MODEL == 2.0) {
        VTEMP = B0 + B1 / D + B2 * D * HT + B3 * D2H;
    }
    else if (MODEL == 3.0) {
        VTEMP = B0 + B1 * D * D + B2 * D * HT * HT +
            B3 * D2H + B4 * HT * HT * HT;
    }
    else if (MODEL == 5.0) {
        VTEMP = B0 + B1 * D + B2 * D * D +
            B3 * HT * HT + B4 * D2H + B5 * D * D2H;
    }
    else if (MODEL == 6.0) {
        VTEMP = B0 + B1 * D * D + B2 * HT + B3 * D2H;
    }
    else if (MODEL == 7.0) {
        VTEMP = std::exp(
            B0 * std::pow(HT, B1) *
            std::pow((1.0 - std::pow(4.0 / (0.78 * D), 2.0)), 2.0 * B2)
        );
    }
    else if (MODEL == 9.0) {
        VTEMP = B0 +
            B1 * D2H / 1000.0 +
            B2 * D * HT * HT / 1000.0 +
            B3 * D * D +
            B4 * HT * HT * HT / 10000.0 +
            B5 * D * D / HT +
            B6 * D * D * HT * HT / 100000.0 +
            B7 * D2H * HT * HT / 100000000.0 +
            B8 * D * HT / 100.0;
    }

    VEQ = VTEMP;
    return VEQ;
}

int findSpeciesIndex(int species, const int arr[], int size) {
    for (int i = 0; i < size; i++) {
        if (arr[i] == species) {
            return i;   // found
        }
    }
    return -1;  // not found
}

TreeOutput r8Lasher(const std::string& VOLEQ, TreeMeasurment tree, VolumeCalculationOptions vco, MerchRules merchRules) {
    TreeOutput out;
    int errflag = 0;
    double dbh = tree.dbh;
    double totalHeight = tree.totalHeight;
    double ht1 = tree.merchHeightSaw;
    double ht2 = tree.merchHeightNonsaw;
    int volSp = std::stoi(VOLEQ.substr(7, 3));
    if (volSp == 268) volSp = 261;

    if (ht1 == 0.0) {
        ht1 = r89MerchHeight(vco.region, vco.forest, volSp, tree.dbh, tree.totalHeight, merchRules.minTopDibSaw, vco.basalArea, vco.siteIndex);
    }

    if (ht2 == 0.0) {
        bool calcPulpHeight = true;
        ht2 = r89MerchHeight(vco.region, vco.forest, volSp, tree.dbh, tree.totalHeight, merchRules.minTopDibNonSaw, vco.basalArea, vco.siteIndex, calcPulpHeight);
    }
    int spIdx = findSpeciesIndex(volSp, r8spList, 78);
    if (spIdx <= 0) {
        out.errflag = 6;
        return out;
    }

    int geoCode = std::stoi(VOLEQ.substr(1, 2));
    if (geoCode == 0 || geoCode > 32) {
        out.errflag = 1;
        return out;
    }

    int coefEqIdxBf  = r8vlist[geoCode - 1][spIdx][1] - 1;
    int coefEqIdxCf  = r8vlist[geoCode - 1][spIdx][2] - 1;
    int coefEqIdxCf2 = r8vlist[geoCode - 1][spIdx][3] - 1;
    int coefEqIdxTw  = r8vlist[geoCode - 1][spIdx][4] - 1;

    if (vco.primaryProduct == 1) {
        out.grossBoardFootPrimary = VEQ(dbh, ht1,
            COFARR[coefEqIdxBf][0], COFARR[coefEqIdxBf][1], COFARR[coefEqIdxBf][2],
            COFARR[coefEqIdxBf][3], COFARR[coefEqIdxBf][4], COFARR[coefEqIdxBf][5], 
            COFARR[coefEqIdxBf][6], COFARR[coefEqIdxBf][7], COFARR[coefEqIdxBf][8],
            COFARR[coefEqIdxBf][9], errflag);

        out.grossCubicFootPrimary = VEQ(dbh, ht1,
            COFARR[coefEqIdxCf][0], COFARR[coefEqIdxCf][1], COFARR[coefEqIdxCf][2],
            COFARR[coefEqIdxCf][3], COFARR[coefEqIdxCf][4], COFARR[coefEqIdxCf][5],
            COFARR[coefEqIdxCf][6], COFARR[coefEqIdxCf][7], COFARR[coefEqIdxCf][8],
            COFARR[coefEqIdxCf][9], errflag);

        out.grossCubicFootSecondary = out.grossCubicFootPrimary * (VEQ(dbh, ht1,
            COFARR[coefEqIdxTw][0], COFARR[coefEqIdxTw][1], COFARR[coefEqIdxTw][2],
            COFARR[coefEqIdxTw][3], COFARR[coefEqIdxTw][4], COFARR[coefEqIdxTw][5],
            COFARR[coefEqIdxTw][6], COFARR[coefEqIdxTw][7], COFARR[coefEqIdxTw][8],
            COFARR[coefEqIdxTw][9], errflag) - 1.0);
    }
    else {
        out.grossCubicFootPrimary = VEQ(dbh, ht2,
            COFARR[coefEqIdxCf2][0], COFARR[coefEqIdxCf2][1], COFARR[coefEqIdxCf2][2],
            COFARR[coefEqIdxCf2][3], COFARR[coefEqIdxCf2][4], COFARR[coefEqIdxCf2][5],
            COFARR[coefEqIdxCf2][6], COFARR[coefEqIdxCf2][7], COFARR[coefEqIdxCf2][8],
            COFARR[coefEqIdxCf2][9], errflag);
    }
    out.errflag = errflag;

    return out;
}

TreeOutput r9Gevorkiantz(const std::string& VOLEQ, TreeMeasurment tree, VolumeCalculationOptions vco, MerchRules merchRules) {
    TreeOutput out;
    int errflag = 0;
    int forest = vco.forest;
    double dbh = tree.dbh;
    double totalHeight = tree.totalHeight;
    double ht1 = tree.merchHeightSaw;
    double ht2 = tree.merchHeightNonsaw;
    int volSp = std::stoi(VOLEQ.substr(7, 3));
    int volTable = std::stoi(VOLEQ.substr(0, 3));
    double scribnerBfVol = 0.0;
    double intlBfVol = 0.0;
    double cfVol = 0.0;
    double cordVol = 0.0;
    double topwoodCf = 0.0;
    double topwoodCord = 0.0;

    out.totalCubicFoot = 0.42 * 3.141592 * (1.0 / 144.0) * (1.0 / 4.0) * dbh * dbh * totalHeight;

    if (vco.volumeCalculationOptions == VolumeCalculationType::FVS) {
        if (ht1 == 0 && ht2 == 0.0) {
            ht1 = r89MerchHeight(vco.region, vco.forest, volSp, tree.dbh, tree.totalHeight, merchRules.minTopDibSaw, vco.basalArea, vco.siteIndex);

            bool calcPulpHeight = true;
            ht2 = r89MerchHeight(vco.region, vco.forest, volSp, tree.dbh, tree.totalHeight, merchRules.minTopDibNonSaw, vco.basalArea, vco.siteIndex, calcPulpHeight);
        }

    }
    else if (vco.volumeCalculationOptions == VolumeCalculationType::CRUISE) {
        if (ht1 == 0) {
            out.errflag = 9;
            return out;
        }
    }

    //convert saw height and pulp height to number of 8' logs
    double ht1Logs = static_cast<int>(ht1 + 3.0 / 8.25);
    double ht2Logs = static_cast<int>(ht2 + 3.0 / 8.25);

    if (ht2 == 0.0) {
        ht2Logs = static_cast<int>(totalHeight - 12.0 / 8.25);
    }

    double CF = 1.0;

    if (ht1Logs > 0.0) {
        double R = ((ht1Logs * dbh - 3.75) / (24.0 * ht1Logs - 10.5));
        double VC = ht1Logs * (1.0757 + 3.002 * R + 8.3776 * std::pow(R, 2));

        //Table A
        if (volTable == 901 || forest == 2 || forest == 3 || forest == 6 ||
            forest == 7 || forest == 9 || forest == 10 || forest == 13) {

            if (volSp == 94) {
                CF = 0.90;
            }
            else if (volSp == 105) {
                CF = 0.92;
            }
            else if (volSp == 125) {
                if (vco.forest == 3) {
                    CF = 0.91;
                }
                else {
                    CF = 0.96;
                }
            }
            else if (volSp == 129) {
                if (vco.forest == 3) {
                    CF = 0.90;
                }
                else {
                    CF = 0.96;
                }
            }
            else if (volSp == 241) {
                CF = 0.80;
            }
            else if (volSp == 261) {
                CF = 0.95;
            }
            else if (volSp == 318) {
                CF = 0.98;
            }
            else if (volSp == 375) {
                CF = 0.93;
            }
            else if (volSp == 531) {
                CF = 1.15;
            }
            else if (volSp == 541) {
                CF = 1.03;
            }
            else if (volSp == 951) {
                CF = 0.97;
            }
            else if (volSp == 970) {
                CF = 1.05;
            }

            scribnerBfVol = 5.527702
                - 4.22 * VC
                - 2.4082 * dbh
                + 5.4072 * ht1Logs;

            scribnerBfVol += 0.2114 * (dbh * dbh * ht1Logs)
                - 0.48097 * (ht1Logs * ht1Logs)
                - 0.00605 * (VC * VC)
                + 0.291865 * (dbh * dbh)
                + 0.00501 * (dbh * dbh * ht1Logs * ht1Logs)
                - 0.00039606323 * (dbh * dbh * ht1Logs * ht1Logs * ht1Logs)
                + 0.0000013109952 * (std::pow(dbh, 4) * std::pow(ht1Logs, 2));

            scribnerBfVol *= CF;
        }
        //Table B
        else if (volTable == 902 || forest == 4 || forest == 5 || forest == 8 ||
            forest == 11 || forest == 12 || forest == 14 || forest == 24) {

            // Species-based CF selection
            if (volSp == 400) {
                CF = 1.06;
            }
            else if (volSp == 602) {
                CF = 0.90;
            }
            else if (volSp == 621) {
                CF = 1.10;
            }
            else if (volSp == 694) {
                CF = 1.15;
            }
            else if (volSp == 731) {
                CF = 0.93;
            }
            else if (volSp == 742) {
                CF = 0.97;
            }
            else if (volSp == 824) {
                CF = 0.80;
            }
            else if (volSp == 830) {
                CF = 0.96;
            }
            else if (volSp == 832) {
                CF = 1.03;
            }
            else if (volSp == 68 && vco.forest != 4) {
                CF = 0.80;
            }
            else if (volSp == 110 && (vco.forest == 8 || vco.forest == 5)) {
                CF = 0.95;
            }
            else if (volSp == 125 && vco.forest == 4) {
                CF = 0.96;
            }
            else if (volSp == 129) {
                if (vco.forest == 11 || vco.forest == 14 || vco.forest == 12)
                    CF = 0.95;
                if (vco.forest == 4)
                    CF = 0.96;
            }
            else if (volSp == 241 && vco.forest == 4) {
                CF = 0.80;
            }
            else if (volSp == 802) {
                if (vco.forest == 8) CF = 1.08;
                if (vco.forest == 5) CF = 0.96;
            }
            else if (volSp == 806) {
                if (vco.forest == 5)
                    CF = 1.03;
                else
                    CF = 1.10;
            }
            else if (volSp == 833) {
                if (vco.forest == 8)
                    CF = 1.11;
                else
                    CF = 1.06;
            }
            else if (volSp == 835) {
                if (vco.forest == 5)
                    CF = 0.94;
                else
                    CF = 0.98;
            }
            else if (volSp == 837) {
                if (vco.forest == 8) CF = 1.05;
                if (vco.forest == 5) CF = 0.96;
                if (vco.forest == 4) CF = 0.95;
            }

            // Volume calculation: VOL(2) -> VOL[1]
            scribnerBfVol = -0.092685
                - 5.98 * VC
                - 2.9715 * dbh
                + 16.7022 * ht1Logs;

            scribnerBfVol += 0.2471 * (dbh * dbh * ht1Logs)
                - 0.91751 * (ht1Logs * ht1Logs)
                - 0.00876 * (VC * VC)
                + 0.351046 * (dbh * dbh)
                + 0.00451 * (dbh * dbh * ht1Logs * ht1Logs)
                - 0.00030183475 * (dbh * dbh * ht1Logs * ht1Logs * ht1Logs)
                + 0.0000019222413 * (std::pow(dbh, 4) * std::pow(ht1Logs, 2));

            scribnerBfVol *= CF;
        }
        //Table C
        else if (volTable == 903 || forest == 19) {
            if (volSp == 129) {
                CF = 1.06;
            }
            else if (volSp == 261) {
                CF = 0.88;
            }
            else if (volSp == 531) {
                CF = 1.09;
            }
            else if (volSp == 541 || volSp == 543 ||
                volSp == 621 || volSp == 951) {
                CF = 0.91;
            }
            else if (volSp == 806 || volSp == 813 ||
                volSp == 823 || volSp == 824 ||
                volSp == 830 || volSp == 833 ||
                volSp == 835 || volSp == 837) {
                CF = 0.97;
            }

            scribnerBfVol = 12.900801
                + 4.48 * VC
                + 1.2218 * dbh
                - 18.267 * ht1Logs;

            scribnerBfVol += 0.1993 * (dbh * dbh * ht1Logs)
                - 0.37290 * (ht1Logs * ht1Logs)
                + 0.02714 * (VC * VC)
                - 0.15071 * (dbh * dbh)
                - 0.01703 * (dbh * dbh * ht1Logs * ht1Logs)
                + 0.00039033000 * (dbh * dbh * ht1Logs * ht1Logs * ht1Logs)
                - 0.0000066236 * (std::pow(dbh, 4) * std::pow(ht1Logs, 2));

            scribnerBfVol *= CF;
        }
        //Table D
        else if (volTable == 904 || forest == 20 || forest == 22 || forest == 30) {
            // CF selection
            if (volSp == 12 || volSp == 261) {
                CF = 0.86;
            }
            else if (volSp == 94 || volSp == 97 ||
                volSp == 125 || volSp == 129) {
                CF = 0.97;
            }
            else if (volSp == 316 || volSp == 746 ||
                volSp == 762 || volSp == 833 ||
                volSp == 951 || volSp == 970) {
                CF = 0.94;
            }
            else if (volSp == 375 || volSp == 541 ||
                volSp == 376) {
                CF = 0.92;
            }
            else if (volSp == 371) {
                if (vco.forest == 20)
                    CF = 0.92;
                else if (vco.forest == 22)
                    CF = 0.97;
            }

            scribnerBfVol = -3.592279
                - 2.74 * VC
                - 0.6274 * dbh
                + 1.5333 * ht1Logs;

            scribnerBfVol += 0.2697 * (dbh * dbh * ht1Logs)
                + 0.10400 * (ht1Logs * ht1Logs)
                + 0.00009 * (VC * VC)
                + 0.14129 * (dbh * dbh)
                - 0.01104 * (dbh * dbh * ht1Logs * ht1Logs)
                + 0.00029155 * (dbh * dbh * ht1Logs * ht1Logs * ht1Logs)
                - 0.00000007493 * (std::pow(dbh, 4) * std::pow(ht1Logs, 2));

            scribnerBfVol *= CF;
        }
        //Table E
        else if (volTable == 905 || (forest == 21 && volSp >= 300)) {
            // CF selection
            if (volSp == 316 || volSp == 371 || volSp == 802 || volSp == 951) {
                CF = 1.03;
            }
            else if (volSp == 318) {
                CF = 1.09;
            }
            else if (volSp == 373) {
                CF = 0.94;
            }
            else if (volSp == 531) {
                CF = 1.12;
            }
            else if (volSp == 541) {
                CF = 0.97;
            }
            else if (volSp == 621 || volSp == 651 || volSp == 261) {
                CF = 1.06;
            }
            else if (volSp == 833) {
                CF = 0.91;
            }

            scribnerBfVol = 16.775055
                + 4.06 * VC
                + 0.8709 * dbh
                - 18.343 * ht1Logs;

            scribnerBfVol += 0.1940 * (dbh * dbh * ht1Logs)
                - 0.19833 * (ht1Logs * ht1Logs)
                + 0.02401 * (VC * VC)
                - 0.13224 * (dbh * dbh)
                - 0.0163 * (dbh * dbh * ht1Logs * ht1Logs)
                + 0.00037154 * (dbh * dbh * ht1Logs * ht1Logs * ht1Logs)
                - 0.0000057358 * (std::pow(dbh, 4) * std::pow(ht1Logs, 2));

            scribnerBfVol *= CF;
        }
        //Table F
        else if (volTable == 906 || (forest == 21 && volSp < 300)) {
            if (volSp == 97) {
                CF = 1.15;
            }
            else if (volSp == 129) {
                CF = 0.94;
            }

            // Volume calculation
            scribnerBfVol = 14.522237
                - 9.40 * VC
                - 0.415 * dbh
                + 15.639 * ht1Logs;

            scribnerBfVol += 0.3655 * (dbh * dbh * ht1Logs)
                + 0.00563 * (ht1Logs * ht1Logs)
                - 0.01959 * (VC * VC)
                + 0.3088 * (dbh * dbh)
                - 0.00844 * (dbh * dbh * ht1Logs * ht1Logs)
                + 0.00030875 * (dbh * dbh * ht1Logs * ht1Logs * ht1Logs)
                + 0.0000055105 * (std::pow(dbh, 4) * std::pow(ht1Logs, 2));

            scribnerBfVol *= CF;
        }
        intlBfVol = scribnerBfVol;
        out.grossBoardFootPrimary = scribnerBfVol;
        out.grossInternationalBoardFoot = intlBfVol;
    }

    //Cubic foot volume
    if ((volTable == 911 ||vco.primaryProduct != 1) && ht2Logs > 0.0) {
        cfVol =
            (0.001 * (dbh * dbh)) *
            (1.9 + (0.01 * dbh)) *
            ((0.208 * ht2Logs)
                - (0.009984 * (ht2Logs * ht2Logs))
                + (0.04 / ht2Logs)) *
            79;
        //out.grossCubicFootPrimary = cfVol;
    }
    else if ((volTable == 912 || vco.primaryProduct == 1) && ht1Logs > 0.0) {

        if (volSp == 71 || volSp == 94 || volSp == 95 ||
            volSp == 97 || volSp == 105 || volSp == 241 ||
            volSp == 460 || volSp == 543 || volSp == 601 ||
            volSp == 602 || volSp == 731 || volSp == 742 ||
            volSp == 823 || volSp == 824) {
            CF = 0.95;
        }
        else if (volSp == 400 || volSp == 404 || volSp == 651 ||
            volSp == 694 || volSp == 813 || volSp == 830) {
            CF = 1.05;
        }
        else if (volSp == 531) {
            CF = 1.10;
        }
        else if (volSp == 920) {
            CF = 0.90;
        }
        else if (volSp == 970) {
            CF = 1.08;
        }
        else if (dbh < 13.0 && volSp == 110) {
            CF = 1.06;
        }
        else if (dbh < 15.0) {
            if (volSp == 621 || volSp == 746) {
                CF = 1.03;
            }
            else if (volSp == 125) {
                CF = 1.04;
            }
            else if (volSp == 837) {
                CF = 1.05;
            }
            else if (volSp == 835 || volSp == 951) {
                CF = 1.06;
            }
            else if (volSp == 371 || volSp == 833) {
                CF = 1.08;
            }
            else if (volSp == 129 || volSp == 318 || volSp == 802) {
                CF = 1.10;
            }
            else if (volSp == 806) {
                CF = 1.11;
            }
            else if (volSp == 375) {
                CF = 1.12;
            }
            else if (volSp == 762) {
                CF = 1.16;
            }
            else if (volSp == 316) {
                CF = 1.18;
            }
        }
        else {   // dbh >= 15.0
            if (volSp == 746) {
                CF = 0.95;
            }
            else if (volSp == 129) {
                CF = 0.96;
            }
            else if (volSp == 835) {
                CF = 1.01;
            }
            else if (volSp == 371) {
                CF = 1.03;
            }
            else if (volSp == 318 || volSp == 375 || volSp == 951) {
                CF = 1.04;
            }
            else if (volSp == 833) {
                CF = 1.05;
            }
            else if (volSp == 837) {
                CF = 1.06;
            }
            else if (volSp == 802) {
                CF = 1.07;
            }
            else if (volSp == 621) {
                CF = 1.08;
            }
            else if (volSp == 762 || volSp == 806) {
                CF = 1.09;
            }
            else if (volSp == 316) {
                CF = 1.12;
            }
        }

        // Compute term1, term2, term3
        double term1 =
            -1.70774
            + 0.051321 * dbh
            + 0.58857 * ht1Logs
            + 0.0193547 * (dbh * dbh)
            + 0.0237324 * (ht1Logs * dbh * dbh);

        double term2 =
            -(0.04821 * (ht1Logs * ht1Logs))
            - (0.0002174 * dbh * dbh * ht1Logs * ht1Logs)
            - (0.0000239 * dbh * dbh * ht1Logs * ht1Logs * ht1Logs)
            + (0.00000795 * dbh * dbh * dbh * ht1Logs * ht1Logs);

        double term3 =
            -(0.00000057 * dbh * dbh * dbh * ht1Logs * ht1Logs * ht1Logs)
            - (0.000000035 * dbh * dbh * dbh * dbh * ht1Logs * ht1Logs);

        cfVol = (term1 + term2 + term3) * CF;
    }
    out.grossCubicFootPrimary = cfVol;

    //Cord volume
    if ((volTable == 921 || vco.primaryProduct != 1) && ht2Logs > 0.0) {
        cordVol =
            (0.001 * (dbh * dbh)) *
            (1.9 + (0.01 * dbh)) *
            ((0.208 * ht2Logs)
                - (0.009984 * (ht2Logs * ht2Logs))
                + (0.04 / ht2Logs));
    }
    out.cordMerchantable = cordVol;

    //Topwood volume
    if (ht2Logs > 0.0 && vco.primaryProduct == 1) {
        // Compute GCB
        double GCB =
            (0.001 * (dbh * dbh)) *
            (1.9 + (0.01 * dbh)) *
            ((0.208 * ht2Logs)
                - (0.009984 * (ht2Logs * ht2Logs))
                + (0.04 / ht2Logs)) *
            79.0;

        double CRD =
            (0.001 * (dbh * dbh)) *
            (1.9 + (0.01 * dbh)) *
            ((0.208 * ht2Logs)
                - (0.009984 * (ht2Logs * ht2Logs))
                + (0.04 / ht2Logs));

        // Compute VOL(7) -> VOL[6]
        if (ht1Logs > 0) {
            double P = (ht1Logs / ht2Logs) * 100.0;
            double PT = (98.461 - (1.394 * P) + (0.004 * P * P)) * 0.01;
            topwoodCf = PT * GCB;
            topwoodCord = PT * CRD;
        }
        else {
            topwoodCf = GCB;
            topwoodCord = CRD;
        }
        out.grossCubicFootSecondary = topwoodCf;

    }

    return out;
}

double volumeHahn(double C0, double C1, double D, double H) {
    return C0 + C1 * std::pow(D, 2) * H;
}

TreeOutput r9Hahn(const std::string& VOLEQ, TreeMeasurment tree, VolumeCalculationOptions vco, MerchRules merchRules) {
    TreeOutput out;

    static const double volCoef[49][6] = {
     {12,0.2514,0.002679,4.658,0.01694,    .009967},
     { 68,0.5905,0.002168,17.167,0.01404,   .008877},
     { 71,1.4109,0.002227,17.592,0.01427,   .008877},
     { 94,0.3365,0.002599,6.81,0.01611,     .010699},
     { 95,0.2631,0.002706,0,0.01735,        .008877},
     { 105,1.2446,0.002165,19.149,0.01307,  .007017},
     { 125,2.0822,0.002046,24.848,0.01298,  .007176},
     { 129,0,0.002364,0,0.01625,            .008269},
     { 132,2.0822,0.002046,24.848,0.01298,  .007176},
     { 241,1.0406,0.002408,12.532,0.0156,   .011946},
     { 261,0,0.001856,0,0.01054,            .008579},
     { 299,0.5905,0.002168,17.167,0.01404,  .008877},
     { 313,1.7283,0.002189,63.992,0.01215,  .008476},
     { 314,1.3746,0.002206,36.859,0.01534,  .008894},
     { 316,1.7283,0.002189,63.992,0.01215,  .008476},
     { 317,1.7283,0.002189,63.992,0.01215,  .008476},
     { 318,1.3746,0.002206,36.859,0.01534,  .008894},
     { 371,0,0.00248,14.575,0.01766,        .009968},
     { 373,0,0.002485,34.832,0.01458,       .008380},
     { 375,2.3037,0.00181,37.619,0.01404,   .008380},
     { 400,2.4364,0.001881,57.449,0.01122,  .008980},
     { 460,0,0.002325,28.875,0.01466,       .010422},
     { 531,2.2793,0.002395,56.5,0.01465,    .010202},
     { 541,1.528,0.002021,65.124,0.01124,   .008728},
     { 543,2.599,0.001792,70.167,0.01015,   .011016},
     { 544,1.528,0.002021,65.124,0.01124,   .008728},
     { 601,2.6341,0.001887,67.801,0.01109,  .008980},
     { 602,2.6341,0.001887,67.801,0.01109,  .008980},
     { 611,0,0.002485,34.832,0.01458,       .008980},
     { 621,2.6341,0.001887,67.801,0.01109,  .008980},
     { 694,0,0.002485,34.832,0.01458,       .008980},
     { 731,0,0.002485,34.832,0.01458,       .008980},
     { 741,0,0.002335,17.978,0.01578,       .006594},
     { 742,0,0.002485,34.832,0.01458,       .011145},
     { 743,0.9461,0.002247,31.842,0.01483,  .006594},
     { 746,2.0756,0.001913,29.329,0.0148,   .007369},
     { 762,2.6341,0.001887,67.801,0.01109,  .008980},
     { 802,0.7316,0.001951,46.038,0.01173,  .009727},
     { 806,0.7554,0.002174,34.677,0.0137,   .009727},
     { 812,0.7554,0.002174,34.677,0.0137,   .009727},
     { 830,0.7554,0.002174,34.677,0.0137,   .009727},
     { 833,1.6378,0.002032,41.41,0.01326,   .008908},
     { 834,0.7554,0.002174,34.677,0.0137,   .009727},
     { 837,0.7554,0.002174,34.677,0.0137,   .009727},
     { 920,0,0.002485,34.832,0.01458,       .011145},
     { 951,0.9239,0.002206,36.821,0.01435,  .009639},
     { 970,0,0.002325,28.875,0.01466,       .010422},
     { 998,1.4824,0.001796,36.341,0.01339,  .008980},
     { 999,0.867,0.00194,36.341,0.01339,    .008980} 
    };

    int errflag = 0;
    double dbh = tree.dbh;
    double totalHeight = tree.totalHeight;
    double ht1 = tree.merchHeightSaw;
    double ht2 = tree.merchHeightNonsaw;
    int volSp = std::stoi(VOLEQ.substr(7, 3));
    double C0, C1, D0, D1, S0;

    size_t size = std::size(speciesHahn);
    int spIdx = findSpeciesIndex(speciesHahn, size, volSp);

    C0 = volCoef[spIdx][1];
    C1 = volCoef[spIdx][2];
    D0 = volCoef[spIdx][3];
    D1 = volCoef[spIdx][4];
    S0 = volCoef[spIdx][5];

    double totalCubic = volumeHahn(C0, C1, dbh, totalHeight);
    out.totalCubicFoot = totalCubic;

    double merchCf = 0.0;
    bool calcPulpHeight = true;
    bool logHeight = false;

    if (ht2 == 0.0) {
        ht2 = r89MerchHeight(vco.region, vco.forest, volSp, dbh, totalHeight, merchRules.minTopDibNonSaw, vco.basalArea, vco.siteIndex, calcPulpHeight, logHeight);
    }
    merchCf = volumeHahn(C0, C1, dbh, ht2);
    out.grossCubicFootPrimary = merchCf;

    if (vco.primaryProduct == 1) {
        if (ht1 == 0.0) {
            calcPulpHeight = false;
            ht1 = r89MerchHeight(vco.region, vco.forest, volSp, dbh, totalHeight, merchRules.minTopDibSaw, vco.basalArea, vco.siteIndex, calcPulpHeight, logHeight);
        }
        double sawCf = volumeHahn(C0, C1, dbh, ht1);
        out.grossCubicFootPrimary = sawCf;
        out.grossCubicFootSecondary = merchCf - sawCf;

        double intlBf = volumeHahn(D0, D1, dbh, ht1);
        out.grossInternationalBoardFoot = intlBf;
        out.grossBoardFootPrimary = intlBf * 0.89;
    }

    out.stumpCubicFoot = S0 * dbh * dbh;


    return out;
}