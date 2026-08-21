#include "MerchHeightCalculator_R89.h"

int findSpeciesIndex(const int speciesList[], int size, int species) {
    for (int i = 0; i < size; ++i) {
        if (speciesList[i] == species) {
            return i;  // Found
        }
    }
    return -1;  // Not found
}

double r89MerchHeight(int region, int forest, int volSpcd,
    double dbh, double totalHeight, double topDia,
    int basalArea, int siteIndex, bool calcPulpHeight, bool logHeight)
{
    int ba = basalArea;
    if (ba == 0) ba = 90;
    int si = siteIndex;
    int spIdx;
    int htSpcd = volSpcd;
    size_t size;
    double b1, b2, b3, b4, b5, b6;
    double dbhMin = 5.0, bfMinDbh = 9.0;

    if (region == 8) {
        if (si == 0) {
            if (forest == 1 || forest == 3 || forest == 12) {
                si = 80;
            }
            else si = 70;
        }
        size = std::size(r8HtSpecies);
        //need check volSpcd with HtSpecies
        if (htSpcd == 100) htSpcd = 131;
        else if (htSpcd == 197) htSpcd = 90;
        else if (htSpcd == 261) htSpcd = 260;
        else if (htSpcd == 300 || htSpcd == 500) htSpcd = 998;
        else if (htSpcd == 314) htSpcd = 318;
        else if (htSpcd == 404) htSpcd = 400;
        else if (htSpcd == 545 || htSpcd == 546) htSpcd = 540;
        else if (htSpcd == 550) htSpcd = 552;
        else if (htSpcd == 742) htSpcd = 740;
        else if (htSpcd == 800 || htSpcd == 804) htSpcd = 802;
        else if (htSpcd == 817 || htSpcd == 831) htSpcd = 827;
        else if (htSpcd == 823 || htSpcd == 828 || htSpcd == 830) htSpcd = 812;
        else if (htSpcd == 930) htSpcd = 931;

        spIdx = findSpeciesIndex(r8HtSpecies, size, htSpcd);
        b1 = r8B1[spIdx];
        b2 = r8B2[spIdx];
        b3 = r8B3[spIdx];
        b4 = r8B4[spIdx];
        b5 = r8B5[spIdx];
        b6 = r8B6[spIdx];
        dbhMin = 4.0;
        bfMinDbh = 10.0;
        if (htSpcd > 299) bfMinDbh = 12.0;
    }
    else if (region == 9 && logHeight) {
        size = std::size(r9HtSpecies);
        spIdx = findSpeciesIndex(r9HtSpecies, size, volSpcd);
        if (forest == 2 || forest == 3 || forest == 4 || forest == 6 ||
            forest == 7 || forest == 9 || forest == 10) {
            if (si == 0) si = 60;
            b1 = LS_B1[spIdx];
            b2 = LS_B2[spIdx];
            b3 = LS_B3[spIdx];
            b4 = LS_B4[spIdx];
            b5 = LS_B5[spIdx];
            b6 = LS_B6[spIdx];
        }
        else if (forest == 5 || forest == 8 || forest == 12) {
            if (si == 0) si = 65;
            b1 = CS_B1[spIdx];
            b2 = CS_B2[spIdx];
            b3 = CS_B3[spIdx];
            b4 = CS_B4[spIdx];
            b5 = CS_B5[spIdx];
            b6 = CS_B6[spIdx];
        }
        else {
            if (si == 0) si = 55;
            b1 = NE_B1[spIdx];
            b2 = NE_B2[spIdx];
            b3 = NE_B3[spIdx];
            b4 = NE_B4[spIdx];
            b5 = NE_B5[spIdx];
            b6 = NE_B6[spIdx];
        }

        if (volSpcd < 300) {
            bfMinDbh = 9.0;
            if (forest == 4 || forest == 5) dbhMin = 4.0;
            else dbhMin = 5.0;
        }
        else {
            if (forest == 4 || forest == 10 || forest == 13 || forest == 21) {
                dbhMin = 5.0;
                bfMinDbh = 11.0;
            }
            else if (forest == 3) {
                if (volSpcd == 746 || volSpcd == 741 || volSpcd == 743) {
                    bfMinDbh = 11.0;
                }
                else bfMinDbh = 9.0;

                dbhMin = 5.0;
            }
            else if (forest == 5) {
                dbhMin = 5.0;
                bfMinDbh = 9.0;
            }
            else if (forest == 7) {
                if (volSpcd == 746 || volSpcd == 741 || volSpcd == 743) {
                    bfMinDbh = 9.0;
                }
                else bfMinDbh = 11.0;

                dbhMin = 5.0;
            }
            else if (forest == 20) {
                if (volSpcd == 375) {
                    bfMinDbh = 9.0;
                }
                else bfMinDbh = 11.0;
                dbhMin = 8.0;
            }
            else if (forest == 22) {
                if (volSpcd == 375) {
                    bfMinDbh = 8.0;
                }
                else bfMinDbh = 11.0;
                dbhMin = 5.0;
            }
            else {
                dbhMin = 6.0;
                bfMinDbh = 11.0;
            }
        }
    }
    else {
        //merch height for Hahn NC-250 volume
        if (ba == 0) ba = 90;
        if (si == 0) si = 60;
        dbhMin = 5.0;
        if (volSpcd < 300) bfMinDbh = 9.0;
        else bfMinDbh = 11.0;

        size = std::size(speciesHahn);
        spIdx = findSpeciesIndex(speciesHahn, size, volSpcd);
        b1 = htCoefHahn[spIdx][1];
        b2 = htCoefHahn[spIdx][2];
        b3 = htCoefHahn[spIdx][3];
        b4 = htCoefHahn[spIdx][4];
        b5 = htCoefHahn[spIdx][5];
        b6 = htCoefHahn[spIdx][6];
    }

    double factor = 0.0;
    double estTotalHeight = 4.5
        + b1 * pow(1.0 - exp(-1.0 * b2 * dbh), b3)
        * pow(si, b4)
        * pow(1.00001 - factor, b5)
        * pow(ba, b6);

    if (totalHeight <= 0.0) totalHeight = estTotalHeight;

    factor = topDia / dbh;
    double estMerchHeight = 4.5
        + b1 * pow(1.0 - exp(-1.0 * b2 * dbh), b3)
        * pow(si, b4)
        * pow(1.00001 - factor, b5)
        * pow(ba, b6);

    double merchHeight = estMerchHeight * (totalHeight / estTotalHeight);
    merchHeight = static_cast<int>(merchHeight);

    if (calcPulpHeight && dbh < dbhMin) merchHeight = 0.0;
    else if (!calcPulpHeight && dbh < bfMinDbh) merchHeight = 0.0;

    if (region == 9 && logHeight) merchHeight = static_cast<int>(merchHeight / 8.33333);

    return merchHeight;
}