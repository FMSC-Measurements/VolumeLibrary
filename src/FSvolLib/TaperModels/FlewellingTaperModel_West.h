#pragma once
#include "FlewellingShapeCoef.h"
#include <string_view>
#include <array>

inline int find_geo_index(std::string_view code) {
    static constexpr std::array<std::string_view, 8> GCODE =
// (alpha to numeric transform is in SF_2PT)
// region order is : CO  EV  NO  RS  SO  WE  WV  WC
//                   OR  OR  WA  WA  WA  WA  OR  WA
//         IREGION : 1   2   3   4   5   6   7   8

    { "01","02","03","04","05","06","07","08" };

    for (int i = 0; i < 8; ++i)
        if (GCODE[i] == code)
            return i;

    return -1; // GEOSUB = "00" or not found
}

//double bark thickness in West
double FDBT_C1(int JSP, std::string_view GEOSUB, double DBHOB, double HTTOT);

FlewellingShapeParams SHP_W3(double DBHOB, double HTTOT, std::string_view geoSub);

FlewellingShapeParams SHP_W4(double DBHOB, double HTTOT, std::string_view geoSub);

FlewellingShapeParams SHP_W5(double DBHOB, double HTTOT, std::string_view geoSub);

double COR_WS(int JSP, double HTTOT, double HI, double HJ);

double Z_from_sf3(double DBHOB, double HTTOT, double H,  double dibMod, double dibAct);
double Z_from_sf4(double DBHOB, double HTTOT, double H,  double dibMod, double dibAct);
double Z_from_sf5(double DBHOB, double HTTOT, double H,  double dibMod, double dibAct);
double dib_from_Z_sf3(double DBHOB, double HTTOT, double H, double dibMod, double Z);
double dib_from_Z_sf4(double DBHOB, double HTTOT, double H, double dibMod, double Z);
double dib_from_Z_sf5(double DBHOB, double HTTOT, double H, double dibMod, double Z);

double VAR_C1(int JSP, double DBHOB, double HTTOT, double H, double dibMod, double dibAct);

double BRK_WS(int JSP, double DBHOB, double HTTOT, double DBTBH, double h);