#pragma once
#include <cmath>
#include "..\Models\TreeMeasurment.h"
#include "..\Models\TreeOutput.h"
#include "..\array_helper.h"
#include "..\Models\MerchRules.h"

// --- Helper tariff functions -------------------------------------------------
inline double BA(double DBH) {
    return 0.005454154 * DBH * DBH;
}

inline double TRF0(double CVTS, double DBH) {
    return (CVTS * 0.912733) /
        ((1.033 * (1.0 + 1.382937 * exp(-4.015292 * (DBH / 10.0))))
            * (0.005454154 * DBH * DBH + 0.087266) - 0.174533);
}

inline double TRF1(double CVT, double DBH) {
    return CVT * 0.912733 /
        ((0.9679 - 0.1051 * pow(0.5523, DBH - 1.5)) *
            ((1.033 * (1.0 + 1.382937 * exp(-4.015292 * DBH / 10.0))) *
                (0.005454154 * DBH * DBH + 0.087266) - 0.174533));
}

inline double TRF4(double CV4, double DBH) {
    return CV4 * 0.912733 / (0.005454154 * DBH * DBH - 0.087266);
}

inline double TRF8(double CV8, double DBH) {
    return (CV8 * 0.912733) /
        ((0.983 - 0.983 * pow(0.65, DBH - 8.6)) *
            (0.005454154 * DBH * DBH - 0.087266));
}

TreeOutput DNR24_Tarif_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules,
    double CV,
    const std::string& CVTYPE, double CV4_in = 0.0);

double Browne_CVTS(const std::string& VOLEQ, double DBH, double HT);

double Bell_MountainHemlock_CVTS(double DBH, double HT);

double King_DouglasFir_CVTS(double DBH, double HT);

double Summerfield_CVTS(int SPN, double DBH, double HT);

double Chamber_WesternHemlock_CVTS(double DBH, double HT);

double Chittester_WesternJuniper_CVTS(double DBH, double HT);

double Pillsbury_CV8(const std::string& VOLEQ, double DBH, double HT);

double Curtis_RedAlder_CVT(double DBH, double THT);

double Krumland_CVTS(int SPN, double DBH, double HT);

double Maclean_CV4(int SPN, double DBH, double THT);

double Maclean_Otherhardwood_CVTS(double DBH, double HT);

TreeOutput MacLean_Tarif_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules); 

TreeOutput PNW_Tarif_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules); 

TreeOutput DeMars_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules);

TreeOutput Embry_Vol(int SPN, TreeMeasurment tree, MerchRules merchRules);

TreeOutput DeMars_Embry_Vol(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules);

TreeOutput Browne_DeMars_Embry_Vol(std::string VOLEQ, TreeMeasurment tree, MerchRules merchRules);