#include "VolumeEquationResolver.h"

#include <string>
#include <vector>
#include <algorithm>
#include <cctype>

// ----------------- Data tables (from Fortran DATA) -----------------

// LS (Lake States)
static const std::vector<std::string> LSSP = {
    "012","068","071","091","094","095","105","125","125","129",
    "130","241","261","001","313","314","315","316","317","318",
    "319","371","375","391","402","403","407","421","462","491",
    "500","531","541","543","544","601","602","660","693","701",
    "731","741","742","743","746","766","761","762","763","766",
    "802","804","809","823","826","833","837","901","920","922",
    "923","931","935","951","972","975","977","300","999"
};
static const std::vector<int> LSFIA = {
     12,  68,  71,  91,  94,  95, 105, 125, 125, 129,
    130, 241, 261, 299, 313, 314, 315, 316, 317, 318,
    319, 371, 375, 391, 402, 403, 407, 421, 462, 491,
    500, 531, 541, 543, 544, 601, 602, 660, 693, 701,
    731, 741, 742, 743, 746, 760, 761, 762, 763, 766,
    802, 804, 809, 823, 826, 833, 837, 901, 920, 922,
    923, 931, 935, 951, 972, 975, 977, 993, 994
};

// CS (Central States)
static const std::vector<std::string> CSSP = {
    "068","068","110","129","131","132","221","001","316","316",
    "316","318","331","373","391","400","401","402","403","404",
    "405","407","408","409","400","450","460","460","471","490",
    "500","521","531","541","541","543","541","543","543","552",
    "571","601","602","611","621","641","651","653","680","690",
    "691","693","694","701","711","731","742","741","742","746",
    "746","762","802","823","806","812","813","813","813","823",
    "824","823","823","816","813","830","813","832","833","813",
    "835","835","837","901","920","920","931","951","970","970",
    "970","970","970","970","999","999","999"
};
static const std::vector<int> CSFIA = {
     57,  68, 110, 129, 131, 132, 221, 299, 313, 316,
    317, 318, 331, 373, 391, 400, 401, 402, 403, 404,
    405, 407, 408, 409, 410, 450, 461, 462, 471, 491,
    500, 521, 531, 540, 541, 543, 544, 545, 546, 552,
    571, 601, 602, 611, 621, 641, 651, 653, 680, 690,
    691, 693, 694, 701, 711, 731, 740, 741, 742, 743,
    746, 762, 802, 804, 806, 812, 813, 817, 822, 823,
    824, 825, 826, 827, 828, 830, 831, 832, 833, 834,
    835, 836, 837, 901, 920, 922, 931, 951, 970, 971,
    972, 974, 975, 977, 991, 992, 994
};

// NE (Northeast)
static const std::vector<std::string> NESP = {
    "012","068","068","068","071","097","097","094","095","097",
    "105","105","110","105","125","105","105","129","105","131",
    "125","068","261","261","105","313","318","315","316","318",
    "318","330","332","341","355","371","371","373","374","375",
    "375","391","400","400","400","400","400","462","490","500",
    "521","531","541","541","543","541","541","591","601","602",
    "611","621","641","650","651","653","660","691","693","701",
    "711","712","731","741","742","746","742","746","760","761",
    "762","802","802","832","806","833","813","806","823","832",
    "802","806","830","831","832","833","835","837","901","920",
    "931","951","951","970","970","970","999","004"
};
static const std::vector<int> NEFIA = {
     12,  43,  57,  68,  71,  90,  91,  94,  95,  97,
    100, 105, 110, 123, 125, 126, 128, 129, 130, 131,
    132, 241, 260, 261, 299, 313, 314, 315, 316, 317,
    318, 330, 332, 341, 356, 371, 372, 373, 374, 375,
    379, 391, 400, 403, 405, 407, 409, 462, 491, 500,
    521, 531, 540, 541, 543, 544, 545, 591, 601, 602,
    611, 621, 641, 650, 651, 653, 660, 691, 693, 701,
    711, 712, 731, 741, 742, 743, 744, 746, 760, 761,
    762, 800, 802, 804, 806, 812, 813, 817, 823, 825,
    826, 827, 830, 831, 832, 833, 835, 837, 901, 922,
    931, 951, 952, 970, 972, 975, 994, 998
};

// SN (Southern)
static const std::vector<std::string> SNSP = {
    "261","100","115","132","110","111","115","121","126","126","128",
    "129","132","131","132","221","222","261","132","500","500",
    "316","300","500","330","370","370","370","400","300","460",
    "300","300","500","531","541","541","300","544","500","300",
    "300","300","500","500","611","621","652","300","652","653",
    "300","300","300","300","693","694","500","300","300","731",
    "300","300","300","802","806","812","813","800","800","822",
    "800","800","800","827","827","832","833","800","835","800",
    "835","901","300","300","300","970","970","970","970","300","300"
};
static const std::vector<int> SNFIA = {
     10,  57,  90, 107, 110, 111, 115, 121, 123, 126, 128,
    129, 130, 131, 132, 221, 222, 260, 299, 311, 313,
    316, 317, 318, 330, 370, 372, 391, 400, 450, 460,
    471, 491, 521, 531, 540, 541, 543, 544, 552, 555,
    580, 591, 601, 602, 611, 621, 650, 651, 652, 653,
    654, 660, 680, 691, 693, 694, 701, 711, 721, 731,
    740, 743, 762, 802, 806, 812, 813, 819, 820, 822,
    824, 825, 826, 827, 830, 832, 833, 834, 835, 837, 838,
    901, 920, 931, 950, 970, 971, 972, 975, 998, 999
};

// ----------------- Helpers -----------------

// Case-insensitive VAR compare: normalize to uppercase
//static inline std::string to_upper(std::string s) {
//    std::transform(s.begin(), s.end(), s.begin(),
//        unsigned char c{ return static_cast<char>(std::toupper(c)); });
//    return s;
//}

// Pads an integer to 3 digits ('7' -> "007", '73' -> "073")
static inline std::string pad3(int x) {
    char buf[4];
    std::snprintf(buf, sizeof(buf), "%03d", x);
    return std::string(buf);
}

// Binary search on sorted FIA list; returns 1-based index or -1 if not found
static inline int findIndex1Based(const std::vector<int>& sortedFia, int spec) {
    int first = 1, last = static_cast<int>(sortedFia.size());
    int done = 0;
    while (done == 0) {
        int half = (last - first + 1) / 2 + first;
        int idx = half - 1;
        if (sortedFia.at(idx) == spec) {
            return half; // 1-based
        }
        else if (first == last) {
            return -1;
        }
        else if (sortedFia.at(idx) < spec) {
            first = half;
        }
        else {
            last = half - 1;
        }
    }
    return -1;
}

/**
 * @brief C++ translation of Fortran SUBROUTINE R9_EQN(FORST, SPEC, VAR, VOLEQ, ERRFLAG)
 *
 * @param FORST   Two-character forest code (unused in this routine, kept for signature parity).
 * @param SPEC    FIA species code (int). If 9999 → validation branch; may be set to 8888 on success.
 * @param VAR     Two-character variant: "LS", "CS", "NE" (else uses "SN" path) in DVEE mode.
 * @param VOLEQ   Volume equation string (10 chars total). This function sets/updates it.
 * @param ERRFLAG Error flag (0 = OK; 1 = species not found → falls back to last index of chosen list).
 */
VolumeEquation VolumeEquationResolver::GetR9VolumeEquation(VolumeCalculationOptions vco)
//static void R9_EQN_cpp(const std::string& FORST, int& SPEC, const std::string& VAR,
//    std::string& VOLEQ, int& ERRFLAG)
{
    //(void)FORST; // not used in R9 logic
    int ERRFLAG = 0;
    int SPEC = vco.fiaCode;
    std::string VOLEQ;

    

    // --- If VOLEQ already starts with "900CLKE": just set species suffix to padded SPEC ---
    if (VOLEQ.size() >= 7 && VOLEQ.compare(0, 7, "900CLKE") == 0) {
        VOLEQ.resize(7);
        VOLEQ += pad3(SPEC);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);;
    }
    // --- DVEE path: choose species list by VAR and set suffix accordingly ---
    if (VOLEQ.size() >= 7 && VOLEQ.compare(3, 4, "DVEE") == 0) {
        const std::string VARU = GetFvsVariantCode(vco);
        if (VARU == "LS") {
            int idx = findIndex1Based(LSFIA, SPEC);
            if (idx < 0) { ERRFLAG = 1; idx = static_cast<int>(LSFIA.size()); } // fallback to last
            VOLEQ.resize(7);
            VOLEQ += LSSP.at(static_cast<size_t>(idx - 1));
        }
        else if (VARU == "CS") {
            int idx = findIndex1Based(CSFIA, SPEC);
            if (idx < 0) { ERRFLAG = 1; idx = static_cast<int>(CSFIA.size()); }
            VOLEQ.resize(7);
            VOLEQ += CSSP.at(static_cast<size_t>(idx - 1));
        }
        else if (VARU == "NE") {
            int idx = findIndex1Based(NEFIA, SPEC);
            if (idx < 0) { ERRFLAG = 1; idx = static_cast<int>(NEFIA.size()); }
            VOLEQ.resize(7);
            VOLEQ += NESP.at(static_cast<size_t>(idx - 1));
        }
        else {
            // Southern variant running Region 9 forests (default)
            int idx = findIndex1Based(SNFIA, SPEC);
            if (idx < 0) { ERRFLAG = 1; idx = static_cast<int>(SNFIA.size()); }
            VOLEQ.resize(7);
            VOLEQ += SNSP.at(static_cast<size_t>(idx - 1));
        }
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // --- Default: produce a new CLARK profile model code: "900CLKE" + padded SPEC ---
    VOLEQ.assign("900CLKE");
    VOLEQ += pad3(SPEC);
    return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
}

bool VolumeEquationResolver::isValidR9Equation(const std::string& VOLEQ)
{
    // --- Validation branch: check only CLKE/DVEE formats ---
    const bool isCLKE = VOLEQ.size() >= 7 && VOLEQ.compare(0, 7, "900CLKE") == 0;
    const bool isDVEE = VOLEQ.size() >= 7 && VOLEQ.compare(3, 4, "DVEE") == 0;
    if (isCLKE || isDVEE) {
        if (VOLEQ.size() >= 10) {
            const std::string sp3 = VOLEQ.substr(7, 3); // positions 8..10 in Fortran
            if (std::find(LSSP.begin(), LSSP.end(), sp3) != LSSP.end() ||
                std::find(CSSP.begin(), CSSP.end(), sp3) != CSSP.end() ||
                std::find(NESP.begin(), NESP.end(), sp3) != NESP.end() ||
                std::find(SNSP.begin(), SNSP.end(), sp3) != SNSP.end()) {
                return true;
            }
        }
        return false; // not a valid Region 9 code (prefix ok, suffix not in lists)
    }
    else {
        // NOT a valid Region 9 equation (prefix doesn't match)
        return false;
    }
}