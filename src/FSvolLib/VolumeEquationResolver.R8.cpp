#include "VolumeEquationResolver.h"

#include <string>
#include <vector>
#include <algorithm>
#include <cctype>

// ---------- Tables (from the updated Fortran DATA) ----------

// SNFIA (110 entries)
static const std::vector<int> SNFIA_R8 = {
     10,  57,  90, 100, 107, 110, 111, 115, 121, 123,
    126, 128, 129, 130, 131, 132, 197, 221, 222, 260,
    261, 299, 300, 311, 313, 314, 316, 317, 318, 330,
    370, 372, 391, 400, 404, 450, 460, 471, 491, 500,
    521, 531, 540, 541, 543, 544, 545, 546, 550, 552,
    555, 580, 591, 601, 602, 611, 621, 650, 651, 652,
    653, 654, 660, 680, 691, 693, 694, 701, 711, 721,
    731, 740, 742, 743, 762, 800, 802, 804, 806, 812,
    813, 817, 819, 820, 822, 823, 824, 825, 826, 827,
    828, 830, 831, 832, 833, 834, 835, 837, 838, 901,
    920, 930, 931, 950, 970, 971, 972, 975, 998, 999
};

// SNSP (110 entries; each is 3 characters)
static const std::vector<std::string> SNSP_R8 = {
    "261","100","115","100","107","110","111","115","121","123",
    "126","128","129","132","131","132","197","221","222","261",
    "261","132","300","500","313","314","316","317","318","330",
    "370","370","370","400","404","300","460","300","300","500",
    "521","531","541","541","300","544","545","546","550","500",
    "300","580","300","601","602","611","621","650","651","652",
    "653","300","300","300","691","693","694","500","711","300",
    "731","300","742","300","762","800","802","804","806","812",
    "813","817","800","820","822","823","800","825","826","827",
    "828","830","831","832","833","834","835","837","835","901",
    "920","930","300","950","970","970","970","970","300","300"
};

// TOPCODE (5 entries)
static const std::vector<char> TOPCODE_R8 = { '1', '4', '7', '8', '9' };

// ---------- Helpers ----------
static inline std::string trim(const std::string& s) {
    size_t b = 0, e = s.size();
    while (b < e && std::isspace(static_cast<unsigned char>(s[b]))) ++b;
    while (e > b && std::isspace(static_cast<unsigned char>(s[e - 1]))) --e;
    return s.substr(b, e - b);
}

// Build the 10-character validation code: "8" + geo(1..7) + top + "CLKE" + sp(3)
static inline std::string buildR8Code(char geoDigit, char topCode, const std::string& sp3) {
    std::string out;
    out.reserve(10);
    out.push_back('8');            // pos 1
    out.push_back(geoDigit);       // pos 2 ('1'..'7')
    out.push_back(topCode);        // pos 3 ('1','4','7','8','9')
    out += "CLKE";                 // pos 4..7
    out += sp3;                    // pos 8..10
    return out;                    // total length = 10
}

VolumeEquation VolumeEquationResolver::GetR8VolumeEquation(VolumeCalculationOptions vco)
{
    //(void)PROD; (void)VAR; // not used in current logic per commented-out Fortran path
    int ERRFLAG = 0;
    int SPEC = vco.fiaCode;
    std::string VOLEQ;
    int FORNUM = vco.forest, DISTNUM = vco.district;

    // --- Set GEOAREA (single char) per forest/district mapping ---
    char GEOAREA = '1'; // default; will be overridden below
    if (FORNUM == 1) {
        // Alabama
        GEOAREA = '4';
        if (DISTNUM == 3) GEOAREA = '1';
    }
    else if (FORNUM == 2 || FORNUM == 4 || FORNUM == 8 || FORNUM == 60) {
        // Daniel Boone, Cherokee, GW/Jeff, Land Between the Lakes
        GEOAREA = '3';
    }
    else if (FORNUM == 3) {
        // Chattahoochee/Oconee
        GEOAREA = '3';
        if (DISTNUM == 8) GEOAREA = '2';
    }
    else if (FORNUM == 5 || FORNUM == 36) {
        // Florida, Savannah River
        GEOAREA = '1';
    }
    else if (FORNUM == 6 || FORNUM == 13) {
        // Kisatchie, Texas
        GEOAREA = '5';
    }
    else if (FORNUM == 7) {
        // Mississippi
        GEOAREA = '5';
        if (DISTNUM == 6)             GEOAREA = '7';
        else if (DISTNUM == 7 || DISTNUM == 17) GEOAREA = '4';
    }
    else if (FORNUM == 9) {
        // Ouachita
        GEOAREA = '6';
    }
    else if (FORNUM == 10) {
        // Ozark/St Francis
        GEOAREA = '6';
        if (DISTNUM == 7) GEOAREA = '7';
    }
    else if (FORNUM == 11) {
        // North Carolina
        GEOAREA = '3';
        if (DISTNUM == 3)  GEOAREA = '1';
        else if (DISTNUM == 10) GEOAREA = '2';
    }
    else if (FORNUM == 12) {
        // Francis Marion/Sumpter
        GEOAREA = '2';
        if (DISTNUM == 2) GEOAREA = '3';
        else if (DISTNUM == 5) GEOAREA = '1';
    }

    // --- Build VOLEQ prefix: '8' + GEOAREA + '1' + 'CLKE' ---
    // (PROD == '08' path was commented out in Fortran; always set '1')
    VOLEQ.clear();
    VOLEQ.reserve(10);
    VOLEQ.push_back('8');        // pos 1
    VOLEQ.push_back(GEOAREA);    // pos 2
    VOLEQ.push_back('1');        // pos 3 (product fixed to '1' per 2025/05/23 note)
    VOLEQ += "CLKE";             // pos 4..7

    // --- Find species index in SNFIA via binary search (1-based DONE) ---
    int FIRST = 1;
    int LAST = static_cast<int>(SNFIA_R8.size());
    int DONE = 0;

    while (DONE == 0) {
        int HALF = (LAST - FIRST + 1) / 2 + FIRST;
        int idx = HALF - 1; // convert to 0-based
        if (SNFIA_R8.at(idx) == SPEC) {
            DONE = HALF;
        }
        else if (FIRST == LAST) {
            ERRFLAG = 1;
            DONE = -1;
        }
        else if (SNFIA_R8.at(idx) < SPEC) {
            FIRST = HALF;
        }
        else {
            LAST = HALF - 1;
        }
    }

    // --- Fallback species index if not found ---
    if (DONE < 0) {
        if (SPEC < 300) {
            // Other softwood → use index 22 (SNFIA(22) == 299)
            DONE = 22;
        }
        else {
            // Other/unknown → use index 110
            DONE = 110;
        }
    }

    // --- Append species code (3 chars) ---
    VOLEQ += SNSP_R8.at(static_cast<size_t>(DONE - 1)); // pos 8..10
    return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
}

bool VolumeEquationResolver::isValidR8Equation(const std::string& VOLEQ)
{
    // Clark equation
    for (int i = 1; i <= 7; ++i) {
        char geoDigit = static_cast<char>('0' + i);
        for (char top : TOPCODE_R8) {
            for (const auto& sp3 : SNSP_R8) {
                const std::string candidate = buildR8Code(geoDigit, top, sp3);
                if (VOLEQ == candidate) {
                    return true;
                }
            }
        }
    }

    //DVE (Lasher)equation
    if (VOLEQ.substr(3, 3) == "DVE") {
        int geoCode = std::stoi(VOLEQ.substr(1, 2));
        if (geoCode > 0 && geoCode <= 35) {
            int spCode = std::stoi(VOLEQ.substr(7, 3));

            bool exists = std::find(SNFIA_R8.begin(), SNFIA_R8.end(), spCode) != SNFIA_R8.end();

            if (exists) {
                // species code is present
                return true;
            }
            else {
                // not present
                return false;
            }

        }
    }
    // No match; return with SPEC unchanged
    return false;
}