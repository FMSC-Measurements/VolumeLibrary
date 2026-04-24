#include "VolumeEquationResolver.h"

#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <stdexcept>

// ----------------- Tables (from Fortran DATA) -----------------

// FIA species codes (41 entries)
static const std::vector<int> FIA_R7 = {
    11, 15, 17, 19, 20,
    21, 22, 41, 42, 64,
    72, 73, 81, 93, 98,
    101, 103, 108, 116, 117,
    119, 122, 202, 211, 231,
    242, 260, 263, 264, 312,
    351, 352, 361, 431, 542,
    631, 747, 800, 815, 981,
    999
};

// Main EQNUM list (45 entries) — BLM/BIA/Industry
static const std::vector<std::string> EQNUM_R7 = {
    "B00BEHW011","B00BEHW015","B00BEHW017","B00BEHW015","B00BEHW021",
    "B00BEHW021","B00BEHW022","B00BEHW041","B00BEHW042","B00BEHW242",
    "B00BEHW073","B00BEHW073","B00BEHW081","B00BEHW093","B00BEHW098",
    "B00BEHW119","B00BEHW108","B00BEHW108","B00BEHW116","B00BEHW117",
    "B00BEHW119","B00BEHW122","B01BEHW202","B00BEHW211","B00BEHW231",
    "B00BEHW242","B00BEHW260","B00BEHW260","B00BEHW260","B00BEHW312",
    "B00BEHW351","B00BEHW361","B00BEHW361","B00BEHW431","B00BEHW542",
    "B00BEHW631","B00BEHW747","B00BEHW800","B00BEHW800","B00BEHW998",
    "B00BEHW999",
    "B02BEHW202","B03BEHW202","B01BEHW202","B00BEHW263"
};

// INGY (Region 6) — 60 entries
static const std::vector<std::string> EQNUMI_R7 = {
    "I00FW2W012","I00FW2W017","I00FW2W019","I00FW2W073","I00FW2W093",
    "I00FW2W108","I00FW2W119","I00FW2W122","I00FW2W202","I00FW2W242",
    "I00FW2W260","I00FW2W263","I11FW2W012","I11FW2W017","I11FW2W019",
    "I11FW2W073","I11FW2W093","I11FW2W108","I11FW2W119","I11FW2W122",
    "I11FW2W202","I11FW2W242","I11FW2W260","I11FW2W263","I12FW2W012",
    "I12FW2W017","I12FW2W019","I12FW2W073","I12FW2W093","I12FW2W108",
    "I12FW2W119","I12FW2W122","I12FW2W202","I12FW2W242","I12FW2W260",
    "I12FW2W263","I13FW2W012","I13FW2W017","I13FW2W019","I13FW2W073",
    "I13FW2W093","I13FW2W108","I13FW2W119","I13FW2W122","I13FW2W202",
    "I13FW2W242","I13FW2W260","I13FW2W263","I14FW2W012","I14FW2W017",
    "I14FW2W019","I14FW2W073","I14FW2W093","I14FW2W108","I14FW2W119",
    "I14FW2W122","I14FW2W202","I14FW2W242","I14FW2W260","I14FW2W263"
};

// Westside Flewelling — 27 entries
static const std::vector<std::string> EQNUMF_R7 = {
    "F00FW2W202","F00FW2W242","F00FW2W263","F01FW2W202","F01FW2W242",
    "F01FW2W263","F02FW2W202","F02FW2W242","F02FW2W263","F03FW2W202",
    "F03FW2W242","F03FW2W263","F04FW2W202","F04FW2W242","F04FW2W263",
    "F05FW2W202","F05FW2W242","F05FW2W263","F06FW2W202","F06FW2W242",
    "F06FW2W263","F07FW2W202","F07FW2W242","F07FW2W263","F08FW2W202",
    "F08FW2W242","F08FW2W263"
};

// Canadian INGY — 33 entries
static const std::vector<std::string> EQNUMC_R7 = {
    "I21FW2W012","I21FW2W017","I21FW2W019","I21FW2W073","I21FW2W093",
    "I21FW2W108","I21FW2W119","I21FW2W122","I21FW2W202","I21FW2W242",
    "I21FW2W260","I22FW2W012","I22FW2W017","I22FW2W019","I22FW2W073",
    "I22FW2W093","I22FW2W108","I22FW2W119","I22FW2W122","I22FW2W202",
    "I22FW2W242","I22FW2W260","I23FW2W012","I23FW2W017","I23FW2W019",
    "I23FW2W073","I23FW2W093","I23FW2W108","I23FW2W119","I23FW2W122",
    "I23FW2W202","I23FW2W242","I23FW2W260"
};

// Direct Volume Estimators — 8 entries
static const std::vector<std::string> EQNUMD_R7 = {
    "601DVEW205","601DVEW263","601DVEW015","602DVEW122","602DVEW204",
    "602DVEW015","602DVEW108","602DVEW122"
};

// ----------------- Helpers -----------------
//static inline std::string trim(const std::string& s) {
//    size_t b = 0, e = s.size();
//    while (b < e && std::isspace(static_cast<unsigned char>(s[b]))) ++b;
//    while (e > b && std::isspace(static_cast<unsigned char>(s[e - 1]))) --e;
//    return s.substr(b, e - b);
//}
//static inline std::string to_upper(std::string s) {
//    std::transform(s.begin(), s.end(), s.begin(),
//        unsigned char c{ return static_cast<char>(std::toupper(c)); });
//    return s;
//}
static inline bool hasPrefix(const std::string& s, const std::string& p) {
    return s.size() >= p.size() && std::equal(p.begin(), p.end(), s.begin());
}
template <typename Container, typename T>
static inline bool contains(const Container& c, const T& x) {
    return std::find(c.begin(), c.end(), x) != c.end();
}
// Fortran 1-based access for main EQNUM
static inline const std::string& eqR7(int i) { return EQNUM_R7.at(static_cast<size_t>(i - 1)); }

// ----------------- C++ translation of SUBROUTINE R7_EQN ----------------------
/**
 * @brief C++ translation of Fortran SUBROUTINE R7_EQN(FORST, SPEC, VAR, VOLEQ, ERRFLAG)
 *
 * @param FORST   Two-character forest code, parsed as integer (e.g., "12").
 * @param SPEC    FIA species code (int). May be set to 8888 in validation branch.
 * @param VAR     Two-character variant code (e.g., "PN", "WC", "NC", ...).
 * @param VOLEQ   Volume equation identifier (string). Set by this function.
 * @param ERRFLAG Error flag (0 = OK, 1 = not found → fallback "B00BEHW999").
 */
VolumeEquation VolumeEquationResolver::GetR7VolumeEquation(VolumeCalculationOptions vco)
//static void R7_EQN_cpp(const std::string& FORST, int& SPEC, const std::string& VAR,
//    std::string& VOLEQ, int& ERRFLAG)
{
    int ERRFLAG = 0;
    int SPEC = vco.fiaCode;
    std::string VOLEQ;
    int FORNUM = vco.forest;

    // Fortran: DONE starts at 0 and may be set by overrides to an EQNUM index
    int DONE = 0;

    // Normalize VAR to uppercase to simplify case-insensitive comparisons
    const std::string VARU = GetFvsVariantCode(vco);

    // --- Variant/forest-specific overrides (directly set EQNUM index in DONE) ---
    if (SPEC == 202) { // Douglas-fir
        if (VARU == "WC") {
            DONE = 44;
        }
        else if (VARU == "PN" || VARU == "NC" || VARU == "CA" || VARU == "OC" || VARU == "OP") {
            DONE = (FORNUM == 12) ? 42 : 44;
        }
        else {
            DONE = 44;
        }
    }
    else if (SPEC == 41) { // Port Orford cedar
        if (VARU == "CA" || VARU == "OC") DONE = 13;
    }
    else if (SPEC == 263) { // Hemlock (generic)
        if (VARU == "CA" || VARU == "PN" || VARU == "OC" || VARU == "OP") DONE = 45;
    }
    else if (SPEC == 109 || SPEC == 113 || SPEC == 124 || SPEC == 127) { // Coulter/limber/Monterey/gray pine
        if (VARU == "CA" || VARU == "OC") DONE = 18;
    }
    else if (SPEC == 92) { // Brewer spruce
        if (VARU == "CA" || VARU == "OC") DONE = 14;
    }
    else if (SPEC == 212) { // Giant sequoia
        if (VARU == "CA" || VARU == "OC") DONE = 24;
    }
    else if (SPEC == 801 || SPEC == 805 || SPEC == 807 || SPEC == 811 ||
        SPEC == 818 || SPEC == 821 || SPEC == 839 || SPEC == 333 || SPEC == 730) {
        if (VARU == "CA" || VARU == "OC") {
            DONE = 38; // B00BEHW800 (oak group)
        }
        else if (VARU == "NC") {
            if (SPEC == 818) DONE = 38; // California black oak special-case
        }
    }
    else if (SPEC == 542) { // Oregon ash
        if (VARU == "CA" || VARU == "OC") DONE = 30;
    }
    else if (SPEC == 251) { // California nutmeg
        if (VARU == "CA" || VARU == "OC") DONE = 25;
    }
    else if (SPEC == 981) { // California laurel (Oregon myrtle)
        if (VARU == "CA" || VARU == "OC") DONE = 36;
    }

    // --- FINDS SPECIES INDEX TO FIA ARRAY (only if DONE==0) ---
    int FIRST = 1;
    int LAST = static_cast<int>(FIA_R7.size());
    while (DONE == 0) {
        int HALF = (LAST - FIRST + 1) / 2 + FIRST;
        int idx = HALF - 1; // convert to 0-based
        if (FIA_R7.at(idx) == SPEC) {
            DONE = HALF; // EQNUM index equals FIA index (1-based)
        }
        else if (FIRST == LAST) {
            ERRFLAG = 1;
            DONE = -1;
        }
        else if (FIA_R7.at(idx) < SPEC) {
            FIRST = HALF;
        }
        else {
            LAST = HALF - 1;
        }
    }

    // --- Final assignment ---
    if (DONE < 0) {
        VOLEQ = "B00BEHW999"; // Unknown
    }
    else {
        VOLEQ = eqR7(DONE);   // EQNUM(DONE)
    }
    return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
}

bool VolumeEquationResolver::isValidR7Equation(const std::string& VOLEQ)
{
    // --- Validation branch ---
    // Westside/Eastside prefixes
    if (hasPrefix(VOLEQ, "616BEHW") || hasPrefix(VOLEQ, "632BEHW")) {
        return true;
    }
    // Membership checks across known tables
    if (contains(EQNUM_R7, VOLEQ) || contains(EQNUMI_R7, VOLEQ) ||
        contains(EQNUMF_R7, VOLEQ) || contains(EQNUMC_R7, VOLEQ) ||
        contains(EQNUMD_R7, VOLEQ)) {
        return true;
    }
    return false; 
}