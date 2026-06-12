#include "VolumeEquationResolver.h"

#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <stdexcept>
#include <sstream>

// ---------- Tables (from Fortran DATA) ----------

// FIA species codes (53 entries)
static const std::vector<int> FIA_R6 = {
    11, 15, 17, 19, 20,
    21, 22, 42, 64, 66,
    72, 73, 81, 93, 98,
    101, 103, 106, 108, 113,
    116, 117, 119, 122, 202,
    211, 231, 242, 263, 264,
    290, 299, 312, 321, 351,
    352, 361, 375, 431, 475,
    478, 492, 500, 631, 740,
    746, 747, 768, 815, 818,
    920, 998, 999
};

// INGY EQNUMI (87 entries)
static const std::vector<std::string> EQNUMI_R6 = {
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
    "I14FW2W122","I14FW2W202","I14FW2W242","I14FW2W260","I14FW2W263",
    "I00FW2W012","I00FW2W202","I00FW2W093","I00FW2W017","I00FW2W108",
    "I00FW2W122","I00FW2W019","I00FW2W073","I00FW2W119","I00FW2W242",
    "I00FW2W260","I13FW2W202","I11FW2W202","I12FW2W202","I14FW2W017",
    "I13FW2W017","I14FW2W122","I12FW2W122","I13FW2W122","I11FW2W122",
    "I11FW2W017","I11FW2W073","I11FW2W242","I12FW2W017","I12FW2W073",
    "I13FW2W073","I14FW2W073"
};

// Westside Flewelling EQNUMF (50 entries)
static const std::vector<std::string> EQNUMF_R6 = {
    "F00FW2W202","F00FW2W242","F00FW2W263","F01FW2W202","F01FW2W242",
    "F01FW2W263","F02FW2W202","F02FW2W242","F02FW2W263","F03FW2W202",
    "F03FW2W242","F03FW2W263","F04FW2W202","F04FW2W242","F04FW2W263",
    "F05FW2W202","F05FW2W242","F05FW2W263","F06FW2W202","F06FW2W242",
    "F06FW2W263","F07FW2W202","F07FW2W242","F07FW2W263","F08FW2W202",
    "F08FW2W242","F08FW2W263","F03FW2W202","F01FW2W202","F02FW2W202",
    "F00FW2W202","F04FW2W202","F08FW2W202","F07FW2W202","F06FW2W202",
    "F05FW2W202","F00FW2W242","F00FW2W260","F01FW2W242","F01FW2W260",
    "F02FW2W242","F02FW2W260","F03FW2W242","F03FW2W260","F04FW2W242",
    "F04FW2W260","F05FW2W260","F06FW2W260","F07FW2W260","F08FW2W260"
};

// Canadian INGY EQNUMC (39 entries)
static const std::vector<std::string> EQNUMC_R6 = {
    "I21FW2W012","I21FW2W017","I21FW2W019","I21FW2W073","I21FW2W093",
    "I21FW2W108","I21FW2W119","I21FW2W122","I21FW2W202","I21FW2W242",
    "I21FW2W260","I22FW2W012","I22FW2W017","I22FW2W019","I22FW2W073",
    "I22FW2W093","I22FW2W108","I22FW2W119","I22FW2W122","I22FW2W202",
    "I22FW2W242","I22FW2W260","I23FW2W012","I23FW2W017","I23FW2W019",
    "I23FW2W073","I23FW2W093","I23FW2W108","I23FW2W119","I23FW2W122",
    "I23FW2W202","I23FW2W242","I23FW2W260","I22FW2W122","I21FW2W122",
    "I23FW2W122","I21FW2W073","I22FW2W073","I23FW2W073"
};

// Direct Volume Estimators EQNUMD (15 entries)
static const std::vector<std::string> EQNUMD_R6 = {
    "601DVEW205","601DVEW263","601DVEW015","602DVEW122","602DVEW204",
    "602DVEW015","602DVEW108","602DVEW122","601DVEW205","602DVEW204",
    "602DVEW108","600DVEW122","601DVEW015","601DVEW263","602DVEW015"
};

// BLM/BIA/Industry EQNUM (44 entries)
static const std::vector<std::string> EQNUMB_R6 = {
    "B00BEHW011","B00BEHW015","B00BEHW017","B00BEHW015","B00BEHW021",
    "B00BEHW021","B00BEHW022","B00BEHW041","B00BEHW042","B00BEHW242",
    "B00BEHW073","B00BEHW073","B00BEHW081","B00BEHW093","B00BEHW098",
    "B00BEHW119","B00BEHW108","B00BEHW108","B00BEHW116","B00BEHW117",
    "B00BEHW119","B00BEHW122","B01BEHW202","B00BEHW211","B00BEHW231",
    "B00BEHW242","B00BEHW260","B00BEHW260","B00BEHW260","B00BEHW312",
    "B00BEHW351","B00BEHW361","B00BEHW361","B00BEHW431","B00BEHW542",
    "B00BEHW631","B00BEHW747","B00BEHW800","B00BEHW800","B00BEHW998",
    "B00BEHW999","B02BEHW202","B03BEHW202","B01BEHW202"
};

// ---------- Helpers ----------
static inline std::string trim(const std::string& s) {
    size_t b = 0, e = s.size();
    while (b < e && std::isspace(static_cast<unsigned char>(s[b]))) ++b;
    while (e > b && std::isspace(static_cast<unsigned char>(s[e - 1]))) --e;
    return s.substr(b, e - b);
}

static inline const std::string& eqI(int i) { return EQNUMI_R6.at(static_cast<size_t>(i - 1)); }
static inline const std::string& eqF(int i) { return EQNUMF_R6.at(static_cast<size_t>(i - 1)); }
static inline const std::string& eqC(int i) { return EQNUMC_R6.at(static_cast<size_t>(i - 1)); }
static inline const std::string& eqD(int i) { return EQNUMD_R6.at(static_cast<size_t>(i - 1)); }
static inline const std::string& eqB(int i) { return EQNUMB_R6.at(static_cast<size_t>(i - 1)); }

static inline bool hasPrefix(const std::string& s, const std::string& p) {
    return s.size() >= p.size() && std::equal(p.begin(), p.end(), s.begin());
}

static inline bool inWestside(const std::string& VAR) {
    return VAR == "PN" || VAR == "WC" || VAR == "NC" || VAR == "CA" || VAR == "OC" || VAR == "OP";
}

// Build Behre’s hyperbola VOLEQ: "616BEHW" + FIA code padded to 3 digits
static inline std::string behreCodeFromSpecies(int species) {
    if (species < 0) return "616BEHW000";
    std::ostringstream oss;
    oss << "616BEHW";
    if (species < 10)       oss << "00" << species;
    else if (species < 100) oss << "0" << species;
    else                    oss << species;
    return oss.str();
}


template <typename Container, typename T>
bool contains(const Container& c, const T& x) {
    return std::find(c.begin(), c.end(), x) != c.end();
}

// ---------- C++ translation of SUBROUTINE R6_EQN ----------
/**
 * @brief C++ translation of Fortran SUBROUTINE R6_EQN(VAR, FORST, DIST, SPEC, VOLEQ, ERRFLAG)
 *
 * @param VAR     Two-character variant code (e.g., "PN", "WC", ...).
 * @param FORST   Two-character forest code, parsed as integer (e.g., "03" → 3).
 * @param DIST    Two-character district code, parsed as integer (e.g., "01" → 1).
 * @param SPEC    FIA species code (int). May be set to 8888 in validation branch.
 * @param VOLEQ   Volume equation identifier (string). Set by this function.
 * @param ERRFLAG Error flag (0 = OK, 1 = not found in FIA → Behre default).
 */
VolumeEquation VolumeEquationResolver::GetR6VolumeEquation(VolumeCalculationOptions vco)
{
    int ERRFLAG = 0;
    int SPEC = vco.fiaCode;
    std::string VOLEQ;
    std::string VAR = GetFvsVariantCode(vco);

    

    // Parse FORST and DIST to ints (I2)
    int FORNUM = vco.forest, DISTNUM = vco.district;

    int DONEI = 0;
    int DONEF = 0;

    // ----- Westside variants -----
    if (inWestside(VAR)) {
        if (FORNUM == 3) { // Gifford Pinchot
            if (SPEC == 11) DONEI = 26;
            else if (SPEC == 19) DONEI = 6;
            else if (SPEC == 263 || SPEC == 260) {
                DONEF = 3;
                if (DISTNUM == 1)      DONEF = 21;
                else if (DISTNUM == 5) DONEF = 15;
            }
            else if (SPEC == 202) {
                DONEF = 10;
                if (DISTNUM == 1)      DONEF = 22;
                else if (DISTNUM == 5) DONEF = 10;
            }
        }
        else if (FORNUM == 6) { // Mt Hood
            if (SPEC == 11)  DONEI = 26;
            else if (SPEC == 17)  DONEI = 38;
            else if (SPEC == 93)  DONEI = 17;
            else if (SPEC == 108) DONEI = 18;
            else if (SPEC == 122) DONEI = 32;
            else if (SPEC == 263 || SPEC == 260) DONEI = 23;
            else if (SPEC == 22)  DONEI = 38;
            else if (SPEC == 202) {
                DONEF = 10;
                if (DISTNUM == 5 || DISTNUM == 9) DONEF = 10;
            }
        }
        else if (FORNUM == 5) { // Mt Baker - Snoqualmie
            if (SPEC == 263 || SPEC == 260) DONEF = 12;
            else if (SPEC == 202) DONEF = 25;
        }
        else if (FORNUM == 10 || FORNUM == 11) { // Rogue River / Siskiyou
            if (SPEC == 15) {
                if ((VAR != "NC") || (VAR != "nc")) DONEI = 5; // preserves Fortran OR semantics
            }
            else if (SPEC == 122) {
                if ((VAR != "NC") || (VAR != "nc")) DONEI = 4; // preserves Fortran OR semantics
            }
            else if (SPEC == 202) {
                DONEF = 19;
            }
        }
        else if (FORNUM == 12) { // Siuslaw
            if (SPEC == 202) DONEF = 1;
            else if (SPEC == 263) DONEF = 12;
        }
        else if (FORNUM == 9) { // Olympic
            if (SPEC == 202) DONEF = 10;
            else if (SPEC == 98)  DONEF = 12;
            else if (SPEC == 263) DONEF = 3;
        }
        else if (FORNUM == 15) { // Umpqua
            if (SPEC == 15)  DONEI = 2;
            else if (SPEC == 20)  DONEI = 1;
            else if (SPEC == 81)  DONEI = 4;
            else if (SPEC == 93)  DONEI = 5;
            else if (SPEC == 108) DONEI = 6;
            else if (SPEC == 122) DONEI = 4;
            else if (SPEC == 202) DONEF = 1;
            else if (SPEC == 242) DONEI = 1;
            else if (SPEC == 263) DONEI = 23;
            else if (SPEC == 264) DONEI = 10;
        }
        else if (FORNUM == 18) { // Willamette
            if (SPEC == 22)  DONEI = 6;
            else if (SPEC == 202) DONEF = 16;
            else if (SPEC == 263) DONEF = 12;
            // Added (2025-02-27): Incense cedar (81) and Grand fir (17)
            else if (SPEC == 17)  DONEI = 64; // I00FW2W017
            else if (SPEC == 81)  DONEI = 68; // I00FW2W073
        }

        if (DONEI > 0) {
            VOLEQ = eqI(DONEI);
        }
        else if (DONEF > 0) {
            VOLEQ = eqF(DONEF);
        }
        else {
            // Behre’s hyperbola default
            // Binary-search-like over FIA_R6 to preserve Fortran logic
            int first = 1, last = static_cast<int>(FIA_R6.size()), done = 0;
            while (done == 0) {
                int half = (last - first + 1) / 2 + first;
                int idx = half - 1;
                if (FIA_R6.at(idx) == SPEC) {
                    done = half;
                }
                else if (first == last) {
                    ERRFLAG = 1;
                    done = -1;
                }
                else if (FIA_R6.at(idx) < SPEC) {
                    first = half;
                }
                else {
                    last = half - 1;
                }
            }
            if (done < 0) {
                VOLEQ = "616BEHW000";
            }
            else {
                VOLEQ = behreCodeFromSpecies(SPEC);
            }
        }
    }
    // ----- Eastside variants -----
    else {
        if (FORNUM == 1) { // Deschutes
            if (SPEC == 11 || SPEC == 15 || SPEC == 17 || SPEC == 21) {
                if ((VAR != "SO") || (VAR != "so")) DONEI = 2; // preserves Fortran OR semantics
            }
            else if (SPEC == 73) {
                DONEI = 16;
            }
            else if (SPEC == 108) {
                DONEI = 18;
            }
            else if (SPEC == 122) {
                DONEI = 20;
            }
            else if (SPEC == 202) {
                DONEI = 21;
            }
            else if (SPEC == 81) {
                DONEI = 22;
            }
        }
        else if (FORNUM == 2 || FORNUM == 20) { // Fremont
            if (SPEC == 15 || SPEC == 17) DONEI = 14;
            else if (SPEC == 81) DONEI = 9;
            else if (SPEC == 108) DONEI = 6;
            else if (SPEC == 122) DONEI = 8;
            else if (SPEC == 202) DONEI = 2;
        }
        else if (FORNUM == 3) { // Gifford Pinchot
            if (SPEC == 11) DONEI = 26;
            else if (SPEC == 263 || SPEC == 260) {
                if (DISTNUM == 3) DONEF = 3;
            }
            else if (SPEC == 202) {
                if (DISTNUM == 3) DONEI = 10;
            }
        }
        else if (FORNUM == 4) { // Malheur
            if (SPEC == 108) DONEI = 30;
            else if (SPEC == 17 || SPEC == 15) DONEI = 26;
            else if (SPEC == 122) DONEI = 32;
            else if (SPEC == 202) DONEI = 33;
        }
        else if (FORNUM == 6) { // Mt Hood
            if (SPEC == 11)  DONEI = 26;
            else if (SPEC == 17)  DONEI = 38;
            else if (SPEC == 93)  DONEI = 17;
            else if (SPEC == 108) DONEI = 18;
            else if (SPEC == 122) DONEI = 32;
            else if (SPEC == 263 || SPEC == 260) DONEI = 23;
            else if (SPEC == 22)  DONEI = 38;
            else if (SPEC == 202) {
                DONEF = 16;
                if (DISTNUM == 1 || DISTNUM == 6) DONEF = 22;
            }
        }
        else if (FORNUM == 7) { // Ochoco
            if (SPEC == 17 || SPEC == 15) DONEI = 26;
            else if (SPEC == 73)  DONEI = 28;
            else if (SPEC == 122) DONEI = 32;
            else if (SPEC == 202) DONEI = 33;
            else if (SPEC == 108) DONEI = 30;
        }
        else if (FORNUM == 14) { // Umatilla
            if (SPEC == 17 || SPEC == 15) DONEI = 38;
            else if (SPEC == 19) DONEI = 3;
            else if (SPEC == 73) DONEI = 45;
            else if (SPEC == 93) DONEI = 5;
            else if (SPEC == 108) DONEI = 6;
            else if (SPEC == 122) DONEI = 44;
            else if (SPEC == 202) DONEI = 38;
        }
        else if (FORNUM == 16) { // Wallowa-Whitman
            if (SPEC == 17 || SPEC == 15) DONEI = 14;
            else if (SPEC == 73)  DONEI = 40;
            else if (SPEC == 93)  DONEI = 17;
            else if (SPEC == 108) DONEI = 6;
            else if (SPEC == 122) DONEI = 20;
            else if (SPEC == 202) DONEI = 21;
        }
        else if (FORNUM == 8 || FORNUM == 17) { // Okanogan - Wenatchee
            if (SPEC == 17) {
                DONEI = 14;
                if (DISTNUM == 2 || DISTNUM == 3 || DISTNUM == 5 || DISTNUM == 7) DONEI = 14;
            }
            else if (SPEC == 202) {
                DONEI = 33;
                if (DISTNUM == 2 || DISTNUM == 3 || DISTNUM == 4 || DISTNUM == 5 ||
                    DISTNUM == 7 || DISTNUM == 9) DONEI = 33;
            }
            else if (SPEC == 108) {
                DONEI = 30;
                if (DISTNUM == 4) DONEI = 30;
            }
            else if (SPEC == 93) {
                DONEI = 17;
                if (DISTNUM == 9) DONEI = 17;
            }
            else if (SPEC == 122 || SPEC == 73) {
                DONEI = 32;
                if (DISTNUM == 4) {
                    DONEI = 32;
                }
                else if (DISTNUM == 2 || DISTNUM == 3 || DISTNUM == 5 || DISTNUM == 7) {
                    DONEI = 20;
                }
            }
        }
        else if (FORNUM == 21) { // Colville
            if (SPEC == 17) DONEI = 14;
            else if (SPEC == 19) DONEI = 21;
            else if (SPEC == 73) DONEI = 16;
            else if (SPEC == 93) DONEI = 41;
            else if (SPEC == 108) DONEI = 18;
            else if (SPEC == 119) DONEI = 7;
            else if (SPEC == 122) DONEI = 32;
            else if (SPEC == 202) DONEI = 21;
            else if (SPEC == 242) DONEI = 22;
            else if (SPEC == 263 || SPEC == 264) DONEI = 14;
        }

        if (DONEI > 0) {
            VOLEQ = eqI(DONEI);
        }
        else if (DONEF > 0) {
            VOLEQ = eqF(DONEF);
        }
        else {
            // Behre’s hyperbola default
            int first = 1, last = static_cast<int>(FIA_R6.size()), done = 0;
            while (done == 0) {
                int half = (last - first + 1) / 2 + first;
                int idx = half - 1;
                if (FIA_R6.at(idx) == SPEC) {
                    done = half;
                }
                else if (first == last) {
                    ERRFLAG = 1;
                    done = -1;
                }
                else if (FIA_R6.at(idx) < SPEC) {
                    first = half;
                }
                else {
                    last = half - 1;
                }
            }
            if (done < 0) {
                VOLEQ = "616BEHW000";
            }
            else {
                VOLEQ = behreCodeFromSpecies(SPEC);
            }
        }
    }

    // Done: VOLEQ and ERRFLAG set
    return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
}

bool VolumeEquationResolver::isValidR6Equation(const std::string& VOLEQ)
{
    // ----- Validation branch -----
    // Prefix checks
    if (hasPrefix(VOLEQ, "616BEHW") || hasPrefix(VOLEQ, "628BEHW") || hasPrefix(VOLEQ, "632BEHW") ||
        hasPrefix(VOLEQ, "616TRFW") || hasPrefix(VOLEQ, "632TRFW")) {
        return true;
    }

    // Membership checks in BLM/INGY/Flewelling/Canadian/Direct tables
    if (contains(EQNUMB_R6, VOLEQ) || contains(EQNUMI_R6, VOLEQ) || contains(EQNUMF_R6, VOLEQ) ||
        contains(EQNUMC_R6, VOLEQ) || contains(EQNUMD_R6, VOLEQ)) {
        return true;
    }
    //check for 3-point equation
    if (VOLEQ.substr(5, 1) == "3") {
        std::string voleqNew = VOLEQ.substr(0, 5) + "2" + VOLEQ.substr(6, 4);
        if (contains(EQNUMB_R6, voleqNew) || contains(EQNUMI_R6, voleqNew) || contains(EQNUMF_R6, voleqNew) ||
            contains(EQNUMC_R6, voleqNew) || contains(EQNUMD_R6, voleqNew)) {
            return true;
        }
    }
    return false;
}