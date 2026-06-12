#include "VolumeEquationResolver.h"

#include <string>
#include <vector>
#include <algorithm>
#include <cctype>
#include <stdexcept>

// ----------------- Data tables (from Fortran DATA) -----------------

// FIA species codes (27 entries)
// Includes second growth codes: 2042, 2098, 2242, 2263
static const std::vector<int> FIA_R10 = {
    11, 19, 42, 71, 94,
    94, 95, 98, 108, 242,
    263, 264, 299, 350, 351,
    375, 376, 741, 746, 747,
    920, 928, 998, 2042, 2098,
    2242, 2263
};

// Tongass defaults (location codes: 703, 1002, 1003, 1005, 8134, 8135, 8112)
static const std::vector<std::string> TONEQN_R10 = {
    "A00F32W260","A00F32W260","A00F32W042","A00DVEW094","A00DVEW094",
    "A00DVEW094","A00DVEW094","A00F32W098","A00F32W260","A00F32W242",
    "A00F32W260","A00F32W260","A00DVEW094","A32CURW351","A32CURW351",
    "A00DVEW375","A00DVEW375","A00DVEW747","A00DVEW375","A00DVEW747",
    "A00DVEW747","A00DVEW747","A00DVEW747","A02F32W042","A02F32W098",
    "A02F32W242","A02F32W263"
};

// Chugach defaults (location codes: 1004, 713, 720, 7400–7408)
static const std::vector<std::string> CHUEQN_R10 = {
    "A01DEMW000","A01DEMW000","A00DVEW094","A00DVEW094","A00DVEW094",
    "A00DVEW094","A00DVEW094","A00F32W098","A01DEMW000","A01DEMW000",
    "A00F32W260","A01DEMW000","A00DVEW094","A32CURW351","A32CURW351",
    "A00DVEW375","A00DVEW375","A00DVEW747","A00DVEW375","A00DVEW747",
    "A00DVEW747","A00DVEW747","A00DVEW747","A02F32W042","A02F32W098",
    "A02F32W242","A02F32W263"
};

// Other valid non-default equations (27 entries)
static const std::vector<std::string> OTHEREQN_R10 = {
    "A00FW2W042","A16DEMW042","A61DEMW042","A32DEMW042","A01DVEW094",
    "A02DVEW094","A16DEMW098","A00FW2W098","A02F32W098","A02FW2W098",
    "A02DEMW000","A32CURW000","A32DEMW098","A61DEMW098","A00FW2W242",
    "A61DEMW242","A16DEMW242","A32DEMW242","A00FW2W260","A02F32W260",
    "A02FW2W260","A16CURW260","A01DVEW375","A01DVEW747","A00DVEW108",
    "A00DVEW310","A00DVEW351","A00F32W263","A02F32W263","A00FW2W263",
    "A02FW2W263","A00F32W264","A02F32W264","A00FW2W264","A02FW2W264"
};

// ----------------- Helpers -----------------
static inline std::string trim(const std::string& s) {
    size_t b = 0, e = s.size();
    while (b < e && std::isspace(static_cast<unsigned char>(s[b]))) ++b;
    while (e > b && std::isspace(static_cast<unsigned char>(s[e - 1]))) --e;
    return s.substr(b, e - b);
}

// 1-based index accessor (parity with Fortran EQNUM(DONE))
template <typename T>
static inline const T& at1(const std::vector<T>& v, int oneBasedIndex) {
    return v.at(static_cast<size_t>(oneBasedIndex - 1));
}

template <typename Container, typename T>
bool contains(const Container& c, const T& x) {
    return std::find(c.begin(), c.end(), x) != c.end();
}

// ----------------- C++ translation of SUBROUTINE R10_EQN ----------------------
/**
 * @brief C++ translation of Fortran SUBROUTINE R10_EQN(FORST, SPEC, VOLEQ, ERRFLAG)
 *
 * @param FORST   Two-character forest code, parsed as integer (e.g., "04").
 * @param SPEC    FIA species code (int). Special value 9999 triggers validation branch.
 * @param VOLEQ   Volume equation identifier (string). Set by this function.
 * @param ERRFLAG Error flag (0 = OK, 1 = species not found → fallback applied).
 */
VolumeEquation VolumeEquationResolver::GetR10VolumeEquation(VolumeCalculationOptions vco)
{
    int ERRFLAG = 0;
    int SPEC = vco.fiaCode;
    if (vco.auxFlag == VolumeCalculationOptions::AuxFlag::R10YOUNGGROWTH) SPEC += 2000;
    std::string VOLEQ;
    int FORNUM = vco.forest;

    // --- Binary search over FIA(27) to set DONE ---
    int FIRST = 1, LAST = static_cast<int>(FIA_R10.size()), DONE = 0;
    while (DONE == 0) {
        int HALF = (LAST - FIRST + 1) / 2 + FIRST;
        // Convert to 0-based
        const int idx = HALF - 1;
        if (FIA_R10.at(idx) == SPEC) {
            DONE = HALF; // 1-based index
        }
        else if (FIRST == LAST) {
            ERRFLAG = 1;
            DONE = -1;
        }
        else if (FIA_R10.at(idx) < SPEC) {
            FIRST = HALF;
        }
        else {
            LAST = HALF - 1;
        }
    }

    // --- If species not found, use Other Softwood (index 13) ---
    // Lutz spruce is handled by using FIA code 94 (already in list),
    // but this preserves the Fortran fallback behavior.
    if (DONE < 0) DONE = 13;

    // --- Select default VOLEQ based on forest number ---
    // CHUEQN for Chugach (FORNUM == 4), else TONEQN (Tongass)
    if (FORNUM == 4) {
        VOLEQ = at1(CHUEQN_R10, DONE);
    }
    else {
        VOLEQ = at1(TONEQN_R10, DONE);
    }
    return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
}

bool VolumeEquationResolver::isValidR10Equation(const std::string& VOLEQ)
{
    // --- Validation branch: search TONEQN and CHUEQN, then OTHEREQN ---
    for (int i = 1; i <= 27; ++i) {
        if (VOLEQ == at1(TONEQN_R10, i) || VOLEQ == at1(CHUEQN_R10, i)) {
            return true;
        }
    }
    for (int i = 1; i <= 27; ++i) {
        if (VOLEQ == at1(OTHEREQN_R10, i)) {
            return true;
        }
    }
    //check for 3-point equation
    if (VOLEQ.substr(5, 1) == "3") {
        std::string VOLEQ2 = VOLEQ.substr(0, 5) + "2" + VOLEQ.substr(6, 4);
        if (contains(TONEQN_R10, VOLEQ2) || contains(CHUEQN_R10, VOLEQ2) ||
            contains(OTHEREQN_R10, VOLEQ2) ) {
            return true;
        }
    }
    return false; // no match; leave unchanged
}