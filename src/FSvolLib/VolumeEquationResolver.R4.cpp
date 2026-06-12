#include "VolumeEquationResolver.h"

#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <cctype>

// --- Full R4_EQNUM table (57 entries) ---
static const std::vector<std::string> R4_EQNUM = {
    // 1..27 (used directly for FIA-mapped species)
    "400MATW020","300DVEW060","300DVEW060","400DVEW066","400MATW073",
    "400MATW081","300DVEW106","400MATW117","400MATW117","400DVEW133",
    "400DVEW998","400MATW081","400MATW015","401MATW015","400MATW108",
    "400MATW108","400MATW108","400MATW108","400DVEW475","400MATW746",
    "400DVEW998","400MATW108","400MATW108","300DVEW800","300DVEW800",
    "400DVEW998","400DVEW998",
    // 28..57 (used in species-specific overrides)
    "I15FW2W017","401MATW015","400MATW015","I15FW2W017","405MATW019",
    "400MATW019","401DVEW065","400DVEW065","I15FW2W093","407FW2W093",
    "400MATW093","401MATW108","400MATW108","I15FW2W122","401MATW122",
    "402MATW122","403MATW122","400MATW122","I15FW2W202","405MATW202",
    "400MATW202","400DVEW064","400DVEW106","407MATW093","401MATW202",
    "407MATW093","401MATW202","401MATW202","407MATW093","407MATW093",
    //add Flewelling equation for 07 PP
    "407FW2W122"
};

// Helper to access Fortran-style R4_EQNUM(i) with 1-based index.
static inline const std::string& eq(int i) {
    return R4_EQNUM.at(static_cast<size_t>(i - 1));
}
static const std::map<int, std::string> fia_to_eqnum = {
    {  20, eq(1) }, // California red fir
    {  60, eq(2) }, // Juniper
    {  64, eq(3) }, // Western juniper
    {  66, eq(4) }, // Rocky Mtn juniper
    {  73, eq(5) }, // Western larch
    {  81, eq(6) }, // Incense-cedar
    { 106, eq(7) }, // Common pinyon
    { 117, eq(8) }, // Sugar pine
    { 119, eq(9) }, // Western white pine
    { 133, eq(10) }, // Singleleaf pinyon
    { 231, eq(11) }, // Pacific yew
    { 242, eq(12) }, // Western redcedar
    { 263, eq(13) }, // Western hemlock
    { 264, eq(14) }, // Mountain hemlock
    { 299, eq(15) }, // Other softwoods
    { 313, eq(16) }, // Box elder
    { 321, eq(17) }, // Rocky Mtn maple
    { 322, eq(18) }, // Bigtooth maple
    { 475, eq(19) }, // Curlleaf Mtn Mahogany
    { 746, eq(20) }, // Quaking aspen
    { 747, eq(21) }, // Black cottonwood
    { 748, eq(22) }, // Fremont cottonwod
    { 749, eq(23) }, // Narrowleaf cottonwood
    { 800, eq(24) }, // Oak (sp.)
    { 814, eq(25) }, // Gambel oak
    { 998, eq(26) }, // Other hardwoods
    { 999, eq(27) }  // Other
};

// Tiny helper: check membership of an integer in a list
template <typename... Ints>
static inline bool in(int value, Ints... xs) {
    int arr[] = { xs... };
    for (int v : arr) { if (value == v) return true; }
    return false;
}

/**
 * @brief C++ translation of Fortran SUBROUTINE R4_EQN(FORST, SPEC, VOLEQ, ERRFLAG)
 *
 * @param FORST   Two-character forest code, read as integer (e.g., "05", "12").
 * @param SPEC    Species code (modified when SPEC==9999 and VOLEQ validated).
 * @param VOLEQ   Volume equation identifier (10 chars typical). Set by this function.
 * @param ERRFLAG Error flag (0 = OK, 1 = not found in FIA fallback).
 */
VolumeEquation VolumeEquationResolver::GetR4VolumeEquation(VolumeCalculationOptions vco)
{
    //int ERRFLAG = 0;
    int FORNUM = vco.forest;
    int SPEC = vco.fiaCode;
    std::string VOLEQ = "400DVEW998";

    // --- Species-specific overrides (direct translation of Fortran logic) ---
    // White fir
    if (SPEC == 15) {
        if (in(FORNUM, 2, 12, 13, 6))           VOLEQ = eq(28);
        else if (in(FORNUM, 9, 17))             VOLEQ = eq(29);
        else                                    VOLEQ = eq(30);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // Grand fir
    if (SPEC == 17) {
        if (in(FORNUM, 2, 12, 13))              VOLEQ = eq(31);
        else                                    VOLEQ = eq(30);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // Subalpine fir
    if (SPEC == 19) {
        if (FORNUM == 5)                       VOLEQ = eq(32);
        else                                   VOLEQ = eq(33);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // Utah Juniper (SPEC == 64)
    if (SPEC == 64) {
        if (in(FORNUM, 1, 4, 7, 8, 9, 10, 17, 18, 19) ||
            in(FORNUM, 2, 6, 12, 13, 14))
        {
            VOLEQ = eq(49);
            // Note: Fortran has no else here; if not matched, VOLEQ remains as-is.
            return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
        }
    }

    // SPEC == 65
    if (SPEC == 65) {
        if (in(FORNUM, 3, 5, 15, 16))               VOLEQ = eq(34);
        else                                        VOLEQ = eq(35);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // Engelmann spruce (93) OR Blue spruce (96)
    if (SPEC == 93 || SPEC == 96) {
        if (in(FORNUM, 2, 12, 13))              VOLEQ = eq(36);
        else if (in(FORNUM, 7, 8))              VOLEQ = eq(37);
        else                                    VOLEQ = eq(38);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // Whitebark (101), Lodgepole (108), Limber (113), Bristlecone (142)
    if (SPEC == 101 || SPEC == 108 || SPEC == 113 || SPEC == 142) {
        if (in(FORNUM, 9, 17))                  VOLEQ = eq(39);
        else                                    VOLEQ = eq(40);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // Pinyon pine (SPEC == 106)
    if (SPEC == 106) {
        if (in(FORNUM, 1, 4, 7, 8, 9, 10, 17, 18, 19))
        {
            VOLEQ = eq(50);
            // No else (mirrors Fortran)
            return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
        }
    }

    // Ponderosa pine (SPEC == 122)
    if (SPEC == 122) {
        if (in(FORNUM, 2, 12, 13))              VOLEQ = eq(41);
        else if (FORNUM == 1)                   VOLEQ = eq(42);
        else if (in(FORNUM, 7, 8, 10, 18, 19))  VOLEQ = eq(43);
        else if (in(FORNUM, 9, 17))             VOLEQ = eq(44);
        else                                    VOLEQ = eq(45);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // Douglas fir (SPEC == 202)
    if (SPEC == 202) {
        if (in(FORNUM, 2, 12, 13))              VOLEQ = eq(46);
        else if (FORNUM == 5)                   VOLEQ = eq(47);
        else                                    VOLEQ = eq(48);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // Other hardwoods (SPEC == 998)
    if (SPEC == 998) {
        if (in(FORNUM, 2, 6, 12, 13, 14))       VOLEQ = eq(17);
        else                                    VOLEQ = eq(21);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    auto it = fia_to_eqnum.find(SPEC);
    if (it != fia_to_eqnum.end()) {
        VOLEQ = it->second;
    }
    else {
        // Fortran: ERRFLAG = 1 ; VOLEQ = R4_EQNUM(26)  ! Other Hardwood
        //ERRFLAG = 1;
        VOLEQ = eq(26);
    }
    return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    
}

bool VolumeEquationResolver::isValidR4Equation(const std::string& voleq)
{
    auto it = std::find(R4_EQNUM.begin(), R4_EQNUM.end(), voleq);
    if (it != R4_EQNUM.end()) {
        return true;
    }
    //check 3-point equation
    if (voleq.substr(5, 1) == "3") {
        std::string voleqNew = voleq.substr(0, 5) + "2" + voleq.substr(6, 4);
        if (std::find(R4_EQNUM.begin(), R4_EQNUM.end(), voleqNew) != R4_EQNUM.end()) {
            return true;
        }
    }
    return false; 
}