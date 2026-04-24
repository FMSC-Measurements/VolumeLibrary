#include "VolumeEquationResolver.h"

#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <cctype>
#include <stdexcept>

// ----------------- EQNUM table (81 entries; Fortran 1-based) -----------------
static const std::vector<std::string> EQNUM_R1 = {
    // 1..38 (paired to FIA codes by index)
    "I00FW2W012","102DVEW017","I00FW2W017","I00FW2W019","102DVEW060",
    "102DVEW106","I00FW2W019","I00FW2W073","I00FW2W093","102DVEW090",
    "102DVEW090","I00FW2W119","102DVEW108","102DVEW106","I00FW2W108",
    "I00FW2W073","I00FW2W119","I00FW2W122","102DVEW106","102DVEW106",
    "I00FW2W202","616BEHW231","I00FW2W242","I00FW2W260","I00FW2W260",
    "I00FW2W260","200DVEW746","101DVEW375","400DVEW475","101DVEW740",
    "102DVEW740","101DVEW740","102DVEW740","102DVEW746","102DVEW740",
    "102DVEW740","200DVEW746","I00FW2W260",
    // 39..81 (used by overrides / validation)
    "203FW2W122","102DVEW740","101DVEW108","100JB2W202","101DVEW202",
    "102DVEW202","100FW2W202","103DVEW202","101DVEW017","100JB2W017",
    "101DVEW060","100JB2W108","104DVEW108","103DVEW108","102DVEW260",
    "100JB2W122","104DVEW122","102DVEW122","105DVEW122","106DVEW122",
    "101DVEW122","102DVEW019","101DVEW019","100JB2W073","102DVEW073",
    "101DVEW073","101DVEW999","101DVEW260","102DVEW101","I12FW2W012",
    "I11FW2W012","I13FW2W012","I14FW2W012","I21FW2W012","I22FW2W012",
    "102DVEW070","101DVEW240","102DVEW240","102DVEW119","101DVEW119",
    "103DVEW122","100JB2W019","101DVEW090"
};

// Helper: Fortran-style 1-based EQNUM(i)
static inline const std::string& eqR1(int i) {
    // at() throws on out-of-range, which is useful in debugging
    return EQNUM_R1.at(static_cast<size_t>(i - 1));
}

// ----------------- FIA → default EQNUM map (first 38 entries by index) -------
static const std::map<int, std::string> fia_to_eqnum_R1 = {
    {  12, eqR1(1) }, // Balsam fir
    {  15, eqR1(2) }, // White fir
    {  17, eqR1(3) }, // Grand fir
    {  19, eqR1(4) }, // Subalpine fir
    {  64, eqR1(5) }, // Western juniper
    {  66, eqR1(6) }, // Rocky Mt juniper
    {  72, eqR1(7) }, // Subalpine larch
    {  73, eqR1(8) }, // Western larch
    {  93, eqR1(9) }, // Engelmann spruce
    {  94, eqR1(10) }, // White spruce
    {  96, eqR1(11) }, // Blue spruce
    { 101, eqR1(12) }, // Whitebark pine
    { 102, eqR1(13) }, // Bristlecone pine
    { 106, eqR1(14) }, // Pinyon pine
    { 108, eqR1(15) }, // Lodgepole pine
    { 113, eqR1(16) }, // Limber pine
    { 119, eqR1(17) }, // Western white pine
    { 122, eqR1(18) }, // Ponderosa pine
    { 133, eqR1(19) }, // Singleleaf pinyon
    { 134, eqR1(20) }, // Border pinyon
    { 202, eqR1(21) }, // Douglas fir
    { 231, eqR1(22) }, // Pacific yew
    { 242, eqR1(23) }, // Western redcedar
    { 263, eqR1(24) }, // Western hemlock
    { 264, eqR1(25) }, // Mountain hemlock
    { 299, eqR1(26) }, // Other Softwoods
    { 321, eqR1(27) }, // Rocky Mountain maple
    { 375, eqR1(28) }, // Paper birch
    { 475, eqR1(29) }, // Curlleaf mountain mahogany
    { 544, eqR1(30) }, // Green ash
    { 740, eqR1(31) }, // Cottonwood
    { 741, eqR1(32) }, // Balsam poplar
    { 745, eqR1(33) }, // Plains cottonwood
    { 746, eqR1(34) }, // Quaking aspen
    { 747, eqR1(35) }, // Black cottonwood
    { 749, eqR1(36) }, // Narrowleaf cottonwood
    { 998, eqR1(37) }, // Other hardwood
    { 999, eqR1(38) }  // Unknown
};

// ----------------- Small helpers ---------------------------------------------
//static inline std::string trim(const std::string& s) {
//    size_t b = 0, e = s.size();
//    while (b < e && std::isspace(static_cast<unsigned char>(s[b]))) ++b;
//    while (e > b && std::isspace(static_cast<unsigned char>(s[e - 1]))) --e;
//    return s.substr(b, e - b);
//}
//
//static inline std::string to_upper(std::string s) {
//    std::transform(s.begin(), s.end(), s.begin(),
//        [](unsigned char c) { return static_cast<chard::toupper(c)); });
//        return s;
//}

// ----------------- C++ translation of SUBROUTINE R1_EQN ----------------------
/**
 * @brief C++ translation of Fortran SUBROUTINE R1_EQN(FORST, SPEC, VAR, VOLEQ, ERRFLAG)
 *
 * @param FORST   Two-character forest code, read as integer (e.g., "08", "12")
 * @param SPEC    FIA species code (may be remapped; may be set to 8888 in validation branch)
 * @param VAR     FVS variant code (2-char), case-insensitive comparisons used
 * @param VOLEQ   Volume equation identifier (10 chars typical). Set by this function.
 * @param ERRFLAG Error flag (0 = OK, 1 = not found in FIA map → fallback applied)
 */
VolumeEquation VolumeEquationResolver::GetR1VolumeEquation(VolumeCalculationOptions vco)
//static void R1_EQN_cpp(const std::string& FORST, int& SPEC, const std::string& VAR,
//    std::string& VOLEQ, int& ERRFLAG)
{
    int ERRFLAG = 0;
    int FORNUM = vco.forest;
    int SPEC = vco.fiaCode;
    std::string VOLEQ = "I00FW2W260";
    //std::string varU = GetFvsVariantCode(vco);

    // --- Species remapping for CruiseProcessing (YW 09/09/2024) ---
    if (SPEC == 70)  SPEC = 73;
    if (SPEC == 90)  SPEC = 93;
    if (SPEC == 260) SPEC = 263;

    // --- Species-specific overrides ---
    // If (SPEC == 122 and FORNUM == 8) → EQNUM(39)
    if (SPEC == 122 && FORNUM == 8) {
        VOLEQ = eqR1(39);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // If SPEC == 101 and VAR ∈ {EM, IE, CI} (case-insensitive) → EQNUM(1)
    {
        const std::string varU = GetFvsVariantCode(vco);
        if (SPEC == 101 && (varU == "EM" || varU == "IE" || varU == "CI")) {
            VOLEQ = eqR1(1);
            return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
        }
    }

    // If VAR == EM (case-insensitive) AND SPEC ∈ {745, 747, 749} → EQNUM(40)
    {
        const std::string varU = GetFvsVariantCode(vco);
        if (varU == "EM" && (SPEC == 745 || SPEC == 747 || SPEC == 749)) {
            VOLEQ = eqR1(40);
            return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
        }
    }

    // --- Fallback: use FIA → EQNUM map (equivalent to Fortran binary search) ---
    auto it = fia_to_eqnum_R1.find(SPEC);
    if (it != fia_to_eqnum_R1.end()) {
        VOLEQ = it->second;
    }
    else {
        // Not found: set ERRFLAG and choose Other Softwood/Hardwood per SPEC
        ERRFLAG = 1;
        if (SPEC < 300) VOLEQ = eqR1(26); // Other Softwood
        else            VOLEQ = eqR1(36); // Other Hardwood
    }
    return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
}

bool VolumeEquationResolver::isValidR1Equation(const std::string& voleq)
{
    // --- Validation branch: SPEC == 9999 ---
    // If VOLEQ matches any EQNUM[i], SPEC=8888 and return
    if (std::find(EQNUM_R1.begin(), EQNUM_R1.end(), voleq) != EQNUM_R1.end()) {
        return true;
    }
    // Prefix checks on first 7 chars
    const std::string prefix = voleq.size() >= 7 ? voleq.substr(0, 7) : std::string();
    if (prefix == "616BEHW" || prefix == "632BEHW" || prefix == "B00BEHW") {
        return true;
    }
    return false; // No match; leave values unchanged (parity with Fortran)
}