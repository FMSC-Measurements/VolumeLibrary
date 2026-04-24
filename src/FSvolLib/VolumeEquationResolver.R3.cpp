#include "VolumeEquationResolver.h"

#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <cctype>

// ----------------- EQNUM table (52 entries; Fortran 1-based) -----------------
// Updated defaults applied as per your comment (YW 9/9/2024)
static const std::vector<std::string> EQNUM_R3 = {
    // 1..45 (paired to FIA codes by index; defaults)
    "NVB0000015","300DVEW093","NVB0000015","300DVEW093","300DVEW060",
    "300DVEW060","300DVEW060","300DVEW060","300DVEW060","300DVEW060",
    "300DVEW060","301DVEW015","NVBM330093","300DVEW093","300DVEW093",
    "300DVEW113","300DVEW113","300DVEW106","301DVEW202","300DVEW113",
    "NVBM240119","300DVEW122","300FW2W122","300DVEW106","300DVEW106",
    "300DVEW106","300FW2W202","301DVEW015","301DVEW015","300DVEW060",
    "300DVEW310","300DVEW314","300DVEW999","300DVEW999","300DVEW999",
    "300DVEW999","NVB0000746","300DVEW999","300DVEW800","300DVEW800",
    "300DVEW800","300DVEW800","300DVEW800","300DVEW800","300DVEW999",
    // 46..52 (no forest-specific overrides used anymore, but kept for validation)
    "302DVEW202","302DVEW202","302DVEW015","301FW2W122","301FW2W202",
    "301FW2W015","301FW2W108"
};

// Helper: Fortran-style 1-based EQNUM(i)
static inline const std::string& eqR3(int i) {
    return EQNUM_R3.at(static_cast<size_t>(i - 1));
}

// ----------------- FIA → default EQNUM map (first 45 entries by index) -------
static const std::map<int, std::string> fia_to_eqnum_R3 = {
    {  15, eqR3(1) }, // White fir
    {  17, eqR3(2) }, // Grand fir
    {  18, eqR3(3) }, // Corkbark fir
    {  19, eqR3(4) }, // Subalpine fir
    {  57, eqR3(5) }, // Juniper
    {  60, eqR3(6) }, // Juniper (second listing)
    {  63, eqR3(7) }, // Alligator juniper
    {  65, eqR3(8) }, // Utah juniper
    {  66, eqR3(9) }, // Rocky Mtn. Juniper
    {  68, eqR3(10) }, // Eastern redcedar
    {  69, eqR3(11) }, // Oneseed juniper
    {  73, eqR3(12) }, // Western Larch
    {  93, eqR3(13) }, // Engelmann's spruce (updated default)
    {  94, eqR3(14) }, // White spruce
    {  96, eqR3(15) }, // Blue spruce
    { 101, eqR3(16) }, // Whitebark pine
    { 102, eqR3(17) }, // Bristlecone pine
    { 106, eqR3(18) }, // Pinyon Pine
    { 108, eqR3(19) }, // Lodgepole pine
    { 113, eqR3(20) }, // Limber pine
    { 114, eqR3(21) }, // Southwestern white pine (updated default)
    { 118, eqR3(22) }, // Chihuahua pine
    { 122, eqR3(23) }, // Ponderosa pine (FW2W default retained at index 23)
    { 133, eqR3(24) }, // Singleleaf pinyon
    { 134, eqR3(25) }, // Border pinyon
    { 143, eqR3(26) }, // Arizona pinyon
    { 202, eqR3(27) }, // Douglas fir (updated default to 300FW2W202)
    { 242, eqR3(28) }, // Western Redcedar
    { 264, eqR3(29) }, // Mountain Hemlock
    { 299, eqR3(30) }, // Other softwoods
    { 310, eqR3(31) }, // Maple
    { 314, eqR3(32) }, // Black maple
    { 375, eqR3(33) }, // Paper Birch
    { 475, eqR3(34) }, // Mountain Mahogany
    { 740, eqR3(35) }, // Cottonwoods
    { 745, eqR3(36) }, // Plains cottonwood
    { 746, eqR3(37) }, // Quaking aspen (updated default NVB0000746)
    { 749, eqR3(38) }, // Narrowleaf cottonwood
    { 800, eqR3(39) }, // Oak
    { 803, eqR3(40) }, // Arizona white oak
    { 810, eqR3(41) }, // Emory oak
    { 814, eqR3(42) }, // Gambel oak
    { 823, eqR3(43) }, // Bur oak
    { 843, eqR3(44) }, // Silverleaf oak
    { 998, eqR3(45) }  // Other Hardwoods (Unknown default)
};

// ----------------- Small helpers ---------------------------------------------
//static inline std::string trim(const std::string& s) {
//    size_t b = 0, e = s.size();
//    while (b < e && std::isspace(static_cast<unsigned char>(s[b]))) ++b;
//    while (e > b && std::isspace(static_cast<unsigned char>(s[e - 1]))) --e;
//    return s.substr(b, e - b);
//}

/**
 * @brief C++ translation of Fortran SUBROUTINE R3_EQN(FORST, SPEC, VOLEQ, ERRFLAG)
 *
 * @param FORST   Two-character forest code, read as integer (e.g., "02", "11").
 * @param SPEC    FIA species code (int).
 * @param VOLEQ   Volume equation identifier (string). Set by this function.
 * @param ERRFLAG Error flag (0 = OK, 1 = not found in default map).
 */
VolumeEquation VolumeEquationResolver::GetR3VolumeEquation(VolumeCalculationOptions vco)
//static void R3_EQN_cpp(const std::string& FORST, int& SPEC, std::string& VOLEQ, int& ERRFLAG)
{
    int ERRFLAG = 0;
    int SPEC = vco.fiaCode;
    std::string VOLEQ = "300DVEW999";
    
    auto it = fia_to_eqnum_R3.find(SPEC);
    if (it != fia_to_eqnum_R3.end()) {
        VOLEQ = it->second;
    }
    else {
        // Not found: ERRFLAG=1 and Unknown (EQNUM(45) = "300DVEW999")
        ERRFLAG = 1;
        VOLEQ = eqR3(45);
    }
    return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
}

bool VolumeEquationResolver::isValidR3Equation(const std::string& voleq)
{
    // --- Validation branch: SPEC == 9999 ---
    if (std::find(EQNUM_R3.begin(), EQNUM_R3.end(), voleq) != EQNUM_R3.end()) {
        return true;
    }
    return false; // parity with Fortran (no change if not found)
}