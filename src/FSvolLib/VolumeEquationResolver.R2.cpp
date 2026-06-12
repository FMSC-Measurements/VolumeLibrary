#include "VolumeEquationResolver.h"

#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <cctype>

// ----------------- EQNUM table (75 entries; Fortran 1-based) -----------------
static const std::vector<std::string> EQNUM_R2 = {
    // 1..42 (paired to FIA codes by index; defaults)
    "200FW2W015","I00FW2W019","I00FW2W019","I00FW2W019","300DVEW060",
    "300DVEW060","200DVEW065","200DVEW066","300DVEW060","200DVEW069",
    "407FW2W093","407FW2W093","407FW2W093","407FW2W093","200FW2W122",
    "200FW2W122","200DVEW106","200FW2W108","200FW2W122","200FW2W122",
    "200FW2W122","200FW2W122","300DVEW106","300DVEW106","300DVEW106",
    "200FW2W202","407FW2W093","407FW2W093","300DVEW060","300DVEW999",
    "200DVEW475","300DVEW999","300DVEW999","200FW2W746","300DVEW999",
    "300DVEW800","300DVEW800","300DVEW800","200DVEW814","200DVEW823",
    "300DVEW800","200DVEW998",
    // 43..75 (used by overrides/validation)
    "203FW2W122","213FW2W122","202FW2W108","200CZ2W202","200DVEW093",
    "200CZ2W093","210DVEW093","200DVEW814","200DVEW108","210DVEW108",
    "200CZ2W108","212DVEW122","213DVEW122","200CZ2W122","203CZ2W122",
    "210DVEW122","200DVEW122","203DVEW122","200CZ2W746","200DVEW746",
    "210DVEW746","200DVEW066","200CZ2W019","200CZ2W015","200DVEW015",
    "200CZ3W202","200CZ3W093","200CZ3W108","200CZ3W122","203CZ3W122",
    "200CZ3W746","200CZ3W019","200CZ3W015"
};

// Helper: Fortran-style 1-based EQNUM(i)
static inline const std::string& eqR2(int i) {
    return EQNUM_R2.at(static_cast<size_t>(i - 1));
}

// ----------------- FIA ? default EQNUM map (first 42 entries by index) -------
static const std::map<int, std::string> fia_to_eqnum_R2 = {
    {  15, eqR2(1) }, // White fir
    {  17, eqR2(2) }, // Grand fir
    {  18, eqR2(3) }, // Corkbark fir
    {  19, eqR2(4) }, // Subalpine fir
    {  57, eqR2(5) }, // Juniper
    {  63, eqR2(6) }, // Alligator juniper
    {  65, eqR2(7) }, // Utah juniper
    {  66, eqR2(8) }, // Rocky Mtn. juniper (changed default to 200DVEW066)
    {  68, eqR2(9) }, // Eastern redcedar
    {  69, eqR2(10) }, // Oneseed juniper
    {  73, eqR2(11) }, // Western larch
    {  93, eqR2(12) }, // Engelmann's spruce
    {  94, eqR2(13) }, // White spruce
    {  96, eqR2(14) }, // Blue spruce
    { 101, eqR2(15) }, // Whitebark pine
    { 102, eqR2(16) }, // Bristlecone pine
    { 106, eqR2(17) }, // Pinyon pine
    { 108, eqR2(18) }, // Lodgepole pine
    { 113, eqR2(19) }, // Limber pine
    { 114, eqR2(20) }, // Southwestern white pine
    { 118, eqR2(21) }, // Chihuahua pine
    { 122, eqR2(22) }, // Ponderosa pine
    { 133, eqR2(23) }, // Singleleaf pinyon
    { 134, eqR2(24) }, // Border pinyon
    { 143, eqR2(25) }, // Arizona pinyon
    { 202, eqR2(26) }, // Douglas fir
    { 242, eqR2(27) }, // Western redcedar
    { 264, eqR2(28) }, // Mountain hemlock
    { 299, eqR2(29) }, // Other softwoods
    { 375, eqR2(30) }, // Paper birch
    { 475, eqR2(31) }, // Mountain mahogany
    { 740, eqR2(32) }, // Cottonwoods
    { 745, eqR2(33) }, // Plains cottonwood
    { 746, eqR2(34) }, // Quaking aspen
    { 749, eqR2(35) }, // Narrowleaf cottonwood
    { 800, eqR2(36) }, // Oak
    { 803, eqR2(37) }, // Arizona white oak
    { 810, eqR2(38) }, // Emory oak
    { 814, eqR2(39) }, // Gambel oak (changed default to 200DVEW814)
    { 823, eqR2(40) }, // Bur oak
    { 843, eqR2(41) }, // Silverleaf oak
    { 998, eqR2(42) }  // Other hardwoods
};

// ----------------- Small helpers ---------------------------------------------
static inline std::string trim(const std::string& s) {
    size_t b = 0, e = s.size();
    while (b < e && std::isspace(static_cast<unsigned char>(s[b]))) ++b;
    while (e > b && std::isspace(static_cast<unsigned char>(s[e - 1]))) --e;
    return s.substr(b, e - b);
}

/**
 * @brief C++ translation of Fortran SUBROUTINE R2_EQN(FORST, SPEC, VOLEQ, ERRFLAG)
 *
 * @param FORST   Two-character forest code, read as integer (e.g., "03", "13").
 * @param SPEC    FIA species code (int). May be unchanged or used to select defaults/overrides.
 * @param VOLEQ   Volume equation identifier (string). Set by this function.
 * @param ERRFLAG Error flag (0 = OK, 1 = not found in default map).
 */
VolumeEquation VolumeEquationResolver::GetR2VolumeEquation(VolumeCalculationOptions vco)
//static void R2_EQN_cpp(const std::string& FORST, int& SPEC, std::string& VOLEQ, int& ERRFLAG)
{
    int ERRFLAG = 0;
    int SPEC = vco.fiaCode;
    int FORNUM = vco.forest;
    std::string VOLEQ = "200DVEW998";


    // --- Species-specific overrides ---
    if (SPEC == 122 && FORNUM == 3) {
        VOLEQ = eqR2(43);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }
    else if (SPEC == 122 && FORNUM == 13) {
        VOLEQ = eqR2(44);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }
    else if (SPEC == 108 && (FORNUM == 2 || FORNUM == 14)) {
        VOLEQ = eqR2(45);
        return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
    }

    // --- Fallback: use FIA ? EQNUM map (equivalent to Fortran binary search) ---
    auto it = fia_to_eqnum_R2.find(SPEC);
    if (it != fia_to_eqnum_R2.end()) {
        VOLEQ = it->second;
    }
    else {
        // Not found: ERRFLAG=1 and Other Hardwood (EQNUM(42))
        ERRFLAG = 1;
        VOLEQ = eqR2(42);
    }
    return VolumeEquation::ParseVolumeEquationNumber(VOLEQ);
}

bool VolumeEquationResolver::isValidR2Equation(const std::string& voleq)
{
    // --- Validation branch: SPEC == 9999 ---
    if (std::find(EQNUM_R2.begin(), EQNUM_R2.end(), voleq) != EQNUM_R2.end()) {
        return true;
    }
    //check 3-point equation
    else if (voleq.substr(5, 1) == "3") {
        std::string voleqNew = voleq.substr(0, 5) + "2" + voleq.substr(6, 4);
        if (std::find(EQNUM_R2.begin(), EQNUM_R2.end(), voleqNew) != EQNUM_R2.end()) {
            return true;
        }
    }
    return false; // No match; parity with Fortran
}