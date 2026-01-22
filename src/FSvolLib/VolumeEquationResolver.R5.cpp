#include <map>

#include "VolumeEquationResolver.h"

// Map of FIA species codes to default volume equation numbers for Region 5
const std::map<int, std::string> VolumeEquationResolver::fiaToEquationMap_R5_ = {
	{11, "500WO2W015"},   // Pacific silver fir
	{15, "500WO2W015"},   // White fir
	{17, "I15FW2W017"},   // Grand fir (changed from 500WO2W015)
	{19, "500WO2W020"},   // Subalpine fir
	{20, "500WO2W020"},   // California red fir
	{21, "500WO2W020"},   // Shasta red fir
	{22, "500WO2W020"},   // Noble fir
	{41, "500WO2W081"},   // Port Orford cedar
	{62, "500DVEW060"},   // California juniper
	{64, "500DVEW060"},   // Juniper
	{65, "500DVEW060"},   // Utah juniper
	{73, "500WO2W202"},   // Western Larch
	{81, "500WO2W081"},   // Incense cedar
	{92, "500WO2W015"},   // Brewer spruce
	{93, "500WO2W015"},   // Engelmann spruce
	{101, "500WO2W108"},  // Whitebark pine
	{103, "500WO2W108"},  // Knobcone pine
	{104, "500WO2W108"},  // Foxtail pine
	{108, "500WO2W108"},  // Lodgepole pine
	{109, "500WO2W108"},  // Coulter pine
	{113, "500WO2W108"},  // Limber pine
	{116, "500WO2W116"},  // Jeffrey pine
	{117, "500WO2W117"},  // Sugar pine
	{119, "500WO2W117"},  // Western white pine
	{122, "500WO2W122"},  // Ponderosa pine
	{124, "500WO2W108"},  // Monterey pine
	{127, "500WO2W108"},  // Grey pine
	{133, "500WO2W116"},  // Singleleaf pinyon
	{137, "500WO2W117"},  // Washoe pine
	{142, "500WO2W108"},  // Great Basin bristlecone pine
	{201, "500WO2W202"},  // Bigcone Douglas-fir
	{202, "500WO2W202"},  // Douglas-fir
	{211, "500WO2W211"},  // Redwood
	{212, "500DVEW212"},  // Giant sequoia
	{231, "500WO2W108"},  // Pacific yew
	{242, "500WO2W081"},  // Western red cedar
	{251, "500DVEW807"},  // California nutmeg
	{263, "500WO2W015"},  // Western hemlock
	{264, "500WO2W015"},  // Mountain hemlock
	{299, "500WO2W108"},  // Other softwoods
	{301, "H00SN2W301"},  // Koa
	{312, "500DVEW312"},  // Bigleaf maple
	{333, "500DVEW807"},  // California buckeye
	{351, "500DVEW351"},  // Red alder
	{352, "500DVEW351"},  // White alder
	{361, "500DVEW361"},  // Pacific madrone
	{431, "500DVEW431"},  // Golden chinkapin
	{475, "500DVEW801"},  // Curl-leaf Mtn Mahogany
	{478, "500DVEW801"},  // Birchleaf Mtn Mahogany
	{492, "500DVEW807"},  // Pacific dogwood
	{510, "H01SN2W510"},  // Eucalyptus
	{514, "H00SN2W510"},  // Robusta Eucalyptus
	{542, "500DVEW807"},  // Oregon Ash
	{600, "500DVEW818"},  // Walnut
	{631, "500DVEW631"},  // Tanoak
	{671, "H00SN2W671"},  // Ohia
	{730, "500DVEW818"},  // California sycamore
	{746, "500DVEW818"},  // Quaking aspen
	{747, "500DVEW818"},  // Black cottonwood
	{768, "500DVEW801"},  // Bitter Cherry
	{801, "500DVEW801"},  // California live oak
	{805, "500DVEW805"},  // Canyon live oak
	{807, "500DVEW807"},  // Blue oak
	{811, "500DVEW811"},  // Engelmann's oak
	{815, "500DVEW815"},  // Oregon white oak
	{818, "500DVEW818"},  // California black oak
	{821, "500DVEW821"},  // California white oak
	{839, "500DVEW839"},  // Interior live oak
	{920, "500DVEW807"},  // Willow
	{981, "500DVEW981"},  // California laurel
	{998, "500DVEW981"},  // Other hardwoods
	{999, "500DVEW631"},  // Unknown
};


VolumeEquation VolumeEquationResolver::GetR5VolumeEquation(VolumeCalculationOptions vco)
{


	auto fvsVariant = GetFvsVariantCode(vco);
	int fiaCode = vco.fiaCode;

	// Apply variant-specific overrides for certain species
	// Whitebark pine (101) - use different equations based on variant
	if (fiaCode == 101) {
		if (fvsVariant == "SO") {
			fiaCode = 142;  // Map to Great Basin bristlecone pine equation
		}
		// Otherwise uses default mapping (108 group)
	}
	// Other softwoods (299 or 290)
	else if (fiaCode == 299 || fiaCode == 290) {
		if (fvsVariant == "SO" || fvsVariant == "NC") {
			fiaCode = 142;  // Map to Great Basin bristlecone pine equation
		}
		// Otherwise uses default mapping (108 group)
	}
	// Other hardwoods (998) - variant specific handling
	else if (fiaCode == 998) {
		if (fvsVariant == "SO") {
			// Use special index 70 -> maps to DVEW631
			fiaCode = 631;
		}
		else if (fvsVariant == "WS") {
			// Use special index 67 -> stays as DVEW981
			fiaCode = 981;
		}
		else if (fvsVariant == "NC") {
			// Use special index 70 -> maps to DVEW631
			fiaCode = 631;
		}
		// Otherwise uses default mapping (981)
	}

	// Look up the volume equation number
	auto it = fiaToEquationMap_R5_.find(fiaCode);
	if (it != fiaToEquationMap_R5_.end()) {
		return VolumeEquation::ParseVolumeEquationNumber(it->second);
	}

	// If not found, return unknown/default
	return VolumeEquation::ParseVolumeEquationNumber("500DVEW631");
}