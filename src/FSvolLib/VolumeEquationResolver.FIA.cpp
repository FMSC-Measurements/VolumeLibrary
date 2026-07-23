#include "WeightFactorAndRefDataResolver.h"
#include "VolumecalCulators\NationalScaleVolumeBiomass.h"
#include "VolumeEquationResolver.h"
#include <array>
#include <vector>

//woodland species list (38)
static const std::vector<int> woodlandSpcdList = {
	58, 59, 60, 61, 62, 
	63, 65, 66, 69, 106,
	133,134,138,140,141,
	143,300,303,304,321,
	322,363,475,523,755,
	756,757,758,803,810,
	814,829,843,846,847,
	867,902,990
};

static const std::vector<std::string> woodlandVolEq = {
	"R03CHO0065","R03CHO0065","300DVEW999","300DVEW060","R03CHO0065",
	"R03CHO0066","R03CHO0065","R03CHO0066","R03CHO0065","R03CHO0106",
	"400DVEW133","R03CHO0106","300DVEW999","R03CHO0106","R03CHO0106",
	"R03CHO0106","N01HAH0313","300DVEW800","300DVEW999","200DVEW475",
	"200DVEW814","300DVEW999","200DVEW475","300DVEW999","300DVEW800",
	"300DVEW999","300DVEW999","300DVEW999","300DVEW800","300DVEW800",
	"200DVEW814","300DVEW800","300DVEW800","300DVEW800","300DVEW800",
	"300DVEW800","300DVEW999","300DVEW999",
	//OTHER override
	//VolEq         idx  Spc Locations
	"402DVEW065", //38 --63  S26LCAMIX
	"300DVEW060", //39 --63  S33
	"400DVEW065", //40 --65  S26LCA
	"N01HAH0129", //41 --66  S23LCS and S23LPS
	"N01STN0105", //42 --66  S23LLS
	"N01STN0313", //43 --321 S23LLS
	"N01HAH0313", //44 --321 S23LCS and S23LPS
	"300DVEW800", //45 --322 S22LAZN, S22LAZS, S22LNMN, S22LNMS
	"200DVEW475", //46 --322 S26LCA, S26LCAMIX, S26LEOR, S26LWOR, S26LORJJ, S26LEWA, S26LWWA, S26LWACF
	"300DVEW800"  //47 --756 and 758 S33,  //814 S22LAZN, S22LAZS, S22LNMN, S22LNMS
};

VolumeEquation VolumeEquationResolver::GetFiaVolumeEquation(VolumeCalculationOptions vco)
{
	std::string volEqStr;
	std::string ecoRegion;
	RefSpeciesData refSpeciesData = getRefSpeciesData(vco.fiaCode);

	if (refSpeciesData.jenkinsSpeciesGroupCD == 10) {
		volEqStr = "300DVEW999";
		int index = -1;
		//woodland species
		auto it = std::find(woodlandSpcdList.begin(), woodlandSpcdList.end(), vco.fiaCode);
		if (it != woodlandSpcdList.end()) {
			index = it - woodlandSpcdList.begin();
		}

		//check species override for some locations
		if (vco.fiaCode == 63 && vco.region == 8) {
			index = 39;
		}
		else if (vco.fiaCode == 65 && vco.region == 5) {
			index = 40;
		}
		else if (vco.fiaCode == 66 && vco.region == 9) {
			if (GetFvsVariantCode(vco) == "LS") {
				index = 42;
			}
			else index = 41;
		}
		else if (vco.fiaCode == 321 && vco.region == 9) {
			if (GetFvsVariantCode(vco) == "LS") {
				index = 43;
			}
			else index = 44;
		}
		else if (vco.fiaCode == 322 && vco.region == 3) {
			index = 45;
		}
		else if (vco.fiaCode == 322 && (vco.region == 5 || vco.region == 6)) {
			index = 46;
		}
		else if ((vco.fiaCode == 756 || vco.fiaCode == 758) && vco.region == 8) {
			index = 47;
		}
		else if (vco.fiaCode == 814 && vco.region == 3) {
			index = 47;
		}

		if (index >= 0) volEqStr = woodlandVolEq[index];
	}
	else {
		//NSVB equation
		volEqStr = "NVB";
		if (vco.ecoRegion.empty()) {
			if (vco.region >= 1 && vco.region <= 10) {
				//get ecoRegion using region, forest and district number
				int iEcoRegion = NationalScaleVolumeBiomass::getEcoProvince(vco.region, vco.forest, vco.district);
				if (iEcoRegion > 999) {
					volEqStr += "M" + string_helper::pad3(iEcoRegion - 1000);
				}
				else {
					volEqStr += "0" + string_helper::pad3(iEcoRegion);
				}
				volEqStr += string_helper::pad3(vco.fiaCode);
			}
			else {
				ecoRegion = "0000";
				volEqStr += ecoRegion + string_helper::pad3(vco.fiaCode);
			}
		}
		else {
			//check ecoRegion is valid
			if (NationalScaleVolumeBiomass::isValidEcoRegion(vco.ecoRegion)) {
				volEqStr += vco.ecoRegion + string_helper::pad3(vco.fiaCode);
			}
			else {
				volEqStr += "0000" + string_helper::pad3(vco.fiaCode);
			}
		}
		if (volEqStr.size() != 10) volEqStr = "";
	}

	return VolumeEquation::ParseVolumeEquationNumber(volEqStr);
}