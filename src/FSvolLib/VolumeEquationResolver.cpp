//#include "pch.h"
#include "VolumeEquationResolver.h"

std::string VolumeEquationResolver::GetFvsVariantCode(VolumeCalculationOptions vco)
{
	int region = vco.region;
	int forest = vco.forest;
	int district = vco.district;

	// Region 8 - Southern
	if (region == 8) {
		return "SN";
	}
	// Region 1 - Northern
	else if (region == 1) {
		// Inland Empire forests: 4, 5, 17, 3, 14, 16
		if (forest == 4 || forest == 5 || forest == 17 ||
			forest == 3 || forest == 14 || forest == 16) {
			return "IE";
		}
		else {
			return "EM";  // Emmett
		}
	}
	// Region 5 - Pacific Southwest
	else if (region == 5) {
		// Inland California: 5, 6, 8, 11, 14
		if (forest == 5 || forest == 6 || forest == 8 ||
			forest == 11 || forest == 14) {
			return "CA";
		}
		// Southern Oregon
		else if (forest == 9) {
			return "SO";
		}
		// Western Sierra: 17, 16, 15, 13, 3
		else if (forest == 17 || forest == 16 || forest == 15 ||
			forest == 13 || forest == 3) {
			return "WS";
		}
		// Klamath/Northern California
		else if (forest == 5) {
			return "NC";
		}
	}
	// Region 6 - Pacific Northwest
	else if (region == 6) {
		// Blue Mountains: 4, 7, 14, 16
		if (forest == 4 || forest == 7 || forest == 14 || forest == 16) {
			return "BM";
		}
		// Eastern Cascades: 17, 8, or (3 and district 3), or (6 and districts 1,2,6)
		else if (forest == 17 || forest == 8 ||
			(forest == 3 && district == 3) ||
			(forest == 6 && (district == 1 || district == 2 || district == 6))) {
			return "EC";  // Mount Hood Barlow RD
		}
		// Southern Oregon: 1, 2, 20
		else if (forest == 1 || forest == 2 || forest == 20) {
			return "SO";
		}
		// Western Cascades: 5, 15, 18, 10, 3, 6
		else if (forest == 5 || forest == 15 || forest == 18 ||
			forest == 10 || forest == 3 || forest == 6) {
			return "WC";
		}
		// Pacific Northwest: 9, 12
		else if (forest == 9 || forest == 12) {
			return "PN";
		}
		// Northern California: 11
		else if (forest == 11) {
			return "NC";
		}
		// Inland Empire: 21
		else if (forest == 21) {
			return "IE";
		}
	}
	// Region 7
	else if (region == 7) {
		if (forest == 2) {
			return "WC";
		}
		else if (forest == 3) {
			return "NC";
		}
		else {
			return "SO";
		}
	}
	// Region 9 - Eastern
	else if (region == 9) {
		// Lake States: 13, 10, 3, 9, 4, 7, 2, 6
		if (forest == 13 || forest == 10 || forest == 3 ||
			forest == 9 || forest == 4 || forest == 7 ||
			forest == 2 || forest == 6) {
			return "LS";
		}
		// Central States: 12, 8, 5
		else if (forest == 12 || forest == 8 || forest == 5) {
			return "CS";
		}
		// Northeast: 21, 20, 19, 14, 22
		else if (forest == 21 || forest == 20 || forest == 19 ||
			forest == 14 || forest == 22) {
			return "NE";
		}
	}

	// Default or unknown
	return "";
}
