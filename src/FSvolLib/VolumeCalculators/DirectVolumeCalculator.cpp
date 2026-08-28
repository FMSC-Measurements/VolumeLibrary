#include "DirectVolumeCalculator.h"
#include "DirectVolumeCalculator_R1.h"
#include "DirectVolumeCalculator_R234.h"
#include "DirectVolumeCalculator_R5.h"
#include "DirectVolumeCalculator_R610.h"
#include "DirectVolumeCalculator_R89.h"
#include "DirectVolumeCalculator_BIA.h"
#include "DirectVolumeCalculator_FIA_Eastern.h"
#include "DirectVolumeCalculator_FIA_RockyMountain.h"
#include "DirectVolumeCalculator_FIA_Southern.h"
#include "DirectVolumeCalculator_FIA_PacificCoast.h"
#include "HawaiiSharpnackVolume.h"
#include "MerchHeightCalculator_R89.h"
#include "..\SmalianScribnerIntl14.h"
#include <cstring>

TreeOutput DirectVolumeCalculator::CalculateVolume(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
	TreeOutput result;
	switch (volumeEquation_.geoCode) {
	case VolumeEquation::GeoCode::R1:
	{
		if (std::string(volumeEquation_.subregionalCode, 2) != "01") result =  R1KEMP(volumeEquationNumber, vco, tree);
		else
		{
			result = R1ALLENC(volumeEquationNumber, vco, tree, merchRules);
			double bfvol = 0.0;
			if ((volumeEquation_.fiaCode == 108 && tree.dbh >= 6.0) || tree.dbh >= 7.0)
			{
				bfvol = R1ALLENB(volumeEquationNumber, vco, tree, merchRules);
			}
			result.grossBoardFootPrimary = bfvol;
		}
		result.cordMerchantable = result.grossCubicFootPrimary / 90.0;
		return result;
	}
	case VolumeEquation::GeoCode::R2:
		return R2OLDV(volumeEquationNumber, vco, tree, merchRules);
	case VolumeEquation::GeoCode::R3:
		return R3D2HV(volumeEquationNumber, vco, tree, merchRules);
	case VolumeEquation::GeoCode::R4:
		return R4D2H(volumeEquationNumber, vco, tree);
	case VolumeEquation::GeoCode::R5:
	{
		if (volumeEquation_.modelType == VolumeEquation::ModelType::TRF) {
			return PNW_Tarif_Vol(volumeEquationNumber, tree, merchRules); 
		}
		else {
			return r5dve::R5HARV(volumeEquationNumber, tree, merchRules);
		}
	}
	case VolumeEquation::GeoCode::R6:
	{
		if (volumeEquation_.modelType == VolumeEquation::ModelType::TRF) {
			return PNW_Tarif_Vol(volumeEquationNumber, tree, merchRules); 
		}
		else {
			return R6VOL2(volumeEquationNumber, tree);
		}
	}
	case VolumeEquation::GeoCode::R8:
		return r8Lasher(volumeEquationNumber, tree, vco, merchRules);
	case VolumeEquation::GeoCode::R9:
	{
		if (std::string(volumeEquation_.subregionalCode, 2) == "25" ) {
			return r9Hahn(volumeEquationNumber, tree, vco, merchRules);
		}
		else {
			return r9Gevorkiantz(volumeEquationNumber, tree, vco, merchRules);
		}
	}
	case VolumeEquation::GeoCode::R10:
		return r10d2h(volumeEquationNumber, tree, merchRules);
	case VolumeEquation::GeoCode::HAWAII:
		return R12VOL(volumeEquationNumber, vco, tree, merchRules);
	case VolumeEquation::GeoCode::BIA_EAST:   //BIA eastern region DVE C00DVEE***
		return Voleq_Honer(volumeEquationNumber, vco, tree, merchRules);
	case VolumeEquation::GeoCode::INGYMODEL:  //BIA west DVE I00DVEW000
		return VolEq_Johnson(tree);
	case VolumeEquation::GeoCode::ARMY_BASE:  //DOD equation M01DVE0000 and M02DVE0000
	{
		if (tree.dbh < 10) {
			result.errflag = 3;
			return result;
		}

		double numLogs = 0.0;
		if (tree.merchHeightUnit == TreeMeasurment::MerchHeightUnit::LOGS16) {
			if (tree.merchHeightSaw > 0.0) {
				numLogs = tree.merchHeightSaw;
				if (numLogs > 10.0) numLogs = numLogs / 10.0;
			}
		}
		else if (tree.merchHeightSaw > 0.0) {
			//height in feet and need to convert to number of 16-foot logs
			double raw = tree.merchHeightSaw / 16.3;
			numLogs = std::floor(raw * 2.0) / 2.0;
		}
		if (numLogs < 1) {
			result.errflag = 7;
			return result;
		}
		result.grossInternationalBoardFoot = treeFormClass78BoardFootTable(tree.dbh, numLogs, "I");
		if (volumeEquation_.volEqStr.substr(1,2) == "01") {
			result.grossBoardFootPrimary = treeFormClass78BoardFootTable(tree.dbh, numLogs, "D");
		}
		else if (volumeEquation_.volEqStr.substr(1, 2) == "02") {
			result.grossBoardFootPrimary = treeFormClass78BoardFootTable(tree.dbh, numLogs, "I");
		}
		else {
			result.grossBoardFootPrimary = treeFormClass78BoardFootTable(tree.dbh, numLogs, "S");
		}
		return result;
	}
	case VolumeEquation::GeoCode::ROCKYMOUNTAIN:
	{
		if (volumeEquation_.volEqStr.substr(3, 3) == "CHO") {
			double drc = tree.drc > 0.0 ? tree.drc : tree.dbh;
			result =  ChojnackyWoodlandVol(volumeEquation_.fiaCode, drc, tree.totalHeight);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "KEM") {
			result =  Kemp_Vol(volumeEquation_.fiaCode, tree.dbh, tree.totalHeight, merchRules.minimumBoardFootDiameter);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "MOI") {
			result = Moisen_Vol(volumeEquation_.volEqStr, tree.dbh, tree.totalHeight, merchRules.minTopDibSaw, merchRules.minimumBoardFootDiameter);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "RMR") {
			//The equation R00RMR0*** is same as S00SRS0***
			result = SRS_Vol(volumeEquation_.volEqStr, tree, merchRules.minimumBoardFootDiameter);
		}
		return result;
	}
	case VolumeEquation::GeoCode::EASTERN:
	{
		if (volumeEquation_.volEqStr.substr(3, 3) == "HAH") {
			result =  Hahn_NC_Vol(volumeEquation_.fiaCode, tree, vco.siteIndex, vco.basalArea, merchRules.minTopDibSaw, merchRules.minimumBoardFootDiameter);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "STN") {
			result = Stone_NC_Vol(volumeEquation_.fiaCode, tree, merchRules.minTopDibSaw, vco.siteIndex, vco.basalArea);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "SCT") {
			if (tree.totalHeight > 0.0 && tree.merchHeightSaw == 0.0) {
				double merchHeightSaw = r89MerchHeight(9, 1, volumeEquation_.fiaCode, tree.dbh, tree.totalHeight, merchRules.minTopDibSaw, vco.basalArea, vco.siteIndex, false, false);
				if (merchHeightSaw > 0.0) tree.merchHeightSaw = merchHeightSaw;
			}
			if (tree.totalHeight > 0.0 && tree.merchHeightNonsaw == 0.0) {
				double merchHeightNonsaw = r89MerchHeight(9, 1, volumeEquation_.fiaCode, tree.dbh, tree.totalHeight, merchRules.minTopDibNonSaw, vco.basalArea, vco.siteIndex, true, false);
				if (merchHeightNonsaw > 0.0) tree.merchHeightNonsaw = merchHeightNonsaw;
			}

			result = Scott_Vol(volumeEquation_.fiaCode, tree);
		}
		return result;
	}
	case VolumeEquation::GeoCode::SOUTHERN:
	{
		if (volumeEquation_.volEqStr.substr(3, 3) == "SRS") {
			result =  SRS_Vol(volumeEquation_.volEqStr, tree, merchRules.minimumBoardFootDiameter);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "BRA") {
			result =  Brandeis_Vol(volumeEquation_.volEqStr, tree);
		}
		return result;
	}
	case VolumeEquation::GeoCode::PACIFIC_COAST:
	{
		if (volumeEquation_.volEqStr.substr(3, 3) == "DMR") {
			result = DeMars_Vol(volumeEquation_.volEqStr, tree, merchRules);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "EMB") {
			result = Embry_Vol(volumeEquation_.fiaCode, tree, merchRules);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "DEE") {
			result = DeMars_Embry_Vol(volumeEquation_.volEqStr, tree, merchRules);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "BDE") {
			result = Browne_DeMars_Embry_Vol(volumeEquation_.volEqStr, tree, merchRules);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "MAC") {
			result = MacLean_Tarif_Vol(volumeEquation_.volEqStr, tree, merchRules);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "PIL") {
			double CV8 = Pillsbury_CV8(volumeEquation_.volEqStr, tree.dbh, tree.totalHeight);
			result = DNR24_Tarif_Vol(volumeEquation_.volEqStr, tree, merchRules, CV8, "CV8");
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "CRT") {
			double CVT = Curtis_RedAlder_CVT(tree.dbh, tree.totalHeight);
			result = DNR24_Tarif_Vol(volumeEquation_.volEqStr, tree, merchRules, CVT, "CVT");
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "CHT") {
			double CVTS = Chittester_WesternJuniper_CVTS(tree.dbh, tree.totalHeight);
			if (tree.dbh < 5.0 || tree.totalHeight < 10.0) {
				result.totalCubicFoot = CVTS;
				return result;
			}
			else {
				double CV4 = (CVTS + 3.48) / (1.18052 + 0.32736 * std::exp(-0.1 * tree.dbh)) - 2.948;
				result = DNR24_Tarif_Vol(volumeEquation_.volEqStr, tree, merchRules, CVTS, "CVTS", CV4);
			}
		}
		else {
			double CVTS = 0.0;
			if (volumeEquation_.volEqStr.substr(3, 3) == "BRC" || volumeEquation_.volEqStr.substr(3, 3) == "BRI" || volumeEquation_.volEqStr.substr(3, 3) == "BRO") {
				CVTS = Browne_CVTS(volumeEquation_.volEqStr, tree.dbh, tree.totalHeight);
				result = DNR24_Tarif_Vol(volumeEquation_.volEqStr, tree, merchRules, CVTS, "CVTS");
			}
			else if (volumeEquation_.volEqStr.substr(3, 3) == "BEL") {
				CVTS = Bell_MountainHemlock_CVTS(tree.dbh, tree.totalHeight);
			}
			else if (volumeEquation_.volEqStr.substr(3, 3) == "CHA") {
				CVTS = Chamber_WesternHemlock_CVTS(tree.dbh, tree.totalHeight);
			}
			else if (volumeEquation_.volEqStr.substr(3, 3) == "KIN") {
				CVTS = King_DouglasFir_CVTS(tree.dbh, tree.totalHeight);
			}
			else if (volumeEquation_.volEqStr.substr(3, 3) == "KRU") {
				CVTS = Krumland_CVTS(volumeEquation_.fiaCode, tree.dbh, tree.totalHeight);
			}
			else if (volumeEquation_.volEqStr.substr(3, 3) == "SMF") {
				CVTS = Summerfield_CVTS(volumeEquation_.fiaCode, tree.dbh, tree.totalHeight);
			}

			result = DNR24_Tarif_Vol(volumeEquation_.volEqStr, tree, merchRules, CVTS, "CVTS");
		}
	}
	case VolumeEquation::GeoCode::UNKNOWN:
		break;
	default:
		break;
	}
	return result;
}