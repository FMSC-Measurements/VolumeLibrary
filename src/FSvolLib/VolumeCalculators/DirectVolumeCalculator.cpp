#include "DirectVolumeCalculator.h"
#include "DirectVolumeCalculator_R1.h"
#include "DirectVolumeCalculator_R234.h"
#include "DirectVolumeCalculator_R5.h"
#include "DirectVolumeCalculator_R610.h"
#include "DirectVolumeCalculator_R89.h"
#include "DirectVolumeCalculator_BIA.h"
#include "DirectVolumeCalculator_FIA_Eastern.h"
#include "DirectVolumeCalculator_FIA_RockyMountain.h"
#include "HawaiiSharpnackVolume.h"
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
		return r5dve::R5HARV(volumeEquationNumber, tree, merchRules);
	case VolumeEquation::GeoCode::R6:
		return R6VOL2(volumeEquationNumber, tree);
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
		double drc = tree.drc > 0.0 ? tree.drc : tree.dbh;
		return ChojnackyWoodlandVol(volumeEquation_.fiaCode, drc, tree.totalHeight);
	}
	case VolumeEquation::GeoCode::EASTERN:
	{
		if (volumeEquation_.volEqStr.substr(3, 3) == "HAH") {
			return Hahn_NC_Vol(volumeEquation_.fiaCode, tree, vco.siteIndex, vco.basalArea, merchRules.minTopDibSaw, merchRules.minimumBoardFootDiameter);
		}
		else if (volumeEquation_.volEqStr.substr(3, 3) == "STN") {
			return Stone_NC_Vol(volumeEquation_.fiaCode, tree, merchRules.minTopDibSaw, vco.siteIndex, vco.basalArea);
		}
	}
	case VolumeEquation::GeoCode::UNKNOWN:
		break;
	default:
		break;
	}
	return result;
}