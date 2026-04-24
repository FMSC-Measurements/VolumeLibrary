#include "DirectVolumeCalculator.h"
#include "DirectVolumeCalculator_R1.h"
#include "DirectVolumeCalculator_R234.h"
#include "DirectVolumeCalculator_R5.h"
#include "HawaiiSharpnackVolume.h"

TreeOutput DirectVolumeCalculator::CalculateVolume(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
	TreeOutput result;
	switch (volumeEquation_.geoCode) {
	case VolumeEquation::GeoCode::R1:
	{
		if (volumeEquation_.subregionalCode != "01") result =  R1KEMP(volumeEquationNumber, vco, tree);
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
	case VolumeEquation::GeoCode::HAWAII:
		return R12VOL(volumeEquationNumber, vco, tree, merchRules);

	case VolumeEquation::GeoCode::UNKNOWN:
		break;
	default:
		break;
	}
	return result;
}