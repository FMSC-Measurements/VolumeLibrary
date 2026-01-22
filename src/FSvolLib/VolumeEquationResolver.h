#pragma once

#include <exception>
#include <map>

#include "VolumeEquation.h"
#include "Models\VolumeCalculationOptions.h" 

class VolumeEquationResolver
{
private: 
	static const std::map<int, std::string> fiaToEquationMap_R5_;

	static std::string GetFvsVariantCode(VolumeCalculationOptions vco);

	static VolumeEquation GetR5VolumeEquation(VolumeCalculationOptions vco);

public:
	static VolumeEquation GetVolumeEquation(VolumeCalculationOptions vcOpt) {
		switch (vcOpt.region)
		{
			case 5: { return GetR5VolumeEquation(vcOpt); }
			default: { throw std::invalid_argument("VolumeCalculationOptions.region is invalid"); }

		}
	}

};

