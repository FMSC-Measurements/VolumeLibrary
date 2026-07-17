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

	static VolumeEquation GetR1VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR1Equation(const std::string& voleq);

	static VolumeEquation GetR2VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR2Equation(const std::string& voleq);

	static VolumeEquation GetR3VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR3Equation(const std::string& voleq);

	static VolumeEquation GetR4VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR4Equation(const std::string& voleq);

	static VolumeEquation GetR5VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR5Equation(const std::string& voleq);

	static VolumeEquation GetR6VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR6Equation(const std::string& voleq);

	static VolumeEquation GetR7VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR7Equation(const std::string& voleq);

	static VolumeEquation GetR8VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR8Equation(const std::string& voleq);

	static VolumeEquation GetR9VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR9Equation(const std::string& voleq);

	static VolumeEquation GetR10VolumeEquation(VolumeCalculationOptions vco);
	static bool isValidR10Equation(const std::string& voleq);

public:
	static bool isValidEquation(std::string& voleq, VolumeCalculationOptions vco)
	{
		switch (vco.region)
		{
		case 1: { return isValidR1Equation(voleq); }
		case 2: { return isValidR2Equation(voleq); }
		case 3: { return isValidR3Equation(voleq); }
		case 4: { return isValidR4Equation(voleq); }
		case 5: { return isValidR5Equation(voleq); }
		case 6: { return isValidR6Equation(voleq); }
		case 7: { return isValidR7Equation(voleq); }
		case 8: { return isValidR8Equation(voleq); }
		case 9: { return isValidR9Equation(voleq); }
		case 10: { return isValidR10Equation(voleq); }
		default: 
			{ 
				if (voleq.size() == 10) return true;
				else throw std::invalid_argument("VolumeCalculationOptions.region is invalid"); 
			}

		}
	}

	static VolumeEquation GetVolumeEquation(VolumeCalculationOptions vcOpt) {
		if (vcOpt.volumeEquationNumberOverride.size() == 10)
		{
			if (isValidEquation(vcOpt.volumeEquationNumberOverride, vcOpt))
			{
				return VolumeEquation::ParseVolumeEquationNumber(vcOpt.volumeEquationNumberOverride);
			}
		}
		switch (vcOpt.region)
		{
		case 1: { return GetR1VolumeEquation(vcOpt); }
		case 2: { return GetR2VolumeEquation(vcOpt); }
		case 3: { return GetR3VolumeEquation(vcOpt); }
		case 4: { return GetR4VolumeEquation(vcOpt); }
		case 5: { return GetR5VolumeEquation(vcOpt); }
		case 6: { return GetR6VolumeEquation(vcOpt); }
		case 7: { return GetR7VolumeEquation(vcOpt); }
		case 8: { return GetR8VolumeEquation(vcOpt); }
		case 9: { return GetR9VolumeEquation(vcOpt); }
		case 10: { return GetR10VolumeEquation(vcOpt); }
		default: { throw std::invalid_argument("VolumeCalculationOptions.region is invalid"); }

		}
	}

};

