#pragma once

#include <string>
#include <stdexcept>

#include "..\Models\MerchRules.h"
#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\Models\VolumeCalculationOptions.h"
#include "..\VolumeEquation.h"

class VolumeCalculatorBase
{


public:
	const std::string volumeEquationNumber;

	virtual TreeOutput CalculateVolume(VolumeCalculationOptions vco,TreeMeasurment tree, MerchRules merchRules) = 0;


protected:
	const VolumeEquation volumeEquation_;
	double weightFactorDry;
	double weightFactorGreenSaw;
	double weightFactorGreenNonsaw;
	double weightFactorDead;

	VolumeCalculatorBase(VolumeEquation volumeEquation) 
		: volumeEquation_(volumeEquation), volumeEquationNumber(volumeEquation.GetVolumeEquationNumber())
	{

		// validate volume equation number
		if (volumeEquationNumber.length() < 10) { throw std::invalid_argument("Volume equation number should be at least 10 charaters long"); }

	}

};

