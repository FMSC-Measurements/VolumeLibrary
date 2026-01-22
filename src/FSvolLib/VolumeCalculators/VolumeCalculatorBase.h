#pragma once

#include <string>
#include <stdexcept>

#include "..\Models\MerchRules.h"
#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\VolumeEquation.h"

class VolumeCalculatorBase
{
	const VolumeEquation volumeEquation_;
	double weightFactorDry;
	double weightFactorGreen;
	double weightFactorDead;


public:
	const std::string volumeEquationNumber;

	virtual TreeOutput CalculateVolume(TreeMeasurment tree, MerchRules merchRules) = 0;


protected:
	VolumeCalculatorBase(VolumeEquation volumeEquation) 
		: volumeEquation_(volumeEquation), volumeEquationNumber(volumeEquation.GetVolumeEquationNumber())
	{

		// validate volume equation number
		if (volumeEquationNumber.length() < 10) { throw std::invalid_argument("Volume equation number should be at least 10 charaters long"); }

	}
};

