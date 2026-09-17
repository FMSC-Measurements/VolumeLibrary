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

	virtual double GetHeightAtDiameter(VolumeCalculationOptions vco, TreeMeasurment tree, double diameter) { return 0.0; };

	virtual double GetDiameterAtHeight(VolumeCalculationOptions vco, TreeMeasurment tree, double height) { return 0.0; };

	static void SetWeightFactor(double dry, double greenSaw, double greenNonsaw, double dead) {
		weightFactorDry_ = dry;
		weightFactorGreenSaw_ = greenSaw;
		weightFactorGreenNonsaw_ = greenNonsaw;
		weightFactorDead_ = dead;
	}

protected:
	const VolumeEquation volumeEquation_;
	static double weightFactorDry_;
	static double weightFactorGreenSaw_;
	static double weightFactorGreenNonsaw_;
	static double weightFactorDead_;

	VolumeCalculatorBase(VolumeEquation volumeEquation) 
		: volumeEquation_(volumeEquation), volumeEquationNumber(volumeEquation.GetVolumeEquationNumber())
	{

		// validate volume equation number
		if (volumeEquationNumber.length() < 10) { throw std::invalid_argument("Volume equation number should be at least 10 charaters long"); }

	}

};

