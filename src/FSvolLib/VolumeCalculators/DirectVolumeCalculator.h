#pragma once
#include <stdexcept>
#include <vector>

#include "VolumeCalculatorBase.h"
#include "..\Models\MerchRules.h"
#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\VolumeEquation.h"
#include "..\Models\VolumeCalculationOptions.h"

class DirectVolumeCalculator : public VolumeCalculatorBase
{

public:
	DirectVolumeCalculator(VolumeEquation volumeEquation)
		: VolumeCalculatorBase(volumeEquation)
	{}

	TreeOutput CalculateVolume(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules) override;

};