#pragma once

#include <stdexcept>
#include <vector>

#include "VolumeCalculatorBase.h"
#include "..\Models\MerchRules.h"
#include "..\TaperModels\TaperModel.h"
#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\VolumeEquation.h"
#include "..\Models\VolumeCalculationOptions.h"



class ProfileVolumeCalculator : public VolumeCalculatorBase
{
	TaperModel& taperModel_;


public: 
	
	ProfileVolumeCalculator(VolumeEquation volumeEquation, TaperModel& taperModel) 
		: VolumeCalculatorBase(volumeEquation), 
		taperModel_(taperModel)
	{}

	TreeOutput CalculateVolume(VolumeCalculationOptions vco,TreeMeasurment tree, MerchRules merchRules) override;

	std::vector<LogOutput> SegmentLogs(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules);

private:
	std::vector<double> getLogs(double merchLength, MerchRules merchRules, int& numseg);

};

