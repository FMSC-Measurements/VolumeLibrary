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
#include "..\MerchRulesResolver.h"


class ProfileVolumeCalculator : public VolumeCalculatorBase
{
	TaperModel& taperModel_;


public: 
	
	ProfileVolumeCalculator(VolumeEquation volumeEquation, TaperModel& taperModel) 
		: VolumeCalculatorBase(volumeEquation), 
		taperModel_(taperModel)
	{}

	TreeOutput CalculateVolume(VolumeCalculationOptions vco,TreeMeasurment tree, MerchRules merchRules) override;

	double GetHeightAtDiameter(VolumeCalculationOptions vco, TreeMeasurment tree, double diameter) override;

	double GetDiameterAtHeight(VolumeCalculationOptions vco, TreeMeasurment tree, double height) override;

	std::vector<LogOutput> SegmentLogs(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules);

	static std::vector<double> getLogs(double merchLength, MerchRules merchRules, int& numseg);
	
	static double r6BehButtLogVolume(double dbhOb, double d17, double logLen = 16.0) {
		//from Bruce 1982 For Sci vol. 28 no. 3
		return 0.005454 * logLen * ( 0.294 * std::pow(dbhOb, 2) + 0.715 * std::pow(d17, 2));
	};

	static double Beh628BoardfootFix(TreeMeasurment tree) {
		if (tree.merchHeightUnit != TreeMeasurment::MerchHeightUnit::FEET) {
			return 0.4017 + (0.1450 * log(tree.merchHeightSaw)) - (0.0025 * tree.dbh) - (0.0009 * tree.formClass);
		}
		else {
			return 0.1909 + (0.0006 * tree.dbh) + (0.1349 * log(tree.totalHeight)) - (0.0002 * tree.formClass);
		}
	}

private:
	void solveTotalHeight(TreeMeasurment& tree, MerchRules merchRules);

};

