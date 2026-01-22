#pragma once

#include <string>
#include <map>
#include <memory>
#include <exception>

#include "Models\VolumeCalculationOptions.h"
#include "Models\MerchRules.h"
#include "VolumeEquationResolver.h"
#include "VolumeEquation.h"

#include "VolumeCalculators\VolumeCalculatorBase.h"
#include "VolumeCalculators\ProfileVolumeCalculator.h"
#include "TaperModels\WenselOlsonTaperModel.h"

class VolumeCalculatorFactory
{
	std::map<std::string, std::unique_ptr<VolumeCalculatorBase>>  volumeCalculatorCahe_;
	VolumeEquationResolver volumeEquationResolver_;


public: 
	VolumeCalculatorFactory() {}

	~VolumeCalculatorFactory() {
		//volumeCalculatorCahe_.clear();
	}



	VolumeCalculatorBase& MakeVolumeCalculator(VolumeCalculationOptions vco);

private:
};

VolumeCalculatorBase& VolumeCalculatorFactory::MakeVolumeCalculator(VolumeCalculationOptions vco)
{
	auto volumeEquation = volumeEquationResolver_.GetVolumeEquation(vco);
	auto volumeEquationStr = volumeEquation.GetVolumeEquationNumber();

	if (volumeCalculatorCahe_.count(volumeEquationStr))
	{
		return *(volumeCalculatorCahe_[volumeEquationStr]);
	}


	if (volumeEquation.modelType == VolumeEquation::ModelType::WO2)
	{
		auto modelPtr = std::make_unique<WenselOlsonTaperModel>(volumeEquation);

		auto volCalcPtr = new ProfileVolumeCalculator(volumeEquation, *modelPtr); //std::make_unique<ProfileVolumeCalculator>(volumeEquation, *modelPtr);
		volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

		return *volCalcPtr;
	}

	throw std::invalid_argument("could not make volume equation");

}



