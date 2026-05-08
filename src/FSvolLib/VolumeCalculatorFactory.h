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
#include "TaperModels\NationalScaleVolumeBiomassTaperModel.h"
#include "TaperModels\RustagiTaperModel.h"
#include "TaperModels\BehreHyperbolaTaperModel.h"
#include "TaperModels\DeMarsTaperModel.h"
#include "TaperModels\CzaplewskiTaperModel.h"
#include "TaperModels\ClarkTaperModel.h"
#include "VolumeCalculators\DirectVolumeCalculator.h"

class VolumeCalculatorFactory
{
	std::map<std::string, std::unique_ptr<VolumeCalculatorBase>>  volumeCalculatorCahe_;
	VolumeEquationResolver volumeEquationResolver_;


public: 
	VolumeCalculatorFactory() {}

	~VolumeCalculatorFactory() {
		//volumeCalculatorCahe_.clear();
	}



	VolumeCalculatorBase& MakeVolumeCalculator(VolumeCalculationOptions vco)
	{
		auto volumeEquation = volumeEquationResolver_.GetVolumeEquation(vco);
		auto volumeEquationStr = volumeEquation.GetVolumeEquationNumber();

		if (volumeCalculatorCahe_.count(volumeEquationStr))
		{
			return *(volumeCalculatorCahe_[volumeEquationStr]);
		}

		if (volumeEquation.isProfileModel)
		{
			if (volumeEquation.modelType == VolumeEquation::ModelType::WO2)
			{
				auto  model = new WenselOlsonTaperModel(volumeEquation);

				auto volCalcPtr = new ProfileVolumeCalculator(volumeEquation, *model); //std::make_unique<ProfileVolumeCalculator>(volumeEquation, *modelPtr);
				volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

				return *volCalcPtr;
			}
			else if (volumeEquation.modelType == VolumeEquation::ModelType::NVB)
			{
				auto  model = new NationalScaleVolumeBiomassTaperModel(volumeEquation, vco);
				auto volCalcPtr = new ProfileVolumeCalculator(volumeEquation, *model); //std::make_unique<ProfileVolumeCalculator>(volumeEquation, *modelPtr);
				volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

				return *volCalcPtr;
			}
			else if (volumeEquation.modelType == VolumeEquation::ModelType::DVE)
			{
				auto  model = new NationalScaleVolumeBiomassTaperModel(volumeEquation); //for VOLEQ 223DVEW122
				auto volCalcPtr = new ProfileVolumeCalculator(volumeEquation, *model); //std::make_unique<ProfileVolumeCalculator>(volumeEquation, *modelPtr);
				volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

				return *volCalcPtr;
			}
			else if (volumeEquation.modelType == VolumeEquation::ModelType::MAT)
			{
				auto  model = new RustagiTaperModel(volumeEquation);
				auto volCalcPtr = new ProfileVolumeCalculator(volumeEquation, *model); //std::make_unique<ProfileVolumeCalculator>(volumeEquation, *modelPtr);
				volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

				return *volCalcPtr;
			}
			else if (volumeEquation.modelType == VolumeEquation::ModelType::BEH)
			{
				auto model = new BehreHyperbolaTaperModel(volumeEquation);
				auto volCalcPtr = new ProfileVolumeCalculator(volumeEquation, *model);
				volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

				return *volCalcPtr;
			}
			else if (volumeEquation.modelType == VolumeEquation::ModelType::DEM || volumeEquation.modelType == VolumeEquation::ModelType::CUR)
			{
				auto model = new DeMarsTaperModel(volumeEquation);
				auto volCalcPtr = new ProfileVolumeCalculator(volumeEquation, *model);
				volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

				return *volCalcPtr;
			}
			else if (volumeEquation.modelType == VolumeEquation::ModelType::CZ2 || volumeEquation.modelType == VolumeEquation::ModelType::CZ3)
			{
				auto model = new CzaplewskiTaperModel(volumeEquation);
				auto volCalcPtr = new ProfileVolumeCalculator(volumeEquation, *model);
				volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

				return *volCalcPtr;
			}
			else if (volumeEquation.modelType == VolumeEquation::ModelType::CLK)
			{
				auto model = new ClarkTaperModel(volumeEquation);
				auto volCalcPtr = new ProfileVolumeCalculator(volumeEquation, *model);
				volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

				return *volCalcPtr;
			}
		}
		else
		//else if (volumeEquation.modelType == VolumeEquation::ModelType::DVE || volumeEquation.modelType == VolumeEquation::ModelType::SN2)
		{
			auto volCalcPtr = new DirectVolumeCalculator(volumeEquation);
			volumeCalculatorCahe_.emplace(volumeEquationStr, volCalcPtr);

			return *volCalcPtr;
		}

		throw std::invalid_argument("could not make volume equation");

	}

private:
};





