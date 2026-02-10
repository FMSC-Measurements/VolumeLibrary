//#include "pch.h"
#include "VolumeLibrary.h"




TreeOutput VolumeLibrary::CalculateVolume(const VolumeCalculationOptions options, const TreeMeasurment tree, std::optional<MerchRules> maybe_merchRules)
{
	auto& volumeCalculator = volumeCalculatorFactory_.MakeVolumeCalculator(options);

	auto merchRules = (maybe_merchRules.has_value()) ? maybe_merchRules.value() : merchRulesResolver_.GetMerchRules(options);

	auto treeOutput = volumeCalculator.CalculateVolume(options,tree, merchRules);

	return treeOutput;

}


