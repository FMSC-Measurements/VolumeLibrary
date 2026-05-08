//#include "pch.h"
#include "VolumeLibrary.h"




TreeOutput VolumeLibrary::CalculateVolume(const VolumeCalculationOptions options, const TreeMeasurment tree, std::optional<MerchRules> maybe_merchRules)
{
	auto& volumeCalculator = volumeCalculatorFactory_.MakeVolumeCalculator(options);

	auto merchRules = (maybe_merchRules.has_value()) ? maybe_merchRules.value() : merchRulesResolver_.GetMerchRules(options);
	
	//region 7 (BLM) saw top diameter
	if (options.region == 7) merchRules.minTopDibSaw = tree.dbh * 0.184 + 2.24;
	//check override parameters for stump, sawTopDib, nonsawTopDib
	if (tree.stumpHeightOverride > 0.0) merchRules.stumpHeight = tree.stumpHeightOverride;
	if (tree.minTopDibSawOverride > 0.0) merchRules.minTopDibSaw = tree.minTopDibSawOverride;
	if (tree.minTopDibNonSawOverride > 0.0) merchRules.minTopDibNonSaw = tree.minTopDibNonSawOverride;

	auto treeOutput = volumeCalculator.CalculateVolume(options,tree, merchRules);

	return treeOutput;

}


