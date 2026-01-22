#pragma once

#include <string>
#include <optional>

#include "Models\TreeOutput.h"
#include "Models\VolumeCalculationOptions.h"
#include "Models\TreeMeasurment.h"
#include "Models\MerchRules.h"
#include "VolumeCalculatorFactory.hpp"
#include "MerchRulesResolver.h"


class VolumeLibrary
{
private:

	// Private constructor to prevent direct instantiation, as part of singleton pattern implementation
	VolumeLibrary() {};

	// Private destructor, though automatic destruction handles this, as part of singleton pattern implementation
	~VolumeLibrary()
	{
		//volumeCalculatorFactory_.destroy(); // tells volumeCalculatorFactory to free any cached resources
	}

	VolumeCalculatorFactory volumeCalculatorFactory_;
	MerchRulesResolver merchRulesResolver_;

public:
	// Delete copy constructor and assignment operator to prevent copying
	VolumeLibrary(const VolumeLibrary&) = delete;
	VolumeLibrary& operator=(const VolumeLibrary&) = delete;

	// Public static method to access the single instance
	static VolumeLibrary& getInstance() {
		// The local static object is created only on the first call to getInstance()
		// C++11 guarantees thread-safe initialization of local static variables
		static VolumeLibrary instance;
		return instance;
	}



	TreeOutput CalculateVolume(const VolumeCalculationOptions options, const TreeMeasurment tree, std::optional<MerchRules> maybe_merchRules = std::nullopt);

	std::string GetVolumeEquationNumber(VolumeCalculationOptions options) {};

	double GetHeightAtDiameter(VolumeCalculationOptions options, TreeMeasurment tree, double diameter) { return 0.0; };
	double GetDiameterAtHeight(VolumeCalculationOptions options, TreeMeasurment tree, double height) { return 0.0; };

	int GetNumberOfLogs(VolumeCalculationOptions options, TreeMeasurment tree, std::optional<MerchRules> maybe_merchRules = std::nullopt) { return 0; };

	// Q: do we need a get number of logs function for just primary or total? 
	// A: measurment spec requested total logs and grades. Also for FScruiser we would want a function that can be used to get number of logs for log grading. 

	std::string GetVersion() { return "0.0.0.1"; };

};

//#ifdef __cplusplus
//extern "C" {
//#endif
//
//// C interop functions 
//void CalculateVolume(TreeOutput* treeOutput, const VolumeCalculationOptions volOpt, const TreeMeasurment tree)
//{
//	
//	auto result = VolumeLibrary::getInstance().CalculateVolume(volOpt, tree);
//
//	//TODO copy result data to treeOutput
//}
//
//void CalculateVolumeWithMerchRules(TreeOutput* treeOutput, const VolumeCalculationOptions volOpt, const TreeMeasurment tree, MerchRules merchRules)
//{
//	auto result = VolumeLibrary::getInstance().CalculateVolume(volOpt, tree, merchRules);
//
//	//TODO copy result data to treeOutput
//}
//
//// WIP still needto figure out the best way to handle passing strings 
////void GetVolumeEquationNumber(const VolumeCalculationOptions volOpt, char* volEq_out_buffer)
////{
////	auto volEq = VolumeLibrary::getInstance().GetVolumeEquationNumber(volOpt);
////	strncpy(volEq_out_buffer, volEq.c_str(), )
////}
//
//double GetHeightAtDiameter(VolumeCalculationOptions volOpt, TreeMeasurment tree, double diameter)
//{
//	return VolumeLibrary::getInstance().GetHeightAtDiameter(volOpt, tree, diameter);
//}
//
//double GetDiameterAtHeight(VolumeCalculationOptions volOpt, TreeMeasurment tree, double height)
//{
//	return VolumeLibrary::getInstance().GetDiameterAtHeight(volOpt, tree, height);
//}
//
//int GetNumberOfLogs(VolumeCalculationOptions volOpt, TreeMeasurment tree)
//{
//	return VolumeLibrary::getInstance().GetNumberOfLogs(volOpt, tree);
//}
//
//int GetNumberOfLogsWithMerchRules(VolumeCalculationOptions volOpt, TreeMeasurment tree, MerchRules merchRules)
//{
//	return VolumeLibrary::getInstance().GetNumberOfLogs(volOpt, tree, merchRules);
//}
//
//#ifdef __cplusplus
//}
//#endif


