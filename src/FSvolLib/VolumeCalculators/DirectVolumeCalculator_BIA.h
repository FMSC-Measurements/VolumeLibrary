#ifndef DIRECTVOLUMECALCULATOR_BIA_H
#define DIRECTVOLUMECALCULATOR_BIA_H

#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\Models\MerchRules.h"
#include "..\Models\VolumeCalculationOptions.h"

TreeOutput VolEq_Johnson(TreeMeasurment tree);
TreeOutput Voleq_Honer(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules);

#endif
