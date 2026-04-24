#ifndef DIRECTVOLUMECALCULATOR_R1_H
#define DIRECTVOLUMECALCULATOR_R1_H

#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\Models\MerchRules.h"
#include "..\Models\VolumeCalculationOptions.h"

TreeOutput R1KEMP(const std::string& voleq, VolumeCalculationOptions vco, TreeMeasurment tree);
TreeOutput R1ALLENC(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules);
double R1ALLENB(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules);

#endif
