#ifndef DIRECTVOLUMECALCULATOR_R234_H
#define DIRECTVOLUMECALCULATOR_R234_H

#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\Models\MerchRules.h"
#include "..\Models\VolumeCalculationOptions.h"

TreeOutput R2OLDV(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules);
TreeOutput R3D2HV(const std::string& VOLEQU, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules);
TreeOutput R4D2H(const std::string& VOLEQ, VolumeCalculationOptions vco, TreeMeasurment tree);

#endif
