#pragma once
#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\Models\MerchRules.h"
#include "..\Models\VolumeCalculationOptions.h"

TreeOutput R12VOL(const std::string& eqnum, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules);
