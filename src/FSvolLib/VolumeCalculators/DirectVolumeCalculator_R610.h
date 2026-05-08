#ifndef DIRECTVOLUMECALCULATOR_R610_H
#define DIRECTVOLUMECALCULATOR_R610_H

#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\Models\MerchRules.h"

TreeOutput R6VOL2(const std::string& VOLEQ, TreeMeasurment tree);

TreeOutput r10d2h(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules);

#endif