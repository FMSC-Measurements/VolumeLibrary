#ifndef DIRECTVOLUMECALCULATOR_R5_H
#define DIRECTVOLUMECALCULATOR_R5_H

#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\Models\MerchRules.h"

namespace r5dve {
	TreeOutput R5HARV(const std::string& voleq, TreeMeasurment tree, MerchRules merchRules);
}
#endif
