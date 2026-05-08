#pragma once

#include <map>
#include <string>
#include "..\Models\TreeMeasurment.h"
#include "..\Models\MerchRules.h"
#include "..\Models\VolumeCalculationOptions.h"

class TaperModel
{

public:
	TaperModel() {};

	virtual void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) = 0; // for flewelling models

	
	virtual double GetDiameterAtHeight(TreeMeasurment tree, double height) = 0;

	// some taper models are able to calculate height at diameter directly, but for others we need to iterate to find the height at diameter
	virtual double GetHeightAtDiameter(TreeMeasurment tree, double diameter) = 0;
};
