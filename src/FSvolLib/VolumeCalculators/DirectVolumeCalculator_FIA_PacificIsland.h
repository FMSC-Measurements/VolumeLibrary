#pragma once
#include "..\Models\TreeMeasurment.h"
#include "..\Models\TreeOutput.h"
#include "..\array_helper.h"
#include "..\Models\MerchRules.h"

TreeOutput Centroid_CV(TreeMeasurment tree, MerchRules merchRules);

TreeOutput Frustum_Vol(TreeMeasurment tree, MerchRules merchRules);

TreeOutput Cone_Vol(TreeMeasurment tree, MerchRules merchRules);

TreeOutput PacificIsland_Vol(const std::string& voleq, TreeMeasurment tree, MerchRules merchRules);