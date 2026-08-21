#pragma once
#include "..\Models\TreeMeasurment.h"
#include "..\Models\TreeOutput.h"

TreeOutput SRS_Vol(const std::string& VOLEQ, TreeMeasurment tree, double bfMinDbh);

TreeOutput Brandeis_Vol(const std::string& VOLEQ, TreeMeasurment tree);