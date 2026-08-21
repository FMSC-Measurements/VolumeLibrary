#pragma once
#include "..\Models\TreeMeasurment.h"
#include "..\Models\TreeOutput.h"

TreeOutput Hahn_NC_Vol(int fiaCode, TreeMeasurment tree, int siteIndex, int basalArea, double sawTopDib, double sawMinDbh);

TreeOutput Stone_NC_Vol(int fiaCode, TreeMeasurment tree, double sawTopDib, int siteIndex, int basalArea);

TreeOutput Scott_Vol(int fiaCode, TreeMeasurment tree);
