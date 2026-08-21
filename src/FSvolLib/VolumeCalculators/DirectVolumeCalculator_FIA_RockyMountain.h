#pragma once

#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\Models\VolumeCalculationOptions.h"

TreeOutput ChojnackyWoodlandVol(int fiaCode, double drc, double totalHeight);

TreeOutput Kemp_Vol(int fiaCode, double DBHOB, double HTTOT, double BFMIND);

TreeOutput Moisen_Vol(const std::string& VOLEQ, double DBHOB, double HTTOT, double MTOPP, double BFMIND);