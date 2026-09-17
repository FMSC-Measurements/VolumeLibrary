#pragma once
#include "WeightFactorAndRefDataResolver.h"

WeightFactorAndRefData getCachedSpeciesWtfactorAndRefData(int region, int forst, int fiaSpcd);

RefSpeciesData getCachedRefSpeciesData(int fiaSpcd);