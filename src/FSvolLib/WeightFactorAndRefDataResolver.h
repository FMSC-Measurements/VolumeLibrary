#pragma once
//#include "WeightFactorData.h"

struct WeightFactorAndRefData {
	double weightFactorSaw;
	double weightFactorNonsaw;
	double weightFactorDead;
	double weightFactorDry;
	int softHard;				//0 = soft, 1 = hard
	int jenkinsSpeciesGroupCD;	//Jenkins species group code
	double WDSG;				//wood density (lb/cf)
	double carbonFraction;
};

struct RefSpeciesData {
	int softHard;				//0 = soft, 1 = hard
	int jenkinsSpeciesGroupCD;	//Jenkins species group code
	double WDSG;				//wood density (lb/cf)
	double weightFactorGreen;
	double weightFactorDry;
	double carbonFraction;
	int errorCode = 0;
};

WeightFactorAndRefData getSpeciesWtfactorAndRefData(int region, int forst, int fiaSpcd);

RefSpeciesData getRefSpeciesData(int fiaSpcd);