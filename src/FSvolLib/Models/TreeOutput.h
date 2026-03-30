#pragma once

#include "BiomassOutput.h"
#include "LogOutput.h"
#include <vector>

constexpr int MAX_NUMBER_LOGS = 32;

struct TreeOutput
{
	double grossBoardFootPrimary = 0.0;
	double grossBoardFootSecondary = 0.0;
	double grossCubicFootPrimary = 0.0;
	double grossCubicFootSecondary = 0.0;
	
	double totalCubicFoot = 0.0; //from ground to tip
	double stumpCubicFoot = 0.0;
	double tipCubicFoot = 0.0;

	double cordMerchantable = 0.0;

	double greenWeightPrimary = 0.0;
	double greenWeightSecondary = 0.0;
	double dryWeightPrimary = 0.0;
	double dryWeightSecondary = 0.0;

	BiomassOutput greenBio;
	BiomassOutput dryBio;

	double carbonContent = 0.0;

	std::vector<LogOutput> logs;
	int numberOfLogs = 0;

	int errflag = 0;
};

