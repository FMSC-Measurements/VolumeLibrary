#pragma once

#include "BiomassOutput.h"
#include "LogOutput.h"
#include <vector>

constexpr int MAX_NUMBER_LOGS = 32;

struct TreeOutput
{
	double grossBoardFootPrimary;
	double grossBoardFootSecondary;
	double grossCubicFootPrimary;
	double grossCubicFootSecondary;
	double cordPrimary;
	double cordSecondary;

	double greenWeightPrimary;
	double greenWeightSecondary;
	double dryWeightPrimary;
	double dryWeightSecondary;

	BiomassOutput greenBio;
	BiomassOutput dryBio;

	std::vector<LogOutput> logs;
	int numberOfLogs;

};

