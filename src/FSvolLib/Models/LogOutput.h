#pragma once

#include <string>
/// <summary>
/// 
/// </summary>
struct LogOutput
{
	int logNumber = 0;
	int product = 1;
	bool isSecondary = false; // indicates log is topwood

	double smallEndDiameterActual = 0.0;
	double largeEndDiameterActual = 0.0;

	double smallEndDiameterScaled = 0.0;
	double largeEndDiameterScaled = 0.0;
	
	double length = 0.0;
	double heightToLargeEndDiameter = 0.0;
	
	// volumes
	double grossBoardFoot = 0.0;
	double grossCubicFoot = 0.0;
	double internationalBoardFoot = 0.0;

	// biomass
	double greenWeight = 0.0;
	double dryWeight = 0.0;

	
};

