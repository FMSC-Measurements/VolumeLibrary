#pragma once

#include <string>
/// <summary>
/// 
/// </summary>
struct LogOutput
{
	int logNumber;
	int product;
	bool isSecondary; // indicates log is topwood

	double smallEndDiameterScaled;
	double largeEndDiameterScaled;
	
	double length;
	double heightToLargeEndDiameter;
	
	// volumes
	double grossBoardFoot;
	double grossCubicFoot;
	double internationalBoardFoot;

	// biomass
	double greenWeight;
	double dryWeight;

	
};

