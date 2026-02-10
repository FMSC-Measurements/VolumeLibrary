#pragma once
struct MerchRules
{
	int evenOdd = 2;
	int segmentationOption = 22;
	double maxLogLength = 16.0;
	double minLogLength = 2.0;
	double minLengthTop = 2.0;
	double minTopDibSaw = 6.0;
	double minTopDibNonSaw = 4.0;
	double minMerchLength = 8.0;

	double stumpHeight = 1.0;
	double trim = 0.5;
	double barkThicknessRatio = 0.0;
	double doubleBarkThicknessAtBrestHeight = 0.0;
	double minimumBoardFootDiameter = 1.0;
	bool useCorrectedFactor = true;
};

