//#include "pch.h"
#include "MerchRulesResolver.h"

MerchRules MerchRulesResolver::GetMerchRules(VolumeCalculationOptions vco)
{
	MerchRules result;
	result.evenOdd = 2;
	result.segmentationOption = 22;
	result.maxLogLength = 16.0;
	result.minLogLength = 2.0;
	result.minLengthTop = 2.0;
	result.minTopDibSaw = 6.0;
	result.minTopDibNonSaw = 4.0;
	result.minMerchLength = 8.0;
	result.stumpHeight = 1.0;
	result.trim = 0.5;
	result.doubleBarkThicknessAtBrestHeight = 0.0;
	result.barkThicknessRatio = 0.0;
	result.minimumBoardFootDiameter = 1.0;
	result.useCorrectedFactor = true;

	//Reset merch rule items for each region
	if (vco.region == 1)
	{
		result.minTopDibSaw = 5.6;
		result.minLengthTop = 16.0;
	}
	else if (vco.region == 3)
	{
		if (vco.primaryProduct == 1 || vco.primaryProduct == 8)
		{
			result.minLogLength = 10;
			result.minLengthTop = 10;
			result.minimumBoardFootDiameter = 14;
			result.minMerchLength = 10;
			if (vco.primaryProduct == 8)
			{
				result.stumpHeight = 0.5;
				result.minTopDibSaw = 4.0;
			}
		}
		else if (vco.primaryProduct == 14)
		{
			result.minLogLength = 10;
			result.minLengthTop = 10;
			result.minMerchLength = 10;
			result.stumpHeight = 0.5;
			result.minTopDibSaw = 4.0;
			result.minTopDibNonSaw = 1.0;
		}
		else if (vco.primaryProduct == 20)
		{
			result.stumpHeight = 0.5;
			result.minTopDibSaw = 1.0;
			result.minTopDibNonSaw = 1.0;
		}
		else if (vco.primaryProduct == 7)
		{
			result.minLogLength = 4;
			result.minLengthTop = 4;
			result.stumpHeight = 0.5;
			result.minTopDibSaw = 2.0;
			result.minTopDibNonSaw = 2.0;
			
		}
	}
	else if (vco.region == 4)
	{
		result.segmentationOption = 23;
		result.useCorrectedFactor = false;
	}
	else if (vco.region == 6)
	{
		result.segmentationOption = 23;
		result.minTopDibSaw = 2.0;
		result.minTopDibNonSaw = 2.0;
		//result.stumpHeight = 0.0;
		result.useCorrectedFactor = false;
	}
	else if (vco.region == 7)
	{
		result.segmentationOption = 23;
		result.useCorrectedFactor = false;
	}
	else if (vco.region == 8 || vco.region == 9)
	{
		result.maxLogLength = 8.0;
		result.trim = 0.3;
		if (vco.primaryProduct != 1) result.stumpHeight = 0.5;
		//Hardwood saw top DIB
		if (vco.region == 8) result.minTopDibSaw = 9.0;
		else if (vco.region == 9)
		{
			result.minLengthTop = 4.0;
			result.minTopDibSaw = 9.6;
		}
		//Reset saw top DIB for non saw
		if (vco.fiaCode < 300)
		{
			if (vco.region == 8) result.minTopDibSaw = 7.0;
			else if (vco.region == 9) result.minTopDibSaw = 7.6;
		}
	}
	else if (vco.region == 10)
	{
		result.segmentationOption = 23;
		result.minLogLength = 8;
		result.minLengthTop = 8;
	}
	return result;

}