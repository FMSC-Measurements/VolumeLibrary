#pragma once
struct TreeMeasurment
{
	enum class MerchHeightUnit {
		FEET = 0,
		LOGS8 = 8,
		LOGS16 = 16,
		LOGS32 = 32,
	};

	//heights
	double totalHeight;
	double referenceHeight = 0.0; // aka 479, UpperstemHeight
	double merchHeightSaw = 0.0;
	double merchHeightNonsaw = 0.0;
	MerchHeightUnit merchHeightUnit = MerchHeightUnit::FEET; // if zero merch height is in feet else merch height is in 8,16,32 foot logs
	double heightToFirstLiveLimb = 25.0;
	double heightToTopBroken = 0.0;

	bool isLive = true;

	//diameters
	double dbh;
	double drc = 0.0;
	double referenceDiameter = 0.0; // diamater at reference height
	double topBrokenDiameter = 0.0;
	int formClass = 0;

	// merch rule overrides - if not zero these values can be used to overrice merch rule values
	double stumpHeightOverride = -1.0;
	double minTopDibSawOverride = -1.0;
	double minTopDibNonSawOverride = -1.0;

	//cull, decay, cr
	double cull = 0.0;  //percent
	int decaycd = 0;
	double crownRatio = 0.0; //percent
	int stems = 1;  //1 = single stem, others = multi-stems
};

