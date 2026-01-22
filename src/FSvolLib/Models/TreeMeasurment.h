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
	double referenceHeight; // aka 479, UpperstemHeight
	double merchHeightPrimary;
	double merchHeightSecondary;
	MerchHeightUnit merchHeightUnit = MerchHeightUnit::FEET; // if zero merch height is in feet else merch height is in 8,16,32 foot logs
	double heightToFirstLiveLimb;

	bool isLive;

	//diameters
	double dbh;
	double drc;
	double referenceDiameter; // diamater at reference height

	// merch rule overrides - if not zero these values can be used to overrice merch rule values
	double stumpHeightOverride;
	double minTopDibSawOverride;
	double minTopDibNonSawOverride;
};

