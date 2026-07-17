#pragma once

#include <string>

/// <summary>
/// Input values for CalculateVolume
/// 
/// volumeCalculationOptions defaults to FVS
/// auxFlag is optional value used to provide auxilary flags used to indicate species variant info (young/old groth) or appraisal group (R6 dougfur, house logs) 
/// </summary>
struct VolumeCalculationOptions
{

	enum class VolumeCalculationType {
		FVS = 0,
		FIA,
		CRUISE,
		VARIABLELOGLENGTH,
	};

	enum class AuxFlag : char {
		NONE = '\0',
		R10YOUNGGROWTH = 'Y',
		R6DOUGFIR = 'F',
		PLANTATION = 'P',
	};

	int fiaCode;
	AuxFlag auxFlag = AuxFlag::NONE; // optional defaults to null char

	int region;
	int forest;
	int district;
	int primaryProduct;
	int secondaryProduct;
	VolumeCalculationType volumeCalculationOptions = VolumeCalculationType::FVS;
	std::string volumeEquationNumberOverride; 
	std::string ecoRegion; // optional fia only
	int basalArea = 0;
	int siteIndex = 0;
};

struct StemVolume {
	double stumpVol = 0.0;
	double primaryVol = 0.0;
	double topwoodVol = 0.0;
	double tipVol = 0.0;
	//bool volCalculated = false;
	//bool isBEH = false;
};


// Global alias (optional, but convenient)
using VolumeCalculationType = VolumeCalculationOptions::VolumeCalculationType;
using AuxFlag = VolumeCalculationOptions::AuxFlag;

