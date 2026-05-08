#pragma once

#include <string>
#include <string_view>
#include <iomanip>
#include <sstream>
#include <exception>

#include "string_helper.h"

class VolumeEquation
{
public:
	enum class GeoCode : char
	{
		UNKNOWN, 
		R1 = '1',
		R2 = '2',
		R3 = '3',
		R4 = '4',
		R5 = '5',
		R6 = '6',
		R8 = '8',
		R9 = '9',
		R10 = 'A',
		HAWAII = 'H',
		BLM_WO = 'B',
		INGYMODEL = 'I',
		FLWELLINGMODEL_WEST = 'F',
		NATIONAL = 'N',
		BIA_EAST = 'C',
	};

	enum class ModelType {
		UNKNOWN,
		DVE, // Direct Volume Esimator
		BEH, // Behre's Hyperbola
		CLK, // Clark
		CZ2, // Czaplewski 2 point profile model
		CZ3, // Czaplewski 3 point profile model
		DEM, // DeMars and Bruce profile models
		FW2, // Flewelling 2 point profile model
		FW3, // Flewelling 3 point profile model
		MAT, // Mathis (Rastagi and Loveless profile model)
		SN2, // Sharpnack's 2 point profile model
		WO2, // Wensel and Olsen 2 point profile model
		NVB, // National Scale Volume and Biomass
		CUR, // Curtis profile model (R10 red alder)
	};


	GeoCode geoCode;
	char subregionalCode[2] = { '0', '0' };
	ModelType modelType;
	char usRegion; // West('W') or East('E') coast
	int fiaCode = -1;
	bool isNsvbEquation = false;
	char nsvbDivision[4] = { '0', '0', '0', '0'};
	bool isProfileModel = true;
	std::string volEqStr;

	std::string GetFiaCodeString() const
	{
		std::stringstream ss;
		ss << std::setw(3) << std::setfill('0') << fiaCode;

		return ss.str();
	}

	std::string GetVolumeEquationNumber() const
	{
		if (isNsvbEquation)
		{
			return "NVB" + std::string(nsvbDivision, std::size(nsvbDivision)) + GetFiaCodeString();
		}
		else
		{
			char geoCodeChar = static_cast<char>(geoCode);
			return geoCodeChar + 
				std::string(subregionalCode, std::size(subregionalCode)) + 
				ModelTypeToStr(modelType) + 
				usRegion + 
				GetFiaCodeString();
		}
	}

	std::string ModelTypeToStr(ModelType model) const
	{
		switch (model)
		{
			case ModelType::DVE: { return "DVE"; }
			case ModelType::BEH: { return "BEH"; }
			case ModelType::CLK: { return "CLK"; }
			case ModelType::CZ2: { return "CZ2"; }
			case ModelType::CZ3: { return "CZ3"; }
			case ModelType::DEM: { return "DEM"; }
			case ModelType::FW2: { return "FW2"; }
			case ModelType::FW3: { return "FW3"; }
			case ModelType::MAT: { return "MAT"; }
			case ModelType::SN2: { return "SN2"; }
			case ModelType::WO2: { return "WO2"; }
			case ModelType::NVB: { return "NVB"; }
			case ModelType::CUR: { return "CUR"; }
			default: throw std::invalid_argument("Unknown ModelType: " + std::to_string(static_cast<int>(model)));		
		}
	}

	static VolumeEquation ParseVolumeEquationNumber(std::string volumeEquationNumber)
	{
		if (volumeEquationNumber.length() < 10) { throw std::invalid_argument("Volume equation number should be at least 10 charaters long"); }
		if (volumeEquationNumber.length() > 11) { throw std::invalid_argument("Volume equation number should be not be longer than 11 charaters"); }

		VolumeEquation volEq;
		volEq.volEqStr = volumeEquationNumber;

		if (volumeEquationNumber.substr(0, 3) == "NVB")
		{
			volEq.isNsvbEquation = true;
			volumeEquationNumber.copy(volEq.nsvbDivision, 4, 3);
			volEq.fiaCode = std::stoi(volumeEquationNumber.substr(7, 3)); // parse fiaCode
			volEq.modelType = ParseModelType(volumeEquationNumber.substr(0, 3));
		}
		else
		{
			volEq.isNsvbEquation = false;
			volEq.geoCode = static_cast<GeoCode>(volumeEquationNumber[0]); // cast first volEq char to GeoCode, Note this doesn't enforce that GeoCode is a valid value
			volumeEquationNumber.copy(volEq.subregionalCode, 2, 1);
			volEq.modelType = ParseModelType(volumeEquationNumber.substr(3, 3));
			if (volumeEquationNumber.substr(3, 3) == "DVE" || volumeEquationNumber.substr(3, 3) == "SN2") volEq.isProfileModel = false;
			//volume equation 223DVEW122 uses NSVB taper model to calculate volume
			if (volumeEquationNumber == "223DVEW122") volEq.isProfileModel = true;
			volEq.usRegion = volumeEquationNumber[6];
			volEq.fiaCode = std::stoi(volumeEquationNumber.substr(7, 3)); // parse fiaCode
		}
		return volEq;
	}

	static ModelType ParseModelType(std::string modelCode)
	{
		modelCode = string_helper::StrToUpper(modelCode);
		if (modelCode == "DVE") { return ModelType::DVE; }
		if (modelCode == "BEH") { return ModelType::BEH; }
		if (modelCode == "CLK") { return ModelType::CLK; }
		if (modelCode == "CZ2") { return ModelType::CZ2; }
		if (modelCode == "CZ3") { return ModelType::CZ3; }
		if (modelCode == "DEM") { return ModelType::DEM; }
		if (modelCode == "FW2") { return ModelType::FW2; }
		if (modelCode == "FW3") { return ModelType::FW3; }
		if (modelCode == "MAT") { return ModelType::MAT; }
		if (modelCode == "SN2") { return ModelType::SN2; }
		if (modelCode == "WO2") { return ModelType::WO2; }
		if (modelCode == "NVB") { return ModelType::NVB; }
		if (modelCode == "CUR") { return ModelType::CUR; }
		else { return ModelType::UNKNOWN; }
	}

};

