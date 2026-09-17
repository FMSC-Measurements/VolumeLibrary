#pragma once

#include <string>

struct ErrorInfo
{
	std::string errorType;
	std::string errorMessage;
};

enum class ErrorCode {
    NoVolumeEquationMatch = 1,
    NoFormClass,
    DBHLessThanOne,
    TreeHeightLessThan4_5,
    D2HOutOfBounds,
    NoSpeciesMatch,
    IllegalMerchHeightSaw,
    IllegalMerchHeightNonSaw,
    UpperStemRequired,
    IllegalUPSHT1,
    UnableToFitProfile,
    MoreThan20Logs,
    TopDiaGreaterThanDBHIB,
    NegativeDBHIB,
    InvalidBIOEQ,
    RequireHt1prdForBio,
    RequireHt2prdForBio,
    RequireStemCubicVolume
};

const char* errorMessage(ErrorCode code) {
    switch (code) {
    case ErrorCode::NoVolumeEquationMatch: return "No volume equation match";
    case ErrorCode::NoFormClass: return "No form class";
    case ErrorCode::DBHLessThanOne: return "DBH less than one";
    case ErrorCode::TreeHeightLessThan4_5: return "Tree height less than 4.5";
    case ErrorCode::D2HOutOfBounds: return "D2H is out of bounds";
    case ErrorCode::NoSpeciesMatch: return "No species match";
    case ErrorCode::IllegalMerchHeightSaw: return "Illegal primary product log height (Ht1prd)";
    case ErrorCode::IllegalMerchHeightNonSaw: return "Illegal secondary product log height (Ht2prd)";
    case ErrorCode::UpperStemRequired: return "Upper stem measurements required";
    case ErrorCode::IllegalUPSHT1: return "Illegal upper stem height (UPSHT1)";
    case ErrorCode::UnableToFitProfile: return "Unable to fit profile given dbh, merch ht and top dia";
    case ErrorCode::MoreThan20Logs: return "Tree has more than 20 logs";
    case ErrorCode::TopDiaGreaterThanDBHIB: return "Top diameter greater than DBH inside bark";
    case ErrorCode::NegativeDBHIB: return "The bark equation for the VOLEQ does not exist or yields a negative DBHIB.";
    case ErrorCode::InvalidBIOEQ: return "Invalid BIOEQ";
    case ErrorCode::RequireHt1prdForBio: return "Primary prod height (HT1PRD) required for biomass calculation";
    case ErrorCode::RequireHt2prdForBio: return "Secondary prod height (HT2PRD) required for biomass calculation";
    case ErrorCode::RequireStemCubicVolume: return "Stem cubic volume required for the select BIOEQ";
    default: return "Unknown error";
    }
}

// usage example
// throw ErrorCode::DBHLessThanOne;
// 
//try {
//    funcA();
//}
//catch (ErrorCode err) {
//    std::cerr << errorMessage(err) << std::endl;
//}
