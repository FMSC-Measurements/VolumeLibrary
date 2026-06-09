#pragma once
#include "..\VolumeEquation.h"
#include "TaperModel.h"
#include "FlewellingShapeCoef.h"


class FlewellingTaperModel : public TaperModel
{
private:
    VolumeEquation volumeEquation_;

    //Flewelling parameters:
    int JSP = 0;
    double BTR = 0.0;
    double DBTBH = 0.0;
    VolumeEquation::GeoCode geoCode;
    std::string geoSubregion = "00";
    double dbhIb;
    double dbh;
    double totalHeight;
    double merchHeight = 0.0;
    double merchTopD = 0.0;

    FlewellingShapeParams shapeParameter;
    FlewellingTaperCoef taperCoef;
    Flewelling3PointExtra threePointExtra;
    double F; //scallingFactor

    //3-point
    bool threePoint = false;
    double heightExtra = 0.0;
    double diameterExtra = 0.0;
    double zValueExtra = 0.0;

    double getDbhIb(int JSP, std::string_view geoSub, double dbh, double totalHt);
    
    FlewellingShapeParams getShapeParameter(int JSP, std::string_view geoSub, double dbh, double totalHt);
    
    FlewellingTaperCoef getTaperCoef(FlewellingShapeParams shapeParemeter);
    
    double getDibFrom2PointModel(int jsp, double relativeHeight, double totalHt,
        FlewellingShapeParams shapeParemeter, FlewellingTaperCoef taperCoef, 
        double& slope, double scalingFactor = 1.0, bool needSlope = false);
    
    double getScalingFactor(int JSP, double dbhIb, double totalHeight, FlewellingShapeParams shapeParameter, FlewellingTaperCoef taperCoef);
    
    Flewelling3PointExtra getThreePointExtra(int jsp, std::string_view geoSub, double dbh, double totalHt, double dbtBH,
        double heightExtra, double diameterExtra, double scalingFactor,
        FlewellingShapeParams shapeParameter, FlewellingTaperCoef taperCoef);
    
    double getDibFrom3PointModel(int jsp, std::string_view geoSub, double dbh, double totalHt, double H, double DIB, double HEX, double DEX, Flewelling3PointExtra threePointExtra);
    
    double computeDiameterError(int jsp, std::string_view geoSub,
        double DBH,
        double HT_try,
        double DBTBH,
        double H1,
        double D1);
    double solveTotalHeight(int JSP, std::string_view geoSub, double DBH, double dbtBH, double H1, double D1);
    double SF_CORR(int JSP, const std::string_view geosub, double TOTALH, double HI, double HJ);
    double SF_DFZ(int JSP, const std::string_view geoSub, double DBH, double THT, double H, double DIBmod, double Z);
    double SF_DS(
        int JSP,
        const std::string_view GEOSUB,
        double DBH,
        double TOTALH,
        double HTUP,
        double heightExtra,
        double diameterExtra,
        FlewellingShapeParams shapeParameter,
        FlewellingTaperCoef taperCoef,
        Flewelling3PointExtra threePointExtra,
        double scalingFactor,
        double& slope,
        bool needSlope = false);


public:
    FlewellingTaperModel(VolumeEquation volumeEquation);

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override;  // {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob = false) override;

    StemVolume GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override { return { 0.0,0.0,0.0,0.0, false, false }; };

};