#pragma once
#include "..\VolumeEquation.h"
#include "TaperModel.h"
#include "FlewellingShapeCoef.h"


class FlewellingTaperModel : public TaperModel
{
private:
    VolumeEquation volumeEquation_;

    //Flewelling parameters:
    int JSP_ = 0;
    //double BTR = 0.0;
    double dibDobPercent_ = 0.0;
    //double DBTBH = 0.0;
    double doubleBarkThicknessBH_ = 0.0;
    VolumeEquation::GeoCode geoCode_;
    std::string geoSubregion_ = "00";
    double dbhIb_;
    double dbh_;
    double totalHeight_;
    double merchHeight_ = 0.0;
    double merchTopDib_ = 0.0;

    FlewellingShapeParams shapeParameter_;
    FlewellingTaperCoef taperCoef_;
    Flewelling3PointExtra threePointExtra_;
    //double F; //scallingFactor
    double scalingFactor_ = 1.0;

    //3-point
    bool threePoint_ = false;
    double heightExtra_ = 0.0;
    double diameterExtra_ = 0.0;
    double zValueExtra_ = 0.0;

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
    
    double computeDiameterError(int jsp, std::string_view geoSub, double DBH, double HT_try, double DBTBH, double H1, double D1);
    
    double solveTotalHeight(int JSP, std::string_view geoSub, double DBH, double dbtBH, double H1, double D1);
    
    double computeDibErrorCorrelationAtHeights(int JSP, const std::string_view geosub, double TOTALH, double HI, double HJ);
    
    double adjustDibAtHeight(int JSP, const std::string_view geoSub, double DBH, double THT, double H, double DIBmod, double Z);
    
    double estimateDibAtHeight(double upperHeight, double& slope, bool needSlope = false);


public:
    FlewellingTaperModel(VolumeEquation volumeEquation);

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override;  // {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob = false) override;

    StemVolume GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override { return { 0.0,0.0,0.0,0.0, false, false }; };

};