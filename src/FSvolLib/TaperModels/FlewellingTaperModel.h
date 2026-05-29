#pragma once
#include "..\VolumeEquation.h"
#include "TaperModel.h"

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

    double r1, r2, r3, r4, r5, a3;
    double rhi1, rhi2, rhc, rhlongi;
    double a0, a1, a2, a4, b0, b1, b2, b3, b4, c1, c2, e1, e2;
    double F; //scallingFactor

    //3-point
    double heightExtra = 0.0;
    double diameterExtra = 0.0;
    double zValueExtra = 0.0;

public:
    FlewellingTaperModel(VolumeEquation volumeEquation);

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override;  // {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob = false) override;

    StemVolume GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override { return { 0.0,0.0,0.0,0.0, false, false }; };

};