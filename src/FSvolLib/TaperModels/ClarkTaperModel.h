#pragma once
#include "ClarkProfileCoefficients_R9.h"
#include "ClarkProfileCoefficients_R8.h"
#include "..\VolumeEquation.h"
#include "TaperModel.h"
#include <cmath>
#include <string>

struct ClarkCoef {
    double totHt, dbhIb, dib17, fixDi, dx;
    double r, c, e, p, b, a, a4, b4, a17, b17, q, afi, bfi;
    double tr1, tr2, tr3;
    int spgrp;
};

class ClarkTaperModel : public TaperModel
{
private:
    ClarkCoef clarkCoef, clarkCoefOb;
    
    double totHt;
    double dbhIb;
    double dbhOb_;
    int volSp;
    int spgrp;
    int sppIdx;
    int geoSppIdx;
    bool shortHeight = false;
    double shrtHt;
    double topHt = 0.0;
    double topDib = 0.0;
    double sawDib;
    double plpDib;
    double r9VolCorFactor = 1.0; //Correction factors to account for proper merchandising R9

    VolumeEquation volumeEquation_;
    int subRegion_ = 0; // for R8, valid 1, 2, 3, 4, 5, 6, 7, 9
    int eqHeightType_ = 0; // for R8, 0 = total Ht, 4 = height to 4" top, 7 = height to 7" top, 9 = height to 9" top, 1 = any of the heights

    int findSpeciesIndex(int spcd);
    
    void shortTreeReset(double upperHt);

    void setClarkCoef(int spcd);

    double ClarkDib(double stemHt);

    double ClarkDibH479(double stemHt);

    double ClarkHt(double stmDib, bool useDob = false);

    double ClarkHtH479(double stmDib, bool useDob = false);

    double ClarkCubicFootVol(double lowrHt, double upprHt);

    double ClarkCubicVolH479(double lowrHt, double upHt);

    double ClarkTopwoodRatio(double dib17, double ht, double r1, double r2, double r3);

public:
    ClarkTaperModel(VolumeEquation volumeEquation)
        : TaperModel(), volumeEquation_(volumeEquation)
    {
        subRegion_ = volumeEquation_.subregionalCode[0] - '0';
        eqHeightType_ = volumeEquation_.subregionalCode[1] - '0';
        setClarkCoef(volumeEquation_.fiaCode);
    }

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override; // {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob = false) override;

    StemVolume GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override;

};
