#pragma once
#include "ClarkProfileCoefficients_R9.h"
#include "..\VolumeEquation.h"
#include "TaperModel.h"
#include <cmath>
#include <string>

struct ClarkCoef {
    double totHt, dbhIb, dib17, fixDi, dx;
    double r, c, e, p, b, a, a4, b4, a17, b17, q, afi, bfi;
    int spgrp;
};

class ClarkTaperModel : public TaperModel
{
private:
    //ClarkCoef clarkCoef;
    double totHt, dbhIb, dib17, fixDi, dx;
    double r, c, e, p, b, a, a4, b4, a17, b17, q, afi, bfi;
    int spgrp;
    int sppIdx;
    bool shortHeight = false;
    double shrtHt;
    double topHt;
    double topDib;
    double sawDib;
    double plpDib;
    double r9VolCorFactor = 1.0; //Correction factors to account for proper merchandising R9

    VolumeEquation volumeEquation_;
    int subRegion = 0; // for R8, valid 1, 2, 3, 4, 5, 6, 7, 9
    int heightType = 0; // for R8, 0 = total Ht, 4 = height to 4" top, 7 = height to 7" top, 9 = height to 9" top, 1 = any of the heights

    int findSpeciesIndex(int spcd);
    
    void setClarkCoef(int spcd);

    double ClarkDib(double stemHt);

    double ClarkHt(double stmDib);

    double ClarkCubicFootVol(double lowrHt, double upprHt);

public:
    ClarkTaperModel(VolumeEquation volumeEquation)
        : TaperModel(), volumeEquation_(volumeEquation)
    {
        setClarkCoef(volumeEquation_.fiaCode);
    }

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override; // {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter) override;

    std::array<double, 4> GetStemVolumes(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco);
};
