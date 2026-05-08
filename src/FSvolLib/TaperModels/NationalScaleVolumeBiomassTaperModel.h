#pragma once
#include "..\VolumeEquation.h"
#include "..\Models\VolumeCalculationOptions.h"
#include "..\VolumeCalculators\NationalScaleVolumeBiomass.h"
#include "TaperModel.h"

class NationalScaleVolumeBiomassTaperModel : public TaperModel
{
private:
    VolumeEquation volEq;
    NationalScaleVolumeBiomass nsvb;
    
public:
    NationalScaleVolumeBiomassTaperModel(VolumeEquation volumeEquation, VolumeCalculationOptions vco)
        :TaperModel(), volEq(volumeEquation), nsvb(NationalScaleVolumeBiomass(volEq.GetVolumeEquationNumber(), vco))
    {

    }

    NationalScaleVolumeBiomassTaperModel(VolumeEquation volumeEquation)
        :TaperModel(), volEq(volumeEquation), nsvb(NationalScaleVolumeBiomass(volEq.volEqStr))
    {

    }


    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override;

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter) override;

    std::array<double, 4> GetStemVolumes(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco);
};
