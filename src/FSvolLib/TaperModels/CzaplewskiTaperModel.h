#pragma once
#include "..\VolumeEquation.h"
#include "TaperModel.h"
#include <cmath>
#include <string>
#include <array>

class CzaplewskiTaperModel : public TaperModel
{
private:
    const int fiaCode_;
    const VolumeEquation::ModelType modelType_;
    std::string volEqStr;
    double top6Ht = 0.0;
    double heightExtra = 0.0;
    double diameterExtra = 0.0;
    double dbtbh = 0.0;

    // --- Coefficients for Two Parameter Model ---
    // Arrays adjusted for 0-based indexing [SP][Coefficient]
    static constexpr double R2MBB[8][4] = {
        {-5.18995, 2.57262, -3.85160, 117.934}, // 1: ASPEN
        {-3.65010, 1.45492, -2.20082, 52.058},   // 2: LODGEPOLE
        {-2.59737, 0.96927, -1.43195, 50.867},   // 3: POND. PINE BLACK HILLS
        {-3.80739, 1.75784, -3.56366, 55.776},   // 4: POND. PINE OTHER
        {-2.91187, 1.26772, -3.76391, 58.596},   // 5: WHITE FIR
        {-3.11638, 1.46021, -2.63725, 105.472},  // 6: SUBALPINE FIR
        {-2.26300, 0.92540, -0.80682, 382.694},  // 7: ENGELMANN SPRUCE
        {-5.86345, 2.98778, -4.12919, 82.838}    // 8: DOUGLAS FIR
    };

    static constexpr double R2MBA[8][2] = {
        {0.69, 0.09}, {0.77, 0.11}, {0.75, 0.11}, {0.62, 0.13},
        {0.50, 0.13}, {0.55, 0.09}, {0.65, 0.05}, {0.72, 0.12}
    };

    static constexpr double R2CZC[8][3] = {
        {1.0, 0.0, 0.0}, {1.0876, -0.0080764, 0.0}, {1.1331, -0.0095335, 0.0},
        {1.1251, -0.0082315, 0.0}, {1.0, 0.0, 0.0}, {1.0, 0.0, 0.0},
        {1.1263, -0.0080396, 0.0}, {1.0, 0.0, 0.0}
    };

    // --- Coefficients for Three Parameter Model ---
    static constexpr double EDBH[8][2] = {
        {-0.018759, 0.931816}, {-0.161248, 0.977231}, {-0.75702, 0.961789},
        {-0.653970, 0.950056}, {-1.078182, 0.969977}, {-0.330462, 0.961494},
        {-0.664774, 0.995785}, {-0.710803, 0.942278}
    };

    static constexpr double R23PA[8][2] = { // Same as R2MBA in original logic
        {0.69, 0.09}, {0.77, 0.11}, {0.75, 0.11}, {0.62, 0.13},
        {0.50, 0.13}, {0.55, 0.09}, {0.65, 0.05}, {0.72, 0.12}
    };

    static constexpr double R23PB[8][2] = {
        {-4.29592, 117.934}, {-3.00413, 52.058}, {-2.40572, 50.867},
        {-4.45250, 55.776},  {-4.42031, 58.596}, {-3.12030, 105.472},
        {-1.11960, 382.694}, {-6.62047, 82.838}
    };

    //Czaplewski taper to calculate dib at a given height ht2
    double R2taper(double dbh, double totalHeight, double heightExtra, double diameterExtra, double top6Ht, double dbtbh, double ht2, int& errflg);

    //calculate the height to a 6" top using Czaplewski taper
    double top6Height(double dbh, double totalHeight, double heightExtra, double diameterExtra, double stump, double dbtbh, int& errflag);

public:
    CzaplewskiTaperModel(VolumeEquation volumeEquation)
        : TaperModel(), fiaCode_(volumeEquation.fiaCode), modelType_(volumeEquation.modelType), volEqStr(volumeEquation.volEqStr)
    {}

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override; // {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob = false) override;

    StemVolume GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override { return { 0.0,0.0,0.0,0.0, false, false }; };
};