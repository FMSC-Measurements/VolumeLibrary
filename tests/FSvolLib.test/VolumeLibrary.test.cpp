#include "pch.h"
#include "CppUnitTest.h"

#include "VolumeLibrary.h"

#include <string>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;


TEST_CLASS(VolumeLibraryTest)
{
public:

    TEST_METHOD(CalculateVolumeTest)
    {
        // Arrange
        VolumeLibrary& volLib = VolumeLibrary::getInstance();
        VolumeCalculationOptions volOpt;
        volOpt.region = 4;
        volOpt.forest = 1;
        volOpt.fiaCode = 133;
        volOpt.primaryProduct = 1;
        volOpt.secondaryProduct = 2;
        volOpt.volumeCalculationOptions = VolumeCalculationType::FIA;
        //volOpt.ecoRegion = "M260";
        // ...
        auto volEqDefault = volLib.GetVolumeEquationNumber(volOpt);

        TreeMeasurment tree;
        tree.dbh = 19.7;
        tree.totalHeight = 76.0;
        tree.referenceHeight = 0.0;
        tree.referenceDiameter = 0.0;
        tree.minTopDibNonSawOverride = 0.0;
        tree.minTopDibSawOverride = 0.0;
        tree.merchHeightSaw = 0.0;
        tree.merchHeightNonsaw = 0.0;
        tree.formClass = 0;
        tree.heightToTopBroken = 0.0;
        tree.topBrokenDiameter = 0.0;
        // ...
        //int numLogs = volLib.GetNumberOfLogs(volOpt, tree);

        //double ht2 = volLib.GetHeightAtDiameter(volEqDefault, tree, 3.0);

        //double dia2 = volLib.GetDiameterAtHeight(volEqDefault, tree, 35.5);
        
        //volOpt.volumeEquationNumberOverride = "200FW2W122";
        auto treeOutput = volLib.CalculateVolume(volOpt, tree);
        
        //test input merch rules for BEH
        MerchRules merchRules;
        merchRules.evenOdd = 2;
        merchRules.segmentationOption = 24;
        merchRules.maxLogLength = 16.0;
        merchRules.minLogLength = 4.0;
        merchRules.minLengthTop = 2.0;
        merchRules.minTopDibSaw = 6.0;
        merchRules.minTopDibNonSaw = 6.0;
        merchRules.minMerchLength = 8.0;
        merchRules.stumpHeight = 1.0;
        merchRules.trim = 0.3;
        merchRules.barkThicknessRatio = 0.0;
        merchRules.doubleBarkThicknessAtBrestHeight = 0.0;
        merchRules.minimumBoardFootDiameter = 1.0;
        merchRules.useCorrectedFactor = false;

        // Act
        //auto treeOutput = volLib.CalculateVolume(volOpt, tree, merchRules);

        //// Assert
        Assert::IsTrue(treeOutput.grossCubicFootPrimary > 0);
    }


};