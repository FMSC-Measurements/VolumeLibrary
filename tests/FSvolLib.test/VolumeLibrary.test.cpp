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
        volOpt.region = 10;
        volOpt.forest = 1;
        volOpt.fiaCode = 98;
        volOpt.primaryProduct = 1;
        volOpt.secondaryProduct = 2;
        volOpt.volumeEquationNumberOverride = "A00FW2W098";
        // ...
        
        TreeMeasurment tree;
        tree.dbh = 19.7;
        tree.totalHeight = 76.0;
        tree.referenceHeight = 0.0;
        tree.referenceDiameter = 0.0;
        tree.minTopDibNonSawOverride = 0.0;
        tree.minTopDibSawOverride = 0.0;
        tree.merchHeightSaw = 0.0;
        tree.merchHeightNonsaw = 0.0;
        tree.formClass = 80;
        // ...

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
        auto treeOutput = volLib.CalculateVolume(volOpt, tree);
        //auto treeOutput = volLib.CalculateVolume(volOpt, tree, merchRules);

        //// Assert
        Assert::IsTrue(treeOutput.greenWeightPrimary > 0);
    }


};