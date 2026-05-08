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
        volOpt.region = 9;
        volOpt.forest = 1;
        volOpt.fiaCode = 742;
        volOpt.primaryProduct = 1;
        volOpt.secondaryProduct = 2;
        volOpt.volumeEquationNumberOverride = "900CLKE742";
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
        tree.formClass = 0;
        // ...

        // Act
        auto treeOutput = volLib.CalculateVolume(volOpt, tree);
        

        //// Assert
        Assert::IsTrue(treeOutput.greenWeightPrimary > 0);
    }


};