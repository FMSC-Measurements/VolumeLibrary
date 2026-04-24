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
        volOpt.region = 5;
        volOpt.forest = 1;
        volOpt.fiaCode = 65;
        volOpt.primaryProduct = 1;
        volOpt.secondaryProduct = 2;
        volOpt.volumeEquationNumberOverride = "223DVEW122";
        // ...
        
        TreeMeasurment tree;
        tree.dbh = 24;
        tree.totalHeight = 57.0;
        tree.minTopDibNonSawOverride = 0.0;
        tree.minTopDibSawOverride = 3.0;
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