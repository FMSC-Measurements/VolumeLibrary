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
        volOpt.fiaCode = 122;
        volOpt.primaryProduct = 1;
        volOpt.secondaryProduct = 2;
        // ...
        
        TreeMeasurment tree;
        tree.dbh = 19.7;
        tree.totalHeight = 72.0;
        tree.minTopDibNonSawOverride = 3.5;
        // ...

        // Act
        auto treeOutput = volLib.CalculateVolume(volOpt, tree);
        

        //// Assert
        //Assert::IsTrue(treeOutput.greenWeightPrimary > 0);
    }


};