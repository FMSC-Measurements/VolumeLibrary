#include "pch.h"
#include "CppUnitTest.h"

#include "TaperModels/DeMarsTaperModel.h"
//#include "TaperModels/CzaplewskiTaperModel.h"
#include "VolumeEquation.h"

#include <string>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;


TEST_CLASS(DeMarsTaperModelTest)
{
public:

    TEST_METHOD(GetDiameterAtHeightTest_With_FiaCode_351)
    {
        // Arrange
        VolumeEquation volEq = VolumeEquation::ParseVolumeEquationNumber("A16DEMW042");

        TreeMeasurment tree;
        tree.dbh = 19.7;
        tree.totalHeight = 76.0;
        double d2 = 13.4;
        double ht2 = 40;
        // ...

        DeMarsTaperModel& taperModel = DeMarsTaperModel(volEq);
        //Test CZ2 and CZ3 model
        //CzaplewskiTaperModel& taperModel = CzaplewskiTaperModel(volEq);
        
        // Act
        auto dib = taperModel.GetDiameterAtHeight(tree, ht2);
        auto ht = taperModel.GetHeightAtDiameter(tree, d2);

        //// Assert
        //Assert::IsTrue(result > 6.285 && result <= 6.29); // TODO need to create helper methods to test if floating point numbers are aproxamatly equal. 

    }


};