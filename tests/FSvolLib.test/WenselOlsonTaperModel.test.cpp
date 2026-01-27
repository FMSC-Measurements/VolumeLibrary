#include "pch.h"
#include "CppUnitTest.h"

#include "TaperModels/WenselOlsonTaperModel.h"
#include "VolumeEquation.h"

#include <string>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;


TEST_CLASS(WenselOlsonTaperModelTest)
{
public:

    TEST_METHOD(GetDiameterAtHeightTest_With_FiaCode_122)
    {
        // Arrange
        VolumeEquation volEq = VolumeEquation::ParseVolumeEquationNumber("500WO2W122");
        //volEq.geoCode = VolumeEquation::GeoCode::R5;
        //volEq.modelType = VolumeEquation::ModelType::WO2;
        //volEq.usRegion = 'W';
        //volEq.fiaCode = 122;

        TreeMeasurment tree;
        tree.dbh = 15;
        tree.totalHeight = 30;
        // ...

        WenselOlsonTaperModel& taperModel = WenselOlsonTaperModel(volEq);

        // Act
        auto result = taperModel.GetDiameterAtHeight(tree, 20);


        //// Assert
        Assert::IsTrue(result > 6.285 && result <= 6.29); // TODO need to create helper methods to test if floating point numbers are aproxamatly equal. 
        
    }


};