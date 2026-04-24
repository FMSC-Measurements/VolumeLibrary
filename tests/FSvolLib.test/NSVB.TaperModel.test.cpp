#include "pch.h"
#include "CppUnitTest.h"

#include "TaperModels/NationalScaleVolumeBiomassTaperModel.h"
#include "VolumeEquation.h"

#include <string>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;

TEST_CLASS(NationalScaleVolumeBiomassTaperModelTest)
{
public:

    TEST_METHOD(GetDiameterAtHeightTest_With_NVB_122)
    {
        // Arrange
        VolumeEquation volEq = VolumeEquation::ParseVolumeEquationNumber("NVBM310122");
        //volEq.geoCode = VolumeEquation::GeoCode::R5;
        //volEq.modelType = VolumeEquation::ModelType::WO2;
        //volEq.usRegion = 'W';
        //volEq.fiaCode = 122;

        VolumeCalculationOptions vco;
        vco.region = 3;
        vco.forest = 1;
        vco.fiaCode = volEq.fiaCode;

        TreeMeasurment tree;
        tree.dbh = 15;
        tree.totalHeight = 60;
        // ...

        NationalScaleVolumeBiomassTaperModel& taperModel = NationalScaleVolumeBiomassTaperModel(volEq, vco);

        // Act
        auto dia = taperModel.GetDiameterAtHeight(tree, 20);
        auto ht = taperModel.GetHeightAtDiameter(tree, 6.0);

        //// Assert
        Assert::IsTrue(dia > 0.0); // TODO need to create helper methods to test if floating point numbers are aproxamatly equal. 

    }


};