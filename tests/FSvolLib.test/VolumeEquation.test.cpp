#include "pch.h"
#include "CppUnitTest.h"

#include "TaperModels/WenselOlsonTaperModel.h"
#include "VolumeEquation.h"

#include <string>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;


TEST_CLASS(VolumeEquationTest)
{
public:

    TEST_METHOD(ParseVolumeEquationNumber)
    {
        // Arrange
        std::string volumeEquationNumber = "500WO2W122";

        // Act
        VolumeEquation volEq = VolumeEquation::ParseVolumeEquationNumber(volumeEquationNumber);

        //// Assert
        Assert::IsTrue(volEq.geoCode == VolumeEquation::GeoCode::R5); 
        Assert::IsTrue(volEq.modelType == VolumeEquation::ModelType::WO2);
        Assert::IsTrue(volEq.usRegion == 'W');
        Assert::IsTrue(volEq.fiaCode == 122);

    }

    TEST_METHOD(GetVolumeEquationNumber)
    {
        // Arrange
        std::string expectedVolumeEquationNumber = "500WO2W122";

        VolumeEquation volEq;
        volEq.geoCode = VolumeEquation::GeoCode::R5;
        volEq.modelType = VolumeEquation::ModelType::WO2;
        volEq.usRegion = 'W';
        volEq.fiaCode = 122;

        // Act
        auto volumeEquationNumber = volEq.GetVolumeEquationNumber();

        //// Assert
        Assert::IsTrue(volumeEquationNumber == expectedVolumeEquationNumber);

    }


};