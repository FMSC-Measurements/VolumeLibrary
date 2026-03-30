#include "pch.h"
#include "CppUnitTest.h"

#include "VolumeCalculators/NationalScaleVolumeBiomass.h"
#include "VolumeEquation.h"

#include <string>

using namespace Microsoft::VisualStudio::CppUnitTestFramework;


TEST_CLASS(NationalScaleVolumeBiomassTest)
{
public:

    TEST_METHOD(NSVB)
    {
        // Arrange
        VolumeCalculationOptions vco;
        int spcd = 202;
        int jkSpGrp = 8;
        double dbh = 20.0;
        double tht = 110.0;
        std::string_view typeVolWt = "VolIb";
        double WDSG = 62.4;
        int ecoRegion = 240;
        int standOrigin = 0;
        std::string myVolWT = "VolIB";
        double upHt = 98.28;
        double upDia = 4.0;
        double totCF = 0.0;
        std::string IbOb = "OB";

        // Act


        //double value = NationalScaleVolumeBiomass::getVolWt(myVolWT,spcd,dbh,tht,jkSpGrp,WDSG,ecoRegion,standOrigin);
        //myVolWT = "VolOB";
        //double value2 = NationalScaleVolumeBiomass::getVolWt(myVolWT, spcd, dbh, tht, jkSpGrp, WDSG, ecoRegion, standOrigin);
        //myVolWT = "VolBK";
        //double value3 = NationalScaleVolumeBiomass::getVolWt(myVolWT, spcd, dbh, tht, jkSpGrp, WDSG, ecoRegion, standOrigin);
        //
        //upDia = 4.0;
        //IbOb = "OB";
        //double ht2o = NationalScaleVolumeBiomass::getHeightAtDiameter(spcd, dbh, tht, jkSpGrp, ecoRegion, standOrigin, upDia, totCF, IbOb);
        //IbOb = "IB";
        //double ht2i = NationalScaleVolumeBiomass::getHeightAtDiameter(spcd, dbh, tht, jkSpGrp, ecoRegion, standOrigin, upDia, totCF, IbOb);

        //upDia = 7.0;
        //double ht7 = NationalScaleVolumeBiomass::getHeightAtDiameter(spcd, dbh, tht, jkSpGrp, ecoRegion, standOrigin, upDia, totCF, IbOb);
        //upHt = 50;
        //totCF = value2; //VolOB
        //IbOb = "OB";
        //double dob = NationalScaleVolumeBiomass::getDiaAtHeight(spcd, dbh, tht, jkSpGrp, ecoRegion, standOrigin, upHt, totCF,IbOb);
        //IbOb = "IB";
        //totCF = value; //VolIB
        //double dib = NationalScaleVolumeBiomass::getDiaAtHeight(spcd, dbh, tht, jkSpGrp, ecoRegion, standOrigin, upHt, totCF, IbOb);

        //upHt = 16.5;
        //double dib4 = NationalScaleVolumeBiomass::getDiaAtHeight(spcd, dbh, tht, jkSpGrp, ecoRegion, standOrigin, upHt, totCF, IbOb);
        ////upHt = ht7;
        //IbOb = "OB";
        //double dib7 = NationalScaleVolumeBiomass::getDiaAtHeight(spcd, dbh, tht, jkSpGrp, ecoRegion, standOrigin, upHt, totCF, IbOb);
 
        TreeMeasurment tree;
        tree.dbh = 19.7;
        tree.totalHeight = 0.0;
        tree.isLive = true;
        tree.heightToTopBroken = 50.0;
        tree.topBrokenDiameter = 8.1;
        tree.minTopDibNonSawOverride = 4.0;
        tree.minTopDibSawOverride = 7.0;
        tree.cull = 0.0;
        tree.decaycd = 0;
        tree.crownRatio = 0.0;

        vco.fiaCode = 202;
        vco.region = 5;
        vco.forest = 1;
        vco.primaryProduct = 1;
        vco.secondaryProduct = 2;
        vco.volumeCalculationOptions = VolumeCalculationType::FIA;
        vco.ecoRegion = "M260";
        bool isDIB = true;
        double upht = 50.0;
        double upDib = 6.0;

        MerchRules merchRules{ 2,22,16.0,2.0,2.0,6.0,4.0,8.0,1.0,0.5,0.0,0.0,1.0, true };

        NationalScaleVolumeBiomass nsvb = NationalScaleVolumeBiomass(vco,tree);
        TreeOutput result = nsvb.CalculateVolumeBiomass(vco, tree, merchRules);
        double dib = nsvb.getDiaAtHeight(tree.dbh, tree.totalHeight, upht, isDIB);
        double ht2 = nsvb.getHeightAtDiameter(tree.dbh, tree.totalHeight, upDib, isDIB);
        int i = 0;
        //// Assert

        //static double getDiaAtHeight(int spcd,
        //    double dbh,
        //    double totalHt,
        //    int jkSpGrp,
        //    int ecoRegion,
        //    int standOrigin,
        //    double upperHeight,
        //    double totalCubic = 0.0,
        //    std::string_view IbOrOb = "Ib");

        //static double getHeightAtDiameter(int spcd,
        //    double dbh,
        //    double totalHt,
        //    int jkSpGrp,
        //    int ecoRegion,
        //    int standOrigin,
        //    double upperDiameter,
        //    double totalCubic = 0.0,
        //    std::string_view IbOrOb = "Ib");

    }
};
