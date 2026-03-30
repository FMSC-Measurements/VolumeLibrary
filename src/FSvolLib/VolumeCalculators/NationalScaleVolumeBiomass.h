#pragma once
#include <stdexcept>
#include <vector>

#include "VolumeCalculatorBase.h"
#include "NationalScaleVolumeBiomass.TableStruct.h"
#include "..\Models\TreeOutput.h"
#include "..\Models\TreeMeasurment.h"
#include "..\VolumeEquation.h"
#include "..\Models\VolumeCalculationOptions.h"
#include "..\WeightfactorAndRefDataResolver.h"

struct EqCoeffs {
    int    equation;
    double a, a0, a1;
    double b, b0, b1, b2;
    double c, c1;
};

struct JkCoeffs {
    int equation;
    double a, b, c;
};

class NationalScaleVolumeBiomass //: public VolumeCalculatorBase
{

    EqCoeffs volIB_eqCoeffs;        //table 1
    EqCoeffs volBK_eqCoeffs;        //table 2
    EqCoeffs volOB_eqCoeffs;        //table 3
    EqCoeffs ratioOB_eqCoeffs;      //table 4
    EqCoeffs ratioIB_eqCoeffs;      //table 5
    EqCoeffs wtBark_eqCoeffs;       //table 6
    EqCoeffs wtBranch_eqCoeffs;     //table 7
    EqCoeffs wtTotal_eqCoeffs;      //table 8
    EqCoeffs wtFoliage_eqCoeffs;    //table 9

    WeightFactorAndRefData weightFactorAndRefData;

    std::string volEqStr;

    double volIB = 0.0;
    double volBK = 0.0;
    double volOB = 0.0;
    double wtBark = 0.0;
    double wtBranch = 0.0;
    double wtTotal = 0.0;
    double wtFoliage = 0.0;
    double ibToObRatio = 1.0;

    //NVB equation component
    int spcd;
    double dbh;
    double totalHt;
    int iDivision = 0;
    int iProvince = 0;
    int iStandOrigin = 0;
    int jkSpGrp = weightFactorAndRefData.jenkinsSpeciesGroupCD;

    const std::string strVolIB = "VolIB";
    const std::string strVolBK = "VolBK";
    const std::string strVolOB = "VolOB";
    const std::string strWtBark = "WtBark";
    const std::string strWtBranch = "WtBranch";
    const std::string strWtTotal = "WtTotal";
    const std::string strWtFoliage = "WtFoliage";


public:
    NationalScaleVolumeBiomass(VolumeCalculationOptions vco, TreeMeasurment tree)
        : weightFactorAndRefData(getSpeciesWtfactorAndRefData(vco.region, vco.forest, vco.fiaCode)),
        volEqStr(vco.volumeEquationNumberOverride), spcd(vco.fiaCode), dbh(tree.dbh), totalHt(tree.totalHeight)
    {
        //No calculation for woodland species.
        if (!(weightFactorAndRefData.jenkinsSpeciesGroupCD > 0 && weightFactorAndRefData.jenkinsSpeciesGroupCD < 10))
        {
            throw std::invalid_argument("NSVB cannot calculate for woodland species!");
        }
        //get NVB equation components
        if (!isValidNVBeq(volEqStr))
        {
            //build NVB equation using ecoRegion, spFiaCode, standOrigin (for 110 and 131 species only)
            if (spcd == 110 || spcd == 131)
            {
                if (static_cast<char>(vco.auxFlag) == 'P') iStandOrigin = 1;
            }
            if (isValidEcoRegion(vco.ecoRegion))
            {
                if (vco.ecoRegion.size() == 3) iDivision = std::stoi(vco.ecoRegion);
                if (vco.ecoRegion.size() == 4)
                {
                    iDivision = std::stoi(vco.ecoRegion.substr(1,3));
                    if (vco.ecoRegion[0] == 'M') iDivision += 1000;
                }
                if (iDivision % 10 > 0 && iDivision % 10 <= 9)
                {
                    iProvince = iDivision;
                    iDivision -= iDivision % 10;
                }
            }
            else // invalid ecoRegion, then get ot from Region, forest and district
            {
                //iProvince 
                iProvince = getEcoProvince(vco.region, vco.forest, vco.district);
                iDivision = iProvince - (iProvince % 10);
            }
            buildVolEqStr();
        }
        else setDivisionFromVolEq();

        //set the coefficients for the volEqStr
        setNSVBcoeffs();
    }
        
    static bool isValidNVBeq(std::string_view s);
    void setNSVBcoeffs();
    void setDivisionFromVolEq();
    void buildVolEqStr();
    bool isValidEcoRegion(std::string s);
    int getEcoProvince(int regn, int forst, int dist);

 /*   static double getVolWt(std::string typeVolWt,
        int spcd,
        double dbh,
        double totalHt,
        int jkSpGrp,
        double WDSG,
        int ecoRegion,
        int standOrigin);

    static double getDiaAtHeight(int spcd,
        double dbh,
        double totalHt,
        int jkSpGrp,
        int ecoRegion,
        int standOrigin,
        double upperHeight,
        double totalCubic = 0.0,
        std::string_view IbOrOb = "Ib");

    static double getHeightAtDiameter(int spcd,
        double dbh,
        double totalHt,
        int jkSpGrp,
        int ecoRegion,
        int standOrigin,
        double upperDiameter,
        double totalCubic = 0.0,
        std::string_view IbOrOb = "Ib");*/


    template <std::size_t N>
    EqCoeffs find_spEqCoef2(const std::array<spCoefRow, N>& SPcoef, const std::array<jkCoefRow, 9>& JKcoef);
    double getVolWt_impl(double D, double H, EqCoeffs eqCoeffs);
    double getVolWt(std::string typeVolWt, double dbh, double totalHt);
    double getDiaAtHeight(double dbh, double totalHt, double upperHeight, bool isDIB = true);
    double getHeightAtDiameter(double dbh, double totalHt, double upperDiameter, bool isDIB = true);
    double getEstimatedTotalHeight(double dbh, double upperHt, double upperDia, EqCoeffs eqCoeffs);
    TreeOutput CalculateVolumeBiomass(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules);

};