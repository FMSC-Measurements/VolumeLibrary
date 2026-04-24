#include <span>
#include <array>
#include <optional>
#include <cmath>
#include <vector>
#include <string>
#include "NationalScaleVolumeBiomass.h"
#include "NationalScaleVolumeBiomass.Table1.h" // contains SPcoef and spCoefRow
#include "NationalScaleVolumeBiomass.Table2.h"
#include "NationalScaleVolumeBiomass.Table3.h"
#include "NationalScaleVolumeBiomass.Table4.h"
#include "NationalScaleVolumeBiomass.Table5.h"
#include "NationalScaleVolumeBiomass.Table6.h"
#include "NationalScaleVolumeBiomass.Table7.h" 
#include "NationalScaleVolumeBiomass.Table8.h" 
#include "NationalScaleVolumeBiomass.Table9.h" 
#include "NationalScaleVolumeBiomass.Table11.h" 
#include "NationalScaleVolumeBiomass.DistrictProvinceData.h"
#include "..\string_helper.h"

// What we return from the search:
//struct EqCoeffs {
//    int    equation;
//    double a, a0, a1;
//    double b, b0, b1, b2;
//    double c, c1;
//};
//
//struct JkCoeffs {
//    int equation;
//    double a, b, c;
//};

// Helper: choose the best match among exact and wildcard rows
// Priority order:
//  1) (spcd, division, standOrigin)
//  2) (spcd, 0,        standOrigin)
//  3) (spcd, division, 0)
//  4) (spcd, 0,        0)

bool NationalScaleVolumeBiomass::isValidNVBeq(std::string_view s) {
    // Must be length 10 or 11
    if (s.size() != 10 && s.size() != 11) return false;

    // First three characters: "NVB"
    if (s.substr(0, 3) != "NVB") return false;

    // Fourth character: '0' or 'M'
    char c4 = s[3];
    if (!(c4 == '0' || c4 == 'M')) return false;

    // 5th to 10th characters: digits
    for (size_t i = 4; i < 10; ++i) {
        if (!std::isdigit(static_cast<unsigned char>(s[i]))) return false;
    }

    // Optional 11th character: 'P' or digit
    if (s.size() == 11) {
        char c11 = s[10];
        if (!(c11 == 'P' || std::isdigit(static_cast<unsigned char>(c11)))) return false;
        //char c11 = s[10];
        if (c11 == 'P') {
            // 'P' allowed only if 8–10 == "110" or "131"
            std::string_view last3 = s.substr(7, 3);
            if (last3 != "110" && last3 != "131") return false;
        }
        else if (!std::isdigit(static_cast<unsigned char>(c11))) {
            // Otherwise, must be a digit
            return false;
        }
    }

    return true;
}

bool NationalScaleVolumeBiomass::isValidEcoRegion(std::string s)
{
    if (s.size() == 3) {

        for (char c : s) {
            if (!std::isdigit(static_cast<unsigned char>(c))) return false;
        }
    }
    else if (s.size() == 4) {
        // First character must be '0' or 'M'
        if (s[0] != '0' && s[0] != 'M') return false;
        // Next three must satisfy the 3-character rule
        for (char c : s.substr(1)) {
            if (!std::isdigit(static_cast<unsigned char>(c))) return false;
        }
    }
    return true;
}

template <std::size_t N>
static std::optional<EqCoeffs>
find_spEqCoef(int spcd, int jkSpeciesGroup, const std::array<spCoefRow, N>& SPcoef, const std::array<jkCoefRow, 9>& JKcoef, int division = 0, int standOrigin = 0)
{
    const spCoefRow* best = nullptr;
    int bestRank = -1;

    for (const auto& row : SPcoef) {
        if (row.spcd != spcd) continue;

        // Determine how well this row matches requested division/origin
        int rank = -1;
        const bool divExact = (row.division == division);
        const bool divWild = (row.division == 0);
        const bool orgExact = (row.standOrigin == standOrigin);
        const bool orgWild = (row.standOrigin == 0);

        if (divExact && orgExact)      rank = 4; // exact+exact
        else if (divWild && orgExact)  rank = 3; // wildcard division, exact origin 
        else if (divExact && orgWild) rank = 2; // exact division, wildcard origin
        else if (divWild && orgWild)  rank = 1; // wildcard both
        else                           continue; // not a match
        
        // Prefer higher rank; if tie, keep the first encountered
        if (rank > bestRank) {
            best = &row;
            bestRank = rank;
            if (bestRank == 4) break; // exact hit — we can stop
        }
    }

    if (best)  
    {
        return EqCoeffs{
            best->equation,
            best->a, best->a0, best->a1,
            best->b, best->b0, best->b1, best->b2,
            best->c, best->c1
        };
    }
    else // Not find in the species coefficients list then search Jenkins species group
    {
        for (const auto& row : JKcoef)
        {
            if (row.spgrpcd == jkSpeciesGroup)
            {
                return EqCoeffs{
                    row.equation,
                    row.a,0.0,0.0,
                    row.b,0.0,0.0,0.0,
                    row.c,0.0
                };
            }
        }
    }
    return std::nullopt;
}

template <std::size_t N>
EqCoeffs NationalScaleVolumeBiomass::find_spEqCoef2(const std::array<spCoefRow, N>& SPcoef, const std::array<jkCoefRow, 9>& JKcoef)
{
    const spCoefRow* best = nullptr;
    int bestRank = -1;

    for (const auto& row : SPcoef) {
        if (row.spcd != spcd) continue;

        // Determine how well this row matches requested division/origin
        int rank = -1;
        const bool divExact = (row.division == iDivision);
        const bool divWild = (row.division == 0);
        const bool orgExact = (row.standOrigin == iStandOrigin);
        const bool orgWild = (row.standOrigin == 0);

        if (divExact && orgExact)      rank = 4; // exact+exact
        else if (divWild && orgExact)  rank = 3; // wildcard division, exact origin 
        else if (divExact && orgWild) rank = 2; // exact division, wildcard origin
        else if (divWild && orgWild)  rank = 1; // wildcard both
        else                           continue; // not a match

        // Prefer higher rank; if tie, keep the first encountered
        if (rank > bestRank) {
            best = &row;
            bestRank = rank;
            if (bestRank == 4) break; // exact hit — we can stop
        }
    }

    if (best)
    {
        return EqCoeffs{
            best->equation,
            best->a, best->a0, best->a1,
            best->b, best->b0, best->b1, best->b2,
            best->c, best->c1
        };
    }
    else // Not find in the species coefficients list then search Jenkins species group
    {
        for (const auto& row : JKcoef)
        {
            if (row.spgrpcd == jkSpGrp)
            {
                return EqCoeffs{
                    row.equation,
                    row.a,0.0,0.0,
                    row.b,0.0,0.0,0.0,
                    row.c,0.0
                };
            }
        }
    }
    return EqCoeffs{ 0 };
}

//set the coefficients for use in the NSVB calculation
void NationalScaleVolumeBiomass::setNSVBcoeffs()
{
    volIB_eqCoeffs = find_spEqCoef2(SPcoef1, JKcoef1);
    volBK_eqCoeffs = find_spEqCoef2(SPcoef2, JKcoef2);
    volOB_eqCoeffs = find_spEqCoef2(SPcoef3, JKcoef3);
    ratioOB_eqCoeffs = find_spEqCoef2(SPcoef4, JKcoef4);
    ratioIB_eqCoeffs = find_spEqCoef2(SPcoef5, JKcoef5);
    wtBark_eqCoeffs = find_spEqCoef2(SPcoef6, JKcoef6);
    wtBranch_eqCoeffs = find_spEqCoef2(SPcoef7, JKcoef7);
    wtTotal_eqCoeffs = find_spEqCoef2(SPcoef8, JKcoef8);
    wtFoliage_eqCoeffs = find_spEqCoef2(SPcoef9, JKcoef9);
    //get the volume and ratio
    //volIB = getVolWt(strVolIB, dbh, totalHt);
    //volOB = getVolWt(strVolOB, dbh, totalHt);
    //volBK = getVolWt(strVolBK, dbh, totalHt);
    //if (volIB > 0.0) ibToObRatio = sqrt(volIB / (volIB + volBK));
}

//set the volume and ratio
void NationalScaleVolumeBiomass::setIbToObRatio(double dbh, double totalHt)
{
    //get the volume and ratio
    volIB = getVolWt(strVolIB, dbh, totalHt);
    volOB = getVolWt(strVolOB, dbh, totalHt);
    volBK = getVolWt(strVolBK, dbh, totalHt);
    if (volIB > 0.0) ibToObRatio = sqrt(volIB / (volIB + volBK));
}

void NationalScaleVolumeBiomass::setDivisionFromVolEq()
{
    iDivision = std::stoi(volEqStr.substr(4, 3));
    if (volEqStr[3] == 'M') iDivision += 1000;
    if (iProvince == 0) iProvince = iDivision;
    iDivision = iDivision - (iDivision % 10);

    if (volEqStr.size() == 11)
    {
        if (volEqStr[10] == 'P' && (volEqStr.substr(7, 3) == "110" || volEqStr.substr(7, 3) == "131"))
        {
            iStandOrigin = 1;
        }
    }
}

void NationalScaleVolumeBiomass::buildVolEqStr()
{

    volEqStr = "NVB";

    if (iDivision > 999) {
        volEqStr += 'M';
        volEqStr += string_helper::pad3(iDivision-1000);
    }
    else {
        volEqStr += '0';
        volEqStr += string_helper::pad3(iDivision);
    }

    // Append spcd as 3-digit, zero-padded
    volEqStr += string_helper::pad3(spcd);

    if (iStandOrigin == 1) {
        volEqStr += 'P';
    }

}



inline double NationalScaleVolumeBiomass::getVolWt_impl(double D, double H, EqCoeffs eqCoeffs)
{
    int equation = eqCoeffs.equation;
    double a = eqCoeffs.a;
    double a0 = eqCoeffs.a0;
    double a1 = eqCoeffs.a1;
    double b = eqCoeffs.b;
    double b0 = eqCoeffs.b0;
    double b1 = eqCoeffs.b1;
    double b2 = eqCoeffs.b2;
    double c = eqCoeffs.c;
    double c1 = eqCoeffs.c1;

    if (equation <= 0) return 0.0;

    switch (equation) {
    case 1: {
        return a * std::pow(D, b) * std::pow(H, c);
    }
    case 2: {
        int k = 11;
        if (spcd < 300) k = 9;
        if (D < k) {
            return a0 * std::pow(D, b0) * std::pow(H, c);
        }
        else {
            return a0 * std::pow(static_cast<double>(k), (b0 - b1)) * std::pow(D, b1) * std::pow(H, c);
        }
    }
    case 3: {
        // D^( a1 * (1 - exp(-b1*D))^c1 )
        const double inner = 1.0 - std::exp(-b1 * D);
        const double expo = a1 * std::pow(inner, c1);
        return a * std::pow(D, expo) * std::pow(H, c);
    }
    case 4: {
        return a * std::pow(D, b) * std::pow(H, c) * std::exp(-(b2 * D));
    }
    case 5: {
        // Weight with WDSG; 62.4 lb/ft^3 divisor as in Fortran
        return a * std::pow(D, b) * std::pow(H, c) * (weightFactorAndRefData.WDSG / 62.4);
    }
    default:
        return 0.0;
    }
}
//calculate diameter at a given height
inline double getDiaAtHeight_impl(double totalCubic, double totalHeight, double upperHeight, EqCoeffs eqCoeffs)
{
    double a = eqCoeffs.a;
    double b = eqCoeffs.b;
    double THT = totalHeight;
    double HT2 = upperHeight;
    double TCUFT = totalCubic;

    if (upperHeight > totalHeight) return 0.0;
    return std::sqrt(TCUFT / 0.005454154 / THT * (a * b * std::pow(1.0 - HT2 / THT, a - 1.0) * std::pow(1.0 - std::pow(1.0 - HT2 / THT, a), b - 1.0)));
}

//calculate height at a given diameter
inline double getHeightAtDiameter_impl(double totalCubic, double totalHeight, double topDiameter, EqCoeffs eqCoeffs)
{
    // Binary search for HT2 meeting diameter TOPD from ratio function.
    double a = eqCoeffs.a;
    double b = eqCoeffs.b;
    double HT2 = 0.0;
    double HTTOT = totalHeight;
    double low = 0.0, hi = totalHeight;
    double diff = 1.0;
    int loopcnt = 0;
    while (std::abs(diff) > 0.001) {
        double mid = (low + hi) / 2.0;
        if (mid < 0.5) { HT2 = 0.0; break; }   // Fortran: stop when mid < stump ht (approx)
        double est = getDiaAtHeight_impl(totalCubic, totalHeight, mid, eqCoeffs);
        diff = topDiameter - est;
        if (std::abs(diff) < 0.001) { HT2 = mid; break; }
        if (diff < 0.0) low = mid; else hi = mid;
        if (++loopcnt > 1000) { HT2 = mid; break; }
    }
    return HT2;
}

inline double getRatio_impl(double H, double h1, EqCoeffs eqCoeffs)
{
    double r = 0.0;
    double a = eqCoeffs.a;
    double b = eqCoeffs.b;
    int equation = eqCoeffs.equation;
    if (equation == 6 && (h1 > 0.0 && h1 <= H)) {
        r = pow(1.0 - pow(1.0 - h1 / H, a), b);
    }
    return r;
}

// Helper to parse the vol/wt type into an enum.
// Valid typeVolWt: volib(table1), volbk(table2), volob(table3), wtBark(table6), wtBranch(table7), wtTotal(table8), wtFoliage(table9)
enum class VolWtType {
    VolIB,      // table1
    VolBK,      // table2
    VolOB,      // table3
    WtBark,     // table6
    WtBranch,   // table7
    WtTotal,    // table8
    WtFoliage,  // table9
    Unknown
};

inline VolWtType parseVolWtType(std::string s) noexcept {
    if (s == "VolIB")     return VolWtType::VolIB;
    if (s == "VolBK")     return VolWtType::VolBK;
    if (s == "VolOB")     return VolWtType::VolOB;
    if (s == "WtBark")    return VolWtType::WtBark;
    if (s == "WtBranch")  return VolWtType::WtBranch;
    if (s == "WtTotal")   return VolWtType::WtTotal;
    if (s == "WtFoliage") return VolWtType::WtFoliage;
    return VolWtType::Unknown;
}



double NationalScaleVolumeBiomass::getVolWt(std::string typeVolWt, double dbh, double totalHt)
{
    double volwt = 0.0;
    double totalCubic = 0.0;

    // If you can change call sites, prefer std::string_view here:
    const VolWtType t = parseVolWtType(typeVolWt);

    switch (t) {
    case VolWtType::VolIB:
        volwt = getVolWt_impl(dbh, totalHt, volIB_eqCoeffs);
        break;
    case VolWtType::VolBK:
        volwt = getVolWt_impl(dbh, totalHt, volBK_eqCoeffs);
        break;
    case VolWtType::VolOB:
        volwt = getVolWt_impl(dbh, totalHt, volOB_eqCoeffs);
        break;
    case VolWtType::WtBark:
        volwt = getVolWt_impl(dbh, totalHt, wtBark_eqCoeffs);
        break;
    case VolWtType::WtBranch:
        volwt = getVolWt_impl(dbh, totalHt, wtBranch_eqCoeffs);
        break;
    case VolWtType::WtTotal:
        volwt = getVolWt_impl(dbh, totalHt, wtTotal_eqCoeffs);
        break;
    case VolWtType::WtFoliage:
        volwt = getVolWt_impl(dbh, totalHt, wtFoliage_eqCoeffs);
        break;
    case VolWtType::Unknown:
    default:
        // Unknown type — keep current behavior (returns 0.0).
        // Optionally, log an error here.
        break;
    }

    return volwt;
}



double NationalScaleVolumeBiomass::getDiaAtHeight(double dbh, double totalHt, double upperHeight, bool isDIB)
{
    double value = 0.0;
    double totCF = volOB;

    if (totCF == 0.0)
    {
        totCF = getVolWt(strVolOB, dbh, totalHt);
    }
    value = getDiaAtHeight_impl(totCF, totalHt, upperHeight, ratioOB_eqCoeffs);
    
    // the diameter is always calculated using outside bar coefficients. it convert into inside bark diameter by mutily the ratio
    if (ibToObRatio == 1.0) setIbToObRatio(dbh, totalHt);
    if (isDIB) value = value * ibToObRatio;

    return value;
}



//changed this function to only use outside bark coefficient and outside bark diameter
double NationalScaleVolumeBiomass::getHeightAtDiameter(double dbh, double totalHt, double upperDiameter, bool isDIB)
{
    double value = 0.0;
    double totCF = volOB;
    double upDia = upperDiameter;

    // if upperDiameter is inside bark, convert it to outside bark for calculation
    if (ibToObRatio == 1.0) setIbToObRatio(dbh, totalHt);
    if (isDIB) upDia = upDia / ibToObRatio;

    if (totCF == 0.0)
    {
        totCF = getVolWt(strVolOB, dbh, totalHt);
    }
    value = getHeightAtDiameter_impl(totCF, totalHt, upDia, ratioOB_eqCoeffs);
    return value;
}

//get the species average crown ratio by Province (not Division)
//from table 11
double getAverageCrownRatio(int DIVISION, int SPCD)
{
    const auto& table = (SPCD < 300) ? DIVCRs : DIVCRh;

    // DIVISION == 0 → last row (Fortran used Tbl11Cnt)
    if (DIVISION == 0) {
        return table[Tbl11Cnt - 1].cr_percent / 100.0;
    }

    // Search for DIVISION in first column; else default to last row
    for (std::size_t i = 0; i < Tbl11Cnt; ++i) {
        if (table[i].division == DIVISION) {
            return table[i].cr_percent / 100.0;
        }
    }

    return table[Tbl11Cnt - 1].cr_percent / 100.0;
}

//calculate branch remaining for broken height tree
double getBranchRem(int province, int spcd, double totalHeight, double brokenHeight, double crownRatioDecimal)
{
    double BrchRem = 1.0;
    double CR = crownRatioDecimal;
    double HTTOT = totalHeight;
    double BRKHT = brokenHeight;
    if (CR > 0.0) {
        if (CR >= 1.0) CR = CR / 100.0;
        double CRh = (HTTOT - BRKHT * (1 - CR)) / HTTOT;
        BrchRem = (BRKHT - HTTOT * (1 - CRh)) / (HTTOT * CRh);
    }
    else {
        double CRh = 0.0; 
        CRh = getAverageCrownRatio(province, spcd);
        if (CRh >= 1.0) CRh = CRh / 100.0;
        double CrownLen = HTTOT * CRh;
        BrchRem = (CrownLen - (HTTOT - BRKHT)) / CrownLen;
    }
    if (BrchRem <= 0.01) BrchRem = 0.0;
    return BrchRem;
}

//estimate total height from upper stem height and upper stem diameter
double NationalScaleVolumeBiomass::getEstimatedTotalHeight(double dbh, double upperHt, double upperDia, EqCoeffs eqCoeffs)
{
    double a = eqCoeffs.a;
    double b = eqCoeffs.b;
    if (upperHt <= 0.0 || upperDia <= 0.0) return 0.0;
    if (upperDia <= 1.0) return upperHt + 1.0;
    double tht = upperHt + 1.0;
    int i = 1;
    double diaDiff = 0.0;
    while (i < 100) {
        double Vtotob = getVolWt(strVolOB, dbh, tht);
        double dobAtUpperHt = getDiaAtHeight_impl(Vtotob, tht, upperHt, eqCoeffs);
        diaDiff = upperDia - dobAtUpperHt;
        if (abs(diaDiff) < 0.05) break;
        tht += diaDiff;
        i += 1;
    }
    if (tht < upperHt) tht = upperHt + 1.0;
    return tht;
}

//get Province from region, forest and district number
int NationalScaleVolumeBiomass::getEcoProvince(int REGN, int FORST, int DIST) {
    int iPROV = 0;

    const int DistNum = REGN * 10000 + FORST * 100 + DIST;
    const int ForstNum = REGN * 100 + FORST;

    if (int idx = findIndexBinary(distProvData.data(), distProvData.size(), DistNum); idx >= 0) {
        iPROV = distProvData[idx].prov;
    }
    else if (int idx = findIndexBinary(forstProvData.data(), ForstCnt, ForstNum); idx >= 0) {
        iPROV = forstProvData[idx].prov;
    }
    else if (int idx = findIndexLinear(regnProvData.data(), RegnCnt, REGN); idx >= 0) {
        iPROV = regnProvData[idx].prov;
    }
    return iPROV;
}

//get decay densitity proportion and dead carbon fraction.
double getDecayDenProp(int SFTHRD, int DECAYCD,  double& DeadCF)
{
    double DenProp = 0.97;
    // Softwood SFTHRD==0; Hardwood SFTHRD!=0
    if (SFTHRD == 0) {
        if (DECAYCD == 1) { DenProp = 0.97; DeadCF = 0.501; }
        else if (DECAYCD == 2) { DenProp = 1.00; DeadCF = 0.504; }
        else if (DECAYCD == 3) { DenProp = 0.92; DeadCF = 0.506; }
        else if (DECAYCD == 4) { DenProp = 0.55; DeadCF = 0.52; }
        else if (DECAYCD == 5) { DenProp = 0.55; DeadCF = 0.527; }
    }
    else {
        if (DECAYCD == 1) { DenProp = 0.99; DeadCF = 0.47; }
        else if (DECAYCD == 2) { DenProp = 0.80; DeadCF = 0.473; }
        else if (DECAYCD == 3) { DenProp = 0.54; DeadCF = 0.481; }
        else if (DECAYCD == 4) { DenProp = 0.43; DeadCF = 0.48; }
        else if (DECAYCD == 5) { DenProp = 0.43; DeadCF = 0.472; }
    }
    return DenProp;
}

//calculate NSVB volume and biomass
TreeOutput NationalScaleVolumeBiomass::CalculateVolumeBiomass(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;
    //species weight factor data
    double weightFactorGreen = weightFactorAndRefData.weightFactorSaw;
    double weightFactorDead = weightFactorAndRefData.weightFactorDead;
    double weightFactorDry = weightFactorAndRefData.weightFactorDry;
    double mc = 0.0;

    double dbh = tree.dbh;
    if (dbh < 1.0) { out.errflag = 3; return out; }
    if (jkSpGrp == 10 || spcd < 10) { 
        //woodland species 
        out.errflag = 6; 
        return out; 
    }
    
    double totalHt = tree.totalHeight;
    if (totalHt < 4.5) 
    { 
        //total height needed for the calculation
        //try to get the estimated total height from upper height and diameter
        double upperHt = 0.0;
        double upperDia = 0.0;
        if (tree.referenceHeight > 0.0 && tree.referenceDiameter > 0.0)
        {
            upperHt = tree.referenceHeight;
            upperDia = tree.referenceDiameter;
        }
        else if (tree.heightToTopBroken > 0.0 && tree.topBrokenDiameter)
        {
            upperHt = tree.heightToTopBroken;
            upperDia = tree.topBrokenDiameter;
        }
        else if (tree.merchHeightNonsaw > 0.0 && tree.minTopDibNonSawOverride > 0.0)
        {
            upperHt = tree.merchHeightNonsaw;
            upperDia = tree.minTopDibNonSawOverride;
        }
        else if (tree.merchHeightSaw > 0.0 && tree.minTopDibSawOverride > 0.0)
        {
            upperHt = tree.merchHeightSaw;
            upperDia = tree.minTopDibSawOverride;
        }
        if (upperHt > 0.0 && upperDia > 0.0)
        {
            totalHt = getEstimatedTotalHeight(dbh, upperHt, upperDia, ratioOB_eqCoeffs);
        }
        if (totalHt < 4.5)
        {
            //trees with DBH only, use Jenkins method to calculate biomass
            //add jenkins method here
            out.errflag = 4; 
            return out; 
        }
    }
    
    //check stump, sawDia, nonsawDia override, decaycd
    if (spcd == 204) spcd = 202;
    double stump = merchRules.stumpHeight;
    if (tree.stumpHeightOverride > 0.0) stump = tree.stumpHeightOverride;
    double sawTopDia = merchRules.minTopDibSaw;
    if (tree.minTopDibSawOverride > 0.0) sawTopDia = tree.minTopDibSawOverride;
    double nonsawTopDia = merchRules.minTopDibNonSaw;
    if (tree.minTopDibNonSawOverride > 0.0) nonsawTopDia = tree.minTopDibNonSawOverride;
    int decaycd = tree.decaycd;
    if (!tree.isLive && decaycd == 0) decaycd = 3;
    if (tree.isLive && decaycd > 0) decaycd = 0;
    //if (nonsawTopDia > sawTopDia) nonsawTopDia = sawTopDia;
    double cullReduction = 1.0 - tree.cull / 100.0;

    //Broken top removal & branch remaining
    double Rrem = 1.0;
    double BrchRem = 1.0;
    if (tree.heightToTopBroken > 0.0)
    {
         Rrem = getRatio_impl(totalHt, tree.heightToTopBroken, ratioOB_eqCoeffs);
         if (iProvince == 0) iProvince = getEcoProvince(vco.region, vco.forest, vco.district);
         double cr = tree.crownRatio;
         if (tree.isLive && cr == 0.0) cr = 1.0;
         BrchRem = getBranchRem(iProvince, spcd, totalHt, tree.heightToTopBroken, cr);
         //BrchRem will be used for branch weight calculation
    }

    // Dead-tree adjustments
    double CullDenProp = (weightFactorAndRefData.softHard == 0) ? 0.92 : 0.54;
    double DenProp = 1.0, RemBkProp = 1.0, RemBrchProp = 1.0, DeadCF = 0.5;
    if (decaycd > 0) {
        if (decaycd == 1) { RemBkProp = 1.0; RemBrchProp = 1.0; }
        else if (decaycd == 2) { RemBkProp = 0.8; RemBrchProp = 0.5; }
        else if (decaycd == 3) { RemBkProp = 0.5; RemBrchProp = 0.1; }
        else if (decaycd == 4) { RemBkProp = 0.2; RemBrchProp = 0.0; }
        else if (decaycd == 5) { RemBkProp = 0.0; RemBrchProp = 0.0; }
        DenProp=getDecayDenProp(weightFactorAndRefData.softHard, decaycd, DeadCF);
    }

    // (1) total stem wood volume inside bark
    double Vtotib = getVolWt(strVolIB, dbh, totalHt);
    if (Vtotib <= 0.0) { out.errflag = 1; return out; }
    double Vibmiss = Vtotib * (1.0 - Rrem);
    double VtotibSound = Vtotib * Rrem * cullReduction;
    out.totalCubicFoot = VtotibSound;

    // (2) total stem bark volume
    double Vtotbk = getVolWt(strVolBK, dbh, totalHt);
    double Vbkmiss = Vtotbk * (1.0 - Rrem);
    double VtotbkSound = Vtotbk * Rrem;
    ibToObRatio = sqrt(Vtotib / (Vtotib + Vtotbk));

    // (3) outside bark total
    double Vtotob = Vtotib + Vtotbk;
    double Vobmiss = Vibmiss + Vbkmiss;
    double VtotobSound = VtotibSound + VtotbkSound;

    // (4) stump volume via ratio
    double Rstump = getRatio_impl(totalHt, stump, ratioIB_eqCoeffs);
    double Vstumpib = Vtotib * Rstump;
    double Vstumpob = Vtotob * Rstump;
    double Vstumpbk = Vstumpob - Vstumpib;
    double VstumpibSound = Vstumpib * cullReduction;
    out.stumpCubicFoot = VstumpibSound;

    // (5) for saw timber (prod = 1) calculate saw volume and topwood
    // for other prod, only calculate merch volume
    // first get the merch height to nonsaw top dia. It is needed for saw and nonsaw calculation
    double merchHeightNonsaw = tree.merchHeightNonsaw;
    volOB = getVolWt(strVolOB, dbh, totalHt);
    
    // merch height is calculated using outside bark diameter. Top diameter input for FIA is already outside bark
    // for calculation type FVS and Cruise, the top diameter input is inside bark, need to convert to outside bark 
    if (vco.volumeCalculationOptions != VolumeCalculationType::FIA)
    {
        nonsawTopDia = nonsawTopDia / ibToObRatio;
        sawTopDia = sawTopDia / ibToObRatio;
    }

    if (merchHeightNonsaw <= 0.0)
    {
        // get merch saw height to nonsawTopDob
        merchHeightNonsaw = getHeightAtDiameter(dbh, totalHt, nonsawTopDia, false);
    }
    if (tree.heightToTopBroken > 0.0 && tree.heightToTopBroken < merchHeightNonsaw) merchHeightNonsaw = tree.heightToTopBroken;

    double Rmrch = getRatio_impl(totalHt, merchHeightNonsaw, ratioIB_eqCoeffs);
    double Vsawib = 0.0, Vsawob = 0.0, Vsawbk = 0.0, VsawibSound = 0.0, Vtwbk = 0.0, VtwibSound = 0.0;
    double Vmrchbk = 0.0, VmrchibSound = 0.0;

    if (vco.primaryProduct == 1)
    {
        double merchHeightSaw = tree.merchHeightSaw;
        if (merchHeightSaw <= 0.0)
        {
            // get merch saw height to sawTopDob
            merchHeightSaw = getHeightAtDiameter(dbh, totalHt, sawTopDia, false);
        }
        if (tree.heightToTopBroken > 0.0 && tree.heightToTopBroken < merchHeightSaw) merchHeightSaw = tree.heightToTopBroken;

        if ((merchHeightSaw - stump) >= merchRules.minMerchLength)
        {
            double Rsaw = getRatio_impl(totalHt, merchHeightSaw, ratioIB_eqCoeffs);
            Vsawib = Vtotib * Rsaw - Vstumpib;
            Vsawob = Vtotob * Rsaw - Vstumpob;
            Vsawbk = Vsawob - Vsawib;
            VsawibSound = Vsawib * cullReduction;
            out.grossCubicFootPrimary = VsawibSound;
        }
        else merchHeightSaw = stump;

        // calculate topwood
        if (merchHeightNonsaw - merchHeightSaw >= merchRules.minLengthTop)
        {
            double Vmrchib = Vtotib * Rmrch - Vstumpib;
            double Vmrchob = Vtotob * Rmrch - Vstumpob;
            double Vmrchbk = Vmrchob - Vmrchib;
            double Vtwib = Vmrchib - Vsawib;
            double Vtwob = Vmrchob - Vsawob;
            Vtwbk = Vtwob - Vtwib;
            VtwibSound = Vtwib * cullReduction;
            out.grossCubicFootSecondary = VtwibSound;
        }
    }
    else //for nonsaw product
    {
        if ((merchHeightNonsaw - stump) >= merchRules.minMerchLength)
        {
            double Vmrchib = Vtotib * Rmrch - Vstumpib;
            double Vmrchob = Vtotob * Rmrch - Vstumpob;
            Vmrchbk = Vmrchob - Vmrchib;
            VmrchibSound = Vmrchib * cullReduction;
            out.grossCubicFootPrimary = VmrchibSound;
        }
    }

    // (6) calculate tip volume
    double Vtipib = Vtotib * (1.0 - Rmrch);
    double Vtipob = Vtotob * (1.0 - Rmrch);
    double Vtipbk = Vtipob - Vtipib;
    double VtipibSound = (Vtipib - Vibmiss) * cullReduction;
    double VtipbkSound = Vtipbk - Vbkmiss;
    if (VtipibSound < 0.0) VtipibSound = 0.0;
    if (VtipbkSound < 0.0) VtipbkSound = 0.0;
    double VtipobSound = VtipibSound + VtipbkSound;
    out.tipCubicFoot = VtipibSound;

    // (7) stem wood weight (dead trees ignore CULL)
    double cull = tree.cull;
    if (!tree.isLive) cull = 0.0;
    double Wtotib = Vtotib * weightFactorAndRefData.WDSG;
    double WtotibRed = Wtotib * Rrem * (1.0 - cull / 100.0 * (1.0 - CullDenProp)) * DenProp;

    // (8) bark weight
    double Wtotbk = getVolWt(strWtBark, dbh, totalHt);
    double WtotbkRed = Wtotbk * Rrem * DenProp * RemBkProp;

    // (9) branches weight
    double Wbrch = getVolWt(strWtBranch, dbh, totalHt);
    double WbrchRed = Wbrch * DenProp * RemBrchProp * BrchRem;

    // (10) AGB total prediction
    double AGBpred = getVolWt(strWtTotal, dbh, totalHt);
    double AGBcompRed = WtotibRed + WtotbkRed + WbrchRed;
    double AGBred = AGBcompRed / (Wtotib + Wtotbk + Wbrch);
    double AGBpredRed = AGBpred * AGBred;
    double AGBdiff = AGBpredRed - AGBcompRed;
    // Allocate difference proportionally
    double WoodR = WtotibRed / AGBcompRed;
    double BarkR = WtotbkRed / AGBcompRed;
    double BrchR = WbrchRed / AGBcompRed;
    double WoodAdd = AGBdiff * WoodR;
    double BarkAdd = AGBdiff * BarkR;
    double BrchAdd = AGBdiff * BrchR;
    double WoodHarm = WtotibRed + WoodAdd;
    double BarkHarm = WtotbkRed + BarkAdd;
    double BrchHarm = WbrchRed + BrchAdd;

    // (11) foliage
    double Wfol = getVolWt(strWtFoliage, dbh, totalHt);
    double WfolRem = Wfol * BrchRem;
    if (!tree.isLive) WfolRem = 0.0;

    // (12) adjusted densities
    double WDSGadj = (VtotibSound > 0.0) ? (WoodHarm / VtotibSound) : 0.0;
    double BKSGadj = (VtotbkSound > 0.0) ? (BarkHarm / VtotbkSound) : 0.0;

    // (13) weights for components
    double Wsawib = VsawibSound * WDSGadj;
    double Wsawbk = Vsawbk * BKSGadj;
    double Wtwib = VtwibSound * WDSGadj;
    double Wtwbk = Vtwbk * BKSGadj;
    double Wtipib = VtipibSound * WDSGadj;
    double Wtipbk = VtipbkSound * BKSGadj;
    double Wstumpib = VstumpibSound * WDSGadj;
    double Wstumpbk = Vstumpbk * BKSGadj;
    double Wmrchib = VmrchibSound * WDSGadj;
    double Wmrchbk = Vmrchbk * BKSGadj;

    // (14) DRYBIO & carbon
    out.dryBio.aboveGroundTotal = AGBpredRed;
    out.dryBio.stemWoodTotal = WoodHarm;
    out.dryBio.stemBarkTotal = BarkHarm;
    out.dryBio.stumpWood = Wstumpib;
    out.dryBio.stumpBark = Wstumpbk;
    if (vco.primaryProduct == 1)
    {
        out.dryBio.stemPrimaryWood = Wsawib;
        out.dryBio.stemPrimaryBark = Wsawbk;
        out.dryBio.stemSecondaryWood = Wtwib;
        out.dryBio.stemSecondaryBark = Wtwbk;
        out.dryWeightPrimary = Wsawib + Wsawbk;
        out.dryWeightSecondary = Wtwib + Wtwbk;
    }
    else
    {
        out.dryBio.stemPrimaryWood = Wmrchib;
        out.dryBio.stemSecondaryBark = Wmrchbk;
        out.dryWeightPrimary = Wmrchib + Wmrchbk;
    }
    out.dryBio.stemTipWood = Wtipib;
    out.dryBio.stemTipBark = Wtipbk;
    out.dryBio.branches = BrchHarm;
    out.dryBio.foliage = WfolRem;
    out.dryBio.stemTopAndLimb = AGBpredRed - Wstumpib - Wstumpbk - Wsawib - Wsawbk - Wtwib - Wtwbk;
    
    double carbonFraction = weightFactorAndRefData.carbonFraction;
    if (!tree.isLive && decaycd > 0) carbonFraction = DeadCF;
    out.carbonContent = AGBpredRed * carbonFraction;

    // (15) GRNBIO via moisture content
    //double weightFactorGreen = weightFactorAndRefData.weightFactorSaw;
    //double weightFactorDead = weightFactorAndRefData.weightFactorDead;
    weightFactorDry = (WoodHarm + BarkHarm) / Vtotib;
    if(weightFactorDry < 10.0) weightFactorDry = weightFactorAndRefData.weightFactorDry;
    //double mc = 0.0;
    if (tree.isLive) mc = (weightFactorGreen - weightFactorDry) / weightFactorDry;
    else mc = (weightFactorDead - weightFactorDry) / weightFactorDry;
    double factor = 1.0 + mc;
    //convert dry to green
    out.greenBio = scale(out.dryBio, factor);
    out.greenWeightPrimary = out.dryWeightPrimary * factor;
    out.greenWeightSecondary = out.dryWeightSecondary * factor;

    // (16) cord volume
    double cordConvertor = 90.0;
    if (vco.region == 3 || vco.region == 8 || vco.region == 9) cordConvertor = 79.0;
    out.cordMerchantable = (VtotibSound - VstumpibSound - VtipibSound) / cordConvertor;

    return out;
}