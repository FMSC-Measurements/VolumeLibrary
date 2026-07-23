#include <cmath>
#include <string>
#include <array>
#include <algorithm>
#include <cctype>
#include <stdexcept>
#include "WoodlandBiomass.h"
#include "VolumeCalculators\JenkinsBiomass.h"

// --------------------------
// Helper: safe substring
// --------------------------
inline std::string safe_substr(const std::string& s, std::size_t pos, std::size_t len) {
    if (pos >= s.size()) return std::string();
    return s.substr(pos, std::min(len, s.size() - pos));
}

// --------------------------
// Helper: parse int from substring (like Fortran READ)
// --------------------------
inline int parse_int(const std::string& s) {
    // Trim spaces
    std::size_t start = 0;
    while (start < s.size() && std::isspace(static_cast<unsigned char>(s[start]))) ++start;
    std::size_t end = s.size();
    while (end > start && std::isspace(static_cast<unsigned char>(s[end - 1]))) --end;
    if (start >= end) return 0;
    return std::stoi(s.substr(start, end - start));
}

// --------------------------
// Helper: linear search (Fortran SEARCH emulation)
// Returns 1-based index if found, 0 if not found.
// Sets errflag=1 if not found.
// --------------------------
inline int searchLinear(int last, const int* ary, int spc) {
    for (int i = 0; i < last; ++i) {
        if (ary[i] == spc) {
            return i + 1; // 1-based index
        }
    }
    return 0;
}

// -----------------------------------------------------------------------------
// SUBROUTINE CHO_WDBK_1530(SPN, DRC, HT, CV15, STEMS, SG, BIO3)
// Converts CV at 1.5" top to 3" top and computes biomass (BIO3 = SG * CV3 * 62.4)
// -----------------------------------------------------------------------------
double cv15ToCv3(int SPN, double DRC, double HT, int STEMS, double CV15)
{
    double DSH = 0.0;
    double VR = 1.0;    // volume ratio CV3 / CV15
    double CV3 = 0.0;

    if (SPN == 106) {
        if (STEMS <= 1) {
            DSH = -0.03 + (0.9826 * DRC) - 0.20;
            VR = 1.0 - (0.27612 * std::pow(1.5, 0.67360)) / std::pow(DSH, 0.21114);
        }
        else {
            DSH = -0.30 + (0.9826 * DRC);
            VR = 1.0 - (0.66949 * std::pow(1.5, 0.62895)) / std::pow(DSH, 0.44205);
        }
    }
    else {
        if (SPN == 57 || SPN == 63 || SPN == 65 || SPN == 69) {
            if (STEMS <= 1) {
                DSH = -0.77 + (0.9603 * DRC) + 0.22;
                VR = 1.0 - (0.44761 * std::pow(1.5, 0.65698)) / std::pow(DSH, 0.38835);
            }
            else {
                DSH = -0.77 + (0.9603 * DRC);
                VR = 1.0 - (0.82565 * std::pow(1.5, 0.59404)) / std::pow(DSH, 0.45831);
            }
        }
    }

    if (SPN == 57 || SPN == 63 || SPN == 65 || SPN == 69 || SPN == 106) {
        CV3 = CV15 * VR;
    }
    else if (SPN == 475) {
        CV3 = 0.00356 * std::pow(DRC, 2.920);
    }
    else {
        CV3 = CV15;
    }

    // Compute small tree volume if none provided (1 <= DRC < 5 and not SPN=475)
    if (DRC >= 1.0 && DRC < 5.0 && SPN != 475) {
        CV3 = 0.25 * 0.005454 * DRC * DRC * HT;
    }
    return CV3;
}

// -----------------------------------------------------------------------------
// SUBROUTINE WOODLAND_BIO(SPCD, DRC, THT, STEMS, CV15, DRYBIO, ERRFLG)
// Computes biomass components for woodland species.
// -----------------------------------------------------------------------------
BiomassOutput woodlandBiomass(VolumeCalculationOptions vco, TreeMeasurment tree, double CV15)
{
    BiomassOutput out;
    int SPCD = vco.fiaCode;
    double DRC = (tree.drc > 0.0) ? tree.drc : tree.dbh;
    double THT = tree.totalHeight;
    int STEMS = tree.stems;
    std::array<double, 15> DRYBIO;
    DRYBIO.fill(0.0);

    int ERRFLG = 0;

    static const int SPLIST[13] = { 57,63,65,69,106,310,475,755,756,757,758,810,814 };
    static const double SG[13] = { 0.533,0.517,0.523,0.558,0.496,0.65,0.7,0.69,0.69,0.69,0.69,0.567,0.634 };

    int SPN = SPCD;

    // Map SPCD to consolidated woodland SPN groups (per Fortran logic)
    if ((SPN >= 57 && SPN <= 66) && (SPN != 63 && SPN != 65)) {
        SPN = 57;
    }
    else if (SPN >= 133 && SPN <= 143) {
        SPN = 106;
    }
    else if (SPN == 303 || SPN == 304 || SPN == 363) {
        SPN = 757;
    }
    else if (SPN == 321 || SPN == 322) {
        SPN = 310;
    }
    else if (SPN == 522 || SPN == 7532) {
        SPN = 475;
    }
    else if (SPN >= 523 && SPN <= 758) {
        SPN = 757;
    }
    else if (SPN == 803) {
        SPN = 810;
    }
    else if (SPN >= 829 && SPN <= 867) {
        SPN = 810;
    }

    int LAST = 13;
    int DONE = 0;
    DONE = searchLinear(LAST, SPLIST, SPN);
    if (DONE == 0) {
        ERRFLG = 6; // species not recognized for woodland biomass
        return out;
    }

    const double SPSG = SG[DONE - 1];

    // Initialize outputs to zero
    //for (int i = 0; i < 15; ++i) DRYBIO[i] = 0.0;

    // For woodland species, VOL(1) corresponds to OB volume from ground to 1.5" top
    // DRYBIO(2) := wood & bark (stem) to 1.5" top (lb)
    DRYBIO[1] = CV15 * SPSG * 62.4;
    out.aboveGroundTotal = CV15 * SPSG * 62.4;

    //for regions not 5 or 6, also compute BIO3 and branches
    if (vco.region != 5 && vco.region != 6)
    {
        // Compute BIO to 3" top
        double BIO3 = cv15ToCv3(SPN, DRC, THT, STEMS, CV15) * SPSG * 62.4;
        DRYBIO[5] = BIO3; // DRYBIO(6) in Fortran

        // Branches & foliage
        const double BIO3_M = BIO3 / 2.2046; // lb → kg
        double WT_FOL = 0.0, WT_BRA = 0.0;

        if (SPN >= 300) {
            // lb via base-10 exponentials
            WT_FOL = std::pow(10.0, (-0.5655 + 0.8382 * std::log10(BIO3) - 0.0094 * THT));
            WT_BRA = std::pow(10.0, (0.3036 + 0.7752 * std::log10(BIO3) - 0.0049 * THT));
        }
        else {
            double WT_FOL_M = 0.0;
            if (SPN == 106) {
                if (BIO3_M <= 468.028) {
                    WT_FOL_M = std::exp(1.0254 + 0.559 * std::log(BIO3_M));
                }
                else {
                    WT_FOL_M = std::exp(1.0254 + 0.559 * (1.0 + std::log(468.028) - 468.028 / BIO3_M));
                }
            }
            else {
                if (BIO3_M <= 150.0) {
                    WT_FOL_M = std::exp(1.2867 + 0.649 * std::log(BIO3_M));
                }
                else {
                    WT_FOL_M = std::exp(1.2867 + 0.649 * (1.0 + std::log(150.0) - 150.0 / BIO3_M));
                }
            }
            WT_FOL = WT_FOL_M * 2.2046; // kg → lb
            WT_BRA = WT_FOL * 0.75;
            WT_FOL = WT_FOL * 0.25;
        }
        DRYBIO[11] = WT_BRA; // DRYBIO(12)
        DRYBIO[12] = WT_FOL; // DRYBIO(13)
        DRYBIO[0] = DRYBIO[5] + DRYBIO[11]; // DRYBIO(1) = BIO3 + branches
        out.aboveGroundTotal = DRYBIO[0];
        out.stemWoodTotal = DRYBIO[1];
        out.branches = DRYBIO[11];
        out.stemPrimaryWood = DRYBIO[5];
    }

    //use Jenkins' method to calculate foliage biomass
    BiomassOutput jenkinsBiomass = jenkins(SPCD, DRC);
    out.foliage = jenkinsBiomass.foliage;
    if (!tree.isLive) out.foliage = 0.0;

    return out;
}

