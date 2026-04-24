#include <array>
#include <string>
#include <cmath>
#include <algorithm>
#include <cctype>
#include "DirectVolumeCalculator_R5.h"

namespace r5dve {

    // Helper: trim and uppercase if needed (VOLEQ often contains padded blanks)
    inline std::string safe_substr_8_10(const std::string& s) {
        if (s.size() < 10) return "";
        return s.substr(7, 3); // Fortran VOLEQ(8:10) is C++ substr(7, 3)
    }

    // Main function matching Fortran semantics
    // Arguments:
    //   voleq : species equation string (CHARACTER*10 in Fortran)
    //   dbhob : diameter at breast height (REAL)
    //   httot : total height (REAL)
    //   mtopp : merchantability top (REAL); logic uses ranges [4,6,8] inches or tip (0)
    //   vol   : outputs array; uses indices 1..15 in Fortran -> vol[0..14] in C++
    //   bfpflg, cupflg : flags (INTEGER)
    //   errflag : output error flag (INTEGER) same as Fortran
    TreeOutput r5dve::R5HARV(const std::string& voleq, TreeMeasurment tree, MerchRules merchRules)
    {
        TreeOutput out;
        double dbhob = tree.dbh;
        double httot = tree.totalHeight;
        double mtopp = merchRules.minTopDibSaw; //tree.minTopDibSawOverride;
        if (tree.minTopDibSawOverride > 0.0) mtopp = tree.minTopDibSawOverride;

        std::array<double, 15> vol;
        int errflag;
    
        // Coefficient tables (Fortran REAL*8 -> double)
        // Indexed 1..15; row 0 unused to simplify mapping.
        static const std::array<std::array<double, 4>, 16> COFA = { {
            {0,0,0,0}, // 0 unused
            {0.0,0.0,0.0,0.0},                        // 1 RED ALDER (handled in special branch)
            {0.0034214162,2.35347,0.69586,0.0},       // 2 BIGLEAF MAPLE (76,95)
            {0.0036795695,2.12635,0.83339,0.0},       // 3 CALIFORNIA BLACK OAK (81)
            {0.0042324071,2.53987,0.50591,0.0},       // 4 BLUE OAK (88)
            {0.0031670596,2.32519,0.74348,0.0},       // 5 CANYON LIVE OAK (84)
            {0.0055212937,2.07202,0.77467,0.0},       // 6 GIANT CHINKAPIN (93)
            {0.0024574847,2.53284,0.60764,0.0},       // 7 COAST LIVE OAK (82 / 96 & 98)
            {0.0041192264,2.14915,0.77843,0.0},       // 8 INTERIOR LIVE OAK (85)
            {0.0016380753,2.05910,1.05293,0.0},       // 9 CALIFORNIA LAUREL (91)
            {0.0025616425,1.99295,1.01532,0.0},       // 10 PACIFIC MADRONE (94)
            {0.0024277027,2.25575,0.87108,0.0},       // 11 OREGON WHITE OAK (86)
            {0.000577497, 2.19576,1.14078,0.0},       // 12 TANOAK (87 / 72 73 75)
            {0.0009684363,2.39565,0.98878,0.0},       // 13 CALIFORNIA WHITE OAK (83)
            {0.0,0.0,0.0,0.0},                        // 14 NOT TO BE USED
            {0.0053866353,2.61268,0.31103,0.0}        // 15 ENGELMANN OAK (79)
        } };

        static const std::array<std::array<double, 4>, 16> COFB = { {
            {0,0,0,0}, // 0 unused
            {0.0,0.0,0.0,0.0},                        // 1 RED ALDER (special branch)
            {0.0004236332,2.10316,1.08584,0.40017},   // 2
            {0.0012478663,2.68099,0.42441,0.28385},   // 3
            {0.0036912408,1.79732,0.838884,0.15958},  // 4
            {0.0006540144,2.24437,0.81358,0.43381},   // 5
            {0.0018985111,2.38285,0.77105,0.0},       // 6
            {0.0006540144,2.24437,0.81358,0.43381},   // 7
            {0.0006540144,2.24437,0.81358,0.43381},   // 8
            {0.0007741517,2.23009,1.037,  0.0},       // 9
            {0.000618153, 1.72635,1.26462,0.37867},   // 10
            {0.0008281647,2.10651,0.91215,0.32652},   // 11
            {0.0002526443,2.30949,1.21069,0.0},       // 12
            {0.0001880044,1.87346,1.62443,0.0},       // 13
            {0.0,0.0,0.0,0.0},                        // 14
            // CHANGED TO USE CALIFORNIA WHITE OAK COFB FOR ENGELMANN OAK (YW 2018/10/30)
            {0.0001880044,1.87346,1.62443,0.0}        // 15
        } };

        static const std::array<std::array<double, 4>, 16> COFC = { {
            {0,0,0,0}, // 0 unused
            {0.0,0.0,0.0,0.0},                        // 1 RED ALDER (special branch)
            {0.0101786350,2.22462,0.57561,0.0},       // 2
            {0.0070538108,1.97437,0.85034,0.0},       // 3
            {0.0125103008,2.33089,0.46100,0.0},       // 4
            {0.0097438611,2.20527,0.61190,0.0},       // 5
            {0.0120372263,2.02232,0.68638,0.0},       // 6
            {0.0065261029,2.31958,0.62528,0.0},       // 7
            {0.0136818837,2.02989,0.63257,0.0},       // 8
            {0.0057821322,1.94553,0.88389,0.0},       // 9
            {0.0067322665,1.96628,0.83458,0.0},       // 10
            {0.0072695058,2.14321,0.74220,0.0},       // 11
            {0.0058870024,1.94165,0.86562,0.0},       // 12
            {0.0042870077,2.33631,0.74872,0.0},       // 13
            {0.0,0.0,0.0,0.0},                        // 14
            {0.0191453191,2.40248,0.28060,0.0}        // 15
        } };

        // GIANT SEQUOIA coefficients
        static const std::array<double, 3> COEFSEQB = { 0.001682608, 1.755956, 1.490641 };
        static const std::array<double, 3> COEFSEQC = { 0.002438339, 1.694874, 1.098957 };

        const double IV = 10.0; // per original Fortran

        // Initialize outputs
        vol.fill(0.0);
        errflag = 0;

        if (dbhob < 1.0) {
            out.errflag = 3;
            return out; // matches Fortran GOTO 999 -> RETURN
        }

        // Species mapping (only when flags indicate, per Fortran)
        int spec = -1;
        const std::string code = safe_substr_8_10(voleq);
        if (code == "060" || code == "064")      spec = 0;  // JUNIPER
        else if (code == "351")                  spec = 1;  // RED ALDER
        else if (code == "312")                  spec = 2;  // BIGLEAF MAPLE
        else if (code == "818")                  spec = 3;  // CALIFORNIA BLACK OAK
        else if (code == "807")                  spec = 4;  // BLUE OAK
        else if (code == "805")                  spec = 5;  // CANYON LIVE OAK
        else if (code == "431")                  spec = 6;  // GIANT CHINKAPIN
        else if (code == "801")                  spec = 7;  // COAST LIVE OAK
        else if (code == "839")                  spec = 8;  // INTERIOR LIVE OAK
        else if (code == "981")                  spec = 9;  // CALIFORNIA LAUREL
        else if (code == "361")                  spec = 10; // PACIFIC MADRONE
        else if (code == "815")                  spec = 11; // OREGON WHITE OAK
        else if (code == "631")                  spec = 12; // TANOAK
        else if (code == "821")                  spec = 13; // CALIFORNIA WHITE OAK
        else if (code == "212")                  spec = 14; // GIANT SEQUOIA
        else if (code == "811")                  spec = 15; // ENGELMANN OAK
        else {
            spec = -1;
            out.errflag = 1;
            return out;
        }

        // Local variables (mirror Fortran names)
        double CV4 = 0, CV6 = 0, CV8 = 0, CVT = 0;
        double CUFTGROS = 0, BDFTGROS = 0, INTLGROS = 0, BALOG = 0, DLOG = 0, TLOG = 0;
        double DE = 0, HE = 0, BA = 0, HLOG = 0, TERM1 = 0;
        double B4 = 0, RS616 = 0, RS616L = 0, RI6 = 0, XINT6 = 0, RI8 = 0, XINT8 = 0, CVTS = 0;
        double TOPTW = 0, TOPC = 0, TOPB = 0;
        double MTOPP = mtopp;
        double F = 0, V = 0, D = 0, RS816 = 0, SV616 = 0, SV816 = 0, R16 = 0, R18 = 0;
        double TARIF = 0;

        // Branches
        if (spec == 0) {
            // JUNIPER SPECIES
            D = dbhob;
            TOPC = MTOPP;
            if (D < 5.0 || httot < 10.0) {
                CVTS = 0.00272708 * D * D * httot;
                V = 0.0;
            }
            else {
                F = 0.307 + 0.00086 * httot - 0.0037 * D * httot / (httot - 4.5);
                BA = 0.005454154 * D * D;
                CVTS = BA * F * httot * std::pow(httot / (httot - 4.5), 2.0);
                // If TOP not equal to 0 default to 4 inch top (per Fortran)
                if (TOPC > 0.0) {
                    V = (CVTS + 3.48) / (1.18052 + 0.32736 * std::exp(-0.1 * D)) - 2.948;
                }
                else {
                    V = CVTS;
                }
            }
            // Fortran: VOL(4) = ANINT(V*10 + 0.5)/10.0
            vol[3] = std::nearbyint(V * 10.0 + 0.5) / 10.0;
            vol[0] = CVTS; // VOL(1)

        }
        else if (spec == 1) {
            // RED ALDER SPECIES
            DE = dbhob;
            HE = httot;
            BA = DE * DE * 0.005454154;

            TERM1 = (1.033 * (1.0 + 1.382937 * std::exp(-4.015292 * (DE / 10.0))))
                * (BA + 0.087266) - 0.174533;

            DLOG = std::log10(DE);
            HLOG = std::log10(HE);

            CVTS = std::pow(10.0, (-2.672775 + 1.920617 * DLOG + 1.074024 * HLOG));
            TARIF = (CVTS * 0.912733) / TERM1;
            if (TARIF <= 0.0) TARIF = 0.01;

            // Cubic foot volumes for tip, 4", 6", 8"
            TOPC = MTOPP;
            CV4 = TARIF * (BA - 0.087266) / 0.912733;
            CV8 = CV4 * (0.983 - 0.983 * std::pow(0.65, (DE - 8.6)));
            CVT = TARIF * (0.9679 - 0.1051 * std::pow(0.5523, (DE - 1.5))) * TERM1 / 0.912733;
            CV6 = CV4 * (0.993 - 0.993 * std::pow(0.62, (DE - 6.0)));

            if (TOPC >= 3 && TOPC < 5)      vol[3] = CV4;
            else if (TOPC >= 5 && TOPC < 7) vol[3] = CV6;
            else if (TOPC >= 7 && TOPC <= 9)vol[3] = CV8;
            else if (TOPC < 3)              vol[3] = CVT;
            vol[6] = CV4 - vol[3];
            vol[0] = CVT; // VOL(1)

            if (vol[3] <= 0.0) vol[3] = 0.0;

            // Scribner board foot (DBHOB >= 7.0)
            if (dbhob >= 7.0) {
                TOPB = MTOPP;
                B4 = TARIF / 0.912733;
                BALOG = std::log10(B4);
                RS616L = 0.174439 + 0.117594 * DLOG * BALOG - 8.210585 / (DE * DE)
                    + 0.236693 * BALOG - 0.00001345 * B4 * B4 - 0.00001937 * DE * DE;
                RS616 = std::pow(10.0, RS616L);
                SV616 = RS616 * CV6;
                RS816 = 0.99 - 0.58 * std::pow(0.484, (DE - 9.5));
                SV816 = RS816 * SV616;

                if (TOPB >= 5 && TOPB < 7) vol[1] = SV616; // VOL(2)
                else if (TOPB >= 7 && TOPB <= 9) vol[1] = SV816;
                else                             vol[1] = 0.0;

                // International 1/4"
                TLOG = std::log10(DE * TARIF);
                R16 = -2.904157 + 3.466328 * TLOG - 0.02765985 * DE - 0.00008025 * TARIF * TARIF
                    + 11.29598 / (DE * DE);
                XINT6 = R16 * CV6;
                R18 = 0.99 - 0.55 * std::pow(0.485, (DE - 9.5));
                XINT8 = R18 * XINT6;

                if (TOPB >= 5 && TOPB < 7) vol[9] = XINT6; // VOL(10)
                else if (TOPB >= 7 && TOPB <= 9) vol[9] = XINT8;
                else                             vol[9] = 0.0;

            }
            // End RED ALDER

        }
        else if (spec == 14) {
            // GIANT SEQUOIA (Pillsbury et al., 1991)
            vol[1] = COEFSEQB[0] * std::pow(dbhob, COEFSEQB[1]) * std::pow(httot, COEFSEQB[2]); // VOL(2)
            vol[3] = COEFSEQC[0] * std::pow(dbhob, COEFSEQC[1]) * std::pow(httot, COEFSEQC[2]); // VOL(4)
            vol[0] = vol[3]; // VOL(1) = tip cubic

        }
        else {
            // MISC HARDWOOD SPECIES
            TOPC = MTOPP;
            D = dbhob;
            BA = D * D * 0.005454154;

            CV4 = COFA[spec][0] * std::pow(dbhob, COFA[spec][1])
                * std::pow(httot, COFA[spec][2]) * std::pow(IV, COFA[spec][3]);

            CV8 = COFB[spec][0] * std::pow(dbhob, COFB[spec][1])
                * std::pow(httot, COFB[spec][2]) * std::pow(IV, COFB[spec][3]);

            if (CV4 > 0.0 && CV8 > 0.0) {
                CV6 = CV4 - ((CV4 - CV8) * 0.4);
            }
            else {
                CV6 = 0.0;
            }

            CVT = COFC[spec][0] * std::pow(dbhob, COFC[spec][1])
                * std::pow(httot, COFC[spec][2]) * std::pow(IV, COFC[spec][3]);

            if (TOPC >= 3 && TOPC < 5)  CUFTGROS = CV4;
            else if (TOPC >= 5 && TOPC < 7)  CUFTGROS = CV6;
            else if (TOPC >= 7 && TOPC <= 9) CUFTGROS = CV8;
            else if (TOPC < 3)               CUFTGROS = CVT;
            else                             CUFTGROS = 0.0;

            vol[3] = CUFTGROS;       // VOL(4)
            vol[6] = CV4 - CUFTGROS; // VOL(7) topwood
            vol[0] = CVT;            // VOL(1) tip cubic

            // Board foot rules for DBHOB >= 5.0
            TOPB = MTOPP;
            if (dbhob >= 5.0) {
                if (dbhob < 11.0) {
                    if (TOPB >= 5 && TOPB < 7) { BDFTGROS = CV6 * 4.0; INTLGROS = CV6 * 5.0; }
                    else if (TOPB >= 7 && TOPB <= 9) { BDFTGROS = CV8 * 4.0; INTLGROS = CV8 * 5.0; }
                    else { BDFTGROS = 0.0; INTLGROS = 0.0; }
                    vol[1] = BDFTGROS; // VOL(2)
                    vol[9] = INTLGROS; // VOL(10)
                }
                else {
                    // Scribner via TARIF
                    TARIF = (CV8 * 0.912733) /
                        ((0.983 - 0.983 * std::pow(0.65, (D - 8.6))) * (BA - 0.087266));
                    if (TARIF <= 0.0) TARIF = 0.01;

                    B4 = TARIF / 0.912733;
                    DLOG = std::log10(D);
                    BALOG = std::log10(B4);
                    RS616L = 0.174439 + 0.117594 * DLOG * BALOG - 8.210585 / (D * D)
                        + 0.236693 * BALOG - 0.00001345 * B4 * B4 - 0.00001937 * D * D;
                    RS616 = std::pow(10.0, RS616L);
                    SV616 = RS616 * CV6;
                    RS816 = 0.99 - 0.58 * std::pow(0.484, (D - 9.5));
                    SV816 = RS816 * SV616;

                    if (TOPB >= 5 && TOPB < 7)  BDFTGROS = SV616;
                    else if (TOPB >= 7 && TOPB <= 9) BDFTGROS = SV816;
                    else                              BDFTGROS = 0.0;

                    vol[1] = BDFTGROS; // VOL(2)

                    // International 1/4"
                    TLOG = std::log10(D * TARIF);
                    RI6 = -2.904154 + 3.466328 * TLOG - 0.02765985 * D
                        - 0.00008025 * TARIF * TARIF + 11.29598 / (D * D);
                    XINT6 = RI6 * CV6;
                    RI8 = 0.99 - 0.55 * std::pow(0.485, (D - 9.5));
                    XINT8 = RI8 * XINT6;

                    if (TOPB >= 5 && TOPB < 7)  INTLGROS = XINT6;
                    else if (TOPB >= 7 && TOPB <= 9) INTLGROS = XINT8;
                    else                              INTLGROS = 0.0;

                    vol[9] = INTLGROS; // VOL(10)
                }
            }
        }

        // Merchantability options: top diameter greater than DBH -> no merch volume
        if (MTOPP > dbhob) {
            vol[1] = 0.0;  // VOL(2)
            vol[9] = 0.0;  // VOL(10)
            vol[3] = 0.0;  // VOL(4)
        }

        if (vol[6] <= 0.0) vol[6] = 0.0;
        out.errflag = errflag;
        out.grossCubicFootPrimary = vol[3];
        out.grossCubicFootSecondary = vol[6];
        out.totalCubicFoot = vol[0];
        out.grossBoardFootPrimary = vol[1] > 0.0 ? vol[1] : vol[9];

        return out;
        // NOTE: stump (VOL(14)) and tip (VOL(15)) volumes moved to voinit in original Fortran.
        // They remain unset here, matching commented-out lines in the source.
    }

} // namespace r5
