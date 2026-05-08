#include <string>
#include <array>
#include <cmath>
#include <algorithm>
#include "DirectVolumeCalculator_R610.h"

//R6 DVE for boardfoot volume -----------------------------

// C++ translation of the Fortran subroutine:
//   SUBROUTINE R6VOL2(VOLEQ, DBHOB, HT, VOL, ERRFLAG)
// Inputs:
//   VOLEQ  : volume equation identifier, 10 characters (e.g., "601...263...")
//   DBHOB  : diameter at breast height over bark (inches)
//   HT     : total height (feet)
// Outputs:
//   VOL    : array of length 15; this routine sets VOL(2) and VOL(3) (Fortran 1-based).
//            In C++ we map VOL[1] to VOL(2), VOL[2] to VOL(3).
//   ERRFLAG: set to 0 (as in the Fortran) even when no equation is matched.
TreeOutput R6VOL2(const std::string& VOLEQ, TreeMeasurment tree)
{
    TreeOutput out;
    double DBHOB = tree.dbh;
    double HT = tree.totalHeight;
    // Coefficients COEFF(EQU, j) for EQU = 1..6, j = 1..5.
    // These are taken directly from the Fortran DATA lines.
    static const double COEFF[6][5] = {
        {  0.0000000,  1.9510920, -0.1313649, -0.3926337,  0.0115273 },
        { -38.6517248, 14.7619230, -0.2773275, -1.3283856, 0.0142368 },
        { -50.9378600, 21.4280400, -0.4476645, -2.1493860, 0.0203710 },
        {  0.0000000,  3.9434930, -0.3229676, -0.5601420,  0.0144415 },
        { -50.9602199, 21.9511399, -0.5506570, -2.3294660, 0.0247944 },
        {  8.7000000,  0.0000000,  0.0000000, -0.7600000,  0.0200000 }
    };

    // Safely extract substrings analogous to Fortran's VOLEQ(1:3) and VOLEQ(8:10)
    // Fortran indices are 1-based; C++ substr is 0-based.
    const std::string forest = (VOLEQ.size() >= 3) ? VOLEQ.substr(0, 3) : std::string();
    const std::string spec = (VOLEQ.size() >= 10) ? VOLEQ.substr(7, 3) : std::string();

    // Determine which equation (EQU) applies
    int EQU = 0;
    if (spec == "205" && forest == "601") {
        EQU = 1;
    }
    else if (spec == "263" && forest == "601") {
        EQU = 2;
    }
    else if (spec == "015" && forest == "601") {
        EQU = 3;
    }
    else if (spec == "204" && forest == "602") {
        EQU = 4;
    }
    else if (spec == "015" && forest == "602") {
        EQU = 5;
    }
    else if (spec == "108" && forest == "602") {
        EQU = 6;
    }
    else if (spec == "122") {
        EQU = 7; // Special case equation (independent of forest)
    }
    else {
        // Match not found; Fortran sets ERRFLAG=0 and returns
        out.errflag = 1; // ERRFLAG;
        return out;
    }

    // Compute VOL(2) (Fortran 1-based). We map to VOL[1] in C++.
    double vol2 = 0.0;
    if (EQU == 7) {
        // Special formula for species 122
        // VOL(2) = -2.9815 + (-0.2013*DBHOB^2) + (0.000141*DBHOB^3*HT) + (0.0084*DBHOB^2*HT)
        vol2 = -2.9815
            + (-0.2013 * DBHOB * DBHOB)
            + (0.000141 * DBHOB * DBHOB * DBHOB * HT)
            + (0.0084 * DBHOB * DBHOB * HT);
    }
    else {
        // General coefficient formula:
        // VOL(2) = c1 + c2*DBHOB + c3*HT + c4*DBHOB^2 + c5*DBHOB^2*HT
        const double* c = COEFF[EQU - 1];
        vol2 = c[0]
            + c[1] * DBHOB
            + c[2] * HT
            + c[3] * DBHOB * DBHOB
            + c[4] * DBHOB * DBHOB * HT;
    }

    out.grossBoardFootPrimary = vol2;
    return out;
}

//R10----------------------------------
TreeOutput r10d2h(const std::string& VOLEQ, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput out;
    double DBHOB = tree.dbh;
    double HTTOT = tree.totalHeight;
    double MTOPP = merchRules.minTopDibSaw;
    std::array<double, 15> VOL;
    VOL.fill(0.0);

    int ERRFLAG = 0;

    // Basic validity checks (match Fortran behavior)
    if (DBHOB <= 1.0) { ERRFLAG = 3; out.errflag = ERRFLAG; return out; }
    if (HTTOT <= 0.0) { ERRFLAG = 4; out.errflag = ERRFLAG; return out; }

    // Initialize only variables actually used in the logic below
    double D2H = DBHOB * DBHOB * HTTOT; // DBH^2 * Height
    std::string EQN;

    // Determine equation group based on VOLEQ characters:
    //   If VOLEQ(3:3) is '0', 'O', 'o' => '00'
    //   If VOLEQ(3:3) is '1', 'l', 'i', 'I', 'L' => '01'
    //   Else if VOLEQ(2:3) is "02" => '02'
    //   Else ERRFLAG=1 and return
    char c3 = (VOLEQ.size() >= 3 ? VOLEQ[2] : '\0');
    if (c3 == '0' || c3 == 'O' || c3 == 'o') {
        EQN = "00";
    }
    else if (c3 == '1' || c3 == 'l' || c3 == 'i' || c3 == 'I' || c3 == 'L') {
        EQN = "01";
    }
    else {
        std::string c23 = (VOLEQ.size() >= 3 ? VOLEQ.substr(1, 2) : "");
        if (c23 == "02") {
            EQN = "02";
        }
        else {
            ERRFLAG = 1;
            out.errflag = 1;
            return out;
        }
    }

    // Species string from VOLEQ(8:10)
    const std::string spnStr = (VOLEQ.size() >= 10 ? VOLEQ.substr(7, 3) : "");
    auto str_eq = [&](const char* s) { return spnStr == s; };

    // --- Case 1: 094 / 095 ---
    if (str_eq("094") || str_eq("095")) {
        if (EQN == "00") {
            // A00DVEW094 (Coastal AK and Interior AK south/east of Alaska Range)
            VOL[0] = 0.65559 + 0.00191 * D2H;
            if (DBHOB < 6.0 && DBHOB > 0.0) {
                VOL[0] -= 0.65559 * (1.0 - std::pow(DBHOB / 6.0, 3.0)); // small tree correction
            }
            if (DBHOB > 4.0) {
                VOL[3] = -0.21849 + 0.00189 * D2H;
            }
            if (DBHOB > 6.0) {
                VOL[1] = 0.000136 * std::pow(D2H, 1.40338);   // Scribner
                VOL[9] = 0.00078 * std::pow(D2H, 1.26485);   // XINTT
            }
        }
        else if (EQN == "01") {
            // A01DVEW094 (Interior AK north/west of Alaska Range, NOR-5/NOR-6)
            VOL[0] = -1.1843 + 0.205 * DBHOB + 0.01639 * HTTOT + 0.00187 * D2H;
            if (DBHOB > 4.0) {
                // NOR-5 formula (as adjusted in comments)
                VOL[3] = -2.0555 + (0.2982 * DBHOB) + (0.00181 * D2H);
                // Alternative (NOR-6) was commented out in the Fortran.
            }
            if (DBHOB > 6.0) {
                VOL[1] = 98.7701 + 0.02022 * D2H - 0.77651 * DBHOB * DBHOB - 1.63023 * HTTOT;
                VOL[9] = -67.1116 + 0.013663011 * D2H + (3344.33 / (DBHOB * DBHOB));
            }
        }
        else { // EQN == "02"
            // Malone 2013 statewide AK white spruce (A02DVEW094)
            VOL[0] = std::exp(-6.1352 + 1.8517 * std::log(DBHOB) + 1.069 * std::log(HTTOT));
            if (MTOPP < 3.0) {
                VOL[3] = -0.987 + 0.948 * VOL[0] + 0.0825 * DBHOB + 0.0138 * HTTOT;
            }
            else if (MTOPP < 5.0) {
                VOL[3] = -1.96 + 0.941 * VOL[0] + 0.144 * DBHOB + 0.0116 * HTTOT;
            }
            else {
                VOL[3] = -5.14 + 0.92 * VOL[0] + 0.386 * DBHOB - 0.00095 * HTTOT;
            }
            if (VOL[3] > VOL[0]) VOL[3] = VOL[0]; // merch can't exceed total
        }

        // --- Case 2: 375 / 746 / 920 ---
    }
    else if (str_eq("375") || str_eq("746") || str_eq("920")) {
        if (EQN == "00") {
            // A00DVEW375
            VOL[0] = 0.64456 + 0.00206 * D2H;
            if (DBHOB < 6.0 && DBHOB > 0.0) {
                VOL[0] -= 0.64456 * (1.0 - std::pow(DBHOB / 6.0, 3.0));
            }
            if (DBHOB > 4.0) {
                VOL[3] = -0.7126 + (0.00211 * D2H);
            }
            if (DBHOB > 6.0) {
                VOL[1] = 0.000081 * std::pow(D2H, 1.48459);
                VOL[9] = 0.00043 * std::pow(D2H, 1.34294);
            }
        }
        else { // A01DVEW375
            VOL[0] = -0.01408 * DBHOB * DBHOB + 0.00815 * HTTOT + 0.00227 * D2H;
            if (DBHOB > 4.0) {
                VOL[3] = -1.02411 + (0.0022034075 * D2H);
            }
            if (DBHOB > 6.0) {
                VOL[1] = -27.163 + 0.00995 * D2H;
                VOL[9] = -29.8848 + 0.011913048 * D2H;
            }
        }

        // --- Case 3: 747 ---
    }
    else if (str_eq("747")) {
        if (EQN == "00") {
            // A00DVEW747
            VOL[0] = 0.9864 + 0.00181 * D2H;
            if (DBHOB < 6.0 && DBHOB > 0.0) {
                VOL[0] -= 0.9864 * (1.0 - std::pow(DBHOB / 6.0, 3.0));
            }
            if (DBHOB > 4.0) {
                VOL[3] = -1.39764 + (0.00188 * D2H);
            }
            if (DBHOB > 6.0) {
                VOL[1] = -28.0674 + 0.00937 * D2H;
                VOL[9] = -17.4877 + 0.01119 * D2H;
            }
        }
        else { // A01DVEW747
            VOL[0] = 0.00806 * HTTOT + 0.00175 * D2H;
            if (DBHOB > 4.0) {
                VOL[3] = -0.8722 + (0.001811522 * D2H);
            }
            if (DBHOB > 6.0) {
                VOL[1] = -46.7415 + 0.00956 * D2H;
                VOL[9] = -49.1199 + 0.010941441 * D2H;
            }
        }

        // --- Case 4: 108 / 310 / 351 / 660 (Brackett 1973-based) ---
    }
    else if (str_eq("108") || str_eq("310") || str_eq("351") || str_eq("660")) {
        // Parse species integer for later group checks
        int spn = 0;
        try {
            spn = std::stoi(spnStr);
        }
        catch (...) {
            spn = 0;
        }

        // Coefficients A, B, C per species
        double A = 0.0, B = 0.0, C = 0.0;
        if (str_eq("108")) {
            A = -2.615591; B = 1.847504; C = 1.085772;
        }
        else if (str_eq("351")) {
            A = -2.672775; B = 1.920617; C = 1.074024;
        }
        else { // "310" or "660"
            A = -2.770324; B = 1.885813; C = 1.119043;
        }

        // Basal area (sq ft)
        const double BA = 0.005454154 * DBHOB * DBHOB;
        // Total cubic volume (entire stem incl. stump & top)
        const double CVTS = std::pow(10.0, A) * std::pow(DBHOB, B) * std::pow(HTTOT, C);

        VOL[0] = CVTS; // VOL(1)

        double CV4 = 0.0, CV6 = 0.0, CV8 = 0.0;
        if (DBHOB > 4.0) {
            // Tarif access constants
            const double TATS = 0.912733 /
                ((1.033 * (1.0 + 1.382937 * std::exp(-4.015292 * (DBHOB / 10.0))))
                    * (BA + 0.087266) - 0.174533);
            const double TARIF = CVTS * TATS;
            const double TV4 = (BA - 0.087266) / 0.912733;

            // Cubic feet, stump to 4" top
            CV4 = TARIF * TV4;

            if (DBHOB > 6.0) {
                // Cubic feet, stump to 6" top
                const double R64 = 0.993 - 0.993 * std::pow(0.62, (DBHOB - 6.0));
                CV6 = CV4 * R64;

                // Board foot conversions (Scribner & International 1/4")
                double SV6 = 0.0, IV6 = 0.0, SV8 = 0.0, IV8 = 0.0;
                if (((spn >= 300) && (DBHOB >= 11.0)) || ((spn < 300) && (DBHOB >= 9.0))) {
                    const double B4 = TARIF / 0.912733;

                    // Scribner BF per cubic foot factor
                    const double BCU1 = std::pow(10.0,
                        0.174439
                        + 0.117594 * std::log10(DBHOB) * std::log10(B4)
                        - 8.210585 / (DBHOB * DBHOB)
                        + 0.236693 * std::log10(B4)
                        - 0.00001345 * B4 * B4
                        - 0.00001937 * DBHOB * DBHOB);

                    SV6 = CV6 * BCU1;

                    // International 1/4" BF per cubic foot factor
                    const double BCU2 = -2.904154
                        + 3.466328 * std::log10(DBHOB * TARIF)
                        - 0.02765985 * DBHOB
                        - 0.00008205 * TARIF * TARIF
                        + 11.29598 / (DBHOB * DBHOB);

                    IV6 = CV6 * BCU2;

                    // Cubic feet to 8" top
                    const double R84 = 0.983 - 0.983 * std::pow(0.65, (DBHOB - 8.6));
                    CV8 = CV4 * R84;

                    // BF multipliers to 8" top
                    const double RS86 = 0.99 - 0.58 * std::pow(0.484, (DBHOB - 9.5));
                    SV8 = SV6 * RS86;

                    const double RI86 = 0.99 - 0.55 * std::pow(0.485, (DBHOB - 9.5));
                    IV8 = IV6 * RI86;

                    // Select outputs depending on species group (softwood vs hardwood proxy via spn code)
                    double CVSL = 0.0, SCFT = 0.0, XINTT = 0.0;
                    if (spn >= 300) {
                        CVSL = CV8;  // cubic to 8"
                        SCFT = SV8;  // Scribner to 8"
                        XINTT = IV8; // International to 8"
                    }
                    else {
                        CVSL = CV6;  // cubic to 6"
                        SCFT = SV6;  // Scribner to 6"
                        XINTT = IV6; // International to 6"
                    }

                    // Assign to outputs per flags
                    VOL[3] = CVSL;  // VOL(4)
                    VOL[6] = CV4 - VOL[3];               // VOL(7) tip = CV4 - merch
                    VOL[1] = SCFT;                   // VOL(2) Scribner BF
                    VOL[9] = XINTT;                  // VOL(10) International BF
                }
                else {
                    // If BF conversion conditions not met, still provide cubic outputs
                        // choose 6" vs 8" cubic like above (CVSL)
                    double CVSL = (spn >= 300) ? (CV4 * (0.983 - 0.983 * std::pow(0.65, (DBHOB - 8.6))))
                        : (CV6);
                    VOL[3] = CVSL;
                    VOL[6] = CV4 - VOL[3]; // VOL(7)
                }
            }
            else {
                // DBH <= 6: only CV4 available
                VOL[3] = CV4;    // VOL(4)
                VOL[6] = CV4 - VOL[3];               // VOL(7)
            }
        }

    }
    else {
        // Species code not handled by this routine—leave ERRFLAG=0 and return
        out.errflag = 1;
        return out;
    }

    // Final nonnegativity clamp (match Fortran guards)
    auto clamp_nonneg = [](double& x) { if (x < 0.0) x = 0.0; };
    clamp_nonneg(VOL[1]);  // VOL(2)
    clamp_nonneg(VOL[3]);  // VOL(4)
    clamp_nonneg(VOL[0]);  // VOL(1)
    clamp_nonneg(VOL[9]);  // VOL(10)
    clamp_nonneg(VOL[6]);  // VOL(7)
    out.totalCubicFoot = VOL[0];
    out.grossBoardFootPrimary = VOL[1];
    out.grossCubicFootPrimary = VOL[3];
    out.grossCubicFootSecondary = VOL[6];
    out.grossInternationalBoardFoot = VOL[9];
    return out;
}