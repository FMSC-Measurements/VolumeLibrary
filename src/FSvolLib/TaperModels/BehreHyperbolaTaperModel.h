#pragma once
#include "..\VolumeEquation.h"
#include "..\Models\MerchRules.h"
#include "TaperModel.h"
#include <array>
#include <cmath>

class BehreHyperbolaTaperModel : public TaperModel
{
private:
    std::string volEqStr;
    double topDibSaw;
    int formClass;
    // -----------------------------
    // Utility helpers
    // -----------------------------

    // Safe substring using Fortran 1-based inclusive indices: s(i:j)
    static std::string slice_1based(const std::string& s, std::size_t i1, std::size_t j1) {
        if (i1 == 0) i1 = 1;
        if (j1 < i1) return "";
        const std::size_t start = i1 - 1;
        const std::size_t len = j1 - i1 + 1;
        if (start >= s.size()) return "";
        return s.substr(start, std::min(len, s.size() - start));
    }

    // Case-insensitive first-character check (Fortran VOLEQ(1:1) == 'B' or 'b')
    static bool starts_with_b(const std::string& s) {
        if (s.empty()) return false;
        return std::toupper(static_cast<unsigned char>(s[0])) == 'B';
    }

    // Trim or pad VOLEQ to at least 10 characters (Fortran CHARACTER*10)
    static std::string normalize_voleq(const std::string& s) {
        std::string t = s;
        if (t.size() < 10) t.resize(10, ' ');
        return t;
    }
// -----------------------------
// Coefficients (from DATA statements)
// -----------------------------

// BLMTHT: 4x10 coefficients for Behr's hyperbola (B0,B1,B2,B3)
    static constexpr std::array<std::array<double, 4>, 10> BLMTHT = { {
            // J=1..10 (Fortran); C++ indices 0..9
            // 01 zone 01
            { 0.6448,   -0.00196,  0.0,        0.0        },
            // 01 zone 02
            { 0.6096,   -0.00196,  0.0,        0.0        },
            // 10, 11
            { 0.31385,   0.0,      0.002985,  -0.00003386 },
            // 13
            { 0.4779,    0.0,      0.0,        0.0        },
            // 14
            { 0.5455,   -0.00196,  0.0,        0.0        },
            // 31 zone 1, 33
            { 0.45648,   0.00289,  0.0,        0.0        },
            // 32, 34, 35
            { 0.6014,    0.0,      0.0,        0.0        },
            // 48 (hemlock) corrected B3=0.00000546
            { 0.54568,   0.0,      0.0,        0.00000546 },
            // 51, 54, 55
            { 0.4606,    0.0,      0.0,        0.0        },
            // ALL OTHER SPECIES
            { 0.6200,    0.0,      0.0,        0.0        }
        } };

// -----------------------------
// BLMTAPEQ: maps VOLEQ to PROFILE and TAPEQU
// -----------------------------
    static void BLMTAPEQ(const std::string& voleq_in, int& PROFILE, int& TAPEQU) {
        const std::string VOLEQ = normalize_voleq(voleq_in);
        const std::string v010 = slice_1based(VOLEQ, 1, 3);
        const std::string v810 = slice_1based(VOLEQ, 8, 10);
        const int ITAPER = 0; // matches Fortran behavior

        if (v810 == "202" && v010 == "B01" && ITAPER > 0 && ITAPER < 4) {
            TAPEQU = ITAPER; PROFILE = 1;
        }
        else if (v810 == "202" && v010 == "B01" && ITAPER == 0) {
            TAPEQU = 1; PROFILE = 1;
        }
        else if (v810 == "202" && v010 == "B02" && ITAPER > 0 && ITAPER < 4) {
            TAPEQU = ITAPER; PROFILE = 2;
        }
        else if (v810 == "202" && v010 == "B02" && ITAPER == 0) {
            TAPEQU = 2; PROFILE = 2;
        }
        else if (v810 == "202" && v010 == "B03" && ITAPER > 0 && ITAPER < 4) {
            TAPEQU = ITAPER; PROFILE = 10;
        }
        else if (v810 == "202" && v010 == "B03" && ITAPER == 0) {
            TAPEQU = 3; PROFILE = 10;
        }
        else if (v810 == "202" && v010 == "B04") {
            TAPEQU = 4; PROFILE = 10;
        }
        else if (v810 == "211") {
            TAPEQU = 5; PROFILE = 10;
        }
        else if (v810 == "202" && v010 == "B05") {
            TAPEQU = 6; PROFILE = 10;
        }
        else if (v810 == "122" && v010 == "B01") {
            TAPEQU = 10; PROFILE = 3;
        }
        else if (v810 == "122" && v010 == "B00") {
            TAPEQU = 11; PROFILE = 3;
        }
        else if (v810 == "116") {
            TAPEQU = 12; PROFILE = 10;
        }
        else if (v810 == "117") {
            TAPEQU = 13; PROFILE = 4;
        }
        else if (v810 == "119") {
            TAPEQU = 14; PROFILE = 5;
        }
        else if (v810 == "108") {
            TAPEQU = 15; PROFILE = 10;
        }
        else if (v810 == "231") {
            TAPEQU = 20; PROFILE = 10;
        }
        else if (v810 == "631") {
            TAPEQU = 21; PROFILE = 10;
        }
        else if (v810 == "351") {
            TAPEQU = 22; PROFILE = 10;
        }
        else if (v810 == "998") {
            TAPEQU = 23; PROFILE = 10;
        }
        else if (v810 == "312") {
            TAPEQU = 24; PROFILE = 10;
        }
        else if (v810 == "361") {
            TAPEQU = 25; PROFILE = 10;
        }
        else if (v810 == "431") {
            TAPEQU = 26; PROFILE = 10;
        }
        else if (v810 == "542") {
            TAPEQU = 27; PROFILE = 10;
        }
        else if (v810 == "747") {
            TAPEQU = 28; PROFILE = 10;
        }
        else if (v810 == "800") {
            TAPEQU = 29; PROFILE = 10;
        }
        else if (v810 == "015" && v010 == "B01") {
            TAPEQU = 30; PROFILE = 6;
        }
        else if (v810 == "015" && (v010 == "B00" || v010 == "B02")) {
            TAPEQU = 31; PROFILE = 10;
        }
        else if (v810 == "021") {
            TAPEQU = 32; PROFILE = 7;
        }
        else if (v810 == "017") {
            TAPEQU = 33; PROFILE = 6;
        }
        else if (v810 == "011") {
            TAPEQU = 34; PROFILE = 7;
        }
        else if (v810 == "022") {
            TAPEQU = 35; PROFILE = 7;
        }
        else if (v810 == "093") {
            TAPEQU = 41; PROFILE = 10;
        }
        else if (v810 == "098") {
            TAPEQU = 42; PROFILE = 10;
        }
        else if (v810 == "260" || v810 == "263") {
            TAPEQU = 48; PROFILE = 8;
        }
        else if (v810 == "081") {
            TAPEQU = 51; PROFILE = 9;
        }
        else if (v810 == "042") {
            TAPEQU = 52; PROFILE = 10;
        }
        else if (v810 == "041") {
            TAPEQU = 53; PROFILE = 10;
        }
        else if (v810 == "242") {
            TAPEQU = 54; PROFILE = 9;
        }
        else if (v810 == "073") {
            TAPEQU = 55; PROFILE = 9;
        }
        else {
            TAPEQU = 56; PROFILE = 10;
        }
    }

    // -----------------------------
    // BLMTAP: computes D2 (Diameter Inside Bark)
    // -----------------------------
    static double BLMTAP(double DBHOB, double HTTOT, double TLH, double HTUP,
        double D17, double TOP, double XLEN, int Profile) 
    {
        // Ensure Profile maps to 0..9 for BLMTHT
        if (Profile < 1 || Profile > 10) {
            // Fallback to "ALL OTHER SPECIES"
            Profile = 10;
        }
        const auto& coeff = BLMTHT[Profile - 1];

        double D2 = 0.0;

        // Height given in FEET (TLH == 0.0)
        if (TLH == 0.0) {
            const double HBUTT = HTTOT - (XLEN + 1.5);
            if (HBUTT <= 0.0) {
                return 0.0;
            }

            const double HTDIB = HTTOT - HTUP;

            const double A = coeff[0]
                + coeff[1] * DBHOB
                + coeff[2] * HTTOT
                + coeff[3] * DBHOB * HTTOT;
            const double B = 1.0 - A;

            const double ratio = HTDIB / HBUTT;
            const double DIBCOR = D17 * (ratio / (A * ratio + B));
            D2 = DIBCOR;

            // (Original comment suggested returning zero for feet mode;
            //  your current code assigns D2 = DIBCOR, so we mirror that.)
        }
        else {
            // Height given in number of LOGS
            if (D17 < 5.0) D17 = 5.0;
            if (D17 < TOP) D17 = TOP;

            if (TLH == 1.0) return D17;
            else if (TLH == 2.0) {
                if (HTUP == 1.0) return D17;
                else return TOP;
            }

            if (TOP == D17)  return D17;

            double RA = 0.0;
            double Tolerance = 1.0;
            const double Tol_Limit = 0.01;

            // Initial estimate
            double A = 0.62;
            double B = 1.0 - A;
            const double C = (TLH - 1.0) * XLEN;
            double H = C / (1.0 - TOP * B / (D17 - A * TOP));
            double HtTot_est = H + 17.8; // retained though not used in the A-formula

            const int iLimit = 20;
            int iCount = 0;

            // Iteration
            while (Tolerance > Tol_Limit && iCount < iLimit) {
                ++iCount;

                // Note: Retains your Fortran formula using actual HTTOT rather than HtTot_est
                A = coeff[0]
                    + coeff[1] * DBHOB
                    + coeff[2] * HTTOT
                    + coeff[3] * DBHOB * HTTOT;
                B = 1.0 - A;

                H = C / (1.0 - TOP * B / (D17 - A * TOP));
                HtTot_est = H + XLEN + 1.5;

                Tolerance = std::fabs(RA - A);
                RA = A;
            }

            const double SMALLH = H - (HTUP - 1.0) * XLEN;
            const double LN = SMALLH / H;
            const double DIBCOR = D17 * LN / (A * LN + B);
            D2 = DIBCOR;

        }

        return D2;
    }

    static double double_bark(int TAPEQU, double DBHOB)
    {
        double DBHIB = 0.0;

        // Douglas-fir (Larsen & Hann, 1985)
        if (TAPEQU == 1 || TAPEQU == 2 || TAPEQU == 3 ||
            TAPEQU == 5 || TAPEQU == 35)
        {
            DBHIB = 0.903563 * std::pow(DBHOB, 0.989388);
        }

        // Ponderosa & Jeffrey Pine (Larsen & Hann, 1985)
        else if (TAPEQU == 11 || TAPEQU == 12)
        {
            DBHIB = 0.809427 * std::pow(DBHOB, 1.016866);
        }

        // Sugar Pine & White Pine
        else if (TAPEQU == 13 || TAPEQU == 14)
        {
            // DBHIB = 0.859045 * DBHOB**1.0, simplified
            DBHIB = 0.859045 * DBHOB;
        }

        // Lodgepole Pine (Spada, 1960)
        else if (TAPEQU == 15)
        {
            DBHIB = DBHOB - (0.3147 + 0.0274 * DBHOB);
        }

        // Pacific Yew & Pacific Madrone
        else if (TAPEQU == 20 || TAPEQU == 25)
        {
            DBHIB = -0.03425 + 0.98155 * DBHOB;
        }

        // Tan Oak (Pillsbury, 1984)
        else if (TAPEQU == 21)
        {
            DBHIB = -4.36852 + 0.95354 * DBHOB + 0.18307 * 4.5;
        }

        // Red Alder, Oregon Myrtle, Big Leaf Maple,
        // Golden Chinquapin, Oregon Ash (Pillsbury, 1984)
        else if (TAPEQU == 22 || TAPEQU == 23 || TAPEQU == 24 ||
            TAPEQU == 26 || TAPEQU == 27)
        {
            // Fortran had a typo: "0.3953 4" → use 0.39534
            DBHIB = 0.39534 + 0.90182 * DBHOB;
        }

        // Black Cottonwood & Oaks (Pillsbury, 1984)
        else if (TAPEQU == 28 || TAPEQU == 29)
        {
            DBHIB = -0.78034 + 0.95956 * DBHOB;
        }

        // White & Grand Fir (Larsen & Hann, 1985)
        else if (TAPEQU == 31 || TAPEQU == 33)
        {
            DBHIB = 0.904973 * DBHOB;
        }

        // Red Fir & Pacific Silver Fir (Dolph, 1989)
        else if (TAPEQU == 32 || TAPEQU == 34)
        {
            DBHIB = 0.86951 * std::pow(DBHOB, 1.00983);
        }

        // Engelmann & Sitka Spruce (Spada, 1960)
        else if (TAPEQU == 41 || TAPEQU == 42)
        {
            DBHIB = DBHOB - (0.2113 + 0.0445 * DBHOB);
        }

        // Hemlock & Misc (Finch, 1948)
        else if (TAPEQU == 48 || TAPEQU == 56)
        {
            DBHIB = DBHOB / 1.071;
        }

        // Alaska Yellow Cedar & Western Red Cedar (Finch, 1948)
        else if (TAPEQU == 52 || TAPEQU == 54)
        {
            DBHIB = DBHOB / 1.053;
        }

        // Incense Cedar & Port Orford Cedar (Larsen & Hann, 1985)
        else if (TAPEQU == 51 || TAPEQU == 53)
        {
            DBHIB = 0.837291 * DBHOB;
        }

        // Western Larch
        else if (TAPEQU == 55)
        {
            DBHIB = DBHOB - (0.1231 + 0.1306 * DBHOB);
        }

        return DBHIB;
    }
// -----------------------------
// BEHTAP: wrapper calculating D17 and calling BLMTAP or simplified flow
// -----------------------------
    static double BEHtaper(const std::string& VOLEQ_in,
        double DBHOB, double HTTOT, double TLH, double HTUP,
        int FCLASS, double TOP)
    {
        const std::string VOLEQ = normalize_voleq(VOLEQ_in);

        double XLEN = 16.3;
        double D2 = 0.0;
        double D17 = (DBHOB * static_cast<double>(FCLASS)) / 100.0;

        if (starts_with_b(VOLEQ)) {
            D17 = std::round(D17); // ANINT in Fortran

            int PROFILE = 10;
            int TAPEQU = 56;
            BLMTAPEQ(VOLEQ, PROFILE, TAPEQU);
            
            if (HTUP == 4.5) D2 = double_bark(TAPEQU, DBHOB);
            else D2 = BLMTAP(DBHOB, HTTOT, TLH, HTUP, D17, TOP, XLEN, PROFILE);
        }
        else {
            // Set XLEN by VOLEQ prefix
            if (slice_1based(VOLEQ, 1, 3) == "632") XLEN = 32.6;
            else XLEN = 16.3;

            const double A = 0.62;

            if (HTTOT > 0.0) {
                // Height in FEET
                const double H1 = HTTOT - XLEN - 1.0;
                if (H1 <= 0.0) return 0.0;

                const double HX = HTTOT - HTUP;
                const double HR = HX / H1;
                const double DR = HR / (0.62 * HR + 0.38);
                D2 = D17 * DR;
            }
            else {
                // Height in number of LOGS
                const double T = TOP / D17;
                const double AT = A / (1.0 - A * T);
                const double BT = (1.0 / (1.0 - T)) - AT;
                const double H1 = (TLH - 1.0) * XLEN - 1.0;
                const double HX = TLH * XLEN - HTUP;
                const double HR = HX / H1;
                const double DR = T + (HR / (AT * HR + BT));
                D2 = D17 * DR;
            }
        }

        return D2;
    }
public:
    BehreHyperbolaTaperModel(VolumeEquation volumeEquation);

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override; // {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter) override;

};