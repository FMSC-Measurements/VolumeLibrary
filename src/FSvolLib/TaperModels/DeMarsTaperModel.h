#pragma once
#include "..\VolumeEquation.h"
#include "TaperModel.h"
#include <cmath>
#include <string>
#include <array>

class DeMarsTaperModel : public TaperModel
{
private:
    const int fiaCode_;
    double topDibSaw = 6.0;
    double totalHeight = 0.0;

    double R10TAP(double DBHOB, double HTTOT, double HTUP)
    {
        double D2 = 0;
        // Local variables
        double RH, DSI, RH32, BK, RH40;
        double H = HTTOT;
        double D = DBHOB;
        
        // Species codes
        std::string ISP;
        if (fiaCode_ == 42) ISP = "AC";   // Alaska cedar
        else if (fiaCode_ == 242) ISP = "RC";   // Western redcedar
        else if (fiaCode_ == 98) ISP = "SS";   // Spruce–hemlock
        else if (fiaCode_ == 351) ISP = "RA";   // Red alder

        // --- Helper functions (direct translations) ---

        auto DD2MI = [&](double rh, double rh32, double d, double h){
            return pow(rh, 1.5)
                + ((-0.0052554 * h + 0.000034947 * h * h + 0.104477 * h / d)
                   * (pow(rh,1.5) - pow(rh,3.0)))
                + ((7.76807 / (d * d) - 0.0000094852 * h * h - 0.011351 * h / d)
                   * (pow(rh,1.5) - pow(rh32, 32.0)));
            };

        auto BB = [&](double d, double h)
            {
                return 0.8467 + 0.0009144 * d + 0.0003568 * h;
            };

        auto DVR = [&](double rh, double rh32, double h, double dbhob){
            return pow(rh,1.5)
                + ((pow(rh,1.5) - pow(rh,3.0))
                   * (5.17703194 / (dbhob * dbhob)
                      - 0.12516819 * dbhob + 0.02537037 * h
                      - 0.00004193 * h * h + 0.00155481 * dbhob * dbhob))
                + ((pow(rh,1.5) - pow(rh32,32.0))
                   * (-0.00002070 * h * h + 0.24125235 / (dbhob * dbhob)));
            };

        auto BKWR = [&](double dbhob, double h){
            return 0.86031485 + 0.00059638 * h - 0.18335961 / dbhob;
            };

        auto DVA = [&](double rh, double rh32, double h, double dbhob)
            {
                return pow(rh,1.5)
                    + ((pow(rh,1.5) - pow(rh,3.0))
                       * (-0.02834001 * dbhob + 0.00007123 * h * h
                          + 0.06709114 * h / dbhob))
                    + ((pow(rh,1.5) - pow(rh32,32.0))
                       * (0.00282021 * dbhob - 0.00002277 * h * h
                          + 1.06064717 / (dbhob * dbhob)
                          - 0.00528349 * h / dbhob));
            };

        auto BKAC = [&](double dbhob, double h){

            return 0.95866817 + 0.00064402 * dbhob - 3.1299972 / h;
            };

        auto DVREDA = [&](double rh, double rh32, double rh40,
        double h, double dbhob)
        {
            return 0.91274 * pow(rh,1.5)
                - 1.9758 * (pow(rh,1.5) - pow(rh,3.0))
                  * (dbhob * pow(10.0, -2))
                + 8.2375 * (pow(rh,1.5) - pow(rh,3.0))
                  * h * pow(10.0,-3)
                - 4.964 * (pow(rh,1.5) - pow(rh32,32.0))
                  * h * dbhob * pow(10.0,-5)
                + 3.773 * (pow(rh,1.5) - pow(rh32,32.0))
                  * sqrt(h) * pow(10.0,-3)
                - 7.417 * (pow(rh,1.5) - pow(rh40,40.0))
                  * (h * h) * pow(10.0,-6);
        };

        // --- Begin main logic ---

        // Alaska cedar
        if (ISP == "AC" && DBHOB < 38.01) {

            RH = (H - HTUP) / (H - 4.5);

            if (RH <= 0.0) {
                D2 = 0.0;
                return D2;
            }

            RH32 = (RH < 0.078 ? 0.078 : RH);
            DSI = DVA(RH, RH32, H, DBHOB);
            if (DSI < 0.0) DSI = 0.0;

            double BKAYC = BKAC(DBHOB, H);
            D2 = sqrt(DSI * BKAYC) * DBHOB;
        }

        // Western redcedar
        else if (ISP == "RC" && DBHOB < 56.01) {

            RH = (H - HTUP) / (H - 4.5);

            if (RH <= 0.0) {
                D2 = 0.0;
                return D2;
            }

            RH32 = (RH < 0.078 ? 0.078 : RH);
            DSI = DVR(RH, RH32, H, DBHOB);
            if (DSI < 0) DSI = 0;

            double BKWRC = BKWR(DBHOB, H);
            D2 = sqrt(DSI * BKWRC) * DBHOB;
        }
    
        // Spruce–Hemlock and Red Alder
        else
        {
            BK = (ISP == "RA" ? 0.0 : BB(D, H));

            RH = (H - HTUP) / (H - 4.5);

            if (RH <= 0.0) {
                D2 = 0.0;
                return D2;
            }

            if (RH < 0.078) {
                RH32 = 0.078;
                RH40 = 0.15;
            }
            else if (RH < 0.15) {
                RH32 = RH;
                RH40 = 0.15;
            }
            else {
                RH32 = RH;
                RH40 = RH;
            }

            if (ISP == "RA") {
                D2 = DVREDA(RH, RH32, RH40, H, D);
                if (D2 < 0.0) D2 = 0.0;
                D2 = sqrt(D2) * D;
                return D2;
            }
            else
            {
                // Spruce–hemlock
                D2 = DD2MI(RH, RH32, D, H);
                if (D2 < 0.0) D2 = 0.0;
                D2 = sqrt(D2 * BK) * D;
            }
        }
        return D2;
    }

public:
    DeMarsTaperModel(VolumeEquation volumeEquation)
        : TaperModel(), fiaCode_(volumeEquation.fiaCode)
    {}

    void InitializeOnTree(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override; // {/* do nothing */ };

    double GetDiameterAtHeight(TreeMeasurment tree, double height) override;

    double GetHeightAtDiameter(TreeMeasurment tree, double diameter, bool useDob = false) override;

    StemVolume GetStemCubicVol(TreeMeasurment tree, MerchRules merchRules, VolumeCalculationOptions vco) override { return { 0.0,0.0,0.0,0.0, false, false }; };
};