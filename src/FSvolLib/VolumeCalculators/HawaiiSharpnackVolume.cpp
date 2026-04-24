#pragma once
#include <array>
#include <string>
#include <vector>
#include <stdexcept>
#include <cmath>
#include <algorithm>
#include "HawaiiSharpnackTaper.h"
#include "HawaiiSharpnackVolume.h"

// Forward declaration: use the R12TAP C++ translation you already have.
// Signature recap (updated to double):
// void R12TAP(const std::string& tapeq,
//             double dbhob, double ht1, double ht2, double tdib,
//             int fclass, int vtype,
//             double& svol);

    inline bool is_valid_species_eqnum(const std::string& eqnum) {
        if (eqnum.size() < 10) return false;
        const std::string tail = eqnum.substr(7, 3); // (8:10)
        const std::string head = eqnum.substr(0, 3); // (1:3)
        if (tail == "301") return true;                     // KOA
        if (tail == "671") return true;                     // OHIA
        if (tail == "510" && (head == "H00" || head == "H01")) return true; // EUC
        return false;
    }

    /**
     * Faithful translation of Fortran subroutine R12VOL.
     *
     * @param eqnum   CHARACTER*10, species/taper code (e.g., "...301", "...671", "...510" with "H00"/"H01" head)
     * @param mtopp   REAL, top diameter inside bark used for segment computations
     * @param ht1prd  REAL, tree height from ground to tip (product height)
     * @param dbhob   REAL, DBH over bark
     * @param httot   REAL, total tree height used for total cubic; modified in-place if < 0.1 (set to ht1prd)
     * @param vol     REAL(15), output volumes (1..15 used). Will be zeroed then filled.
     * @param nologp  REAL, avg number of 8‑ft logs in main stem (output)
     * @param nologs  REAL, avg number of 8‑ft logs in top wood (output)
     * @param fclass  INTEGER, form class (percent)
     * @param cutflg  INTEGER, flag: compute total cubic (1=yes)
     * @param bfpflg  INTEGER, flag: compute board‑foot main stem (1=yes)
     * @param cupflg  INTEGER, flag: compute cubic‑foot main stem (1=yes)
     * @param errflag INTEGER, output error flag: 0=OK, 3=bad DBH, 4=bad HT1PRD, 6=invalid species code
     */

    TreeOutput R12VOL(const std::string& eqnum, VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
    {
        TreeOutput out;
        double mtopp = merchRules.minTopDibSaw;
        if (tree.minTopDibSawOverride > 0.0) mtopp = tree.minTopDibSawOverride;
        double mtops = merchRules.minTopDibNonSaw;
        if (tree.minTopDibNonSawOverride > 0.0) mtops = tree.minTopDibNonSawOverride;
        double ht1prd = tree.merchHeightSaw;
        double ht2prd = tree.merchHeightNonsaw;
        double dbhob = tree.dbh;
        double httot = tree.totalHeight;
        int fclass = tree.formClass;
        int nologp = 0;
        int nologs = 0;

        // --- Early error checks (match Fortran semantics) ---
        if (dbhob <= 1.0) {  // ERRFLAG=3, GOTO 1000
            out.errflag = 3;
            return out; // jump to end (skips tip volume calc as in Fortran)
        }
        if (ht1prd <= 0.0) { // ERRFLAG=4, GOTO 1000
            out.errflag = 4;
            return out;
        }
        if (httot < ht1prd) httot = ht1prd;
        if (ht2prd < ht1prd) {
            ht2prd = ht1prd;
            mtops = mtopp;
        }
        if (fclass == 0) {
            out.errflag = 2;
            return out;
        }
        // Species validation (2018/10/10 YW)
        if (!is_valid_species_eqnum(eqnum)) {
            out.errflag = 6;
            return out; // Fortran: RETURN immediately on invalid species
        }

        // ===========================
        // TOTAL CUBIC VOLUME (VOL(1))
        // ===========================
        // Calculate total cubic volume for every tree
        // As in Fortran: HT2 = HT1PRD; VTYPE=1.
        double ht2 = ht1prd;
        int    vtype = 1;

        // 2018/11/07 YW: Total cubic should use HTTOT (Sharpnack model).
        // If HTTOT < 0.1, set HTTOT = HT1PRD and use TOPD = MTOPP; else TOPD=0.0.
        double topd = 0.0;
        if (httot < 0.1) {
            httot = ht1prd;
            topd = mtopp;
        }

        double tcvol = 0.0;
        tcvol = R12TAP(eqnum, dbhob, httot, ht2, topd, fclass, vtype);
        out.totalCubicFoot = tcvol; // total cubic volume of the tree

        std::vector<LogOutput> logs;
        LogOutput logData;
        nologs = static_cast<int>(ht2prd / 8.15);
        nologp = static_cast<int>(ht1prd / 8.15);
        logs.reserve(nologs);
        double bfTotal = 0.0;
        double cfTotal = 0.0;
        double twTotal = 0.0;

        if (dbhob >= 7.0) {
            for (int i = 1; i <= nologs; ++i) {
                const double ht2 = static_cast<double>(i);
                double lvol = 0.0;
                logData.isSecondary = false;
                logData.length = 8.0;
                logData.logNumber = i;
                logData.product = vco.primaryProduct;

                if (dbhob >= 10) {
                    double bfvol = 0.0;
                    double cfvol = 0.0;
                    if (i <= nologp) {
                        vtype = 2;
                        bfvol = R12TAP(eqnum, dbhob, ht1prd, ht2, mtopp, fclass, vtype);
                        bfTotal += bfvol;
                        vtype = 3;
                        cfvol = R12TAP(eqnum, dbhob, ht1prd, ht2, mtopp, fclass, vtype);
                        cfTotal += cfvol;
                        logData.grossBoardFoot = bfvol;
                        logData.internationalBoardFoot = bfvol;
                        logData.grossCubicFoot = cfvol;
                    }
                    else
                    {
                        vtype = 3;
                        cfvol = R12TAP(eqnum, dbhob, ht2prd, ht2, mtops, fclass, vtype);
                        twTotal += cfvol;
                        logData.isSecondary = true;
                        logData.product = vco.secondaryProduct;
                        logData.grossCubicFoot = cfvol;
                    }
                }
                else
                {
                    vtype = 3;
                    double lvol = 0.0;
                    lvol = R12TAP(eqnum, dbhob, ht2prd, ht2, mtops, fclass, vtype);
                    logData.grossCubicFoot = lvol;
                }
                logs.push_back(logData);
            }
        }

        out.logs = logs;
        out.grossCubicFootPrimary = cfTotal;
        out.grossCubicFootSecondary = twTotal;
        out.grossBoardFootPrimary = bfTotal;
        out.numberOfLogs = nologp;
        return out;
    }

