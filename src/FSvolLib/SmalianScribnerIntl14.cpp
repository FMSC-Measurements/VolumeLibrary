#include "SmalianScribnerIntl14.h"
#include <array>
#include <cctype>   // std::toupper
#include <cmath>    // std::trunc, std::floor
#include <stdexcept>
#include <algorithm>


double smallian(double diameterLarge, double diameterSmall, double logLength)
{
	return 0.002727 * (std::pow(diameterLarge, 2.0) + std::pow(diameterSmall, 2.0)) * logLength;
}


// Scribner Decimal C volume (units of 10 board feet if COR == 'Y')
double scribner(double DIA, double LEN, char COR)
{
    // ---- FACTOR table: 1-based indexing (index 1..132) ----
    static const std::array<double, 133> FACTOR = {
        // index 0 unused to mimic Fortran 1-based arrays
        0.0,
        // 1..66
        0.000,0.143,0.390,0.676,
        1.070,1.160,1.400,1.501,2.084,3.126,3.749,4.900,
        6.043,7.140,8.880,10.000,11.528,13.290,14.990,
        17.499,18.990,20.880,23.510,25.218,28.677,31.249,
        34.220,36.376,38.040,41.060,44.376,45.975,48.990,
        50.000,54.688,57.660,64.319,66.730,70.000,75.240,
        79.480,83.910,87.190,92.501,94.990,99.075,103.501,
        107.970,112.292,116.990,121.650,126.525,131.510,
        136.510,141.610,146.912,152.210,157.710,163.288,
        168.990,174.850,180.749,186.623,193.170,199.120,
        205.685,
        // 67..132
        211.810,218.501,225.685,
        232.499,239.317,246.615,254.040,261.525,269.040,
        276.630,284.260,292.501,300.655,308.970,317.360,
        325.790,334.217,343.290,350.785,359.120,368.380,
        376.610,385.135,393.380,402.499,410.834,419.166,
        428.380,437.499,446.565,455.010,464.150,473.430,
        482.490,491.700,501.700,511.700,521.700,531.700,
        541.700,552.499,562.501,573.350,583.350,594.150,
        604.170,615.010,625.890,636.660,648.380,660.000,
        671.700,683.330,695.011,
        // 121..126 (for LEN 16..31 and DIA 6..11)
        1.249,1.608,1.854,2.410,3.542,4.167,
        // 127..132 (for LEN 32..40 and DIA 6..11)
        1.570,1.800,2.200,2.900,3.815,4.499
    };

    // ---- EXCEPT table: 1-based indexing (index 1..149) ----
    static const std::array<double, 150> EXCEPT = {
        0.0, // index 0 unused
        // 1..77
        40420.,40460.,40510.,
        40520.,40530.,40540.,40550.,40600.,40630.,
        40650.,40720.,40730.,40740.,40750.,40760.,
        40800.,40860.,40880.,40930.,40960.,40980.,
        41010.,41020.,41030.,41040.,41050.,41060.,
        41110.,41120.,41150.,41180.,50100.,50191.,
        50211.,50410.,50430.,50460.,50590.,50640.,
        50650.,50740.,50750.,50810.,50840.,50860.,
        50890.,50900.,50930.,50950.,50970.,50990.,
        51090.,51100.,51130.,51190.,51200.,60201.,
        60211.,60821.,60920.,61080.,61120.,61140.,
        70091.,70110.,70611.,70710.,70791.,80241.,
        80291.,80381.,80640.,80660.,80671.,80690.,
        80711.,80771.,
        // 78..149
        80790.,80831.,90291.,
        90431.,90511.,90611.,90741.,100091.,100230.,
        100711.,100740.,100771.,100831.,100960.,
        101071.,110091.,110250.,110581.,110611.,
        110641.,110731.,120100.,130060.,130470.,
        130521.,130611.,130661.,130691.,130770.,
        130990.,140060.,140440.,140800.,150060.,
        150280.,150451.,150511.,150611.,150741.,
        150801.,170461.,170611.,170641.,170751.,
        170801.,180440.,180701.,180710.,180770.,
        180811.,180830.,180911.,180931.,180951.,
        180981.,181001.,181071.,181111.,190080.,
        190090.,190131.,200080.,200090.,200621.,
        200641.,200661.,200670.,200691.,200710.,
        200770.,200791.,
        999990.
    };

    // Normalize COR to uppercase
    COR = static_cast<char>(std::toupper(static_cast<unsigned char>(COR)));

    // Input validation & edge handling (mirrors Fortran behavior)
    if (DIA < 1.0) {
        return 0.0;
    }
    if (DIA > 120.0) {
        DIA = 120.0;
    }

    // Choose factor index (Q9), mimicking Fortran integer cast & rules
    int Q9 = static_cast<int>(DIA);  // truncation toward zero (DIA is positive)
    if (DIA > 5.0 && DIA <= 11.0) {
        if (LEN > 15.0 && LEN < 32.0) {
            Q9 += 115;   // 121..126
        }
        else if (LEN > 31.0 && LEN < 41.0) {
            Q9 += 121;   // 127..132
        }
    }

    if (Q9 < 1 || Q9 > 132) {
        // Defensive check: should not happen with inputs and logic above
        return 0.0;
    }

    const double VOLFAC = FACTOR[Q9];

    // Compute factor volume with AINT semantics
    double VOL = 0.0;
    if (COR == 'Y') {
        // AINT((LEN*VOLFAC+5)/10): truncate toward zero
        VOL = std::trunc((LEN * VOLFAC + 5.0) / 10.0);
    }
    else if (COR == 'N') {
        // AINT(LEN*VOLFAC + 0.5)
        VOL = std::trunc(LEN * VOLFAC + 0.5);
    }
    else {
        // Fortran just printed an error and left VOL possibly uninitialized.
        // Be safer here and return 0.0. If you want exact Fortran behavior, remove this.
        return 0.0;
    }

    // Apply exceptions only if COR == 'Y'
    if (COR == 'Y') {
        int ILOW = 1;
        int IHIGH = 149;

        // ANUM = LEN*1000 + DIA (Fortran REAL); compare against integer COMPAR
        const double ANUM = LEN * 1000.0 + DIA;

        // Binary search like the Fortran loop with GOTO 100
        while (true) {
            const int ISCRPT = (IHIGH + ILOW) / 2;

            // COMPAR = AINT(EXCEPT(ISCRPT)/10) -> truncate -> integer
            const int COMPAR = static_cast<int>(std::trunc(EXCEPT[ISCRPT] / 10.0));

            if (ANUM == static_cast<double>(COMPAR)) {
                // Determine odd/even: XXX = (EXCEPT/2) - INT(EXCEPT/2)
                const double half = EXCEPT[ISCRPT] / 2.0;
                const double XXX = half - std::floor(half); // fractional part
                if (XXX > 0.0) {
                    // odd -> add 1
                    VOL += 1.0;
                }
                else {
                    // even -> subtract 1
                    VOL -= 1.0;
                }
                break;
            }
            else if (ILOW != IHIGH - 1) {
                if (ANUM > static_cast<double>(COMPAR)) {
                    ILOW = ISCRPT;
                }
                else {
                    IHIGH = ISCRPT;
                }
                continue;
            }
            // No match or narrowed to 1-apart -> done
            break;
        }
    }
    if (COR == 'Y') VOL *= 10.0;
    return VOL;
}


// Compute International 1/4 board foot volume for a log.
// DIB: small-end diameter inside bark (inches)
// LENGTH: log length (feet)
// Returns BFINT (board feet)
double intl14(double DIB, double LENGTH)
{
    // If diameter inside bark is less than 4, no volume
    if (DIB < 4.0) {
        return 0.0;
    }

    double LOGVOL = 0.0;

    // Number of full 4' segments (INT -> trunc toward zero)
    int LOGSEG = static_cast<int>(LENGTH / 4.0);

    // Leftover fraction of a 4' segment (0.0, 0.25, 0.5, 0.75 typically)
    double FF = LENGTH / 4.0 - static_cast<double>(LOGSEG);

    // For each 4' segment, compute small end diameter assuming 1/2" taper per segment
    // Fortran: SEDIAM = DIB + (LOGSEG - J)/2.0
    for (int J = 1; J <= LOGSEG; ++J) {
        double SEDIAM = DIB + (static_cast<double>(LOGSEG - J) / 2.0);
        double SEGVOL = (0.22 * SEDIAM * SEDIAM - 0.71 * SEDIAM) * 0.905;
        LOGVOL += SEGVOL;
    }

    // If there's a leftover fraction, take the top 4' piece volume and scale by fraction FF
    if (FF > 0.0) {
        double SEGVOL = FF * (0.22 * DIB * DIB - 0.71 * DIB) * 0.905;
        LOGVOL += SEGVOL;
    }

    // Rounding logic to nearest 5 board feet per original rules
    if (LOGVOL < 7.5) {
        LOGVOL = 5.0;
    }
    else {
        // Largest multiple of 10 not exceeding LOGVOL (INT -> trunc toward zero)
        int IRNDVOL = static_cast<int>(LOGVOL / 10.0);

        // Remaining (ones & tenths) as integer hundredths out of 10 BF unit
        int JJJ = static_cast<int>(((LOGVOL / 10.0) - IRNDVOL) * 100.0);

        if (JJJ < 25) {
            // < 2.5 BF remainder -> round down to 10
            LOGVOL = static_cast<double>(IRNDVOL * 10);
        }
        else if (JJJ >= 75) {
            // >= 7.5 BF remainder -> round up to next 10
            LOGVOL = static_cast<double>((IRNDVOL + 1) * 10);
        }
        else {
            // Else round to 5
            LOGVOL = static_cast<double>(IRNDVOL * 10 + 5);
        }
    }

    return LOGVOL;
}
