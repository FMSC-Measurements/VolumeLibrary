#pragma once
#include <string>
#include <cmath>


// Case-insensitive first-character check (Fortran VOLEQ(1:1) == 'B' or 'b')
static bool starts_with_b(const std::string& s) {
    if (s.empty()) return false;
    return std::toupper(static_cast<unsigned char>(s[0])) == 'B';
}

// -----------------------------
// BLMTAPEQ: maps VOLEQ to PROFILE and TAPEQU
// -----------------------------
static void getBlmTaperEq(const std::string& VOLEQ, int& PROFILE, int& TAPEQU) {
    //const std::string VOLEQ = normalize_voleq(voleq_in);
    const std::string v010 = VOLEQ.substr(0, 3);  // slice_1based(VOLEQ, 1, 3);
    const std::string v810 = VOLEQ.substr(7, 3);  //slice_1based(VOLEQ, 8, 10);

    if (v810 == "202" && v010 == "B01") {
        TAPEQU = 1; PROFILE = 1;
    }
    else if (v810 == "202" && v010 == "B02") {
        TAPEQU = 2; PROFILE = 2;
    }
    else if (v810 == "202" && v010 == "B03") {
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

//BLM dbhIb for taper equation
static double GetBlmDbhIb(int TAPEQU, double DBHOB)
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