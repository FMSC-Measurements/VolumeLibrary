#ifndef NSVB_TABLE_STRUCT
#define NSVB_TABLE_STRUCT

#pragma once
#include <array>

// Column order for SPcoef rows:
//
// [SPCD, DIVISION, STDORGCD, equation,
//  a, a0, a1,
//  b, b0, b1, b2,
//  c, c1]
//
// JKcoef: [spgrpcd, equation, a, b, c]

struct spCoefRow {
    int spcd;
    int division;
    int standOrigin;
    int equation;
    double a, a0, a1;
    double b, b0, b1, b2;
    double c, c1;
};

struct jkCoefRow {
    int spgrpcd;
    int equation;
    double a;
    double b;
    double c;
};

constexpr int JKrows = 9;

#endif
