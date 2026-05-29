#pragma once

struct FlewellingShapeParams {
    double r1, r2, r3, r4, r5, a3;
    double rhi1, rhi2, rhc, rhlongi;
};

struct FlewellingTaperCoef {
    double a0, a1, a2, a4, b0, b1, b2, b4, c1, c2, e1, e2;
};

struct Flewelling3PointExtra {
    double zValueExtra, fModMax, fModMaxU, hFirstUp;
};