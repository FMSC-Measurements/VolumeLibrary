#pragma once
struct BiomassOutput
{
	double aboveGroundTotal = 0.0;  //no foliage
	double branches = 0.0;
	double foliage = 0.0;
	double stumpWood = 0.0;
	double stumpBark = 0.0;
	double stemWoodTotal = 0.0;
	double stemBarkTotal = 0.0;
	double stemPrimaryWood = 0.0;
	double stemPrimaryBark = 0.0;
	double stemSecondaryWood = 0.0;
	double stemSecondaryBark = 0.0;
	double stemTipWood = 0.0;
	double stemTipBark = 0.0;
	double stemTopAndLimb = 0.0;
};

inline BiomassOutput scale(const BiomassOutput& b, double factor) {
    BiomassOutput r;
    r.aboveGroundTotal = b.aboveGroundTotal * factor;
    r.branches = b.branches * factor;
    r.foliage = b.foliage * factor;
    r.stumpWood = b.stumpWood * factor;
    r.stumpBark = b.stumpBark * factor;
    r.stemWoodTotal = b.stemWoodTotal * factor;
    r.stemBarkTotal = b.stemBarkTotal * factor;
    r.stemPrimaryWood = b.stemPrimaryWood * factor;
    r.stemPrimaryBark = b.stemPrimaryBark * factor;
    r.stemSecondaryWood = b.stemSecondaryWood * factor;
    r.stemSecondaryBark = b.stemSecondaryBark * factor;
    r.stemTipWood = b.stemTipWood * factor;
    r.stemTipBark = b.stemTipBark * factor;
    r.stemTopAndLimb = b.stemTopAndLimb * factor;
    return r;
}
