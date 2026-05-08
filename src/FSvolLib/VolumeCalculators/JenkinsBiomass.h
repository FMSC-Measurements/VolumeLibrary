#pragma once
#include "..\Models\BiomassOutput.h"

struct StumpVolume {
	double woodVol = 0.0;
	double barkVol = 0.0;
};

StumpVolume raileVol(int spcd, double dbh, double stumpHeight);
BiomassOutput jenkins(int spcd, double dbh);
