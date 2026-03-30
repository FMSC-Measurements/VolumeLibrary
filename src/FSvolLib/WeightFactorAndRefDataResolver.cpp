#include "WeightFactorAndRefData.h"
#include "WeightFactorAndRefDataResolver.h"

WeightFactorAndRefData getSpeciesWtfactorAndRefData(int region, int forst, int fiaSpcd)
{
	WeightFactorAndRefData out;
	RefSpeciesData refSpeciesDataOut;
	double weightFactorSaw = 0.0;
	double weightFactorNonsaw = 0.0;
	double weightFactorDead = 0.0;
	double weightFactorDry = 0.0;
	bool found = false;

	if (region > 0 && region <= 10)
	{
		//int sz = regionalDefaultWtFactor.size();
		for (int i = 0; i < 146; ++i)
		{
			const auto& row = regionalDefaultWtFactor[i];
			if ((static_cast<int>(row[0]) == region && static_cast<int>(row[1]) == forst && static_cast<int>(row[2]) == fiaSpcd) || (static_cast<int>(row[0]) == region && static_cast<int>(row[1]) == 0 && static_cast<int>(row[2]) == fiaSpcd))
			{
				weightFactorSaw = row[3];
				weightFactorNonsaw = row[4];
				weightFactorDead = row[6];
				found = true;
				break;
			}
		}
	}

	//get the green and dry weight factor from ref species data
	refSpeciesDataOut = getRefSpeciesData(fiaSpcd);
	if(weightFactorSaw==0.0)  weightFactorSaw = refSpeciesDataOut.weightFactorGreen;
	weightFactorDry = refSpeciesDataOut.weightFactorDry;
	
	if (weightFactorNonsaw == 0.0) weightFactorNonsaw = weightFactorSaw;
	
	if (weightFactorDead == 0.0)
	{
		switch (region) {
		case 1: weightFactorDead = weightFactorSaw * 0.6749; break;
		case 2: weightFactorDead = weightFactorSaw * 0.6381; break;
		case 4: weightFactorDead = weightFactorSaw * 0.6113; break;
		case 5: weightFactorDead = weightFactorSaw * 0.8254; break;
		case 7: weightFactorDead = weightFactorSaw * 0.7951; break;
		default: weightFactorDead = weightFactorSaw * 0.7036; break;
		}
	}
	
	out.weightFactorSaw = weightFactorSaw;
	out.weightFactorNonsaw = weightFactorNonsaw;
	out.weightFactorDead = weightFactorDead;
	out.weightFactorDry = weightFactorDry;
	out.softHard = refSpeciesDataOut.softHard;
	out.jenkinsSpeciesGroupCD = refSpeciesDataOut.jenkinsSpeciesGroupCD;
	out.WDSG = refSpeciesDataOut.WDSG;
	out.carbonFraction = refSpeciesDataOut.carbonFraction;
	return out;
}

RefSpeciesData getRefSpeciesData(int fiaSpcd)
{
	RefSpeciesData out;
	if (fiaSpcd < 10) 
	{
		out.errorCode = 6;
		return out;
	}

	// Compute search range depending on SPCD > 999
	std::size_t first = 0;
	std::size_t last = sp999Index; 
	if (fiaSpcd > 999)
	{
		first = sp999Index + 1;
		last = sizeof(refSpeciesData) / sizeof(refSpeciesData[0]) - 1;
	}

	int found_index = -1;
	std::size_t lo = first, hi = last;
	while (lo <= hi) {
		std::size_t mid = lo + (hi - lo) / 2;
		const auto& row = refSpeciesData[mid];
		int key = static_cast<int>(row[0]);
		if (key == fiaSpcd) 
		{ 
			found_index = mid; 
			break; 
		}
		if (key < fiaSpcd) { lo = mid + 1; }
		else 
		{ 
			if (mid == 0) break; 
			hi = mid - 1; 
		}
	}

	if (found_index < 0) found_index = sp999Index;
	
	const auto& row = refSpeciesData[found_index];
	out.softHard = static_cast<int>(row[1]);
	out.jenkinsSpeciesGroupCD = static_cast<int>(row[2]);
	out.WDSG = row[3];
	out.weightFactorGreen = row[8];
	out.weightFactorDry = row[9];
	out.carbonFraction = row[11];

	return out;
}