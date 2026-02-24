#include "..\SmalianScribnerIntl14.h"
#include "ProfileVolumeCalculator.h"



TreeOutput ProfileVolumeCalculator::CalculateVolume(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
	TreeOutput result;

	// initialize model on tree - i.e fwelling models
    //check override parameters for stump, sawTopDib, nonsawTopDib
    if (tree.stumpHeightOverride > 0.0) merchRules.stumpHeight = tree.stumpHeightOverride;

    if (tree.minTopDibSawOverride > 0.0) merchRules.minTopDibSaw = tree.minTopDibSawOverride;

    if (tree.minTopDibNonSawOverride > 0.0) merchRules.minTopDibNonSaw = tree.minTopDibNonSawOverride;

    //region 7 (BLM) saw top diameter
    if (vco.region == 7) merchRules.minTopDibSaw = tree.dbh * 0.184 + 2.24;
	
    // Segment Logs
    std::vector<LogOutput> logs = SegmentLogs(vco, tree, merchRules);
    result.logs = logs;

	// calculate volume for all logs
    double grossCubicFootPrimary = 0.0;
    double grossCubicFootSecondary = 0.0;
    double grossBoardFootPrimary = 0.0;
    double grossBoardFootSecondary = 0.0;
    double grossIntl14Primary = 0.0;
    double grossIntl14Secondary = 0.0;
    int numberOfLogs = 0;

    //sum log volumes
    for (const auto& item : logs) {
        if (item.isSecondary)
        {
            grossCubicFootSecondary += item.grossCubicFoot;
            grossBoardFootSecondary += item.grossBoardFoot;
            grossIntl14Secondary += item.internationalBoardFoot;
        }
        else
        {
            grossCubicFootPrimary += item.grossCubicFoot;
            grossBoardFootPrimary += item.grossBoardFoot;
            grossIntl14Primary += item.internationalBoardFoot;
            ++numberOfLogs;
        }
    }

    //calculate tip volume above the last log
    double tipVolume = 0.0;
    if (!logs.empty()) {
        size_t topIndex = logs.size() - 1; // Last element index
        if (!logs[topIndex].length > 0.0) topIndex -= 1;
        double merchHeight = logs[topIndex].heightToLargeEndDiameter + logs[topIndex].length;
        double tipLength = tree.totalHeight - merchHeight;
        tipVolume = smallian(logs[topIndex].smallEndDiameterScaled, 0.0, tipLength);
    }

    result.grossBoardFootPrimary = grossBoardFootPrimary;
    result.grossBoardFootSecondary = grossBoardFootSecondary;
    result.grossCubicFootPrimary = grossCubicFootPrimary;
    result.grossCubicFootSecondary = grossCubicFootSecondary;
    result.numberOfLogs = numberOfLogs;

	// calcualte log weights using weight factor

	// sum volume from logs into tree volume


	// calculate total cubic and cords
    //using 4' piece length and no trim for total cibic
    MerchRules merchRulesTotal;
    merchRulesTotal = merchRules;
    merchRulesTotal.maxLogLength = 4.0;
    merchRulesTotal.minLogLength = 0.0;
    merchRulesTotal.trim = 0.0;
    merchRulesTotal.minTopDibSaw = merchRulesTotal.minTopDibNonSaw;
    merchRulesTotal.segmentationOption = 23;
    merchRulesTotal.evenOdd = 0; //not round to foot
    bool cubicOnly = true;
    
    std::vector<LogOutput> totalCubicFoot = SegmentLogs(vco, tree, merchRules, cubicOnly);
    
    //calculate stump volume
    double stumpDib = totalCubicFoot[0].largeEndDiameterScaled; //for cubicOnly, this is the actual Dib
    double stumpVolume = smallian(stumpDib, stumpDib, merchRulesTotal.stumpHeight);
    
    //calculate volume from stump to nonSawTop and cord volume
    double merchCubic = 0.0;
    for (const auto& item : totalCubicFoot)
    {
        merchCubic += item.grossCubicFoot;
    }
    //calculate cord volume using a factor, each region has its own factor
    double cordVolume = std::round((merchCubic / 90.0) * 10.0) / 10.0;

    //calculate tip volume for tree total cubic volume
    tipVolume = 0.0;
    if (!totalCubicFoot.empty()) {
        size_t topIndex = totalCubicFoot.size() - 1; // Last element index
        double merchHeight = totalCubicFoot[topIndex].heightToLargeEndDiameter + totalCubicFoot[topIndex].length;
        double tipLength = tree.totalHeight - merchHeight;
        tipVolume = smallian(merchRulesTotal.minTopDibNonSaw, 0.0, tipLength);
    }

    double totalCubicVolume = stumpVolume + merchCubic + tipVolume;
    
	// return tree 
	return result;

}

std::vector<double> ProfileVolumeCalculator::getLogs(double merchLength, MerchRules merchRules, int &numseg)
{
	std::vector<double>loglen;

    double lmerch{ merchLength };
    double maxlen{ merchRules.maxLogLength };
    double minlen{ merchRules.minLogLength };
    double trim{ merchRules.trim };
    int opt{ merchRules.segmentationOption };
    int evod{ merchRules.evenOdd };


    if (lmerch <= 0.0 || maxlen <= 0.0) {
        return loglen;
    }

    double effectiveTrim = std::max(trim, 0.0);
    double segmentLen = maxlen + effectiveTrim;

    numseg = static_cast<int>(lmerch / segmentLen);
    double leftov = lmerch - segmentLen * static_cast<double>(numseg);

    if (!(numseg > 0 || leftov >= minlen)) {
        return loglen;
    }

    if (opt < 20) {
        if (leftov >= (trim + 0.5)) ++numseg;
    }
    else if (opt == 21 || opt == 22) {
        if (evod == 1) {
            if (leftov >= (trim + 0.5)) ++numseg;
        }
        else if (evod == 2) {
            if (leftov >= (trim + 1.0)) ++numseg;
        }
    }
    else if (opt == 23) {
        if (leftov >= (trim + minlen)) ++numseg;
    }
    else if (opt == 24) {
        if (leftov >= (segmentLen / 4.0)) ++numseg;
    }

    // If there are no segments, set merchantable length to zero and return.
    if (numseg == 0) {
        lmerch = 0.0;
        return loglen;
    }

    loglen.reserve(numseg);

    // Remove trim from merchantable length.
    lmerch = lmerch - (static_cast<double>(numseg) * trim);

    // Round LMERCH to nearest foot (EVOD==1) or nearest even foot (EVOD==2/else).
    if (evod == 1) {
        // Nearest foot: INT(LMERCH + 0.5)
        lmerch = static_cast<double>(static_cast<int>(lmerch + 0.5));
    }
    else if (evod == 2) {
        // Nearest even foot: INT((LMERCH + 1.0)/2.0) * 2.0
        int evenFeet = static_cast<int>((lmerch + 1.0) / 2.0) * 2;
        lmerch = static_cast<double>(evenFeet);
    }

    // Because of rounding, LMERCH might exceed the sum of maximum segment lengths.
    // Force LMERCH to be <= NUMSEG * MAXLEN.
    if (lmerch > static_cast<double>(numseg) * maxlen) {
        lmerch = static_cast<double>(numseg) * maxlen;
    }

    // ONE-LOG TREE CASE
    if (numseg == 1) {
        if (opt == 24) {
            // Option 24: rounds to half log with thresholds
            if (lmerch < (maxlen * 0.25)) {
                loglen[0] = 0.0;
            }
            else if (lmerch >= (maxlen * 0.25) && lmerch <= (maxlen * 0.75)) {
                loglen[0] = maxlen / 2.0;
            }
            else {
                loglen[0] = maxlen;
            }
        }
        else if (lmerch >= minlen) {
            if (lmerch > maxlen) lmerch = maxlen;
            loglen[0] = lmerch;
        }
        else {
            loglen[0] = 0.0;
        }
        return loglen;
    }

    // MORE THAN ONE LOG
    if (opt < 20) {
        // Deal with all segments - odd lengths OK

        // AVLEN is integer average length: INT(LMERCH / NUMSEG)
        int avlen = static_cast<int>(lmerch / static_cast<double>(numseg));
        double leftov = lmerch - static_cast<double>(avlen) * static_cast<double>(numseg);

        // Set all lengths equal to AVLEN.
        for (int i = 0; i < numseg; ++i) {
            loglen[i] = static_cast<double>(avlen);
        }

        // If AVLEN is odd, adjust: add +1 from bottom up, subtract -1 from top down.
        // For 0-based indexing, apply to pairs (i, numseg-1-i) for i < numseg/2.
        //auto is_odd_int(int x) = {(x % 2) != 0;};
        if (avlen%2 !=0) {
            for (int i = 0; i < numseg; ++i) {
                if ((numseg - 2 * (i + 1) + 1) >= 1) { // matches Fortran condition
                    int topIndex = numseg - 1 - i;
                    if (topIndex >= 0) {
                        loglen[i] += 1.0;
                        loglen[topIndex] -= 1.0;
                    }
                }
            }
        }

        if (leftov > 0.0) {
            // If NUMSEG is odd, distribute LEFTOV among odd-length segments first.
            if ((numseg % 2) != 0) {
                for (int i = 0; i < numseg && leftov > 0.0; ++i) {
                    //if (i < 20) {
                        // Check if loglen[i] is odd using Fortran-like test:
                        int half = static_cast<int>(loglen[i] / 2.0);
                        if (loglen[i] > static_cast<double>(half * 2)) {
                            loglen[i] += 1.0;
                            leftov -= 1.0;
                        }
                    //}
                }
            }

            // Distribute the rest of LEFTOV; ensure additions go to lowest/shortest pieces.
            int kntit = 0;
            while (leftov > 0.0) {
                for (int i = 0; i < numseg && leftov > 0.0; ++i) {
                    //if (i >= 20) break; // respect array bounds
                    if (loglen[i] < maxlen) {
                        int topIndex = numseg - 1; // LOGLEN(NUMSEG) in Fortran

                        if (leftov >= 2.0) {
                            // Prefer adding 2 feet
                            if (loglen[i] == loglen[topIndex]) {
                                loglen[i] += 2.0;
                                leftov -= 2.0;
                            }
                            else if ((i + 1) < numseg && loglen[i] > loglen[i + 1]) {
                                loglen[i + 1] += 2.0;
                                leftov -= 2.0;
                            }
                        }
                        else {
                            // Add 1 foot
                            if (loglen[i] == loglen[topIndex]) {
                                loglen[i] += 1.0;
                                leftov -= 1.0;
                            }
                            else if ((i + 1) < numseg && loglen[i] > loglen[i + 1]) {
                                loglen[i + 1] += 1.0;
                                leftov -= 1.0;
                            }
                        }
                    }
                }

                // Safeguard against runaway loops (mirrors Fortran's KNTIT > 500 bail-out)
                ++kntit;
                if (kntit > 500) {
                    return loglen;
                }
            }
        }

    }
    else {
        // NOMINAL LOG SECTION - stack bottom logs, then handle the top one/two
        // LEFTOV computed using INT(MAXLEN) (truncation toward zero).
        int intMaxLen = static_cast<int>(maxlen);
        double leftov = lmerch - static_cast<double>(intMaxLen) * static_cast<double>(numseg - 1);

        // Set all logs to the nominal log length (MAXLEN)
        for (int i = 0; i < numseg; ++i) {
            loglen[i] = maxlen;
        }

        if (opt == 21) {
            if (leftov >= (maxlen / 2.0)) {
                loglen[numseg - 1] = leftov;
            }
            else {
                // Split the top two segments
                double last = static_cast<double>(static_cast<int>((maxlen + leftov) / 2.0));
                double prev = maxlen + leftov - last;
                loglen[numseg - 1] = last;
                if ((numseg - 2) >= 0) loglen[numseg - 2] = prev;

                // If both are odd and equal, move 1' from upper to lower
                if ((numseg - 2) >= 0) {
                    if (loglen[numseg - 1] == loglen[numseg - 2]) {
                        int half = static_cast<int>(loglen[numseg - 1] / 2.0);
                        if (loglen[numseg - 1] > static_cast<double>(half * 2)) {
                            loglen[numseg - 1] -= 1.0;
                            loglen[numseg - 2] += 1.0;
                        }
                    }
                }
            }

        }
        else if (opt == 22) {
            // Always split the top two segments
            double last = static_cast<double>(static_cast<int>((maxlen + leftov) / 2.0));
            double prev = maxlen + leftov - last;

            loglen[numseg - 1] = last;
            if ((numseg - 2) >= 0) loglen[numseg - 2] = prev;

            // Enforce minimum length for the last log
            if (loglen[numseg - 1] < minlen) {
                // Drop the last log
                loglen[numseg - 1] = 0.0;
                if ((numseg - 2) >= 0) loglen[numseg - 2] = maxlen;
                numseg = std::max(0, numseg - 1);
            }
            else {
                // If both are odd and equal, move 1' from upper to lower
                if ((numseg - 2) >= 0) {
                    if (loglen[numseg - 1] == loglen[numseg - 2]) {
                        int half = static_cast<int>(loglen[numseg - 1] / 2.0);
                        if (loglen[numseg - 1] > static_cast<double>(half * 2)) {
                            loglen[numseg - 1] -= 1.0;
                            loglen[numseg - 2] += 1.0;
                        }
                    }
                }
            }

        }
        else if (opt == 23) {
            // Top segment stands on its own if >= MINLEN; otherwise drop it.
            if (leftov >= minlen) {
                loglen[numseg - 1] = leftov;
            }
            else {
                loglen[numseg - 1] = 0.0;
                numseg = std::max(0, numseg - 1);
            }

        }
        else if (opt == 24) {
            // Top segment: <1/4 NNL drop; 1/4..3/4 => half NNL; >3/4 => NNL
            if (leftov < (maxlen * 0.25)) {
                loglen[numseg - 1] = 0.0;
                numseg = std::max(0, numseg - 1);
            }
            else if (leftov >= (maxlen * 0.25) && leftov <= (maxlen * 0.75)) {
                double halfNominalRounded = static_cast<double>(static_cast<int>(maxlen * 0.5 + 0.5));
                loglen[numseg - 1] = halfNominalRounded;
            }
            else {
                loglen[numseg - 1] = maxlen;
            }
        }
    }

	return loglen;
}

std::vector<LogOutput> ProfileVolumeCalculator::getLogData(std::vector<double> loglen, TreeMeasurment tree, MerchRules merchRules, int product, bool cubicOnly)
{
    double stump = merchRules.stumpHeight;
    std::vector<LogOutput> treeLogs;
    double prevHeight = stump;
    double trim = merchRules.trim;
    double actualDiaLarge;
    double actualDiaSmall;
    double diaClass;
    char COR = 'Y';

    if (!merchRules.useCorrectedFactor) COR = 'N';
    int sz = loglen.capacity();

    for (int i = 0; i < sz; ++i) {
        if (loglen[i] == 0.0) break;
        if (i == 0)
        {
            if(cubicOnly) treeLogs[i].heightToLargeEndDiameter = stump;
            else treeLogs[i].heightToLargeEndDiameter = 4.5;

            actualDiaLarge = taperModel_.GetDiameterAtHeight(tree, treeLogs[i].heightToLargeEndDiameter);
        }
        else
        {
            treeLogs[i].heightToLargeEndDiameter = loglen[i] + trim + prevHeight;
            prevHeight = treeLogs[i].heightToLargeEndDiameter;
            actualDiaLarge = actualDiaSmall;
        }
        actualDiaSmall = taperModel_.GetDiameterAtHeight(tree, treeLogs[i].heightToLargeEndDiameter + loglen[i] + trim);
        treeLogs[i].length = loglen[i];
        treeLogs[i].logNumber = i + 1;
        treeLogs[i].product = product;
        treeLogs[i].largeEndDiameterScaled = static_cast<int>(actualDiaLarge + 0.501);
        treeLogs[i].smallEndDiameterScaled = static_cast<int>(actualDiaSmall + 0.501);
        treeLogs[i].isSecondary = false;
        
        //Calculate log cubic and boardfoot volume
        if (cubicOnly)
        {
            //call smalian to calculate cubic volume
            treeLogs[i].grossCubicFoot = smallian(actualDiaLarge, actualDiaSmall, loglen[i]);
        }
        else
        {
            //call smalian, scribner, and intl14 to calculate cubic and boardfoot volume
            treeLogs[i].grossCubicFoot = smallian(treeLogs[i].largeEndDiameterScaled, treeLogs[i].smallEndDiameterScaled, loglen[i]);
            treeLogs[i].grossBoardFoot = scribner(treeLogs[i].smallEndDiameterScaled, loglen[i], COR);
            treeLogs[i].internationalBoardFoot = intl14(treeLogs[i].smallEndDiameterScaled, loglen[i]);
        
            //calculate log green weight and dry weight using cubic volume and weight factor

        }


    }

    return treeLogs;
}

std::vector<LogOutput> ProfileVolumeCalculator::SegmentLogs(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules, bool cubicOnly)
{
	std::vector<LogOutput> result;
    int numseg{ 0 };
    std::vector<double> primaryLogs;
    std::vector<LogOutput> primaryLogData;
    double merchHeight;
    double actualSawHeight = merchRules.stumpHeight;
	// merchendize the tree

	// merchendize primary product
		// get heights
    if (vco.primaryProduct == 1)  //for saw tree
    {
        if (tree.merchHeightSaw > 0) merchHeight = tree.merchHeightSaw;
        else merchHeight = taperModel_.GetHeightAtDiameter(tree, merchRules.minTopDibSaw);
    }
    else // for nonsaw tree
    {
        if (tree.merchHeightNonsaw > 0) merchHeight = tree.merchHeightNonsaw;
        else merchHeight = taperModel_.GetHeightAtDiameter(tree, merchRules.minTopDibNonSaw);
    }
    double merchLength = merchHeight - merchRules.stumpHeight;

    //get logs for the primary product
			// segment logs
				// get log lengths
				// get log diameters
    if (merchLength > merchRules.minMerchLength)
    {
        primaryLogs = getLogs(merchLength, merchRules, numseg);
        if (numseg > 0)
        {
            primaryLogData = getLogData(primaryLogs, tree, merchRules, vco.primaryProduct, cubicOnly);
            actualSawHeight = primaryLogData[numseg - 1].heightToLargeEndDiameter + primaryLogs[numseg - 1] + merchRules.trim;
        }
    }
	// merchendize secondary product
		// get heights
    // only do the secondary product for saw tree
    if (vco.primaryProduct == 1)
    {
        if (tree.merchHeightNonsaw > 0) merchHeight = tree.merchHeightNonsaw;
        else merchHeight = taperModel_.GetHeightAtDiameter(tree, merchRules.minTopDibNonSaw);

        merchLength = merchHeight - actualSawHeight;
        if (merchLength > merchRules.minLengthTop)
        {
				// segment logs
					// get log lengths
					// get log diameters
            int numseg2 = 0;
            std::vector<double> secondaryLogs = getLogs(merchLength, merchRules, numseg2);
            if (numseg2 > 0)
            {
                std::vector<LogOutput> secondaryLogData = getLogData(secondaryLogs, tree, merchRules, vco.secondaryProduct, cubicOnly);
                //add secondary log data into primaryLogData
                for (int i = 0; i < numseg2; ++i)
                {
                    primaryLogs[numseg] = secondaryLogs[i];
                    primaryLogData[numseg] = secondaryLogData[i];
                    primaryLogData[numseg].isSecondary = true;
                    primaryLogData[numseg].logNumber = numseg;
                    numseg += i;
                }
            }
        }
    }

	return primaryLogData;
}