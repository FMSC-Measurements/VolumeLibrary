#include "..\SmalianScribnerIntl14.h"
#include "ProfileVolumeCalculator.h"
#include "..\WeightfactorAndRefDataResolver.h"



TreeOutput ProfileVolumeCalculator::CalculateVolume(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput result;

    // initialize model on tree - i.e fwelling models, nsvb model
    taperModel_.InitializeOnTree(tree, merchRules, vco);

    //small tree volume calculation
    //BLM BEH model
    //if (tree.totalHeight <= 17.8 || SQRT(DBHIB * DBHIB - (DBHIB * DBHIB) * 17.3 / TTH)) < merchRules.minTopDibSaw)
    //{
    //    TOTCUB = 0.00272708 * (DBHIB * DBHIB) * TTH;
    //    return;
    //}

    // Segment Logs
    //for product 07 and 18 no need to to log segmentation
    if (vco.primaryProduct != 7 && vco.primaryProduct != 18)
    {
        std::vector<LogOutput> logs = SegmentLogs(vco, tree, merchRules);
        result.logs = logs;

        // calculate volume for all logs
        double grossCubicFootPrimary = 0.0;
        double grossCubicFootSecondary = 0.0;
        double grossBoardFootPrimary = 0.0;
        double grossBoardFootSecondary = 0.0;
        double grossIntl14Primary = 0.0;
        double grossIntl14Secondary = 0.0;
        double grossGreenWeightPrimary = 0.0;
        double grossGreenWeightSecondary = 0.0;
        double grossDryWeightPrimary = 0.0;
        double grossDryWeightSecondary = 0.0;
        int numberOfLogs = 0;

        //test get regional weight factor
        //WeightFactors weightfactor = getRegionalDefaultWtfactor(vco.region, vco.forest, vco.fiaCode);

        //sum log volumes
        for (const auto& item : logs) {
            if (item.isSecondary)
            {
                grossCubicFootSecondary += item.grossCubicFoot;
                grossBoardFootSecondary += item.grossBoardFoot;
                grossIntl14Secondary += item.internationalBoardFoot;
                grossGreenWeightSecondary += item.greenWeight;
                grossDryWeightSecondary += item.dryWeight;
            }
            else
            {
                grossCubicFootPrimary += item.grossCubicFoot;
                grossBoardFootPrimary += item.grossBoardFoot;
                grossIntl14Primary += item.internationalBoardFoot;
                grossGreenWeightPrimary += item.greenWeight;
                grossDryWeightPrimary += item.dryWeight;
                ++numberOfLogs;
            }
        }
        result.grossCubicFootPrimary = grossCubicFootPrimary;
        result.grossCubicFootSecondary = grossCubicFootSecondary;
        result.grossBoardFootPrimary = grossBoardFootPrimary;
        result.grossBoardFootSecondary = grossBoardFootSecondary;
        result.greenWeightPrimary = grossGreenWeightPrimary;
        result.greenWeightSecondary = grossGreenWeightSecondary;
        result.dryWeightPrimary = grossDryWeightPrimary;
        result.dryWeightSecondary = grossDryWeightSecondary;
        result.grossInternationalBoardFoot = grossIntl14Primary + grossIntl14Secondary;

        //The boardfoot volume is International ¼ board foot volume for Region 8 Forest 8, 9, 10, and 12 (except Andrew Pickens district)
        // and Region 9 Forest 4,5,8,11,12,14,19,20,21,22,24, and 30 when using Clark profile equation
        if (vco.region == 9)
        {
            std::vector<int> listForest9 = { 4,5,8,11,12,14,19,20,21,22,24,30 };
            if (std::count(listForest9.begin(), listForest9.end(), vco.forest) > 0)
            {
                result.grossBoardFootPrimary = grossIntl14Primary;
                result.grossBoardFootSecondary = grossIntl14Secondary;
            }
        }
        else if (vco.region = 8)
        {
            std::vector<int> listForest8 = { 8,9,10,12 };
            if (std::count(listForest8.begin(), listForest8.end(), vco.forest) > 0)
            {
                if (vco.forest == 12 && vco.district == 2) { ; }
                else
                {
                    result.grossBoardFootPrimary = grossIntl14Primary;
                    result.grossBoardFootSecondary = grossIntl14Secondary;
                }
            }
        }

        //calculate tip volume above the last log
        double tipVolume = 0.0;
        if (!logs.empty()) {
            size_t topIndex = logs.size() - 1; // Last element index
            if (logs[topIndex].length == 0.0) topIndex -= 1;
            double merchHeight = logs[topIndex].heightToLargeEndDiameter + logs[topIndex].length;
            double tipLength = tree.totalHeight - merchHeight;
            tipVolume = smallian(logs[topIndex].smallEndDiameterScaled, 0.0, tipLength);
        }
        result.tipCubicFoot = tipVolume;
        result.numberOfLogs = numberOfLogs;

        // calcualte log weights using weight factor

        // sum volume from logs into tree volume
    }
    
	// calculate total cubic and cords
    // total volume for NVB and CLK profile will be calculated differently (will be added later)
    // 
    // stump volume
    double ht2 = merchRules.stumpHeight;
    double stumpDib = taperModel_.GetDiameterAtHeight(tree, ht2);
    double stumpVolume = smallian(stumpDib, stumpDib, ht2);
    double merchCubic = 0.0;
    double segVol = 0.0;
    double dibLarge = stumpDib;
    double dibSmall = 0.0;

    double htToNonsawDib = taperModel_.GetHeightAtDiameter(tree, merchRules.minTopDibNonSaw);
    int htloop = static_cast<int>((htToNonsawDib - ht2)/4.0);
    for (int i = 0; i < htloop; ++i)
    {
        ht2 += 4.0;
        dibSmall = taperModel_.GetDiameterAtHeight(tree, ht2);
        segVol = smallian(dibLarge, dibSmall, 4.0);
        merchCubic += segVol;
        dibLarge = dibSmall;
    }
    //get the last piece volume above the last 4' segment
    segVol = smallian(dibLarge, merchRules.minTopDibNonSaw, htToNonsawDib - ht2);
    merchCubic += segVol;

    //calculate volume for the piece above minTopDibNonSaw
    htloop = static_cast<int>((tree.totalHeight - htToNonsawDib) / 4.0);
    double tipVol = 0.0;
    ht2 = htToNonsawDib;
    dibLarge = merchRules.minTopDibNonSaw;
    for (int i = 0; i < htloop; ++i)
    {
        ht2 += 4.0;
        dibSmall = taperModel_.GetDiameterAtHeight(tree, ht2);
        segVol = smallian(dibLarge, dibSmall, 4.0);
        tipVol += segVol;
        dibLarge = dibSmall;
    }
    //the very last piece
    segVol = smallian(dibLarge, 0.0, tree.totalHeight - ht2);
    tipVol += segVol;

    double totalCubicVolume = stumpVolume + merchCubic + tipVol;

    //calculate cord volume for product 07 (firewood) using a factor, each region has its own factor
    double cordVolume = 0.0;
    if (vco.primaryProduct == 7)
    {
        if (vco.region == 3 || vco.region == 8 || vco.region == 9)
            cordVolume = std::round((merchCubic / 79.0) * 10.0) / 10.0;
        else
            cordVolume = std::round((merchCubic / 90.0) * 10.0) / 10.0;

        result.cordMerchantable = cordVolume;
    }
        
    result.totalCubicFoot = totalCubicVolume;
    result.stumpCubicFoot = stumpVolume;

    //for firewood (prod=7) and biomass (prod=18) merchCubic also include trim
    if (vco.primaryProduct == 7 || vco.primaryProduct == 18)
    {
        result.grossCubicFootPrimary = merchCubic;
        result.tipCubicFoot = tipVol;
    }
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
                loglen.push_back(0.0);
            }
            else if (lmerch >= (maxlen * 0.25) && lmerch <= (maxlen * 0.75)) {
                loglen.push_back(maxlen / 2.0);
            }
            else {
                loglen.push_back(maxlen);
            }
        }
        else if (lmerch >= minlen) {
            if (lmerch > maxlen) lmerch = maxlen;
            loglen.push_back(lmerch);
        }
        else {
            loglen.push_back(0.0);
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
            loglen.push_back(static_cast<double>(avlen));
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
            loglen.push_back(maxlen);
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


std::vector<LogOutput> ProfileVolumeCalculator::SegmentLogs(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
	std::vector<LogOutput> result;
    int numseg{ 0 };
    std::vector<double> loglen;
    LogOutput logData;
    double merchHeight;
    double actualSawHeight = merchRules.stumpHeight;
    double actualDiaLarge;
    double actualDiaSmall;
    double prevHeight = merchRules.stumpHeight;
    char COR = 'Y';

    WeightFactorAndRefData wf = getSpeciesWtfactorAndRefData(vco.region, vco.forest, vco.fiaCode);

    if (!merchRules.useCorrectedFactor) COR = 'N';
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
        loglen = getLogs(merchLength, merchRules, numseg);
        if (numseg > 0)
        {
            result.reserve(numseg);
            for (int i = 0; i < numseg; i++)
            {
                actualSawHeight += loglen[i] + merchRules.trim;

                if (i == 0)
                {
                    logData.heightToLargeEndDiameter = 4.5;
                    actualDiaLarge = taperModel_.GetDiameterAtHeight(tree, logData.heightToLargeEndDiameter);
                }
                else
                {
                    logData.heightToLargeEndDiameter = loglen[i - 1] + merchRules.trim + prevHeight;
                    prevHeight = logData.heightToLargeEndDiameter;
                    actualDiaLarge = actualDiaSmall;
                }
                actualDiaSmall = taperModel_.GetDiameterAtHeight(tree, prevHeight + loglen[i] + merchRules.trim);
                logData.length = loglen[i];
                logData.logNumber = i + 1;
                logData.product = vco.primaryProduct;
                logData.largeEndDiameterScaled = static_cast<int>(actualDiaLarge + 0.501);
                logData.smallEndDiameterScaled = static_cast<int>(actualDiaSmall + 0.501);
                logData.isSecondary = false;

                //Calculate log cubic and boardfoot volume
                //call smalian, scribner, and intl14 to calculate cubic and boardfoot volume
                logData.grossCubicFoot = smallian(logData.largeEndDiameterScaled, logData.smallEndDiameterScaled, loglen[i]);
                logData.grossBoardFoot = scribner(logData.smallEndDiameterScaled, loglen[i], COR);
                logData.internationalBoardFoot = intl14(logData.smallEndDiameterScaled, loglen[i]);

                    //calculate log green weight and dry weight using cubic volume and weight factor
                if (!tree.isLive) logData.greenWeight = logData.grossCubicFoot * wf.weightFactorDead;
                else
                {
                    if (logData.product == 1) logData.greenWeight = logData.grossCubicFoot * wf.weightFactorSaw;
                    else logData.greenWeight = logData.grossCubicFoot * wf.weightFactorNonsaw;
                }
                logData.dryWeight = logData.grossCubicFoot * wf.weightFactorDry;

                result.push_back(logData);

            }
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
            MerchRules merchRules2 = merchRules;
            merchRules2.minLogLength = merchRules.minLengthTop;
            std::vector<double> secondaryLogs = getLogs(merchLength, merchRules2, numseg2);
            if (numseg2 > 0)
            {
                result.reserve(numseg + numseg2);
                double heightToSmallEnd = actualSawHeight;

                //add secondary log data into primaryLogData
                for (int i = 0; i < numseg2; ++i)
                {
                    logData.isSecondary = true;
                    logData.length = secondaryLogs[i];
                    logData.logNumber = numseg + i + 1;
                    logData.product = vco.secondaryProduct;
                    
                    if (i == 0)
                    {
                        if (numseg == 0) logData.heightToLargeEndDiameter = 4.5;
                        else logData.heightToLargeEndDiameter = actualSawHeight;
                        heightToSmallEnd = actualSawHeight + secondaryLogs[i] + merchRules.trim;
                    }
                    else
                    {
                        logData.heightToLargeEndDiameter = heightToSmallEnd;
                        heightToSmallEnd = heightToSmallEnd + secondaryLogs[i] + merchRules.trim;
                    }
                    
                    actualDiaLarge = taperModel_.GetDiameterAtHeight(tree, logData.heightToLargeEndDiameter);
                    actualDiaSmall = taperModel_.GetDiameterAtHeight(tree, heightToSmallEnd);
                    logData.largeEndDiameterScaled = static_cast<int>(actualDiaLarge + 0.501);
                    logData.smallEndDiameterScaled = static_cast<int>(actualDiaSmall + 0.501);
                    //Calculate log cubic and boardfoot volume
                    //call smalian, scribner, and intl14 to calculate cubic and boardfoot volume
                    logData.grossCubicFoot = smallian(logData.largeEndDiameterScaled, logData.smallEndDiameterScaled, secondaryLogs[i]);
                    logData.grossBoardFoot = scribner(logData.smallEndDiameterScaled, secondaryLogs[i], COR);
                    logData.internationalBoardFoot = intl14(logData.smallEndDiameterScaled, secondaryLogs[i]);

                    //calculate log green weight and dry weight using cubic volume and weight factor
                    if (!tree.isLive) logData.greenWeight = logData.grossCubicFoot * wf.weightFactorDead;
                    else logData.greenWeight = logData.grossCubicFoot * wf.weightFactorNonsaw;
                    logData.dryWeight = logData.grossCubicFoot * wf.weightFactorDry;

                    
                    result.push_back(logData);
                }
            }
        }
    }

	return result;
}