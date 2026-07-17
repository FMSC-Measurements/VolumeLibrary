#include "..\SmalianScribnerIntl14.h"
#include "ProfileVolumeCalculator.h"
#include "..\WeightfactorAndRefDataResolver.h"
#include "../DefaultFormClassForFVS.h"
#include <array>


TreeOutput ProfileVolumeCalculator::CalculateVolume(VolumeCalculationOptions vco, TreeMeasurment tree, MerchRules merchRules)
{
    TreeOutput result;

    // initialize model on tree - i.e fwelling models, nsvb model
    taperModel_.InitializeOnTree(tree, merchRules, vco);

    //small tree volume calculation
    //BLM and R6 BEH model 
    if (volumeEquation_.modelType == VolumeEquation::ModelType::BEH) {
        double dbhIb = tree.dbh - merchRules.doubleBarkThicknessAtBrestHeight;
        double formClass = tree.formClass;
        if (formClass == 0.0) {
            formClass = GetFormClass(volumeEquation_.volEqStr, vco.forest, tree.dbh);
        }
        double d17 = tree.dbh * formClass / 100.0;
        if (tree.totalHeight <= 17.8 || 
            tree.dbh < merchRules.minTopDibNonSaw ||
            std::sqrt(dbhIb*dbhIb - dbhIb*dbhIb*17.3/tree.totalHeight) < merchRules.minTopDibSaw) {
            result.totalCubicFoot =   0.00272708 * (dbhIb * dbhIb) * tree.totalHeight;
            return result;
        }
        else if (d17 < merchRules.minTopDibNonSaw) {
            double logvol = 0.00272708 * (dbhIb * dbhIb + std::pow(d17, 2)) * 17.3;
            result.totalCubicFoot = logvol + 0.00272708 * std::pow(d17, 2) * (tree.totalHeight - 17.3);
            return result;
        }
    }

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
    // total volume forBEH,  NVB and CLK profile will be calculated differently.
    if ((volumeEquation_.modelType == VolumeEquation::ModelType::BEH && volumeEquation_.geoCode == VolumeEquation::GeoCode::R6) ||
        volumeEquation_.modelType == VolumeEquation::ModelType::CLK ||
        volumeEquation_.modelType == VolumeEquation::ModelType::NVB) {

        StemVolume stemVol = taperModel_.GetStemCubicVol(tree, merchRules, vco);
        //for Behre's taper to calculate the total cubic volume
        if (volumeEquation_.modelType == VolumeEquation::ModelType::BEH) {
            result.stumpCubicFoot = stemVol.stumpVol;
            result.totalCubicFoot = stemVol.primaryVol;
            result.cordMerchantable = std::round((result.grossCubicFootPrimary / 90.0) * 10.0) / 10.0;
        }
        else {
            //for taper CLK and NVB
            double merchCF = result.grossCubicFootPrimary + result.grossCubicFootSecondary;
            double cubicRatio = 1.0;
            if (merchCF > 0.0) cubicRatio = (stemVol.primaryVol + stemVol.topwoodVol) / merchCF;

            //Correct log cubic volume and weight
            for (auto& item : result.logs) {
                item.grossCubicFoot *= cubicRatio;
                item.greenWeight *= cubicRatio;
                item.dryWeight *= cubicRatio;
            }

            result.stumpCubicFoot = stemVol.stumpVol;
            result.grossCubicFootPrimary = stemVol.primaryVol;
            result.grossCubicFootSecondary = stemVol.topwoodVol;
            result.tipCubicFoot = stemVol.tipVol;
            result.totalCubicFoot = stemVol.primaryVol + stemVol.topwoodVol + stemVol.tipVol;
            double cordFactor = 90.0;
            if (vco.region == 3 || vco.region == 8 || vco.region == 9) cordFactor = 79.0;
            result.cordMerchantable = std::round((stemVol.primaryVol / cordFactor) * 10.0) / 10.0;
        }
        return result;
    }

    //all other profile model calculate stem volume as below
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

    double totalCubicVolume = merchCubic + tipVol;

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
    bool useDob = (vco.region == 8 ? true : false);
    if (vco.primaryProduct == 1)  //for saw tree
    {
        if (tree.merchHeightSaw > 0) merchHeight = tree.merchHeightSaw;
        else merchHeight = taperModel_.GetHeightAtDiameter(tree, merchRules.minTopDibSaw, useDob);
    }
    else // for nonsaw tree
    {
        if (tree.merchHeightNonsaw > 0) merchHeight = tree.merchHeightNonsaw;
        else merchHeight = taperModel_.GetHeightAtDiameter(tree, merchRules.minTopDibNonSaw, useDob);
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
                //reset the last log small end diameter
                if (i == numseg - 1 && actualDiaSmall < merchRules.minTopDibSaw) {
                    actualDiaSmall = merchRules.minTopDibSaw;
                }
                logData.length = loglen[i];
                logData.logNumber = i + 1;
                logData.product = vco.primaryProduct;
                logData.largeEndDiameterActual = actualDiaLarge;
                logData.smallEndDiameterActual = actualDiaSmall;
                logData.largeEndDiameterScaled = static_cast<int>(actualDiaLarge + 0.501);
                logData.smallEndDiameterScaled = static_cast<int>(actualDiaSmall + 0.501);
                logData.isSecondary = false;

                //Calculate log cubic and boardfoot volume
                //call smalian, scribner, and intl14 to calculate cubic and boardfoot volume
                //R6 BEH Butt Log cubic volume uses different method
                if (volumeEquation_.geoCode == VolumeEquation::GeoCode::R6 && volumeEquation_.modelType == VolumeEquation::ModelType::BEH && i == 0) {
                    logData.grossCubicFoot = r6BehButtLogVolume(tree.dbh, logData.smallEndDiameterScaled);
                }
                else {
                    logData.grossCubicFoot = smallian(logData.largeEndDiameterScaled, logData.smallEndDiameterScaled, loglen[i]);
                }

                //BIA Behr using different boardfoot calculation
                if (volumeEquation_.volEqStr.substr(0, 1) == "I" && volumeEquation_.modelType == VolumeEquation::ModelType::BEH) {
                    logData.grossBoardFoot = biaBehBoardfoot(logData.smallEndDiameterActual, loglen[i]);
                }
                else logData.grossBoardFoot = scribner(logData.smallEndDiameterScaled, loglen[i], COR);

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

        //32 foot log equation
        // check for 32 foot log equation to combine the two 16-foot log into one and recalculate log board foot volume
        // find volume for 32 foot logs (Flewelling & Demars equation only)
        if (volumeEquation_.modelType == VolumeEquation::ModelType::DEM ||
            volumeEquation_.modelType == VolumeEquation::ModelType::F32 ||
            volumeEquation_.modelType == VolumeEquation::ModelType::F33 ||
            volumeEquation_.modelType == VolumeEquation::ModelType::FW2 ||
            volumeEquation_.modelType == VolumeEquation::ModelType::FW3)
        {

            if (volumeEquationNumber.substr(3, 2) == "F3" || volumeEquationNumber.substr(1, 2) == "32" ||
                volumeEquationNumber.substr(1, 2) == "61" || volumeEquationNumber.substr(1, 2) == "62")
            {

                std::vector<LogOutput> result32;
                int numseg32 = (numseg + 1) / 2;
                result32.reserve(numseg32);
                int logNumber32 = 0;
                for (int i = 0; i + 1 < numseg; i += 2) {
                    logNumber32 += 1;
                    logData.logNumber = logNumber32;
                    logData.product = vco.primaryProduct;
                    logData.isSecondary = false;
                    logData.length = result[i].length + result[i + 1].length;
                    logData.largeEndDiameterActual = result[i].largeEndDiameterActual;
                    logData.smallEndDiameterActual = result[i + 1].smallEndDiameterActual;
                    logData.largeEndDiameterScaled = std::floor(logData.largeEndDiameterActual);
                    logData.smallEndDiameterScaled = std::floor(logData.smallEndDiameterActual);
                    //logData.largeEndDiameterScaled = result[i].largeEndDiameterScaled;
                    //logData.smallEndDiameterScaled = result[i + 1].smallEndDiameterScaled;
                    logData.heightToLargeEndDiameter = result[i].heightToLargeEndDiameter;
                    logData.grossCubicFoot = result[i].grossCubicFoot + result[i + 1].grossCubicFoot;
                    logData.greenWeight = result[i].greenWeight + result[i + 1].greenWeight;
                    logData.dryWeight = result[i].dryWeight + result[i + 1].dryWeight;
                    logData.grossBoardFoot = scribner(logData.smallEndDiameterScaled, logData.length, COR);
                    logData.internationalBoardFoot = intl14(logData.smallEndDiameterScaled, logData.length);
                    result32.push_back(logData);
                }

                //check the top log
                if (numseg % 2 != 0) {
                    logData.logNumber = numseg32;
                    logData.product = vco.primaryProduct;
                    logData.isSecondary = false;
                    logData.length = result[numseg - 1].length;
                    logData.largeEndDiameterActual = result[numseg - 1].largeEndDiameterActual;
                    logData.smallEndDiameterActual = result[numseg - 1].smallEndDiameterActual;
                    logData.largeEndDiameterScaled = result[numseg - 1].largeEndDiameterScaled;
                    logData.smallEndDiameterScaled = result[numseg - 1].smallEndDiameterScaled;
                    logData.heightToLargeEndDiameter = result[numseg - 1].heightToLargeEndDiameter;
                    logData.grossCubicFoot = result[numseg - 1].grossCubicFoot;
                    logData.greenWeight = result[numseg - 1].greenWeight;
                    logData.dryWeight = result[numseg - 1].dryWeight;
                    logData.grossBoardFoot = scribner(logData.smallEndDiameterScaled, logData.length, COR);
                    if (vco.region == 7 && logData.length < 16) logData.grossBoardFoot = 0.0;
                    logData.internationalBoardFoot = intl14(logData.smallEndDiameterScaled, logData.length);
                    result32.push_back(logData);
                }

                if (vco.region == 10) {
                    //clear the 16-foot log result and reset to the 32-foot log data
                    result.clear();
                    result = result32;
                    numseg = numseg32;
                }
                else {
                    //prorate 32 boardfoot volumes into 16 foot pieces
                    int lcnt = 0;

                    // Loop: i = 2, 4, 6, ..., numseg
                    for (int i = 2; i <= numseg; i += 2)
                    {
                        lcnt += 1;

                        if (vco.region == 7)
                        {
                            result[i - 1].grossBoardFoot = std::nearbyint(result32[lcnt - 1].grossBoardFoot / 2.0);
                            result[i].grossBoardFoot = result32[lcnt - 1].grossBoardFoot - result[i - 1].grossBoardFoot;
                        }
                        else
                        {
                            double topv16 = result[i].grossBoardFoot;
                            double botv16 = result[i - 1].grossBoardFoot;

                            double R = topv16 / (topv16 + botv16);

                            if (R == 0.5)
                            {
                                result[i].grossBoardFoot = static_cast<int>(result32[lcnt - 1].grossBoardFoot * R);
                            }
                            else
                            {
                                result[i].grossBoardFoot = std::nearbyint(result32[lcnt - 1].grossBoardFoot * R);
                            }

                            if (result[i].grossBoardFoot <= 0.0)
                            {
                                result[i].grossBoardFoot = 1.0;
                            }

                            result[i - 1].grossBoardFoot = std::nearbyint(result32[lcnt - 1].grossBoardFoot) - result[i].grossBoardFoot;

                            if (merchRules.useCorrectedFactor)
                            {
                                result[i - 1].grossBoardFoot *= 10;
                                result[i].grossBoardFoot *= 10;
                            }

                        }
                    }
                    // check for top log
                    if (((numseg + 1) / 2) > (numseg / 2))
                    {
                        lcnt += 1;

                        result[numseg].grossBoardFoot = result32[lcnt - 1].grossBoardFoot;

                        if (merchRules.useCorrectedFactor)
                            result[numseg].grossBoardFoot *= 10;
                    }

                }

            }
        }
        //end 32 foot log
    }
	// merchendize secondary product
		// get heights
    // only do the secondary product for saw tree
    if (vco.primaryProduct == 1)
    {
        if (tree.merchHeightNonsaw > 0) merchHeight = tree.merchHeightNonsaw;
        else merchHeight = taperModel_.GetHeightAtDiameter(tree, merchRules.minTopDibNonSaw, useDob);

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
                    logData.largeEndDiameterActual = actualDiaLarge;
                    logData.smallEndDiameterActual = actualDiaSmall;
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
    //end segment for topwood

    

	return result;
}