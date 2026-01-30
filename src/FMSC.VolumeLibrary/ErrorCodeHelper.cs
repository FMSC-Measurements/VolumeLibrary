namespace FMSC.VolumeLibrary
{
    public static class ErrorCodeHelper
    {

        public static readonly string[] ERROR_MEASAGE_LOOKUP = new string[]  {"   ",
                                                    "NO VOLUME EQUATION MATCH",
                                                    "NO FORM CLASS",
                                                    "DBH LESS THAN ONE",
                                                    "TREE HEIGHT LESS THAN 4.5",
                                                    "D2H IS OUT OF BOUNDS",
                                                    "NO SPECIES MATCH",
                                                    "ILLEGAL PP LOG HEIGHT",
                                                    "ILLEGAL SP LOG HEIGHT",
                                                    "NO UPPER STEM MEASUREMENTS",
                                                    "ILLEGAL UPPER STEM HEIGHT",
                                                    "UNABLE TO FIT PROFILE GIVEN DBH, MERCH HT & TOP DIA",
                                                    "TREE HAS MORE THAN 20 LOGS",
                                                    "TOP DIAMETER GREATER THAN DBH INSIDE BARK",
                                                    "BARK EQUATION DOES NOT EXIST OR YIELDS NEGATIVE DBHIB",
                                                    "INVALID BIOMASS EQUATION",
                                                    "PRIMARY PRODUCT HEIGHT REQUIRED FOR BIOMASS CALCULATION",
                                                    "SECONDARY PRODUCT HEIGHT REQUIRED FRO BIOMASS CALCULATION",
                                                    "RECOVERABLE DEFECT GREATER THAN SUM OF DEFECTS -- SUM OF DEFECTS USED IN CALCULATION",
                                                    "SECONDARY PRODUCT WAS BLANK IN SAMPLE GROUPS -- DEFAULT VALUE USED",
                                                    "MORE THAN TWO UOMs DETECTED--THIS FILE WILL NOT LOAD IN TIM",
                                                    "BIOMASS FLAG NOT CHECKED -- NO WEIGHT CALCULATED"};


        public static string GetErrorMessage(int warringCode)
        {
            if (warringCode >= 0 && warringCode < ERROR_MEASAGE_LOOKUP.Length)
            {
                return ERROR_MEASAGE_LOOKUP[warringCode];
            }
            else
            {
                return warringCode.ToString();
            }
        }

    }
}
