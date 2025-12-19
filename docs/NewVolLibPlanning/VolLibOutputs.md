## Volume Library Output Datatypes
The primary output will be the `TreeOutput` data type. 

The `TreeOutput` data type contins the following core metrics:
 - a list of Logs with each log record containing mesurments, volumes and weights of individual log
 - Gross volumes of the tree
 - Green and Dry weights
 - Merch heights


 notes: 
 need to keep seconday. 

```mermaid
classDiagram

    TreeOutput-->LogOutput
    TreeOutput-->Biomass
    TreeOutput-->ErrorInfo

    class TreeOutput{
        Log[] Logs

        float MerchHeight

        int TotalLogs
        float TotalCubic
        float GrossBoardFootPrimary
        float GrossBoardFootSecondary
        float GrossCubicPrimary
        float GrossCubicSecondary
        float CordPrimary
        float CordsSecondary
        float GreenWeightPrimary
        float GreenWeightSecondary
        float DryWeightPrimary
        float DryWeightSecondary

        Biomass GreeBio
        Biomass DryBio

        float MerchHeightPrimary
        float MerchHeightSecondary

        ErrorInfo Error
    }   

    class ErrorInfo{
        int ErrorCode
        string ErrorMessage
    }

    class Biomass {
        float AboveGroundTotal
        float Branches
        float Foliage
        float StemTip
    }


    class LogOutput {
        float SmallEndDiaScaled
        float LargeEndDiaScaled
        string Prod 
        bool IsSecondary?
        float Length
        int LogNumber
        float GrossBoardFoot
        float GrossCubicFoot
        float GreenWeight
        float DryWeight
        float? InternationalBoardFoot
        float HieghtToLargeEndDiameter
    }

```