## Volume Library Output Datatypes
The primary output will be the `TreeOutput` data type. 

The `TreeOutput` data type contins the following core metrics:
 - a list of Logs with each log record containing mesurments, volumes and weights of individual log
 - Gross volumes of the tree
 - Green and Dry weights
 - Merch heights

```mermaid
classDiagram

    TreeOutput-->LogOutput
    TreeOutput-->Weights
    TreeOutput-->ErrorInfo

    class TreeOutput{
        Log[] Logs

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

        Weights GreeWeights
        Weights DryWeights

        float MerchHeightPrimary
        float MerchHeightSecondary

        ErrorInfo Error
    }   

    class ErrorInfo{
        int ErrorCode
        string ErrorMessage
    }

    class Weights {
        float AboveGroundTotal
        float BranchesAndTop
        float DeadBranches
        float Foliage
        float StemTip
    }


    class LogOutput {
        float smallEndDiaScaled
        float largeEndDiaScaled
        string prod 
        bool IsSecondary?
        float Length
        int LogNumber
        float GrossBoardFoot
        float GrossCubicFoot
        float GrossCords 
        float GreenWeight
        float DryWeight
        float? InternationalBoardFoot
        float BaseHeight?
    }

```