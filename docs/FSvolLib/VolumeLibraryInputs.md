## Volume Library input Data Types

MerchRules.Opt -- enum value for different options for dealing with topwood

### VolumeCalulationOptions
```mermaid
classDiagram 
direction LR
    class VolumeCalulationOptions {
        string FiaCode
 
        int Region 
        int Forest
        int District
        int PrimaryProduct
        int SecondaryProduct
        VolumeCalculationType VolumeCalculationType - optional - defaults to FVS
        char AuxFlag - optional 
        string EcoRegion - optional - fia only
    }
    class VolumeCalculationType {
        <<Enumeration>>
        FVS - default
        FIA
        Cruise
        VariableLogLength
    }
```
#### AuxFlag - Single Char Value
Additional flag value to indicate
- Species Variant Info (young/old growth)
- Appraisal Group (R6 Dougfur, house logs)



### TreeMeasurments
```mermaid
classDiagram
    class TreeMeasurments {
        heights:
        real TotalHeight
        real ReferenceHeight - 479 aka UpperstemHeight
        real MerchHeightPrimary
        real MerchHeightSecondary
        int MerchHeightLogs - if zero feet else 8,16,32
        real HeightToFirstLiveLimb
        
---
        diameters:
        real DBH
        real DRC
        real ReferenceDiamater
        real DoubleBarkThicknessAtBrestHeight
---
        other:
        int FormClass
        int NumberOfStems
        real CrownRatio
        int CullPercent
---
        merch rule overrides:
        real StumpHeightOverride
        real MinTopDibSawOverride
        real MinTopDibNonSawOverride
    }

```

#### MerchHeightLogs
Instead of using MerchHeightType we are now using MerchHeightLogs where if a value is provided as either 8, 16, 32 then implicitly the MerchHeight will be Logs, otherwise MerchHeights will be in Feet

### MerchRules

``` mermaid
classDiagram
    class MerchRules {
        int EvenOdd
        int SegmentationOption
        real MaxLogLength
        real MinLogLength
        real MinLengthTop
        real MinTopDibSaw
        real MinTopDibNonSaw
        real MinMerchLength

        real StumpHeight
        real Trim
        real BarkThicknessRatio
        real MinimumBoardFootDiameter - maybe remove
    }
```

