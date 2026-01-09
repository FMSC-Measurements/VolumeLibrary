## VolumeLibrary Main Class
```mermaid
classDiagram
    class VolumeLibrary {
        TreeOutput CalculateVolume(VolumeCalulationOptions options, TreeMeasurments treeData)

        TreeOutput CalculateVolume(VolumeCalulationOptions options, TreeMeasurments treeData, MerchRules merchRules)
        
        string GetVolumeEquationNumber(VolumeCalulationOptions options)

        real GetHeightAtDiamater(VolumeCalulationOptions options, TreeMeasurments treeData, real diameter)

        real GetDiameterAtHeight(VolumeCalulationOptions options, TreeMeasurments treeData, real height)

        int GetNumberOfLog(VolumeCalulationOptions options, TreeMeasurments treeData)

        string VersionNumber() 

        ----
        MerchRules GetDefaultMerchRules(VolumeCalculationOptions options)
    }
```


```mermaid
classDiagram
  class VolumeCalculatorFactory {
        VolumeCalculator GetVolumeCalculator(VolumeCalculationOptions options)
  }

```



### CalculateVolume Outline


```mermaid
flowchart TB

    VLVC -->GetVolumeCalculator-->VolumeCalculator.Initialize--> 
    IsMerchRulesDefined-- yes -->VC 
    IsMerchRulesDefined-- no -->GetDefaultMerchRules 

    GetDefaultMerchRules--->VC

    
    --> BiomassCalculation --> Output

    subgraph VLVC[VolumeLibrary.CalculateVolume ]
        direction TB
        1a[VolumeCalulationOptions]
        1b[TreeMeasurments]
        1c[MerchRules*]
        
        
    end
    
    subgraph GetVolumeCalculator
        2a[VolumeCalulationOptions]
    end


    subgraph VC[VolumeCalculator.CalculateVolume]
        direction TB

        3a[TreeMeasurments]
        3b[MerchRules]


    end

```




## Volume Library input Data Types

MerchRules.Opt -- enum value for different options for dealing with topwood

### VolumeCalulationOptions
```mermaid
classDiagram 
direction LR
    class VolumeCalulationOptions {
        string FiaCode
 
        int Region [something]
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

        real SumpHeight
        real Trim
        real BarkThicknessRatio
        real DoubleBarkThicknessAtBrestHeight
        real MinimumBoardFootDiameter - maybe remove
    }
```




