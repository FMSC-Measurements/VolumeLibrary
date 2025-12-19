
```mermaid
flowchart TB

    VolumeLibraryInputs --> VolumeCalculation --> BiomassCalculation --> Output

    subgraph VolumeLibraryInputs
        direction TB
        MerchRules
        EquationCoefficient
        TreeMeasurments
        
    end
    

    subgraph VolumeCalculation
        direction TB

        A{VolumeCalculatorSelector}
        A-->GetMerchRules-->ProfileVolumeCalculator
        A-->DveVolumeCalculator
        A-->GetMerchRulesb-->FiaVolumeCalculator
    end

```

## Profile Volume Calculation


```mermaid
flowchart TB
    ProfileInputs -->
            GetMerchHeightPrimary --> CalculatePrimaryVolume --> GetMerchHeightSecondary --> CalculateSecondaryVolume

    subgraph ProfileInputs
            direction TB
            MerchRules
            EquationCoefficient
            TreeMeasurments
            TaperModel
            end
            
```

## FIA Volume Calculation
Requited Tree measuremtnts: DBH and (TotalHeight or BrokenTopHeight)
MerchRule: TopDib (calculated)

Doesn't break tree into multple logs. Calculates volume for Stump, Main stem, and top
```mermaid
flowchart TB
    FiaInputs-->CalculateMerchHeightFromTopDib-->GetDaimetersForMainStem-->CalcualteVoluemForMainStem-->CalculateVolumeForTopWood-->CalculateStumpVolume


subgraph FiaInputs
            direction TB
            EquationCoefficient
            TreeMeasurments
            TaperModel
            end



```

## Volume Library input Data Types

MerchRules.Opt -- enum value for different options for dealing with topwood

### Additional Tree Values (AuxData)
Single Char Value

- Species Variant Info
- Appraisal Group


### MerchHeightLogs
Instead of using MerchHeightType we are now using MerchHeightLogs where if a value is provided as either 8, 16, 32 then implicitly the MerchHeight will be Logs, otherwise MerchHeights will be in Feet

``` mermaid
---
  config:
    class:
      hideEmptyMembersBox: true
---
classDiagram


    class VolumeCalulationOptions {
        string FiaCode
 
        int Region
        int Forest
        int District
        int PrimaryProduct
        int SecondaryProduct
        DataType DataType - optional - defaults to FVS
        char AuxData - optional 
        string EcoRegion - optional - fia only
    }

    class TreeMeasurments {
        
        
        heights:
        float TotalHeight
        float ReferenceHeight - 479 aka UpperstemHeight
        float MerchHeightPrimary
        float MerchHeightSecondary
        int MerchHeightLogs - if zero feet else 8,16,32
        float HeightToFirstLiveLimb
        

        diameters:
        float DBH
        float DRC
        float ReferenceDiamater

        other:
        int FormClass
        int NumberOfStems
        float CrownRatio
        int CullPercent

        merch rule overrides:
        real StumpHeightOverride
        real MinTopDibPrimaryOverride
        real MinTopDibSecondaryOverride
    }





    class MerchRules {
        int EvenOdd
        int SegmentationOption
        real MaxLogLength
        real MinLogLength
        real MinLengthTop
        real MinTopDibPrimary
        real MinTopDibSecondary

        real SumpHeight
        real Trim
        real BarkThicknessRatio
        real DoubleBarkThicknessAtBrestHeight
        real MinimumBoardFootDiameter - maybe remove
        ...
    }

    class EquationCoefficientBase {
        ...
    }


    class AuxValue {

        char 
    }
```

### Enums
```mermaid
classDiagram
    class MerchHeightType {
        Feet
        Logs
    }

    class DataType {
        FVS - default
        FIA
        Cruise
        VariableLogLength
    }

```


### VolumeLibrary Main Class
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
    }


```