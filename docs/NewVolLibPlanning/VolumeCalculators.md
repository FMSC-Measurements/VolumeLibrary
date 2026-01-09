## Base Volume Calculator

```mermaid
classDiagram 
    class VolumeCalculatorBase {
        string[11] VolumeEquationNumber 
        VolumeOutput CalculateVolume(TreeMeasurments treeMeasurments)
    }


```




## Profile Volume Calculator
```mermaid
classDiagram 
    class ProfileVolumeCalculator {
        MerchRules merchRules
        TaperModel taperModel
        EquationCoefficents coefficents

        VolumeOutput CalculateVolume(TreeMeasurments treeMeasurments)
    }

```

```mermaid
flowchart TB
    ProfileInputs -->
            GetMerchHeightPrimary --> GenerateLogsPrimaryProduct --> 
            GetMerchHeightSecondary --> GenerateLogsSecondaryProduct --> 
            CalculateSecondaryVolume --> OutputVolume

    subgraph ProfileInputs
            direction TB
            MerchRules
            EquationCoefficient
            TreeMeasurments
            TaperModel
            end
            
```

## FIA Volume Calculator

```mermaid
classDiagram 
    class FiaVolumeCalculator {
        TaperModel taperModel
        DveModel[] devModels

        VolumeOutput CalculateVolume(TreeMeasurments treeMeasurments)
    }

```

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

## DVE Volume Calculator

```mermaid
classDiagram 
    class DveVolumeCalculator {
        DveModel devModels

        VolumeOutput CalculateVolume(TreeMeasurments treeMeasurments)
    }

```

```mermaid
flowchart TB
    DveInputs -->
            CalculateVolume--> OutputVolume

    subgraph DveInputs
            direction TB
            DveModel
            TreeMeasurments
            end
            
```



## DveModel

```mermaid
classDiagram 
 class DveModel{
    TreeVolume GetWholeTreeVolume(TreeMeasurments tree)
 }


```