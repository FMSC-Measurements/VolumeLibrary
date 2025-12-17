
```mermaid
flowchart TB

    VolumeLibaryEntry --> VolumeCalculation --> BiomassCalculation --> Output

    subgraph VolumeLibaryEntry

    end
    

    subgraph VolumeCalculation
        direction TB
        Inputs --> Profile
        Inputs --> DVE
        Inputs --> FIA

    end

```

## Profile Volume Calculation

```mermaid
flowchart TB
    ProfileInputs -->
            GetMerchHeightPrimary --> CalculatePrimaryVolume --> GetMerchHeightSecondary --> CalculateSecondaryVolume

    subgraph ProfileInputs
            direction LR
            MerchRules
            EquationCoefficient
            TreeMeasurments
            end
            
```

## Data Types

``` mermaid
classDiagram

    class TreeMeasurments {
        float TotalHeight
        float DBH
    }

    
    class VolumeCalulationOptions {
        int FiaCode
        int Region
        int Forest
        int PrimaryProduct
        int SecondaryProduct
    }



    class MerchRules {
        int EvenOdd
        int Opt
        real MaxLength
        real MinLength
        real MinLengthTop
        real SumpHeight
        ...
    }

    class EquationCoefficientBase {
        ...
    }
```