      MODULE REGNDFT_MOD

!     Module to store Regional species default variable

!     Created YW 06/1/15

      TYPE REGNDFT
      SEQUENCE
        INTEGER        REGN
        INTEGER        FORST     
        INTEGER        SPCD
        REAL           WF1
        REAL           WF2
        REAL           MC
        
        CHARACTER*12   AGT
        CHARACTER*12   LBR
        CHARACTER*12   DBR
        CHARACTER*12   FOL
        CHARACTER*12   TIP
 
        
      END TYPE REGNDFT
   

      END MODULE REGNDFT_MOD