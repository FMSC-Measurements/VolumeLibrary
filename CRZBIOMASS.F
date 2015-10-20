C      THIS UBROUTINE SEARCH THE REGIONAL SPECIES DEFAULT RECORDS TO FIND THE SPECIES
C      DEFAULT WEIGHT FACTOR AND COMPONENT BIOMASS EQUATION FOR THE REGIONAL/FOREST
       SUBROUTINE CRZSPDFT(REGN, FORST, SPCD, WF, BMSEQ, REF)
       IMPLICIT NONE
       INCLUDE 'WDBKWTDATA.INC'
       INCLUDE 'REGNDFTDATA.INC'
       INCLUDE 'BEQINFO.INC'
       INTEGER REGN, IFORST, SPCD, I, J
       REAL WF(3), DWF, REGNWF(3), NATLWF
       CHARACTER*2 FORST
       CHARACTER*12 BMSEQ(8)
       CHARACTER*40 REF(8)
       INTEGER FIRST, LAST, HALF, DONE
       
       READ(FORST,'(i2)') IFORST
       DO 3, J=1,8
         BMSEQ(J) = '-'
         REF(J) = ''
3      CONTINUE
       DONE = 0
       I = 0
C      First to check if the species has a regional/forest default record       
       DO 5, WHILE (DONE.EQ.0)
         I = I + 1
         IF(SPREGNDFTWF(I,1).EQ.REGN) THEN  
           IF((SPREGNDFTWF(I,2).EQ.IFORST .AND.
     &      SPREGNDFTWF(I,3).EQ.SPCD) .OR.
     &      (SPREGNDFTWF(I,2).EQ.0 .AND.
     &      SPREGNDFTWF(I,3).EQ.SPCD)) THEN
             DONE = I
             REGNWF(1) = SPREGNDFTWF(I,4)
             REGNWF(2) = SPREGNDFTWF(I,5)
             REGNWF(3) = SPREGNDFTWF(I,6)
             BMSEQ(1) = SPREGNDFTBEQ(I,1)
             BMSEQ(2) = SPREGNDFTBEQ(I,2)
             BMSEQ(3) = SPREGNDFTBEQ(I,3)
             BMSEQ(4) = SPREGNDFTBEQ(I,4)
             BMSEQ(7) = SPREGNDFTBEQ(I,5)
           ENDIF
         ENDIF 
         IF(I.GE.TOTDFT) DONE = -1
5      CONTINUE
       IF(DONE.GT.0) THEN
       !Found regional species default, then get the default equation and reference   
         !SECONDARY WEIGHT FACTOR
         IF(WF(2).GT.0) THEN
            REF(6) = 'USER PROVIDED WEIGHT FACTOR (SECOND)'
         ELSEIF(REGNWF(2).GT.0.1)THEN
            WF(2) = REGNWF(2)
            REF(6) = 'USE REGIONAL DEFAULT WEIGHT FACTOR (SECOND)'
         ELSE
           IF(WF(1).GT.0) THEN
             WF(2) = WF(1)
             REF(6) = 'USER PROVIDED WEIGHT FACTOR(PRIM)'
           ELSE
             WF(2) = REGNWF(1)
             REF(6) = 'USE REGIONAL DEFAULT WEIGHT FACTOR (PRIM)'
           ENDIF
         ENDIF
         !PRIMARY WEIGHT FACTOR
         IF(WF(1).GT.0.1)THEN
           REF(5) = 'USER PROVIDED WEIGHT FACTOR(PRIM)'
         ELSEIF(REGNWF(1).GT.0.1)THEN
            WF(1) = REGNWF(1)
            REF(5) = 'USE REGIONAL DEFAULT WEIGHT FACTOR (PRIM)'
         ENDIF
         !MOISTURE CONTENT
         IF(WF(3).GT.0) THEN
           REF(8) = 'USER PROVIDED MOISTURE CONTENT'
         ELSEIF(REGNWF(3).GT.0.1)THEN
           WF(3) = REGNWF(3)
           REF(8) = 'USE REGIONAL DEFAULT MOISTURE CONTENT'
         ENDIF
         !BIOMASS EQN REFERENCE
         DO I=1,TOTEQ
           IF(BEQN(I).EQ.BMSEQ(1)) THEN
             REF(1) = BEQREF(I)
           ELSEIF (BEQN(I).EQ.BMSEQ(2)) THEN
             REF(2) = BEQREF(I)
           ELSEIF(BEQN(I).EQ.BMSEQ(3)) THEN
             REF(3) = BEQREF(I)
           ELSEIF(BEQN(I).EQ.BMSEQ(4)) THEN
             REF(4) = BEQREF(I)
           ELSEIF(BEQN(I).EQ.BMSEQ(7)) THEN
             REF(7) = BEQREF(I)
           ENDIF
         ENDDO
       ENDIF
       IF(DONE.LE.0 .OR. WF(1).EQ.0 .OR. WF(3) .EQ.0) THEN
C      IF THE SPECIES DOES NOT HAVE REGION DEFAULT, SEARCH NATIONAL DEFAULT
         DONE = 0
         I = 0
         FIRST = 1
         LAST = TOTSPC
         IF(WF(1).LT.0.1 .OR. WF(3).LT.0.1) THEN
           DO 10, WHILE (DONE.EQ.0)
             HALF = (LAST - FIRST +1)/2 + FIRST
             IF(WDBKWT(HALF,1).EQ.SPCD) THEN
               DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
               DONE = -1
             ELSEIF(WDBKWT(HALF,1).LT.SPCD) THEN  
               FIRST = HALF
             ELSE
               LAST = HALF
             ENDIF
10         CONTINUE
             IF(DONE.LT.0) DONE = TOTSPC
             IF(WF(1).LT.0.1) THEN
               WF(1) = WDBKWT(DONE,9)
               REF(5) = 'WEIGHT FACTOR FROM MILES & SMITH 2009'
               IF(WF(2).LT.0.1) THEN
                 WF(2) = WF(1)
                 REF(6) = 'WEIGHT FACTOR FROM MILES & SMITH 2009'
               ENDIF
               IF(WF(3).LT.0.1) THEN
                 DWF = WDBKWT(DONE,10)
                 WF(3) = (WF(1) - DWF)/DWF*100
                 REF(8) = 'MIOSTURE CONTENT FROM MILES & SMITH 2009'
               ENDIF
             ELSE
               IF(WF(2).LT.0.1) WF(2) = WF(1)
               IF(WF(3).LT.0.1) THEN
                 DWF = WDBKWT(DONE,10)
                 WF(3) = (WF(1) - DWF)/DWF*100
                 REF(8) = 'MIOSTURE CONTENT FROM MILES & SMITH 2009'
               ENDIF
             ENDIF
         ENDIF 
       ENDIF    
       END
C -----------------------------------------------------------------------------------------------
C THIS SUNROUTINE IS SEARCHING THE SPECIES DEFAULT DATA(SMITH AND MILES 2009) TO GET WOOD DENSITY
       SUBROUTINE WOODDEN(SPCD, DEN)
       IMPLICIT NONE
       INCLUDE 'WDBKWTDATA.INC'
       INTEGER SPCD, FIRST, LAST, HALF, DONE
       REAL DEN
         DONE = 0
         FIRST = 1
         LAST = TOTSPC
           DO 50, WHILE (DONE.EQ.0)
             HALF = (LAST - FIRST +1)/2 + FIRST
             IF(WDBKWT(HALF,1).EQ.SPCD) THEN
               DONE = HALF
             ELSEIF(FIRST .EQ. LAST) THEN
               DONE = -1
             ELSEIF(WDBKWT(HALF,1).LT.SPCD) THEN  
               FIRST = HALF
             ELSE
               LAST = HALF
             ENDIF
50         CONTINUE
         IF(DONE.LT.0) DONE = TOTSPC
         DEN = WDBKWT(DONE,4)
       END       
C--------------------------------------------------------------------------------------------
       SUBROUTINE CRZBIOMASS(REGN,FORST,SPCD,DBHOB,HTTOT,VOL,WF,BMS,
     +  ERRFLG)
       IMPLICIT NONE
       INTEGER REGN, IFORST, SPCD, ERRFLG, I,J, LENGTH
       REAL DBHOB, HTTOT, DBH, THT, VOL(15), WF(3), BMS(8)
       CHARACTER*2 FORST
       CHARACTER*12 BMSEQ(8), BEQ
       CHARACTER*40 REF(8)
       REAL MERCHSTEM, P3, BIOMS, DRY2GRN
       REAL JNKBMS(8), RATIO, DEN, STUMPDRY

       READ(FORST,'(i2)') IFORST
       DO 13, J=1,8
         BMS(J) = 0.0
13     CONTINUE

       DBH = DBHOB
       THT = HTTOT
C      IF WEIGHT FACTOR IS NOT PROVIDED, GET IT FROM REGIONAL OR NATIONAL DEFAULT
       IF(WF(1).LT.0.1) THEN
         CALL CRZSPDFT(REGN,FORST,SPCD,WF,BMSEQ,REF)
       ENDIF
       IF(WF(2).LT.0.1) WF(2) = WF(1)
C      GET THE MERCH STEM MASS (WOOD AND BARK) FROM VOL AND WEIGHT FACTOR
       BMS(5) = VOL(4)*WF(1)
       BMS(6) = VOL(7)*WF(2)
       BMS(7) = VOL(15)*WF(2)
       STUMPDRY = VOL(14)*WF(1)/(1.0+WF(3)/100)
C      GET THE DRY WEIGHT FOR MERCH WOOD ONLY       
       CALL WOODDEN(SPCD, DEN)
       MERCHSTEM = (VOL(4) + VOL(7))*DEN
       
C      CALL JENKINS FUNCTION TO GET BIOMASS FOR TREE COMPONENT
C      NOTE: JENKINS FUNCTION RETURN DRY BIOMASS FOR THE FOLLOWING COMPONENT
C     1 ABOVE GROUND TOTAL
C     2 STEM WOOD
C     3 STEM BARK
C     4 FOLIAGE
C     5 ROOTS
C     6 BRANCHES
C     7 CROWN
C     8 STEM WOOD AND BARK

       CALL JENKINS(SPCD, DBH, JNKBMS)
C      DEDUCT STUMP       
       JNKBMS(7) = JNKBMS(1)-JNKBMS(8)-STUMPDRY
       JNKBMS(6) = JNKBMS(7)-JNKBMS(4)
       
C      GET THE MASS RATIO FOR THE STEM
       RATIO = MERCHSTEM/JNKBMS(2)

C      CONVERT DRY BIOMASS TO GREEN BIOMASS
       DRY2GRN = 1 + WF(3)/100
       JNKBMS = JNKBMS * DRY2GRN

C      ADJUST BIOMASS FROM JENKINS
       BMS(1) = JNKBMS(1)*RATIO
       BMS(2) = JNKBMS(6)*RATIO
       BMS(4) = JNKBMS(4)*RATIO
       
C      IF BIOMASS EQUSTION IS SET IN REGIONAL DEFAULT, USE IT TO CALCULATE BIOMASS
       DO 30, J=1,5
         IF(J.LT.5)THEN
           I=J
         ELSE
           I=7
         ENDIF
         BEQ = BMSEQ(I)
         IF(BEQ.NE.''.AND.(BEQ(12:12).EQ.'D'.OR.BEQ(12:12).EQ.'G'))THEN
           CALL CalcBiomass(BEQ, DBH, THT, P3, BIOMS)
           BMS(I) = BIOMS
           IF(BEQ(12:12).EQ.'D') BMS(I) = BIOMS*DRY2GRN
         ENDIF
30     CONTINUE   
    
       END
C -------------------------------------------------------------------------
       SUBROUTINE CRZSPDFTCS(REGN, FORSTI, SPCD, WF, AGTEQ, LBREQ,
     + DBREQ, FOLEQ, TIPEQ, WF1REF, WF2REF, MCREF, AGTREF, LBRREF,
     + DBRREF, FOLREF, TIPREF)
! Expose subroutine CRZSPDFTCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::CRZSPDFTCS

      IMPLICIT NONE
      INTEGER REGN, SPCD, I,J
      REAL WF(3)
      CHARACTER*(*) FORSTI,AGTEQ,LBREQ,DBREQ, FOLEQ, TIPEQ
      CHARACTER*(*) WF1REF, WF2REF, MCREF, AGTREF, LBRREF
      CHARACTER*(*) DBRREF, FOLREF, TIPREF
      
      CHARACTER(2)   FORST
      CHARACTER(12)  BMSEQ(8), BEQ 
      CHARACTER(40) REF(8)
      
      FORST   = FORSTI(1:2)
      
      CALL CRZSPDFT(REGN, FORST, SPCD, WF, BMSEQ, REF)
      DO 20, J=1,8
         BEQ = BMSEQ(J) 
         IF(BEQ(1:1).EQ.'-') THEN 
           BMSEQ(J) = ''
         ENDIF
20    CONTINUE
      DO J=1,3
        I=J
        IF(J.EQ.3) I=J+1
        IF(REF(I).EQ.'') REF(I)='Heath et al 2009'
      ENDDO

C     !add null terminator required by C# strings
      FORSTI = FORST // char(0)
      AGTEQ = BMSEQ(1) // char(0)
      LBREQ = BMSEQ(2) // char(0)
      DBREQ = BMSEQ(3) // char(0)
      FOLEQ = BMSEQ(4) // char(0)
      TIPEQ = BMSEQ(7) // char(0)
      WF1REF = REF(5) //char(0)
      WF2REF = REF(6) //char(0)
      MCREF = REF(8) //char(0)
      AGTREF = REF(1) //char(0)
      LBRREF = REF(2) //char(0)
      DBRREF = REF(3) //char(0)
      FOLREF = REF(4) //char(0)
      TIPREF = REF(7) //char(0)
      
      END
C ----------------------------------------------------------------------------------------
      SUBROUTINE CRZBIOMASSCS(REGN,FORSTI,SPCD,DBHOB,HTTOT,VOL,WF,BMS,
     +  ERRFLG)
! Expose subroutine CRZBIOMASSCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::CRZBIOMASSCS

      IMPLICIT NONE
      INTEGER REGN, SPCD, ERRFLG
      REAL DBHOB, HTTOT, VOL(15), WF(3), BMS(8), WFI(3)
      CHARACTER*(*) FORSTI
      CHARACTER(2) FORST
      
      FORST   = FORSTI(1:2)
      CALL CRZBIOMASS(REGN,FORST,SPCD,DBHOB,HTTOT,VOL,WF,BMS,ERRFLG)
C     !add null terminator required by C# strings
      FORSTI = FORST // char(0)
      END

C      SUBROUTINE BROWNCROWNFRACTIONCS(SPCD, DBH, THT, CFWT)
C      !DEC$ ATTRIBUTES DLLEXPORT::BrownCrownFractionCS
C      IMPLICIT NONE
C      INTEGER SPCD
C      REAL DBH, THT, CFWT(5)
C      CALL BrownCrownFraction(SPCD, DBH, THT, CFWT)
C      END
