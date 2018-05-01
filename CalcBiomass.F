C WARNING! The codes were created by NBEL Tester program. Please do NOT modify
C Last modified on 7/17/2015
C This sunroutine is generated from NBEL Tester for selected biomass equations
       SUBROUTINE CalcBiomass(BEQ, DBH, THT, CR, BMS)
       CHARACTER*12 BEQ
       REAL DBH, THT, CR, VOL(15),BMS
       INTEGER STEMS, ERRFLG
       REAL HT1PRD, HT2PRD,TOPD
       BMS = 0

       CALL BiomassLibrary(BEQ,DBH,THT,CR,HT1PRD, 
     + HT2PRD,TOPD, STEMS, VOL, BMS, ERRFLG)

       RETURN
       END
C ------------------------------------------------------------------------------------------------------------
C This subroutine calculates biomass for equation stored in the National Biomass Estimator Library (NBEL).
C The file fiabioeq.inc lists all FIA Regional biomass equation. 
C The file bioeqinfo.inc lists published biomass equations in NBEL and the file bioeqcoef.inc contains the
C coefficients for all biomass equations.
C Input variables are:
C BIOEQ -- biomass equatiom number 
C DBHOB -- outside bark diameter at breast height
C HTTOT -- total height 
C CR    -- crown ratio (=crown length/total height)
C HT1PRD -- height for primary prod (saw timber)
C HT2PRD -- height for secondary prod (pulp)
C TOPD -- top diameter for merch part
C STEMS -- number of stems for woodland species
C VOL -- the volume calculated from volume library 
C BIOMS -- the calculated dry biomass for the given BIOEQ.
C ERRFLG -- error code returnd from calculation:
C        15 - invalid BIOEQ
C        16 - primary prod height required
C        17 - secondary prod height is required
C        18 - stem cubic volume is required for FIA biomass equation
       SUBROUTINE BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD, 
     + HT2PRD,TOPD, STEMS, VOL, BIOMS, ERRFLG)
       INCLUDE 'fiabioeq.inc'
       INCLUDE 'bioeqinfo.inc'
       INCLUDE 'bioeqcoef.inc'
       CHARACTER*12 BIOEQ
       REAL DBHOB, HTTOT, TOPD, CR, VOL(15), BIOMS
       REAL HT1PRD, HT2PRD, VOLM(15)
       CHARACTER*3 MDL
       REAL DIA, HT
       INTEGER STEMS, IDX, I, SPEC, ERRFLG
       CHARACTER*2 DiaUnit, HtUnit, BioUnit
       CHARACTER*3 DiaType, HtType
       REAL IN2CM,IN2MM,IN2M,FT2M,FT2CM,KG2LB,G2LB,MG2LB
       REAL SG(11), MC
       INTEGER EqForm

C      List of unit conversion factor       
       IN2CM = 2.54
       IN2MM = 25.4
       IN2M = 0.0254
       FT2M = 0.3048
       FT2CM = 30.48
       KG2LB = 2.20462
       G2LB = 0.00220462
       MG2LB = 2204.62
       
       DIA = DBHOB
       HT = HTTOT
       MDL = BIOEQ(1:3)
       VOLM = VOL
       IF(MDL.EQ.'FNW'.OR.MDL.EQ.'FRM'.OR.MDL.EQ.'FNC'
     +    .OR.MDL.EQ.'FNE'.OR.MDL.EQ.'FSE')THEN
C      Check if the equation is a valid 
       IDX = 0
       DO I = 1, TOTEQ
         IF(FIABEQ(I).EQ.BIOEQ)THEN
           IDX = I
           EXIT
         ENDIF
       END DO
       IF(IDX.EQ.0)THEN
C      Flag for invalid BIOEQ       
        ERRFLG = 15
        RETURN
       ENDIF
     
C      Vol(4) should be the volume to 4" top
C      If top wood volume is saved in VOL(7), add it back to VOL(4)
         IF(VOLM(7).GT.0) VOLM(4) = VOLM(4)+VOLM(7)    
         IF(VOLM(1).EQ.0.AND.VOLM(4).EQ.0)THEN
C          Stem cubic volume is required for FIA biomass equation
           ERRFLG = 18
           RETURN
         ENDIF 
C      For the FIA biomass equation call FIA subroutine     
         IF(MDL.EQ.'FNW')THEN
           CALL FIA_NW(BIOEQ, DIA, HT, VOLM, BIOMS)
         ELSEIF(MDL.EQ.'FRM')THEN
           CALL FIA_RM(BIOEQ, DIA, HT, STEMS, VOLM, BIOMS)
         ELSEIF(MDL.EQ.'FNC')THEN
           CALL FIA_NC(BIOEQ, DIA, HT, VOLM, BIOMS)
         ELSEIF(MDL.EQ.'FNE')THEN
           CALL FIA_NE(BIOEQ, DIA, HT, VOLM, BIOMS)
         ELSEIF(MDL.EQ.'FSE')THEN
           CALL FIA_SE(BIOEQ, DIA, HT, VOL, BIOMS)
         ENDIF
       ELSE
C      For equations saved in the SQLite database, calculate biomass with its equation formula
C      1. check if the BIOEQ is valid. EQNTOT is total number of biomass equations in the database
       IDX = 0
       DO I = 1, EQNTOT
         IF(BIOMSEQ(I)%BIOEQ.EQ.BIOEQ)THEN
           IDX = I
           EXIT
         ENDIF
       END DO
       IF(IDX.EQ.0)THEN
         ERRFLG = 15
         RETURN
       ENDIF
       READ(BIOEQ(4:6),'(I3)') SPEC

C      2. get the EQN info and coef
       DiaUnit = BIOMSEQ(IDX)%DiaUnit
       HtType = BIOMSEQ(IDX)%HtType
       IF(HtType.EQ.'HT1')THEN
         IF(HT1PRD.LE.0)THEN
C          Primary prod height is required for the BIOEQ         
           ERRFLG = 16
           RETURN
         ELSE
           HT = HT1PRD
         ENDIF
       ELSEIF(HtType.EQ.'HT2')THEN
         IF(HT2PRD.LE.0)THEN
C          Secondary prod height is required for the BIOEQ         
           ERRFLG = 17
           RETURN
         ELSE
           HT = HT2PRD
         ENDIF
       ENDIF
       HtUnit = BIOMSEQ(IDX)%HtUnit
       IF(DiaUnit.EQ.'CM'.OR.DiaUnit.EQ.'cm')THEN
         DIA = DIA*IN2CM
       ELSEIF(DiaUnit.EQ.'MM'.OR.DiaUnit.EQ.'mm')THEN
         DIA = DIA*IN2MM
       ELSEIF(DiaUnit.EQ.'M'.OR.DiaUnit.EQ.'m')THEN
         DIA = DIA*IN2M
       ENDIF
       IF(HtUnit.EQ.'M'.OR.HtUnit.EQ.'m')THEN
         HT = HT*FT2M
       ELSEIF(HtUnit.EQ.'CM'.OR.HtUnit.EQ.'cm')THEN
         HT = HT*FT2CM
       ENDIF
       IF(DiaType.EQ.'CBH'.OR.DiaType.EQ.'CRC'.OR.DiaType.EQ.'CBA')THEN
         DIA = DIA*3.24
       ELSEIF(DiaType.EQ.'BA')THEN
         DIA = (DIA/2)**2*3.14
       ELSEIF(DiaType.EQ.'D2')THEN
         DIA = DIA*DIA
       ELSEIF(DiaType.EQ.'BAH')THEN
         DIA = DIA**2*0.005454*HT
       ELSEIF(DiaType.EQ.'D2H')THEN
         DIA = DIA*DIA*HT
       ELSEIF(DiaType.EQ.'D2R')THEN
         DIA = DIA*DIA*CR
       ENDIF
        EqForm = BEQCOEF(IDX,1)
        a = BEQCOEF(IDX,2)
        b = BEQCOEF(IDX,3)
        c = BEQCOEF(IDX,4)
        d = BEQCOEF(IDX,5)
        e = BEQCOEF(IDX,6)
C      3. call the EQN formula to calculate the biomass
       CALL BioeqFormula(EqForm,DIA,HT,CR,TOPD,a,b,c,d,e,BIOMS)
C      4. return dry biomass in lb
       BioUnit = BIOMSEQ(IDX)%BioUnit
       IF(BioUnit.EQ.'KG')THEN
         BIOMS = BIOMS*KG2LB
       ELSEIF(BioUnit.EQ.'G')THEN
         BIOMS = BIOMS*G2LB
       ELSEIF(BioUnit.EQ.'MG')THEN
         BIOMS = BIOMS*MG2LB
       ENDIF
         IF(BIOEQ(12:12).EQ.'G')THEN
           CALL MILESDATA(SPEC, SG)
           MC = (SG(9)-SG(10))/SG(10)
           BIOMS = BIOMS/(1+MC)
         ENDIF      
       ENDIF
       END
C *********************************************************************************************************
C This subroutine the specific gravity data for a given species
C The wood and bark specific gravity data for a species are given as:
C      Column# Name
C            1 Species FIA code
C            2 soft or hard wood (0=soft and 1=hard)
C            3 Jenkins species group code
C            4 Wood dry weight (lb/cf)
C            5 bark dry weight (lb/cf)
C            6 Wood moist content(%)
C            7 Bark moist cotent (%)
C            8 bark to wood vol percent (%)
C            9 Green weight factor (lb/cf)
C           10 Dry weight factor (lb/cf)
C           11 Sapling adjustment factor
      SUBROUTINE MILESDATA(SPEC, SG)
      INCLUDE 'wdbkwtdata.inc'       !'WDBKWTDATA.INC'
      INTEGER SPEC, FIRST, LAST, HALF, DONE
      REAL SG(11)
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
               LAST = HALF - 1
             ENDIF
50         CONTINUE
         IF(DONE.LT.0) DONE = TOTSPC
         SG(1) = WDBKWT(DONE,1)
         SG(2) = WDBKWT(DONE,2)
         SG(3) = WDBKWT(DONE,3)
         SG(4) = WDBKWT(DONE,4)
         SG(5) = WDBKWT(DONE,5)
         SG(6) = WDBKWT(DONE,6)
         SG(7) = WDBKWT(DONE,7)
         SG(8) = WDBKWT(DONE,8)
         SG(9) = WDBKWT(DONE,9)
         SG(10) = WDBKWT(DONE,10)
         SG(11) = WDBKWT(DONE,11)
         RETURN         
      END