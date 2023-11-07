C A national-scale tree volume, biomass, and carbon modeling system for the United States
C Westfall, J.A. etal 2023       
C The subroutine calculate stem wood and bark volumes and also biomass for stem wood and bark, branches, foliage, and above ground total
C The elements in the variable DRYBIO and GRNBIO are weight of following:
      ! 1 above ground biomass (no foliage)
      ! 2 total stem wood
      ! 3 total stem bark
      ! 4 stump wood
      ! 5 stump bark
      ! 6 sawtimber wood (primary)
      ! 7 sawtimber bark
      ! 8 topwood wood (secondary)
      ! 9 topwood bark
      ! 10 tip wood
      ! 11 tip bark
      ! 12 branches
      ! 13 foliage
      ! 14 top and limb
      ! 15 carbon content
      SUBROUTINE NVBC(REGN,FORST,DIST,VOLEQ,DBHOB,HTTOT,MTOPP,MTOPS,
     + HT1PRD,HT2PRD,STUMP,PROD,BRKHT,BRKHTD,LIVE,CR,CULL,DECAYCD,
     + LOGLEN,LOGDIA,LOGVOL,BOLHT,LOGST,NOLOGP,NOLOGS,VOL,DRYBIO,GRNBIO,
     + ERRFLG,FIASPCD,CTYPE)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      CHARACTER*2 FORST,DIST,PROD
      CHARACTER*1 LIVE,CTYPE
      !CTYPE as I = FIA, F = FVS, C = Cruise, B = when other VOLEQ was used in voinitnvb
      INTEGER REGN,ERRFLG,DECAYCD,FIASPCD,SFTHRD
      REAL DBHOB,HTTOT,MTOPP,MTOPS,HT1PRD,HT2PRD,STUMP,BRKHT,BRKHTD
      REAL CR,CULL
      REAL VOL(15),DRYBIO(15),GRNBIO(15)
      INCLUDE 'tables5.inc'
      !Mrules variables
      INTEGER EVOD,OPT
      CHARACTER*1 COR,JKSPGRP
      REAL MAXLEN,MINLEN,MERCHL,MINLENT,TRIM,BTR,DBTBH,MINBFD
      CHARACTER*10 V_EQN
      !Variables for calculation
      REAL a,b,HT2,DIB,DIBL,DIBS,LENTH,LOGV,BFINT,R
      REAL Vtotib,Vtotbk,Vtotob,Rstump,Rsaw,Rmrch
      REAL Vsawib,Vsawbk,Vsawob,Vmrchib,Vmrchbk,Vmrchob
      REAL Vtwib,Vtwob,Vtwbk,Vstumpib,Vstumpob,Vstumpbk
      REAL Vtipib,Vtipob,Vtipbk,LMERCH,HTsaw,HTmrch
      REAL LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),BOLHT(21),LOGLENT(20)  !,SG(11)
      INTEGER RatioEQ,LOGST,NUMSEG,I,SPCD,SPGRPCD,DIVISION,EcoProv 
      REAL NOLOGP,NOLOGS,NLOGP,NLOGS,WDSG
      REAL Wtotib,Wtotbk,Wbrch,AGBpred,AGBcomp,AGBred,AGBdiff
      REAL WoodR,BarkR,BrchR,WoodAdd,BarkAdd,BrchAdd,WoodHarm,BarkHarm
      REAL BrchHarm,WDSGadj,BKSGadj,Wsawib,Wsawbk,Wtwib,Wtwbk,Wtipib
      REAL Wtipbk,Wstumpib,Wstumpbk,Wfol,Rrem,Vibmiss,Vbkmiss,Vobmiss
      !CHARACTER*12 BMSEQ(8)
      !CHARACTER*40 REF(8)
      REAL GRNWF,DRYWF,MC,CF,VtotibSound,VtotbkSound,VtotobSound !WF(3),
      REAL VstumpibSound,VtipibSound,VtipbkSound,VtipobSound,AGBpredRed
      REAL VsawibSound,VtwibSound
      REAL CullDenProp,DenProp,RemBkProp,RemBrchProp,WfolRem
      REAL WtotibRed,WtotbkRed,WtotobRed,WbrchRed,AGBcompRed,BrchRem
      REAL Vtotob2,Vsawib2,Vsawob2,Vsawbk2,Vmrchib2,Vmrchob2,Vmrchbk2
      REAL Vtwib2,Vtwob2,Vtwbk2,Vtipib2,Vtipbk2,Rsaw2,Rmrch2
      REAL DeadCF,SPGRNWF,SPDRYWF, SPREGNWF
      CHARACTER*3 SPC
      !Check VOLEQ is valid
      IF(VOLEQ(1:3).NE.'NVB')THEN
          ERRFLG = 1
          RETURN
      ENDIF
      !Check if EcoPROV is provided in the VOLEQ
      EcoProv = 0
      IF(VOLEQ(7:7).NE.'0')THEN
          READ (VOLEQ(5:7),*) EcoProv
          IF(VOLEQ(4:4).EQ.'M')THEN
              EcoProv = 1000 + EcoProv
          ENDIF
          VOLEQ(7:7) = '0'
      ENDIF
      DO I = 7, 10
          IF(VOLEQ(I:I).LT.'0'.OR.VOLEQ(I:I).GT.'9')THEN
              ERRFLG = 1
              RETURN
          ENDIF
      END DO
      VOL = 0.0
      DRYBIO = 0.0
      GRNBIO = 0.0
      LOGLEN = 0.0
      LOGVOL = 0.0
      LOGDIA = 0.0
      BOLHT = 0.0
      ERRFLG = 0
      AGBpred = 0
      WoodHarm = 0
      BARKHarm = 0
      Wstumpib = 0
      Wstumpbk = 0
      Wsawib = 0
      Wsawbk = 0
      Wtwib = 0
      Wtwbk = 0
      Wtipib = 0
      Wtipbk = 0
      BrchHarm = 0
      Wfol = 0
      LOGST = 0
      NOLOGP = 0
      NOLOGS = 0
      Rrem = 1
      BrchRem = 1
      Vtotib = 0
      Vtotbk = 0
      Vtotob = 0
      Vsawib = 0
      Vsawbk = 0
      Vsawob = 0
      Vtwib = 0
      Vtwob = 0
      Vtwbk = 0
      Vibmiss = 0
      Vbkmiss = 0
      Vobmiss = 0
      RatioEQ = 6
      WfolRem = 0
      Vtotob2 = 0
      BTR = 0
      DBTBH = 0
      DeadCF = 0
      !Move the MTOPS check to after call MRULES 2023/08/16
      !IF(MTOPS.GT.MTOPP) MTOPS = MTOPP
      SPGRPCD = 0
      WDSG = 0
      SFTHRD = 0
      CF = 0
      !should get the division from a crowsswalk using district and ecoregion
      READ (VOLEQ(5:7),'(i3)') DIVISION
      IF(VOLEQ(4:4).EQ.'M') DIVISION = DIVISION + 1000
      IF(FIASPCD.LE.0) THEN
          IF(LEN_TRIM(VOLEQ).EQ.10)THEN
            READ (VOLEQ(8:10),'(i3)') SPCD
          ELSEIF(LEN_TRIM(VOLEQ).EQ.11)THEN
              IF(VOLEQ(11:11).GE.'0'.AND.VOLEQ(11:11).LE.'9')THEN
              !Allow 4-digit SPCD i the VOLEQ
                  READ (VOLEQ(8:11),'(i4)') SPCD
              ELSE
                  READ (VOLEQ(8:10),'(i3)') SPCD
              ENDIF
          ENDIF
      ELSE
          SPCD = FIASPCD
      ENDIF
      IF(SPCD.EQ.204) SPCD = 202
      IF(SPCD.LT.10)THEN
          !FIASPCD is required for Jenkins SPGRP equation
          ERRFLG = 4
          RETURN
      ENDIF
      
      CALL NVB_RefSpcData(SPCD,SPGRPCD,WDSG,SFTHRD,CF,ERRFLG,
     & SPGRNWF, SPDRYWF)
      IF(ERRFLG.GT.0.AND.ERRFLG.NE.6) RETURN
      !Reset VOLEQ if it has 4-digit SPCD
      IF(LEN_TRIM(VOLEQ).EQ.11.AND.VOLEQ(11:11).NE.'P'
     & .AND.(VOLEQ(11:11).GE.'0'.AND.VOLEQ(11:11).LE.'9'))THEN
          IF(SPCD.GT.999)THEN
              WRITE (JKSPGRP, '(i1)') SPGRPCD
              VOLEQ = 'NVB000000'//JKSPGRP
          ELSE
              WRITE (SPC, '(i3)') SPCD
              IF(SPCD.LT.10)THEN
                  ERRFLG = 6
              ELSEIF(SPCD.LT.100)THEN
                  VOLEQ = VOLEQ(1:7)//'0'//SPC(2:3)
              ELSE
                  VOLEQ = VOLEQ(1:7)//SPC
              ENDIF
          ENDIF
      ENDIF
      IF(HTTOT.LE.0)THEN
          IF(BRKHT.LE.0.AND.BRKHTD.LE.0)THEN
              ERRFLG = 4
              RETURN
          ELSEIF(BRKHT.LE.0.OR.BRKHTD.LE.0)THEN
              ERRFLG = 9
              RETURN
          ELSEIF(BRKHT.GT.0.AND.BRKHTD.GT.0)THEN
              CALL NVB_ESTTHT(VOLEQ,DBHOB,BRKHT,BRKHTD,HTTOT,ERRFLG,
     &         SPGRPCD,WDSG)
          ENDIF
      ENDIF
      
      IF(BRKHT.LT.4.5) BRKHT = 0
      IF(BRKHT.GT.0)THEN
          CALL NVB_GetRatioCOEF(VOLEQ,Tbl5Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
          CALL CalcRatio(HTTOT,BRKHT,RatioEQ,a,b,Rrem)
          !this DIVISION needs to be accurate, like 331 not 330
          IF (EcoProv.EQ.0) CALL NVB_EcoProv(REGN,FORST,DIST,EcoProv)
          IF(LIVE.EQ.'L'.AND.CR.EQ.0) CR = 1
          CALL NVB_BrchRem(EcoProv,SPCD,BRKHT,HTTOT,CR,BrchRem)
      ENDIF
      CullDenProp = 0.54
      !IF(SPCD.LT.300) CullDenProp = 0.92
      IF(SFTHRD.EQ.0) CullDenProp = 0.92
      DenProp = 1
      RemBkProp = 1
      RemBrchProp = 1
      !Table 1 FIA dead tree wood density proportion and remaining bark and branch proportion
      !For live trees, ignore DECAYCD
      IF(LIVE.EQ.'L'.AND.DECAYCD.GT.0) DECAYCD = 0
      IF(DECAYCD.GT.0)THEN
          IF(DECAYCD.eq.1)THEN
              RemBkProp = 1
              RemBrchProp = 1
          ELSEIF(DECAYCD.EQ.2)THEN
              RemBkProp = 0.8
              RemBrchProp = 0.5
          ELSEIF(DECAYCD.EQ.3)THEN
              RemBkProp = 0.5
              RemBrchProp = 0.1
          ELSEIF(DECAYCD.EQ.4)THEN
              RemBkProp = 0.2
              RemBrchProp = 0
          ELSEIF(DECAYCD.EQ.5)THEN
              RemBkProp = 0
              RemBrchProp = 0     
          ENDIF
          !IF(SPCD.LT.300)THEN
          IF(SFTHRD.EQ.0)THEN
              IF(DECAYCD.eq.1)THEN
                  DenProp = 0.97
                  DeadCF = 0.501
              ELSEIF(DECAYCD.EQ.2)THEN
                  DenProp = 1
                  DeadCF = 0.504
             ELSEIF(DECAYCD.EQ.3)THEN
                  DenProp = 0.92
                  DeadCF = 0.506
             ELSEIF(DECAYCD.EQ.4)THEN
                  DenProp = 0.55
                  DeadCF = 0.52
             ELSEIF(DECAYCD.EQ.5)THEN
                  DenProp = 0.55
                  DeadCF = 0.527
             ENDIF
          ELSE
              IF(DECAYCD.eq.1)THEN
                  DenProp = 0.99
                  DeadCF = 0.47
              ELSEIF(DECAYCD.EQ.2)THEN
                  DenProp = 0.8
                  DeadCF = 0.473
             ELSEIF(DECAYCD.EQ.3)THEN
                  DenProp = 0.54
                  DeadCF = 0.481
             ELSEIF(DECAYCD.EQ.4)THEN
                  DenProp = 0.43
                  DeadCF = 0.48
             ELSEIF(DECAYCD.EQ.5)THEN
                  DenProp = 0.43
                  DeadCF = 0.472     
             ENDIF
          ENDIF
      ENDIF
      !First call the mrules to get the region defaults
      V_EQN = VOLEQ(1:10)
      CALL MRULES(REGN,FORST,V_EQN,DBHOB,COR,EVOD,OPT,MAXLEN,
     >   MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,
     >   PROD)
      IF(MTOPS.GT.MTOPP) MTOPS = MTOPP
      ! (1) calculate total stem wood volume inside bark
      CALL NVB_Vib(VOLEQ,DBHOB,HTTOT,Vtotib,ERRFLG,SPGRPCD,WDSG)
      IF(ERRFLG.GT.0) RETURN
      IF(Vtotib.LE.0)THEN
          ERRFLG = 1
          RETURN
      ENDIF
      VtotibSound = Vtotib*Rrem*(1-CULL/100)
      Vibmiss = Vtotib*(1-Rrem)
      VOL(1) = VtotibSound
      ! (2) calculate total stem bark volume
      CALL NVB_Vbk(VOLEQ,DBHOB,HTTOT,Vtotbk,ERRFLG,SPGRPCD,WDSG)
      Vbkmiss = Vtotbk*(1-Rrem)
      VtotbkSound = Vtotbk*Rrem
      ! (3) addup for the Vtotob
      Vtotob = Vtotib + Vtotbk
      Vobmiss = Vibmiss + Vbkmiss
      VtotobSound = VtotibSound+VtotbkSound
      ! (4) calculate stump vol
      CALL NVB_GetRatioCOEF(VOLEQ,Tbl5Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
      CALL CalcRatio(HTTOT,STUMP,RatioEQ,a,b,Rstump)
      Vstumpib = Vtotib * Rstump
      Vstumpob = Vtotob * Rstump
      Vstumpbk = Vstumpob - Vstumpib
      VstumpibSound = Vstumpib*(1-CULL/100)
      VOL(14) = VstumpibSound
      ! (5) calculate saw volume
      !FIA calculate merch volume different than cruise and FVS
      !FIA uses the very simple way, i.e. the volume from stump to merch top (DOB)
      !Timber Cruise and FVS calculates it as sum of log volumes with trim removed between logs
      IF(HT1PRD.LE.0)THEN
          IF(CTYPE.EQ.'I'.OR.CTYPE.EQ.'i')THEN
              CALL NVB_HT2TOPDob(VOLEQ,DBHOB,HTTOT,Vtotob2,MTOPP,HT1PRD,
     &        ERRFLG,SPGRPCD,WDSG)
          ELSE    
              CALL NVB_HT2TOPDib(VOLEQ,DBHOB,HTTOT,Vtotib,MTOPP,HT1PRD,
     &        ERRFLG,SPGRPCD,WDSG)
          ENDIF
          !Check the broken height
          IF(BRKHT.GT.0.AND.BRKHT.LT.HT1PRD) HT1PRD=BRKHT
      ENDIF
      IF(HT1PRD.LT.STUMP) HT1PRD = STUMP
      !Calculate sawtimber volume for FIA
      !CTYPE = B is set in noinitnvb when VOLEQ is not NSVB EQ
      IF(CTYPE.EQ.'I'.OR.CTYPE.EQ.'B')THEN
          CALL CalcRatio(HTTOT,HT1PRD,RatioEQ,a,b,Rsaw2)
          Vsawib2 = Vtotib*Rsaw2 - Vstumpib
          Vsawob2 = Vtotob*Rsaw2 - Vstumpob
          Vsawbk2 = Vsawob2 - Vsawib2
      ENDIF
      LMERCH = HT1PRD - STUMP
      IF(LMERCH.LT.0) LMERCH = 0
      LOGST = 0
      NLOGP = 0
      NUMSEG = 0
      R = 0
      HTsaw = STUMP
      !Moved the DIBL (DBHIN) calculation here avoid DIBL=0 for trees with no saw prod 
      HT2 = 4.5
      CALL NVB_CalcDiaAtHT(Vtotib,a,b,HTTOT,HT2,DIB)
      LOGDIA(1,2)= DIB
      LOGDIA(1,1)=NINT(DIB)
      BOLHT(1) = HT2
C--   USE DIB AT DBHOB FOR LARGE END BUTT LOG              
      DIBL = LOGDIA(1,1)
      HT2 = STUMP
      IF(LMERCH.GE.MERCHL)THEN
          CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)  
          CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG,LOGLEN)
          CALL NVB_CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,Vtotib,TRIM,HTTOT,
     +  LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR,a,b)
          LOGST = NUMSEG
          NOLOGP = NUMSEG
          HTsaw = HT2
          CALL CalcRatio(HTTOT,HTsaw,RatioEQ,a,b,Rsaw)
          Vsawib = Vtotib * Rsaw - Vstumpib
          Vsawob = Vtotob * Rsaw - Vstumpob
          Vsawbk = Vsawob - Vsawib
          R = Rsaw
      ENDIF
      HTmrch = HTsaw
      IF(HT1PRD.LE.0) HT1PRD = HTsaw
      ! (6) calculate merch, topwood(tw) and tip volumes
      NLOGS = 0
      IF(HT2PRD.LE.0)THEN
          IF(CTYPE.EQ.'I'.OR.CTYPE.EQ.'i')THEN
              CALL NVB_HT2TOPDob(VOLEQ,DBHOB,HTTOT,Vtotob2,MTOPS,HT2PRD,
     &        ERRFLG,SPGRPCD,WDSG)
          ELSE
              CALL NVB_HT2TOPDib(VOLEQ,DBHOB,HTTOT,Vtotib,MTOPS,HT2PRD,
     &        ERRFLG,SPGRPCD,WDSG)
          ENDIF
          !Check the broken height
          IF(BRKHT.GT.0.AND.BRKHT.LT.HT2PRD) HT2PRD=BRKHT
      ENDIF
      !Calculate topwood volume for FIA
      IF(CTYPE.EQ.'I'.OR.CTYPE.EQ.'B')THEN
          CALL CalcRatio(HTTOT,HT2PRD,RatioEQ,a,b,Rmrch2)
          Vmrchib2 = Vtotib*Rmrch2 - Vstumpib
          Vmrchob2 = Vtotob*Rmrch2 - Vstumpob
          Vmrchbk2 = Vmrchob2 - Vmrchib2
          Vtwib2 = Vmrchib2 - Vsawib2
          Vtwob2 = Vmrchob2 - Vsawob2
          Vtwbk2 = Vtwob2 - Vtwib2
      ENDIF
      IF(HT2PRD.LT.HTsaw) HT2PRD = HTsaw
      LMERCH = HT2PRD - HTsaw
      IF(LMERCH.GE.MINLENT)THEN
          NUMSEG = 0
          CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)  
          CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLENT,TRIM,NUMSEG,
     &     LOGLENT)
          NOLOGS = NUMSEG
          !Add secondary log length to LOGLEN
          DO 600 I = 1, NUMSEG
            LOGLEN(I+LOGST) = LOGLENT(I)
 600      CONTINUE
          CALL NVB_CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,Vtotib,TRIM,HTTOT,
     +    LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR,a,b)
          LOGST = LOGST + NUMSEG
          HTmrch = HT2
          CALL CalcRatio(HTTOT,HTmrch,RatioEQ,a,b,Rmrch)
          Vmrchib = Vtotib * Rmrch - Vstumpib
          Vmrchob = Vtotob * Rmrch - Vstumpob
          Vmrchbk = Vmrchob - Vmrchib
          ! topwood volume
          Vtwib = Vmrchib - Vsawib
          Vtwob = Vmrchob - Vsawob
          Vtwbk = Vtwob - Vtwib
      ENDIF
      IF(HT2PRD.LE.0) HT2PRD = HTmrch
      CALL CalcRatio(HTTOT,HTmrch,RatioEQ,a,b,Rmrch)
      ! tip volume
      !IF(NLOGS.GT.0) R = Rmrch
      IF(CTYPE.EQ.'I'.OR.CTYPE.EQ.'i')THEN
          R = Rmrch2
      ELSE
          R = Rmrch
      ENDIF
      Vtipib = Vtotib * (1 - R)
      Vtipob = Vtotob * (1 - R)
      Vtipbk = Vtipob - Vtipib
      VtipibSound = (Vtipib-Vibmiss)*(1-CULL/100)
      VtipbkSound = Vtipbk - Vbkmiss
      IF(VtipibSound.LT.0) VtipibSound = 0
      IF(VtipbkSound.LT.0) VtipbkSound = 0
      VtipobSound = VtipibSound+VtipbkSound
      VOL(15) = VtipibSound
      !FIA is not using LOGVOL
      IF(CTYPE.EQ.'I'.OR.CTYPE.EQ.'B')THEN
          VOL(4) = Vsawib2*(1-CULL/100)
          VOL(7) = Vtwib2*(1-CULL/100)
          Vsawib = Vsawib2
          Vsawbk = Vsawbk2
          Vtwib = Vtwib2
          Vtwbk = Vtwbk2
      ELSE
          VOL(4) = VOL(4)*(1-CULL/100)
          VOL(7) = VOL(7)*(1-CULL/100)
      ENDIF
      !Apply CULL to VOL(2),VOL(4),VOL(7),VOL(10) calculated in LOGVOL
      VOL(2) = VOL(2)*(1-CULL/100)
      VOL(10) = VOL(10)*(1-CULL/100)
      LOGVOL = LOGVOL*(1-CULL/100)
      VsawibSound = Vsawib*(1-CULL/100)
      VtwibSound = Vtwib*(1-CULL/100)
      ! (7) calculate stem wood weight
      ! CULL is not deducted for Dead trees to avoid double deduction
      IF(LIVE.EQ.'D') CULL = 0
      Wtotib = Vtotib*WDSG
      WtotibRed = Wtotib*Rrem*(1-CULL/100*(1-CullDenProp))*DenProp
      ! (8) calculate total stem bark weight
      CALL NVB_Wbk(VOLEQ,DBHOB,HTTOT,Wtotbk,ERRFLG,SPGRPCD,WDSG)
      WtotbkRed = Wtotbk*Rrem*DenProp*RemBkProp
      ! (9) calculate total branches weight
      CALL NVB_Wbrch(VOLEQ,DBHOB,HTTOT,Wbrch,ERRFLG,SPGRPCD,WDSG)
      WbrchRed = Wbrch*DenProp*RemBrchProp*BrchRem
      ! (10) calculate total above ground biomass
      CALL NVB_AGB(VOLEQ,DBHOB,HTTOT,AGBpred,ERRFLG,SPGRPCD,WDSG)
      AGBcompRed = WtotibRed + WtotbkRed + WbrchRed
      AGBred = AGBcompRed/(Wtotib+Wtotbk+Wbrch)
      AGBpredRed = AGBpred * AGBred
      AGBdiff = AGBpredRed - AGBcompRed
      WoodR = WtotibRed/AGBcompRed
      BarkR = WtotbkRed/AGBcompRed
      BrchR = WbrchRed/AGBcompRed
      WoodAdd = AGBdiff*WoodR
      BarkAdd = AGBdiff*BarkR
      BrchAdd = AGBdiff*BrchR
      WoodHarm = WtotibRed + WoodAdd
      BarkHarm = WtotbkRed + BarkAdd
      BrchHarm = WbrchRed + BrchAdd
      
      ! (11) calculate foliage weight for LIVE only
      IF(LIVE.EQ.'L')THEN
          CALL NVB_Wfo(VOLEQ,DBHOB,HTTOT,Wfol,ERRFLG,SPGRPCD,WDSG)
          WfolRem = Wfol*BrchRem
      ENDIF
      ! (12) Adjust wood and bark density
      WDSGadj = WoodHarm/VtotibSound
      BKSGadj = BarkHarm/VtotbkSound
      ! (13) calculate weights for primary(saw), secondary(topwood) and tip using the adjusted density
      Wsawib = VsawibSound*WDSGadj
      Wsawbk = Vsawbk*BKSGadj
      Wtwib = VtwibSound*WDSGadj
      Wtwbk = Vtwbk*BKSGadj
      Wtipib = VtipibSound*WDSGadj
      Wtipbk = VtipbkSound*BKSGadj
      Wstumpib = VstumpibSound*WDSGadj
      Wstumpbk = Vstumpbk*BKSGadj
      ! (14) add weight to DRYBIO and GRMBIO
      DRYBIO(1) = AGBpredRed
      DRYBIO(2) = WoodHarm
      DRYBIO(3) = BARKHarm
      DRYBIO(4) = Wstumpib
      DRYBIO(5) = Wstumpbk
      DRYBIO(6) = Wsawib
      DRYBIO(7) = Wsawbk
      DRYBIO(8) = Wtwib
      DRYBIO(9) = Wtwbk
      DRYBIO(10) = Wtipib
      DRYBIO(11) = Wtipbk
      DRYBIO(12) = BrchHarm
      DRYBIO(13) = WfolRem
      DRYBIO(14) = AGBpredRed-Wstumpib-Wstumpbk-Wsawib-Wsawbk
     &  -Wtwib-Wtwbk
      !Calculate carbon
      IF(LIVE.EQ.'D'.AND.DECAYCD.GT.0)THEN
          CF = DeadCF
      ENDIF
      !Round CF to 3 decimals
      CF = ANINT(CF*1000)/1000
      DRYBIO(15) = DRYBIO(1)*CF
      ! (15) get the regional green weight factor to calculate green biomass (GRNBIO)
      SPREGNWF = 0
      CALL GetRegnWF(REGN,FORST,SPCD,SPREGNWF)
      IF(SPREGNWF.GT.0) THEN
          GRNWF = SPREGNWF
      ELSE
          GRNWF = SPGRNWF
      ENDIF
      DRYWF = (WoodHarm+BarkHarm)/Vtotib
      IF(DRYWF.EQ.0) THEN
          DRYWF = SPDRYWF
      ENDIF
      MC = (GRNWF-DRYWF)/DRYWF
      GRNBIO = DRYBIO*(1+MC)
      
      RETURN
      END
C----------------------------------------------------------------------
C Total stem inside bark volume from TableS1
      SUBROUTINE NVB_Vib(VOLEQ,DBHOB,HTTOT,Vib,ERRFLG,SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB, HTTOT, Vib,WDSG 
      INCLUDE "tables1.inc"
      
      CALL CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl1Cnt,SPcoef,JKcoef,Vib,ERRFLG,
     & SPGRPCD,WDSG)
      RETURN
      END
!----------------------------------------------------------------------
C Total stem bark volume from TableS2
      SUBROUTINE NVB_Vbk(VOLEQ,DBHOB,HTTOT,Vbk,ERRFLG,SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB, HTTOT, Vbk,WDSG 
      INCLUDE "tables2.inc"
      
      CALL CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl2Cnt,SPcoef,JKcoef,Vbk,ERRFLG,
     & SPGRPCD,WDSG)
      RETURN
      END
!----------------------------------------------------------------------
C Total stem outside bark volume from TableS3
      SUBROUTINE NVB_Vob(VOLEQ,DBHOB,HTTOT,Vob,ERRFLG,SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB, HTTOT, Vob,WDSG 
      INCLUDE "tables3.inc"
      
      CALL CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl3Cnt,SPcoef,JKcoef,Vob,ERRFLG,
     & SPGRPCD,WDSG)
      RETURN
      END
!----------------------------------------------------------------------
C Stem height to a top diameter outside bark
      SUBROUTINE NVB_HT2TOPDob(VOLEQ,DBHOB,HTTOT,Vob,TOPD,HT2,ERRFLG,
     & SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB,HTTOT,Vob,TOPD,HT2,a,b,WDSG 
      INCLUDE "tables4.inc"
      HT2=0
      CALL NVB_GetRatioCOEF(VOLEQ,Tbl4Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
      IF(Vob.LE.0)THEN
          CALL NVB_Vob(VOLEQ,DBHOB,HTTOT,Vob,ERRFLG,SPGRPCD,WDSG)
      ENDIF
      CALL NVB_CalcHT2TOPD(Vob,a,b,HTTOT,TOPD,HT2)
      RETURN
      END
!----------------------------------------------------------------------
C Stem height to a top diameter inside bark
      SUBROUTINE NVB_HT2TOPDib(VOLEQ,DBHOB,HTTOT,Vib,TOPD,HT2,ERRFLG,
     & SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB,HTTOT,Vib,TOPD,HT2,a,b,WDSG 
      INCLUDE "tables5.inc"
      HT2=0
      CALL NVB_GetRatioCOEF(VOLEQ,Tbl5Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
      IF(Vib.LE.0)THEN
          CALL NVB_Vib(VOLEQ,DBHOB,HTTOT,Vib,ERRFLG,SPGRPCD,WDSG)
      ENDIF
      CALL NVB_CalcHT2TOPD(Vib,a,b,HTTOT,TOPD,HT2)
      RETURN
      END
!----------------------------------------------------------------------
C Calc Dia outside bark at a given height
      SUBROUTINE NVB_DobAtHT(VOLEQ,DBHOB,HTTOT,Vob,HT2,DOB,ERRFLG,
     & SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB,HTTOT,Vob,DOB,HT2,a,b,WDSG 
      INCLUDE "tables4.inc"
      DOB=0
      CALL NVB_GetRatioCOEF(VOLEQ,Tbl4Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
      IF(Vob.LE.0)THEN
          CALL NVB_Vob(VOLEQ,DBHOB,HTTOT,Vob,ERRFLG,SPGRPCD,WDSG)
      ENDIF
      CALL NVB_CalcDiaAtHT(Vob,a,b,HTTOT,HT2,DOB)
      RETURN
      END
!----------------------------------------------------------------------
C Calc Dia inside bark at a given height
      SUBROUTINE NVB_DibAtHT(VOLEQ,DBHOB,HTTOT,Vib,HT2,DIB,ERRFLG,
     & SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB,HTTOT,Vib,DIB,HT2,a,b,WDSG 
      INCLUDE "tables5.inc"
      DIB=0
      CALL NVB_GetRatioCOEF(VOLEQ,Tbl5Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
      IF(Vib.LE.0)THEN
          CALL NVB_Vib(VOLEQ,DBHOB,HTTOT,Vib,ERRFLG,SPGRPCD,WDSG)
      ENDIF
      CALL NVB_CalcDiaAtHT(Vib,a,b,HTTOT,HT2,DIB)
      RETURN
      END
!----------------------------------------------------------------------
C Total stem bark dry weight from TableS6
      SUBROUTINE NVB_Wbk(VOLEQ,DBHOB,HTTOT,Wbk,ERRFLG,SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB, HTTOT, Wbk,WDSG 
      INCLUDE "tables6.inc"
      
      CALL CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl6Cnt,SPcoef,JKcoef,Wbk,ERRFLG,
     & SPGRPCD,WDSG)
      RETURN
      END
!----------------------------------------------------------------------
C Total branch dry weight from TableS7
      SUBROUTINE NVB_Wbrch(VOLEQ,DBHOB,HTTOT,Wbrch,ERRFLG,SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB, HTTOT, Wbrch,WDSG 
      INCLUDE "tables7.inc"
      
      CALL CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl7Cnt,SPcoef,JKcoef,Wbrch,
     & ERRFLG,SPGRPCD,WDSG)
      RETURN
      END
!----------------------------------------------------------------------
C Total above ground biomass from TableS8
      SUBROUTINE NVB_AGB(VOLEQ,DBHOB,HTTOT,AGB,ERRFLG,SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB, HTTOT, AGB,WDSG 
      INCLUDE "tables8.inc"
      
      CALL CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl8Cnt,SPcoef,JKcoef,AGB,ERRFLG,
     & SPGRPCD,WDSG)
      RETURN
      END
!----------------------------------------------------------------------
C Total foliage dry weight from TableS6
      SUBROUTINE NVB_Wfo(VOLEQ,DBHOB,HTTOT,Wfo,ERRFLG,SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER ERRFLG,SPGRPCD
      REAL DBHOB, HTTOT, Wfo,WDSG 
      INCLUDE "tables9.inc"
      
      CALL CALC_EQVW(VOLEQ,DBHOB,HTTOT,Tbl9Cnt,SPcoef,JKcoef,Wfo,ERRFLG,
     & SPGRPCD,WDSG)
      RETURN
      END
!----------------------------------------------------------------------
! Calculate the volume or weight from a given VOLEQ
      SUBROUTINE CALC_EQVW(VOLEQ,D,H,CoefCnt,SPcoef,JKcoef,VW,ERRFLG,
     & SPGRPCD,WDSG )
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER I,EQN,ERRFLG,SPCD,SPGRPCD,CoefCnt  
      REAL D, H, VW, WDSG, SPcoef(CoefCnt,13),JKcoef(9,5)
      REAL a,a0,a1,b,b0,b1,b2,c,c1
      
      ERRFLG = 0
      EQN = 0
      !WDSG = 0
      IF(D.LT.1.0)THEN
          ERRFLG = 3
          RETURN
      ENDIF
      IF(H.LT.5.0)THEN
          ERRFLG = 4
          RETURN
      ENDIF
      READ (VOLEQ(8:10),'(i3)') SPCD
      IF(SPCD.GT.0.AND.SPCD.LT.10)THEN
          !SPCD is Jenkins group code
          CALL JKEQCOEF(SPCD,JKcoef,EQN,a,b,c)
          !CALL JKSPGRP(SPCD, SPGRPCD, WDSG)
          CALL CalcVOLWT(SPCD,D,H,EQN,a,a0,a1,b,b0,b1,b2,c,c1,WDSG,VW)
          RETURN
      ENDIF
      CALL SPEQCOEF(VOLEQ,CoefCnt,SPcoef,SPCD,EQN,
     & a,a0,a1,b,b0,b1,b2,c,c1)
      IF(EQN.EQ.0)THEN
      !If the equation is not in the list, then using Jenkins group equation
          !CALL JKSPGRP(SPCD, SPGRPCD, WDSG)
          CALL JKEQCOEF(SPGRPCD,JKcoef,EQN,a,b,c)
          IF(EQN.EQ.0)THEN
              !No species match, then use species 999
              CALL OTEQCOEF(CoefCnt,SPcoef,EQN,a,a0,a1,b,b0,b1,b2,c,c1)
          ENDIF
      ENDIF
      !Calculate total inside bark volume
      CALL CalcVOLWT(SPCD,D,H,EQN,a,a0,a1,b,b0,b1,b2,c,c1,WDSG,VW)
      RETURN
      END
!----------------------------------------------------------------------
! Search FIA_REF_Species to get species Jenkins SPGRPCD
      SUBROUTINE JKSPGRP(SPCD, SPGRPCD)
      IMPLICIT NONE
      INTEGER SPCD, SPGRPCD, FIRST,LAST,HALF,DONE
      !REAL WDSG, CF
      INCLUDE 'wdbkwtdata.inc'

      FIRST = 1
      LAST = CNT999
      IF(SPCD.GT.999)THEN
          FIRST = CNT999+1
          LAST = TOTSPC
      ENDIF
      DONE = 0
      SPGRPCD = 0
      !CF = 0
      !WDSG = 0
      IF(SPCD.LT.10)THEN
          SPGRPCD = SPCD
          RETURN
      ENDIF
C     FIND THE SPECIES GROUP CODE FROM THE ARRAY      
      DO 5, WHILE (DONE.EQ.0)
          HALF = (LAST - FIRST +1)/2 + FIRST
          IF(WDBKWT(HALF,1) .EQ. SPCD)THEN
              DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
              DONE = -1
          ELSEIF (WDBKWT(HALF,1) .LT. SPCD) THEN
              FIRST = HALF
          ELSE
              LAST = HALF - 1
          ENDIF
  5   CONTINUE 
      IF(DONE.GT.0) THEN
          SPGRPCD = WDBKWT(DONE,3) 
      ELSE
          SPGRPCD = WDBKWT(CNT999,3)
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------      
! Get Jenkins group equation coefficient
      SUBROUTINE JKEQCOEF(SPGRPCD,JKCOEF,equation,a,b,c)
      IMPLICIT NONE
      INTEGER SPGRPCD,equation,DONE,I
      REAL JKCOEF(9,5),a,b,c
      
      equation = 0
      DONE = 0
      DO I = 1, 9
          IF(JKCOEF(I,1).EQ.SPGRPCD)THEN
              DONE = I
              EXIT
          ENDIF
      END DO
      IF(DONE.GT.0)THEN
          equation = JKCOEF(DONE,2)
          a = JKCOEF(DONE,3)
          b = JKCOEF(DONE,4)
          c = JKCOEF(DONE,5)
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------
! Calculate volume or weight from the equations
      SUBROUTINE CalcVOLWT(SPCD,D,H,equation,a,a0,a1,b,b0,b1,b2,c,c1,
     & WDSG,VOLWT)
      IMPLICIT NONE
      INTEGER equation,k,SPCD
      REAL D,H,a,a0,a1,b,b0,b1,b2,c,c1,WDSG,VOLWT
      VOLWT = 0
      IF(equation.GT.0)THEN
          IF(equation.eq.1)THEN
              VOLWT = a*D**b*H**c
          ELSEIF(equation.eq.2)THEN
              k = 11
              IF(SPCD.LT.300) k = 9
              IF(D.LT.k)THEN
                  VOLWT = a0*D**b0*H**c
              ELSE
                  VOLWT = a0*k**(b0-b1)*D**b1*H**c
              ENDIF
          ELSEIF(equation.eq.3)THEN
              VOLWT = a*D**(a1*(1-EXP(-b1*D))**c1)*H**c
          ELSEIF(equation.EQ.4)THEN
              VOLWT = a*D**b*H**c*EXP(-(b2*D))
          ELSEIF(equation.EQ.5)THEN
              VOLWT = a*D**b*H**c*WDSG/62.4
          ENDIF
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------
! Calculate volme ratio from Table S4 or S5
      SUBROUTINE CalcRatio(H,h1,equation,a,b,R)
      IMPLICIT NONE
      INTEGER equation
      REAL H,h1,a,b,R
      R=0
      IF(equation.EQ.6.AND.(h1.GT.0.AND.h1.LT.H))THEN
          R = (1-(1-h1/H)**a)**b
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------
! Get the equation coefficients for SPCD
      SUBROUTINE SPEQCOEF(VOLEQ,TOTROW,SPCOEF,SPCD,
     & equation,a,a0,a1,b,b0,b1,b2,c,c1)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      INTEGER SPCD,DIVISION,STDORG,TOTROW,equation,I,DONE
      REAL SPCOEF(TOTROW,13),a,a0,a1,b,b0,b1,b2,c,c1
      
      STDORG = 0
      equation = 0
      DONE = 0
      READ (VOLEQ(8:10),'(i3)') SPCD
      DO I = 5, 7
          IF(VOLEQ(I:I).LT.'0'.OR.VOLEQ(I:I).GT.'9')THEN
              VOLEQ(I:I) = '0'
          ENDIF
      END DO
      READ (VOLEQ(5:6), '(i2)') DIVISION
      DIVISION = DIVISION*10
      IF(VOLEQ(4:4).EQ.'M') DIVISION = DIVISION+1000
      IF(LEN_TRIM(VOLEQ).EQ.11.AND.VOLEQ(11:11).EQ.'P') STDORG = 1
      DO I = 1, TOTROW
          IF(SPCOEF(I,1).EQ.SPCD)THEN
              IF(SPCOEF(I,2).EQ.DIVISION.AND.SPCOEF(I,3).EQ.STDORG)THEN
                  DONE = I
                  EXIT
              ELSEIF(SPCOEF(I,2).EQ.0.AND.SPCOEF(I,3).EQ.STDORG)THEN
                  DONE = I
                  EXIT
              ENDIF
          ENDIF
      END DO
      IF(DONE.GT.0)THEN
          equation = SPCOEF(DONE,4)
          a = SPCOEF(DONE,5)
          a0 = SPCOEF(DONE,6)
          a1 = SPCOEF(DONE,7)
          b = SPCOEF(DONE,8)
          b0 = SPCOEF(DONE,9)
          b1 = SPCOEF(DONE,10)
          b2 = SPCOEF(DONE,11)
          c = SPCOEF(DONE,12)
          c1 = SPCOEF(DONE,13)
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------
! Get the coef for species 999
      SUBROUTINE OTEQCOEF(TOTROW,SPCOEF,equation,
     & a,a0,a1,b,b0,b1,b2,c,c1)
      IMPLICIT NONE
      INTEGER TOTROW, equation
      REAL SPCOEF(TOTROW,13), a,a0,a1,b,b0,b1,b2,c,c1
      equation = SPCOEF(TOTROW,4)
      a = SPCOEF(TOTROW,5)
      a0 = SPCOEF(TOTROW,6)
      a1 = SPCOEF(TOTROW,7)
      b = SPCOEF(TOTROW,8)
      b0 = SPCOEF(TOTROW,9)
      b1 = SPCOEF(TOTROW,10)
      b2 = SPCOEF(TOTROW,11)
      c = SPCOEF(TOTROW,12)
      c1 = SPCOEF(TOTROW,13)
      RETURN
      END
C----------------------------------------------------------------------
C Calculate height to a given TOPD     
C For TOPD outside bark, input TCUFT outside bark and outside bark ratio coef (a,b)
C For TOPD inside bark, input TCUFT inside bark and inside bark coef (a,b)            
      SUBROUTINE NVB_CalcHT2TOPD(TCUFT,a,b,HTTOT,TOPD,HT2)
      IMPLICIT NONE
      REAL HTTOT,TOPD,HT2,HTLOW
      REAL low,hi,diff,X,a,b,TCUFT,mid
      INTEGER loopcnt
      
      HT2 = 0
      low = 0
      hi = HTTOT
      diff = 1.0
      loopcnt = 0
      DO WHILE (ABS(diff).GT.0.01)
          mid = (low+hi)/2  
          X = mid/HTTOT  
          diff = TOPD - ((TCUFT/0.005454154/HTTOT*a*b*
     +    (1-X)**(a-1)*(1-(1-X)**a)**(b-1)))**0.5
          IF(ABS(diff).LT.0.05) THEN
              EXIT
          ENDIF
          IF(diff.LT.0.0)THEN
              low = mid
          ELSE
              hi = mid
          ENDIF
          loopcnt = loopcnt +1
          IF(loopcnt.GT.1000) EXIT
      ENDDO
      HT2 = mid      
      RETURN
      END  
C----------------------------------------------------------------------
C Calculate the diameter at a given height
C For outside bark diameter, input TCUFT outside bark and outside bark ratio coef (a,b)
C For inside bark diameter, input TCUFT inside bark and inside bark coef (a,b)      
      SUBROUTINE NVB_CalcDiaAtHT(TCUFT,a,b,THT,HT2,DIA)
      IMPLICIT NONE
      REAL HT2,DIA,a,b,TCUFT,THT
      
      DIA = 0
      IF(HT2.GE.THT) RETURN
      DIA=SQRT((TCUFT/ 0.005454154 / THT) * (a * b * 
     >    (1-HT2/THT)**(a-1)*(1-(1 - HT2/THT)**a)**(b-1)))
      RETURN
      END
C----------------------------------------------------------------------
C Get ratio coef alpha (a) and belta (b) for the VOLEQ
      SUBROUTINE NVB_GetRatioCOEF(VOLEQ,COEFCNT,SPCOEF,JKCOEF,a,b,
     & SPGRPCD)
      CHARACTER*11 VOLEQ
      INTEGER COEFCNT,EQN,SPCD,SPGRPCD
      REAL SPCOEF(COEFCNT,13),JKCOEF(9,5),WDSG
      REAL a,a0,a1,b,b0,b1,b2,c,c1
      
      CALL SPEQCOEF(VOLEQ,COEFCNT,SPCOEF,SPCD,EQN,
     & a,a0,a1,b,b0,b1,b2,c,c1)
      IF(EQN.EQ.0)THEN
      !If the equation is not in the list, then using Jenkins group equation
          !CALL JKSPGRP(SPCD, SPGRPCD)
          CALL JKEQCOEF(SPGRPCD,JKCOEF,EQN,a,b,c)
          IF(EQN.EQ.0)THEN
              !No species match, then use species 999
              CALL OTEQCOEF(COEFCNT,SPCOEF,EQN,a,a0,a1,b,b0,b1,b2,c,c1)
          ENDIF
      ENDIF
      RETURN
      END
C**************************************************************
C Calculate LOGDIA, LOGVOL, BOLHT and VOL      
      SUBROUTINE NVB_CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,TCUFT,TRIM,THT,
     + LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR,a,b)
      IMPLICIT NONE
      INTEGER LOGST,NUMSEG,I
      REAL DIBL,HT2,TCUFT,TRIM,DIB,a,b,THT,DIBS,LOGCV,LENTH,LOGV,BFINT
      REAL LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),BOLHT(21),VOL(15)
      CHARACTER*1 COR
      
      DO 500 I=1+LOGST,NUMSEG+LOGST
          HT2=HT2+TRIM+LOGLEN(I)
          CALL NVB_CalcDiaAtHT(TCUFT,a,b,THT,HT2,DIB)
          LOGDIA(I+1,2)= DIB
          LOGDIA(I+1,1)=NINT(DIB)
          BOLHT(I+1) = HT2
          DIBS = LOGDIA(I+1,1)
          LOGCV = .00272708*(DIBL*DIBL+DIBS*DIBS)*LOGLEN(I)
          LOGVOL(4,I) = ANINT(LOGCV*10)/10
          DIBL = LOGDIA(I+1,1)
          !Calculate boardfoot volume
          DIB=LOGDIA(I+1,1)
          LENTH=LOGLEN(I)
          CALL SCRIB (DIB,LENTH,COR,LOGV)
          if(cor.eq.'Y') then
              LOGVOL(1,I) = LOGV * 10
          else
              LOGVOL(1,I) = ANINT(LOGV)
          endif
          CALL INTL14(DIB,LENTH,BFINT)
          LOGVOL(7,I) = BFINT
          IF(LOGST.GT.0) THEN
              VOL(7)=VOL(7)+LOGVOL(4,I)
              VOL(12) = VOL(12) + LOGVOL(1,I)
          ELSE
              VOL(4)=VOL(4)+LOGVOL(4,I)
              VOL(2) = VOL(2) + LOGVOL(1,I)
              VOL(10) = VOL(10) + BFINT
          ENDIF          
 500  CONTINUE
      
      RETURN
      END      
!----------------------------------------------------------------------
! Estimate total height for a broken top tree
      SUBROUTINE NVB_ESTTHT(VOLEQ,DBHOB,BRKHT,BRKHTD,HTTOT,ERRFLG,
     & SPGRPCD,WDSG)
      IMPLICIT NONE
      CHARACTER*11 VOLEQ
      REAL DBHOB,BRKHT,BRKHTD,HTTOT
      INTEGER ERRFLG,I,SPGRPCD
      INCLUDE 'tables4.inc'
      REAL TCUFT,a,b,THT, diff,X,WDSG
      !Height to top broken and diameter at broken point are required
      IF(BRKHT.LE.0.OR.BRKHTD.LE.0)THEN
          ERRFLG = 9
          RETURN
      ENDIF
      !Get the outbark ratio coefficient
      CALL NVB_GetRatioCOEF(VOLEQ,Tbl4Cnt,SPCOEF,JKCOEF,a,b,SPGRPCD)
      THT = BRKHT
      I = 3
      diff = 1
      DO WHILE (I < 100)
          THT = THT + I
          CALL NVB_Vob(VOLEQ,DBHOB,THT,TCUFT,ERRFLG,SPGRPCD,WDSG)
          X = BRKHT/THT  
          diff = BRKHTD - ((TCUFT/0.005454154/THT*a*b*
     +    (1-X)**(a-1)*(1-(1-X)**a)**(b-1)))**0.5
          IF(diff.LT.0.05) THEN
              EXIT
          ENDIF
          I = I+1
      END DO
      HTTOT = THT
      RETURN
      END
!----------------------------------------------------------------------
! Get species carbon fraction
      SUBROUTINE NVB_CarbonFrac(SPCD,CF)
      IMPLICIT NONE
      INTEGER SPCD,FIRST,LAST,DONE,HALF
      REAL CF
      INCLUDE 'tables10.inc'
      DONE = 0
      FIRST = 1
      LAST = Tbl10Cnt
      DO WHILE (DONE.EQ.0)
          HALF = (LAST - FIRST +1)/2 + FIRST
          IF(SPCF(HALF,1).EQ.SPCD) THEN
               DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
               DONE = -1
          ELSEIF(SPCF(HALF,1).LT.SPCD) THEN  
               FIRST = HALF
          ELSE
               LAST = HALF - 1
          ENDIF
      END DO
      IF(DONE.LE.0) DONE = Tbl10Cnt
      CF = SPCF(DONE,2)/100
      RETURN
      END
!----------------------------------------------------------------------
! Get average Crown ration
      SUBROUTINE NVB_AvgCR(DIVISION,SPCD,CR)
      IMPLICIT NONE
      INTEGER DIVISION,SPCD,I,DONE
      REAL CR
      INCLUDE 'tables11.inc'
      !DIVCR = DIVCRh
      IF(SPCD.LT.300)THEN
          DIVCR = DIVCRs
      ELSE
          DIVCR = DIVCRh
      ENDIF
      IF(DIVISION.EQ.0) THEN
          CR = DIVCR(Tbl11Cnt,2)/100
          RETURN
      ENDIF
      DONE = 0
      DO I = 1, Tbl11Cnt
          IF(DIVCR(I,1).EQ.DIVISION)THEN
              DONE = I
              EXIT
          ENDIF
      END DO
      IF(DONE.EQ.0) DONE = Tbl11Cnt
      CR = DIVCR(DONE,2)/100
      RETURN
      END
!----------------------------------------------------------------------
! Calculate branches remaining portion for broken top tree
! CR is in decimal, not percent  
! BrchRem is the remaining portion of the crown in decimal, not percent      
      SUBROUTINE NVB_BrchRem(DIVISION,SPCD,BRKHT,HTTOT,CR,BrchRem)
      IMPLICIT NONE
      INTEGER DIVISION,SPCD
      REAL BRKHT,HTTOT,CR,BrchRem,CrownLenRem,CRh,CrownLen
      BrchRem = 1
      IF(CR.GT.0) THEN 
          IF(CR.GE.1) CR = CR/100
          CrownLen = BRKHT*CR
          BrchRem = CrownLen/(CrownLen+HTTOT-BRKHT)
      ELSE
          CALL NVB_AvgCR(DIVISION,SPCD,CRh)
          IF(CRh.GE.1) CRh = CRh/100
          CrownLen = HTTOT*CRh
          BrchRem = (CrownLen - (HTTOT-BRKHT))/CrownLen
      ENDIF
      IF(BrchRem.LE.0.01) BrchRem = 0
      RETURN
      END
!----------------------------------------------------------------------
! Get EcoProvince for a given national forest district
      SUBROUTINE NVB_EcoProv(REGN, FORST, DIST, iPROV)
      IMPLICIT NONE
      INTEGER REGN,iPROV,DistNum,iFORST,iDist,ForstNum
      CHARACTER*2 FORST, DIST
      INCLUDE 'dist_ecoprov.inc'
      INTEGER DONE,FIRST,LAST,HALF,I
      iPROV = 0
      IF(FORST(2:2) .LT. '0') THEN
        FORST(2:2) = FORST(1:1)
        FORST(1:1) = '0'
        IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF
      READ (FORST,'(i2)') iFORST
      IF(DIST(2:2) .LT. '0') THEN
        DIST(2:2) = DIST(1:1)
        DIST(1:1) = '0'
        IF(DIST(2:2) .LT. '0') DIST(2:2) = '0'
      ENDIF
      READ (DIST,'(i2)') iDIST
      DistNum = REGN*10000 + iFORST*100 + iDIST
      DONE = 0
      FIRST = 1
      LAST = DistCnt
      DO WHILE (DONE.EQ.0)
          HALF = (LAST - FIRST +1)/2 + FIRST
          IF(DistProv(HALF,1).EQ.DistNum) THEN
               DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
               DONE = -1
          ELSEIF(DistProv(HALF,1).LT.DistNum) THEN  
               FIRST = HALF
          ELSE
               LAST = HALF - 1
          ENDIF
      END DO
      IF(DONE.GT.0) THEN
          iPROV = DistProv(DONE,2)
      ELSE
          !Checking forest default EcoProv
          ForstNum = REGN*100+iFORST
          DONE = 0
          FIRST = 1
          LAST = ForstCnt
          DO WHILE (DONE.EQ.0)
              HALF = (LAST - FIRST +1)/2 + FIRST
              IF(ForstProv(HALF,1).EQ.ForstNum) THEN
                  DONE = HALF
              ELSEIF(FIRST .EQ. LAST) THEN
                  DONE = -1
              ELSEIF(ForstProv(HALF,1).LT.ForstNum) THEN  
                  FIRST = HALF
              ELSE
                  LAST = HALF - 1
              ENDIF
          END DO
          IF(DONE.GT.0) THEN
              iPROV = ForstProv(DONE,2)
          ELSE
              !Check Regional default EcoProv
              DONE = 0
              I = 0
              DO WHILE (DONE.EQ.0)
                  I = I + 1
                  IF(RegnProv(I,1).EQ.REGN) THEN
                      DONE = I
                  ELSEIF(I.EQ.RegnCnt) THEN
                      DONE = -1
                  ENDIF
              END DO
              IF(DONE.GT.0) iPROV = RegnProv(DONE,2)
          ENDIF
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------
! Get default for a species in a given national forest district
      SUBROUTINE NVB_DefaultEq(REGN,FORST,DIST,SPCD,NVBEQ,ERRFLG)
      IMPLICIT NONE
      INTEGER REGN,SPCD,ERRFLG,V_SPCD
      CHARACTER*10 NVBEQ
      CHARACTER*2 FORST,DIST
      CHARACTER*3 SPC
      INTEGER iPROV,I,DONE,SPCDFIND,SPGRPCD,SFTHRD
      CHARACTER*4 PROV
      REAL WDSG,CF,SPGRNWF,SPDRYWF
      INCLUDE "tables1.inc"
      
      ERRFLG = 0
      V_SPCD = SPCD
      CALL NVB_EcoProv(REGN,FORST,DIST,iPROV)
      iPROV = (iPROV/10)*10
      !Check if the SPCD has a equation for the DIVISION
      !IF(SPCD.GT.999) SPCD = 999
      DONE = 0
      SPCDFIND = 0
      DO I = 1, Tbl1Cnt
          IF(SPCOEF(I,1).EQ.SPCD)THEN
              SPCDFIND = 1
              IF(SPCOEF(I,2).EQ.iPROV.OR.SPCOEF(I,2).EQ.0)THEN
                  DONE = I
                  IF(SPCOEF(I,2).EQ.0) iPROV = 0
                  EXIT
              ENDIF
          ENDIF
      END DO
      !When no equation for the SPCD, set iPROV to 0 and check Jenkin's species group 
      IF(DONE.EQ.0)THEN
          iPROV = 0
          CALL NVB_RefSpcData(SPCD,SPGRPCD,WDSG,SFTHRD,CF,ERRFLG,
     &      SPGRNWF,SPDRYWF)
          IF(ERRFLG.GT.0) RETURN
          IF(SPGRPCD.EQ.0.OR.SPGRPCD.EQ.10)THEN
              ERRFLG = 1
              RETURN
          ENDIF
          V_SPCD = SPGRPCD
      ENDIF
      WRITE (PROV, '(I4)') iPROV
      IF(iPROV.EQ.0) PROV = '0000'
      IF(iPROV.GT.999)THEN
          NVBEQ = 'NVBM'//PROV(2:4)
      ELSE
          NVBEQ = 'NVB0'//PROV(2:4)
      ENDIF
      
      WRITE (SPC, '(I3)') V_SPCD
      IF(V_SPCD.LT.10)THEN
          NVBEQ = NVBEQ(1:7)//'00'//SPC(3:3)
      ELSEIF(V_SPCD.LT.100)THEN
          NVBEQ = NVBEQ(1:7)//'0'//SPC(2:3)
      ELSEIF(V_SPCD.LT.1000)THEN
          NVBEQ = NVBEQ(1:7)//SPC(1:3)
      ELSE
          ERRFLG = 1
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------
! Search FIA_REF_Species to get species Jenkins SPGRPCD and WDSG (wood density lb/cf), carbon ratio, soft or hard wood
      SUBROUTINE NVB_RefSpcData(SPCD,SPGRPCD,WDSG,SFTHRD,CF,ERRFLG,
     & SPGRNWF,SPDRYWF)
      IMPLICIT NONE
      INTEGER SPCD, SPGRPCD, FIRST,LAST,HALF,DONE,ERRFLG,SFTHRD
      REAL WDSG, CF, SPGRNWF, SPDRYWF
      INCLUDE 'wdbkwtdata.inc'

      FIRST = 1
      LAST = CNT999
      ERRFLG = 0
      IF(SPCD.LT.10)THEN
          ERRFLG = 6
          RETURN
      ENDIF
      IF(SPCD.GT.999)THEN
          FIRST = CNT999+1
          LAST = TOTSPC
      ENDIF
      DONE = 0
      SPGRPCD = 0
      CF = 0
      WDSG = 0
C     FIND THE SPECIES GROUP CODE FROM THE ARRAY      
      DO 5, WHILE (DONE.EQ.0)
          HALF = (LAST - FIRST +1)/2 + FIRST
          IF(WDBKWT(HALF,1) .EQ. SPCD)THEN
              DONE = HALF
          ELSEIF(FIRST .EQ. LAST) THEN
              DONE = -1
          ELSEIF (WDBKWT(HALF,1) .LT. SPCD) THEN
              FIRST = HALF
          ELSE
              LAST = HALF - 1
          ENDIF
  5   CONTINUE 
      IF(DONE.GT.0) THEN
          SFTHRD = WDBKWT(DONE,2)
          SPGRPCD = WDBKWT(DONE,3) 
          WDSG = WDBKWT(DONE,4)
          CF = WDBKWT(DONE,12)
          SPGRNWF = WDBKWT(DONE,9)
          SPDRYWF = WDBKWT(DONE,10)
      ELSE
          ! no species match, use species 999 for calc
          ERRFLG = 6
          SFTHRD = WDBKWT(CNT999,2)
          SPGRPCD = WDBKWT(CNT999,3) 
          WDSG = WDBKWT(CNT999,4)
          CF = WDBKWT(CNT999,12)
          SPGRNWF = WDBKWT(CNT999,9)
          SPDRYWF = WDBKWT(CNT999,10)
      ENDIF
      IF(WDSG.EQ.0) WDSG = WDBKWT(CNT999,4)
      IF(SPGRNWF.EQ.0) SPGRNWF = WDBKWT(CNT999,9)
      IF(SPDRYWF.EQ.0) SPDRYWF = WDBKWT(CNT999,10)
      RETURN
      END
!----------------------------------------------------------------------
      !get species regional weight factor
      SUBROUTINE GetRegnWF(REGN,FORST,SPCD,WtFac)
      IMPLICIT NONE
      INTEGER REGN, SPCD,DONE,I,IFORST
      CHARACTER*2 FORST
      REAL WtFac
      INCLUDE 'regndftdata.inc'
      
      DONE = 0
      I = 0
      READ(FORST,'(i2)') IFORST
      IF(REGN.EQ.0) DONE = -1  
      DO WHILE (DONE.EQ.0)
         I = I + 1
         IF(SPREGNDFTWF(I,1).EQ.REGN) THEN  
           IF((SPREGNDFTWF(I,2).EQ.IFORST .AND.
     &      SPREGNDFTWF(I,3).EQ.SPCD) .OR.
     &      (SPREGNDFTWF(I,2).EQ.0 .AND.
     &      SPREGNDFTWF(I,3).EQ.SPCD)) THEN
             DONE = I
             WtFac = SPREGNDFTWF(I,4)
           ENDIF
         ENDIF 
         IF(I.GE.TOTDFT.AND.DONE.EQ.0) DONE = -1
      END DO
      RETURN
      END
      