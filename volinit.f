      SUBROUTINE VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)

!...  Prepare parameters and data for call to appropriate volume
!...  equations. 

			USE CHARMOD
			USE DEBUG_MOD
            USE VOLINPUT_MOD
      IMPLICIT NONE

!REV  Created TDH 04/01/09 
!REV  Revised TDH 11/18/09
!REV  YW 08/23/12 added stump and tip volume calc and saved in vol(14) and vol(15)
!REV  YW 01/18/2013 modified stump and tip calculation logic. Here is the calculation from FIA.
!     If volume equation does not calculate stump and tip, they will ba calculated here.
!REV  YW 05/16/2013 set HTTYPE to F if total height is entered
!REV  YW 02/27/2014 For region 6 Behre equation, if merch height is entered in feet, convert it to log ht
!REV  YW 07/30/2014 For region 6 Behre equation, if merch height is entered in feet, use log rules to convert to log ht.
C     YW 2016/01/13 Added BTR default value for Region 3 Santa Fe forest DF and PP
C     YW 04/19/2016 Added input variable DIST.
C     YW 09/15/2016 Added output variable LOGDIA,LOGLEN,LOGVOL to R4vol subroutine
!REV  Added manual debugging for use with pro vollib09 calls and
!REV  code to check for forest = null which caused blm problems
C     YW 04/13/2017 changed stump DIB calc using CALCDIA for profile model and stump volume as cylinder of stump DIB and stump height.
C                   also changed stem tip volume calc using DIB from last log and Samlian method with tip length
C 2018/11/07 YW ADDED HTTOT TO R12VOL FOR TOTAL CUBIC VOLUME
C YW 2019/02/14 Set the default stump and MTOPP for FIA equation only
C YW 2019/04/04 Added call to BIA behr and johnson equation
C YW 2020/03/18 R6 requests to use 102.4 for cuft to Cord 
C YW 2020/11/06 Changed the subroutine name to call FIA volume equations
C YW 2022/05/03 Set the total cubic vol to VOL(4) if MTOPP=0.1
C YW 2022/08/08 added variable STUMP to the call DVEST
C YW 2022/10/25 Fix the change made on 2022/05/03 to check VOL(1) before reset VOL(4)
C YW 2022/11/23 Added the call TOP6LEN R2 Black Hills equation 223DVEW122
C YW 2023/06/05 Modified VOLINITNVB for resetting CTYPE
! YW 2023/08/18 Check STUMP>0 before calc stump vol
! YW 2025/02/19 Set log weight to LOGVOL for CTYPE = 'C' (Cruise only)
!**********************************************************************
      CHARACTER*1  HTTYPE,LIVE,CTYPE,VOLEQREGN
      CHARACTER*2  FORST,PROD,PROD2
      character*4  CONSPEC
      CHARACTER*10 VOLEQ,FIAVTYPE,GEOSUB,NVELEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      CHARACTER*10 EQNUM
      INTEGER      SPEC

!   MERCH VARIABLES 
      INTEGER        REGN,HTTFLL,BA,SI
      REAL           STUMP,MTOPP,MTOPS,THT1,MAXLEN
      INTEGER        CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
      REAL         TIPDIB,TIPLEN
      
!   Tree variables
      REAL 			HTTOT,HT1PRD,HT2PRD,LEFTOV 
      REAL 			DBHOB,DRCOB,DBTBH,BTR,CR,TRIM
      INTEGER        FCLASS,HTLOG,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL           UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 			 HTREF
    
!   OUTPUTS
      REAL           NOLOGP,NOLOGS
      INTEGER        TLOGS,IFORST, IDIST
    
!   ARRAYS
      INTEGER        I15,I21,I20,I7,I3,I,J
      REAL 					 VOL(I15),LOGVOL(I7,I20)
      REAL				   LOGDIA(I21,I3),LOGLEN(I20),BOLHT(I21)
C  variables for stump dia and vol
      INTEGER SPN
      REAL STUMPDIB, STUMPDOB, VOLIB, VOLOB    
      REAL DIB,DOB,HTUP,MHT,FIAVOL,BFMIND,LMERCH
      
c  test biomass calc variable
      REAL WF(3), BMS(8), GRNWF,DeadWF,WtFac2
      INTEGER SPCD, FOREST 

C Test fiaeq2nveleq 2019/08/21
!      CHARACTER*12 FIABEQ,NVELBEQ,GEOSUB2
!      INTEGER BEQNUM,STEMS
!      REAL TOPD,BIOMS
!      VOL(1) = 20.0
!      VOL(4) = 20.0
!      SPN = HTREF 
!      BEQNUM = FCLASS
!      GEOSUB2 = '1'
!      CALL FIABEQ2NVELBEQ(BEQNUM,SPN,NVELBEQ,GEOSUB2,ERRFLAG)  
!      ERRFLAG = 0
!      CALL BiomassLibrary2(NVELBEQ,DBHOB,HTTOT,CR,HT1PRD, 
!     + HT2PRD,TOPD,STEMS,VOL,BIOMS,ERRFLAG,SPN,GEOSUB2)
!      CONTINUE
          
 !********************************************************************

!=====================================================================

!----------------------------------------------------------------------
!--------the following code is for manual debugging of older-----------
!--------versions of the call.  comment or uncomment to use------------
!--------also need to comment/uncomment close at bottom----------------

 !     SPECIES = VOLEQ(8:10)
 !     READ(SPECIES,'(i3)') SPCODE

 !     IF(SPCODE .EQ. 202 .AND. DBHOB .EQ. 10.0 .AND. 
 !    &     HTTOT == 90.0) THEN
!      IF(VOLEQ .EQ.'F06FW2W202') THEN
!        ANY_DEBUG = .TRUE.
!        DEBUG%VOLEQ = .TRUE.
!        DEBUG%MODEL = .TRUE.
!      ELSE
!        ANY_DEBUG = .FALSE.
!        DEBUG%VOLEQ = .FALSE.
!        DEBUG%MODEL = .FALSE.
!      ENDIF
 !     ANY_DEBUG = .TRUE.
 !     DEBUG%MODEL = .TRUE.
      IF (ANY_DEBUG) THEN
	       OPEN (UNIT=LUDBG, FILE='Debug.txt', STATUS='UNKNOWN')
	       WRITE (LUDBG,1)'Debugging NVEL'
   1     FORMAT(A)
      END IF
      
!---------end of manual debug code----------------------------------------
!-------------------------------------------------------------------------      
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 2) ' -->Enter VOLINIT'
    2    FORMAT (A)   
   		END IF
  
  
  
!  VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,FCLASS,
!     +               VOL,LOGDIA,LOGLEN,LOGVOL,TLOGS,NOLOGP,NOLOGS,
!     +               CONSPEC, BFPFLG,CUPFLG,errflag
  
  
  
   		IF (DEBUG%MODEL) THEN
        WRITE  (LUDBG, 100)'FORST VOLEQ     MTOPP HTTOT HT1PRD DBHOB 
     &   HTTYPE FCLASS'
100     FORMAT (A)
  		  WRITE  (LUDBG, 104)FORST,VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,
     +    FCLASS
104     FORMAT(A,2X,A, 1X, F5.1, 1X, F5.1, 2X, F5.1, 2X, F5.1, 5X,
     &    A, 1X, I5)
      
      
        WRITE (LUDBG, 300)'TLOGS NOLOGP NOLOGS CONSPEC 
     &    BFPFLG CUPFLG ERRFLAG'
  300   FORMAT (A)
  		  WRITE (LUDBG, 320)TLOGS,NOLOGP,NOLOGS,CONSPEC,BFPFLG,CUPFLG,
     +    ERRFLAG
  320   FORMAT(1X, I2, 2X, F5.1, 2X, F5.1, 5X, A,1X, I5, I5, 1X,I5)
       ENDIF
       
!Check for a DBH of less than 1.  Drop out of volume if true.  10/97
      IF(DBHOB.LT.1 .AND. DRCOB.LT.1) THEN
        ERRFLAG = 3
        GOTO 4000
      ENDIF
!Check for FIA volume equation (2018/10/02)
! P = PACIFIC COAST REGION
! R = ROCKY MOUNTAIN REGION
! N = NORTHERN REGION
! S = SOUTHERN REGION
      IF(VOLEQ(1:2).EQ.'cu') VOLEQ(1:2)='CU'
      IF(VOLEQ(1:2).EQ.'bd') VOLEQ(1:2)='BD'
      IF(VOLEQ(1:2).EQ.'CU'.OR.VOLEQ(1:2).EQ.'BD') THEN
        IF(CONSPEC(1:1).GE.'0'.AND.CONSPEC(1:1).LE.'9')THEN
          IF(CONSPEC(4:4).GE.'0'.AND.CONSPEC(4:4).LE.'9')THEN
           READ(CONSPEC(1:4),'(I4)')SPN
          ELSEIF(CONSPEC(3:3).GE.'0'.AND.CONSPEC(3:3).LE.'9')THEN
           READ(CONSPEC(1:3),'(I3)')SPN
          ELSEIF(CONSPEC(2:2).GE.'0'.AND.CONSPEC(2:2).LE.'9')THEN
           READ(CONSPEC(1:2),'(I2)')SPN
          ELSE
           READ(CONSPEC(1:1),'(I1)')SPN
          ENDIF
        ELSE
          SPN = 0
        ENDIF
        CALL FIAEQ2NVELEQ(VOLEQ,SPN,GEOSUB,MTOPP,NVELEQ,FIAVTYPE,
     & ERRFLAG)
        IF(ERRFLAG.GT.0) RETURN
        VOLEQ = NVELEQ
      ELSE
        SPN = 0
      ENDIF
      VOLEQREGN = VOLEQ(1:1)
      IF(VOLEQREGN.EQ.'P'.OR.VOLEQREGN.EQ.'R'.OR.
     &   VOLEQREGN.EQ.'N'.OR.VOLEQREGN.EQ.'S') THEN
        GEOSUB = '0'
        FIAVOL = 0.0
        VOL = 0.0
C       the default for stump and mtopp should be here for FIA equation (20190214)        
        IF(STUMP.EQ.0.0) STUMP = 1.0
        IF(MTOPP.EQ.0.0) MTOPP = 6.0
        IF(SI.EQ.0) SI = 65
        IF(BA.EQ.0) BA = 80
        BFMIND = 9.0
!        CALL FIA_VOLINIT(VOLEQ,SPN,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,
!     &  STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLAG,
!     &  FIAVTYPE,FIAVOL,BFMIND)
        CALL FIAVOLUME(VOLEQ,DBHOB,HTTOT,HT1PRD,HT2PRD,MTOPP,
     &  STUMP,DRCOB,UPSHT1,UPSD1,UPSHT2,UPSD2,VOL,BA,SI,GEOSUB,ERRFLAG,
     &  BFMIND)
     
        RETURN
      ENDIF     
!-----Set the default DIST to 01---------------------      
!      IF(IDIST.NE.IDIST) IDIST = 1
      IF(IDIST.EQ.0) IDIST = 1
      WRITE (DIST, '(I2)') IDIST
      IF(DIST(1:1) .LT. '0') DIST(1:1) = '0'
!-----End for DIST (04/19/2016)

      IF(VOLEQ .EQ. "" .AND. CTYPE.EQ.'F')THEN
        VAR = '  '
        READ(CONSPEC,'(I3)')SPEC
C        DIST = '01'
        CALL VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,EQNUM,ERRFLAG)
        VOLEQ = EQNUM
        ERRFLAG = 1
        GOTO 4000
      ENDIF
c  save FCLASS value
      FORMCLASS = FCLASS
!-----If forest is null it will cause problems---------------------      
      IF(FORST(1:1) .EQ. CHAR(0)) THEN
!        WRITE  (LUDBG, *)' -->VOLINIT FORST = NULL'
        FORST(1:2) = '01'
      ENDIF
      
      IF(FORST(2:2) .LT. '0') THEN
        FORST(2:2) = FORST(1:1)
	    FORST(1:1) = '0'
        IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF
      READ(FORST,'(i2)') IFORST

      DO 3 I=1,10
         IF(ICHAR(VOLEQ(I:I)).GE.97 .AND. ICHAR(VOLEQ(I:I)).LE.122)
     +      VOLEQ(I:I) = CHAR(ICHAR(VOLEQ(I:I))-32)
 3    CONTINUE

      MDL = VOLEQ(4:6)
! When total height is entered, the height type has to be feet. (2013/05/16)
      IF(HTTOT.GT.0) HTTYPE = 'F'              
      
      IF(MDL.EQ.'FW2' .OR. MDL.EQ.'fw2' .OR. MDL.EQ.'FW3' .OR.
     +   MDL.EQ.'fw3' .OR. MDL.EQ.'CZ2' .OR. MDL.EQ.'cz2' .OR.
     +   MDL.EQ.'CZ3' .OR. MDL.EQ.'cz3' .OR. MDL.EQ.'WO2' .OR.     
     +   MDL.EQ.'wo2' .OR. MDL.EQ.'F32' .OR. MDL.EQ.'f32' .OR.
     +   MDL.EQ.'F33' .OR. MDL.EQ.'f33' .OR. MDL.EQ.'JB2' .OR.
     +   MDL.EQ.'jb2') THEN
!************************
!    FLEWELLING MODELS  *
!    REGION 2 MODELS    *
!    REGION 5 MODELS    * 
!************************
        
        CALL PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,HTTYPE,
     +      HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,
     +      AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,
     +      TLOGS, NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,
     +      DRCOB,CTYPE,FCLASS,PROD,ERRFLAG)
      ELSEIF (MDL.EQ.'MAT' .OR. MDL.EQ.'mat') THEN
!**********************
!    REGION 4 MODEL  * 
!**********************

        CALL R4VOL(REGN,VOLEQ,MTOPP,HTTOT,DBHOB,HT1PRD,VOL,NOLOGP,
     +             NOLOGS,LOGDIA,LOGLEN,LOGVOL,BOLHT, 
     +             CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG)
        TLOGS = ANINT(NOLOGP + NOLOGS)
      ELSEIF (MDL.EQ.'TRF' .OR. MDL.EQ.'trf')THEN
C********************************
C      PNW terif VOLUME EQUATION
C*******************************
        CALL PNWTARIF(VOLEQ,DBHOB,HTTOT,MTOPP,VOL,BFPFLG,CUPFLG,
     >                    ERRFLAG)

      ELSEIF (VOLEQ(1:1).EQ.'6') THEN
!******************************
!   REGION 6 VOLUME ROUTINES *
!******************************
          THT1 = 0.0
          IF(HTTOT.GT.0) THEN
              THT1 = HTTOT
          ELSEIF(HTTYPE.EQ.'L' .OR. HTTYPE.EQ.'l') THEN
              THT1 = HT1PRD
          ELSE  
C Merch height is entered in feet. It need to convert to log height and set HTTYPE to L. (YW 02/27/14)
            IF(VOLEQ(1:3).EQ.'632')THEN          
              THT1 = INT(HT1PRD/32.6*10)
            ELSE
c              THT1 = INT(HT1PRD/16.3*10)
C ADDED ON 07/30/2014 ROUND LOGS BASED ON JEFF PENMAN LOG RULES
              IF(HT1PRD.GE.13.AND.HT1PRD.LE.20) THEN
                WHOLELOGS=1
                THT1=10
              ELSE
                WHOLELOGS = INT(HT1PRD/16.5)
                THT1 = WHOLELOGS*10
              ENDIF
              IF(HT1PRD.GT.20)THEN
                LEFTOV = HT1PRD-INT(WHOLELOGS*16.5)
                IF(LEFTOV.GE.(12+MOD(WHOLELOGS,2))) THEN
                  THT1=WHOLELOGS*10+10
                ELSEIF(LEFTOV.GE.(4+MOD(WHOLELOGS,2))) THEN
                  THT1=WHOLELOGS*10+5
                ENDIF
              ENDIF

            ENDIF
            HTTYPE = 'L'
            IF(THT1.EQ.0) THEN
                ERRFLAG = 4
                RETURN
            ENDIF    
          ENDIF

          NOLOGP=0.0
          NOLOGS=0.0
!****   If not top dib is specified then
!****   use the one from the vol equation

          IF((MDL.EQ.'BEH' .OR. MDL.EQ.'beh').AND.
     +            (BFPFLG.EQ.1 .OR. CUPFLG.EQ.1)) THEN
             IF(MTOPP.eq.0) THEN
                MTOPP = 6.0
             ENDIF
             CALL R6VOL(VOLEQ,FORST,DBHOB,BTR,FCLASS,MTOPP,THT1,
     +            HTTYPE,VOL,LOGVOL,NOLOGP,LOGDIA,LOGLEN,DBTBH,
     +            HT1PRD,CTYPE,errflag)
             TLOGS = ANINT(NOLOGP)
          ELSEIF((MDL.EQ.'DVE'.OR.MDL.EQ.'dve').AND.BFPFLG.EQ.1)THEN
             call R6VOL2(VOLEQ,DBHOB,HTTOT,VOL,errflag)
          ELSE
             ERRFLAG = 1
          endif


      ELSEIF(VOLEQ(1:1).EQ.'B' .or. voleq(1:1).eq.'b') THEN
!*************************
!   BLM VOLUME ROUTINES *
!*************************
         call BLMVOL(VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,FCLASS,
     +               VOL,LOGDIA,LOGLEN,LOGVOL,TLOGS,NOLOGP,NOLOGS,
     +               BFPFLG,CUPFLG,errflag)

      ELSEIF(VOLEQ(1:6).EQ.'I16BEH'.OR.VOLEQ(1:6).EQ.'i16beh'
     +   .OR.VOLEQ(1:6).EQ.'I00DVE'.OR.VOLEQ(1:6).EQ.'i00dve') THEN
!*************************
!   BIA behres I16BEHW000 and Johnson I00DVEW000 VOLUME ROUTINES *
!*************************
        IF(FCLASS .LE. 0) THEN
          IF(CTYPE .EQ. 'F') THEN
          !  GET FORMCLASS
            CALL GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
          ENDIF
        ENDIF
        IF(MDL.EQ.'BEH' .OR. MDL.EQ.'beh')THEN
          CALL BIA_Behres_Hyperobla(DBHOB,HTTOT,FCLASS,
     &         MTOPP, VOL)
        ENDIF
        IF(MDL.EQ.'DVE' .OR. MDL.EQ.'dve')THEN
          CALL VolEq_Johnson(DBHOB, HTTOT, FCLASS, VOL)
        ENDIF
      ELSEIF (VOLEQ(1:1).EQ.'8') THEN
!********************
!    REGION 8 MODEL  * 
!********************
        IF(MDL.EQ.'CLK' .OR. MDL.EQ.'clk') THEN
          IF(VOLEQ(3:3).EQ.'1'.OR.VOLEQ(3:3).EQ.'I')THEN
C         THe new R8 Clark equation using number 1 for the third character in the VOLEQ.
C         The new equation using R9 Clark equation codes
C 02/24/2021 The UPSHT1 can be height to 4 or 7/9. If it is the height to 4, set to HT2PRD
            IF (HTTOT.LE.0.0)THEN
              IF(UPSHT1.GT.0.0)THEN
                IF(UPSD1.EQ.4.0.OR.(UPSD1.EQ.0.0.AND.PROD.NE.'01'))THEN
                  HT2PRD = UPSHT1
                  UPSHT1 = 0.0
                ENDIF
              ENDIF
            ENDIF
            
            CALL R9CLARK (VOLEQ,STUMP,MTOPP,MTOPS,DBHOB,HT1PRD,HT2PRD,
     +                HTTOT, LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,CUTFLG,
     +                BFPFLG,CUPFLG, CDPFLG,SPFLG,PROD,ERRFLAG,CTYPE,
     +                UPSHT1,TLOGS,NOLOGP,NOLOGS)
          ELSE   
C         The old R8 Clark equation          
C ADDED TO TEST R8 CLARK PROFILE FOR LOG BOARDFOOT VOLUME        
            CALL R8CLARK(VOLEQ,FORST,STUMP,MTOPP,MTOPS,DBHOB,HT1PRD,
     +             HT2PRD,HTTOT, LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,CUTFLG,
     +             BFPFLG,CUPFLG, CDPFLG,SPFLG,PROD,ERRFLAG,CTYPE,
     +             UPSHT1,TLOGS,NOLOGP,NOLOGS)
          ENDIF
C 04/19/2016
C PUT THE INTL BDFT TO VOL(2) FOR GW/JF(08),OUACHITA(09),OZARK-ST FRANCIS(10) 
C AND ALL OTHER RD (EXCEPT ANDREW PICKENS(02)) OF FRANCIS MARION & SUTTER(12)
          IF(IFORST.EQ.8.OR.IFORST.EQ.9.OR.IFORST.EQ.10.OR.
     +       (IFORST.EQ.12.AND.IDIST.NE.2)) THEN
             VOL(2) = VOL(10)
          ENDIF  
        ELSE        
          CALL R8VOL (VOLEQ,DBHOB,HTTOT,UPSHT1,HT1PRD,MTOPP,PROD,VOL,
     +                FORST,SI,BA,CTYPE,BFPFLG,CUPFLG,SPFLG,ERRFLAG)
        ENDIF
      ELSEIF (VOLEQ(1:1).EQ.'9' .AND. 
     +       (MDL.EQ.'CLK' .OR. MDL.EQ.'clk')) THEN
!********************
!    REGION 9 MODEL  * 
!********************

!... need to add NOLOGP,NOLOGS,TLOGS to this call for log volumes      
          CALL R9CLARK (VOLEQ,STUMP,MTOPP,MTOPS,DBHOB,HT1PRD,HT2PRD,
     +                HTTOT, LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,CUTFLG,
     +                BFPFLG,CUPFLG, CDPFLG,SPFLG,PROD,ERRFLAG,CTYPE,
     +                UPSHT1,TLOGS,NOLOGP,NOLOGS)
          
          IF(IFORST.EQ.4 .OR. IFORST.EQ.5 .OR. IFORST.EQ.8 .OR. 
     +       IFORST.EQ.11 .OR. IFORST.EQ.12 .OR. IFORST.EQ.14 .OR. 
     +       IFORST.EQ.19 .OR. IFORST.EQ.20 .OR. IFORST.EQ.21 .OR. 
     +       IFORST.EQ.22 .OR. IFORST.EQ.24 .OR. IFORST.EQ.30)THEN
!              Put international bdft volumes into vol(2) bucket
               VOL(2) = VOL(10) 
          ENDIF  

      ELSEIF (MDL.EQ.'DEM' .OR. MDL.EQ.'dem' .OR.  
     +        MDL.EQ.'CUR' .OR. MDL.EQ.'cur' .OR.
     +        MDL.EQ.'BRU' .OR. MDL.EQ.'bru') THEN
!***********************
!    REGION 10 MODEL  * 
!***********************
         IF(HTTOT.LE.0.AND.HT1PRD.GT.0)THEN
             CALL R10HTS(VOLEQ,HTTOT,HT1PRD,DBHOB,HTTYPE,STUMP,MTOPP,
     >          LMERCH)
         ENDIF
         IF ((VOLEQ(1:3).EQ.'A01'.OR.VOLEQ(1:3).EQ.'A02' .or.
     &        VOLEQ(1:3).EQ.'a01'.OR.VOLEQ(1:3).EQ.'a02' ) .or.
     &     (((HTTYPE.EQ.'F'.AND.HTTOT.LE.40) .OR. DBHOB.LT.9.0) .AND.
     &      REGN.EQ.10)) THEN
      
            CALL R10VOL(VOLEQ,MTOPP,MTOPS,HTTOT,HT1PRD,DBHOB,
     &          HTTYPE,VOL,NOLOGP,NOLOGS,TLOGS,LOGLEN,LOGDIA,LOGVOL,
     &          BFPFLG,CUPFLG,SPFLG,errflag)
         ELSE
            CALL PROFILE (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +         HTTYPE, HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +         UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,
     +         LOGVOL,VOL, TLOGS,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,
     +         CDPFLG,SPFLG, DRCOB,CTYPE,FCLASS,PROD,ERRFLAG)
         ENDIF

      ELSEIF(MDL.EQ.'SN2') THEN
!********************
!    HAWAII MODEL  * 
!********************
        if(FCLASS.eq.0) then
           ERRFLAG = 2
        else
          call R12VOL(VOLEQ,MTOPP,HT1PRD,DBHOB,HTTOT,VOL,NOLOGP,
     &                NOLOGS,FCLASS,CUTFLG,BFPFLG,CUPFLG,errflag)
          TLOGS = ANINT(NOLOGP + NOLOGS)
        endif

      ELSEIF (MDL.EQ.'DVE' .OR. MDL.EQ.'dve') THEN
!********************
!  DVE MODELS FOR  * 
!    REGION 1      *
!    REGION 2      *
!    REGION 3      *
!    REGION 5      *
!    REGION 9      *
!    ARMY BASE     * 
!********************
        IF(VOLEQ.EQ.'223DVEW122')THEN
          CALL BH_NonSawPP(DBHOB,HTTOT,STUMP,MTOPP,MTOPS,
     +      HT1PRD,HT2PRD,VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,
     +      NOLOGP,NOLOGS,SPFLG,PROD,ERRFLAG)
        ELSE
          CALL DVEST (VOLEQ,DBHOB,DRCOB,HTTOT,MTOPP,FCLASS,HTLOG,
     +              HT1PRD,HT2PRD, FORST,BTR,VOL,CUTFLG,BFPFLG,CUPFLG,
     +              CDPFLG,SPFLG,PROD,HTTYPE,HTTFLL,NOLOGP,LIVE,BA,
     +              SI,CTYPE,errflag,MTOPS,STUMP)
        ENDIF
      ELSEIF (MDL.EQ.'HAN' .OR. MDL.EQ.'han')THEN
!******************************
!  HANN PROFILE MODEL FOR BLM * 
!******************************
          SPECIES = VOLEQ(8:10)
          READ(SPECIES,'(i3)') SPCODE

          IF(VOLEQ(2:3) .EQ. '32') THEN
              MAXLEN = 32
              TRIM = 8.0
          ELSE
              MAXLEN = 16
              TRIM = 4.0
          ENDIF
!          CALL VOLCAL(SPCODE,MTOPP,STUMP,MAXLEN,0.0,MTOPP,STUMP,
!      +                TRIM,DBHOB,HTTOT,
!     &       CR,VERROR,TERROR,VWARNING,TWARNING,IERROR,VOL(4),VOL(2))

      ELSEIF (MDL.EQ.'HAB' .OR. MDL.EQ.'hab')THEN
!******************************
!  HANN and Bare equation for Ponderosa pine young-growth and old-growth
!  blackjack pine and Yellow pine
!******************************
        CALL HANN_PP(VOLEQ,DBHOB,HTTOT,PROD,MTOPP,VOL,ERRFLAG)
      ELSE 
!        ERROR MESSAGE
         ERRFLAG = 1
         DO 5 I=1,15
            VOL(I)=0.0
 5       CONTINUE
         DO 10 I=1,21
            BOLHT(I)=0.0
 10      CONTINUE
         DO 15 I=1,7
            DO 16 J=1,20
               LOGVOL(I,J)=0.0
 16         CONTINUE
 15      CONTINUE
         DO 17,I=1,3
            DO 18,J=1,21
               LOGDIA(J,I)=0.0
 18         CONTINUE
 17      CONTINUE
         DO 19, I=1,20
            LOGLEN(I) = 0.0
 19      CONTINUE 
      ENDIF
      IF(VOL(7).LT.0.0) VOL(7) = 0.0
C  calc Tip volume and save to VOL(15)
      IF(ERRFLAG.GT.0) RETURN
      IF(VOL(14).LE.0.0.AND.STUMP.GT.0) THEN  !start stump vol calc
      !Do not calculate stump vol for STUMP=0    
        !IF(STUMP.LE.0) STUMP = 1.0
C  calc stump DIB
          HTUP = STUMP
          IF((REGN.EQ.9.OR.(VOLEQ(1:1).EQ.'8'.AND.VOLEQ(3:3).EQ.'1'))
     +    .AND.HTTOT.EQ.0)THEN
c        UPSHT1=HT1PRD
c        UPSD1 = MTOPP
c        UPSHT2 = HT2PRD
c        UPSD2 = MTOPS
          CALL CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,
     &    DRCOB,HTTOT,HT1PRD,HT2PRD,MTOPP,MTOPS,HTREF,AVGZ1,
     &    AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)   

        ELSE
          CALL CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,
     &    DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,
     &    AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)   
        ENDIF
!     Not returning error code from stump DIB calc 12/28/2022 (YW)
        IF(ERRFLAG.GT.0) ERRFLAG = 0
        IF(DIB.GT.0.0) VOL(14)=0.005454154*DIB**2*STUMP
C  If stump DIB is not calculated, use the following to calculate stump VOL         
        IF(VOL(14).LE.0.0)THEN
          SPECIES = VOLEQ(8:10)
          READ(SPECIES,'(i3)') SPEC
          HTUP = STUMP
          CALL RAILEVOL(SPEC, DBHOB, HTUP, VOLIB, VOLOB)
          VOL(14) = VOLIB
        ENDIF
      ENDIF  !end stump vol calc
      
!     Calculate the tip volume as the volume above last log
      IF(TLOGS.GT.0.AND.LOGDIA(TLOGS+1,2).GT.0.AND.VOL(15).EQ.0)THEN
          MHT = 0.0
          IF(BOLHT(TLOGS+1).GT.0.0)THEN
            MHT = BOLHT(TLOGS+1)
          ELSEIF(HT2PRD.GT.0)THEN
            MHT = HT2PRD
          ELSEIF(HT1PRD.GT.0)THEN
            MHT = HT1PRD
          ELSE
            MHT = STUMP
            TRIM = 0.5
            DO 30 I=1,TLOGS
              MHT = MHT + LOGLEN(I) + TRIM
 30         CONTINUE
          ENDIF
          IF(MHT.GT.0.0)THEN
          VOL(15) =0.002727*LOGDIA(TLOGS+1,2)*LOGDIA(TLOGS+1,2)
     +     *(HTTOT-MHT)  
          ENDIF     
      ENDIF      
C YW comment out the following on 3/20/2017
c      IF(VOL(15).LT.0.01 .AND. VOL(4).GT.0.0)THEN
c        VOL(15) = VOL(1)-VOL(4)-VOL(7)-VOL(14)
c      ENDIF
       IF(VOL(15).LT.0.0) VOL(15) = 0.0
C YW R3 requests to use 80 for CF to Cord 2019/05/16
      IF(REGN.EQ.3) VOL(6)=VOL(4)/80.0
C YW R6 requests to use 102.4 for cuft to Cord 2020/03/18
      IF(REGN.EQ.6) VOL(6) = AINT((VOL(4)/102.4)*10)/10.0     
C YW Set the total cubic vol to VOL(4) for R2 if MTOPP=0.1 (2022/05/03)
      IF(REGN.EQ.2.AND.MTOPP.EQ.0.1)THEN
        IF(VOL(1).GT.0.0) THEN
          VOL(4) = VOL(1)
        ENDIF
      ENDIF
      !Added VOL(1) > 0 check (20240426)
      IF(VOL(1).LT.0) VOL(1) = 0
      !Set the log weight to LOGVOL for CTYPE ='C' (Cruise only) 20250219
      IF(CTYPE.EQ.'C')THEN
          SPCD = 0
          IF(VERIFY(CONSPEC,"0123456789 ").EQ.0) THEN
              READ(CONSPEC,'(I3)') SPCD
          ENDIF
          IF(SPCD.EQ.0) READ(VOLEQ(8:10),'(I3)') SPCD
          CALL GetRegnWF(REGN,FORST,SPCD,GRNWF,DeadWF,PROD)
          WtFac2 = GRNWF
          IF(NOLOGS.GT.0)THEN
              PROD2 = '20'
              CALL GetRegnWF(REGN,FORST,SPCD,WtFac2,DeadWF,PROD2)
          ENDIF
          IF(LIVE.EQ.'L')THEN
              CALL CruiseLogWt(LOGVOL,NOLOGP,NOLOGS,GRNWF,WtFac2)
          ELSE
              CALL CruiseLogWt(LOGVOL,NOLOGP,NOLOGS,DeadWF,DeadWF)
          ENDIF
      ENDIF
      IF (DEBUG%MODEL) THEN
        WRITE  (LUDBG, 100)'FORST VOLEQ     MTOPP HTTOT HT1PRD DBHOB 
     &   HTTYPE FCLASS'
500     FORMAT (A)
  		  WRITE  (LUDBG, 104)FORST,VOLEQ,MTOPP,HTTOT,HT1PRD,DBHOB,HTTYPE,
     +    FCLASS
504     FORMAT(A,2X,A, 1X, F5.1, 1X, F5.1, 2X, F5.1, 2X, F5.1, 5X,
     &    A, 1X, I5)
      
      
        WRITE (LUDBG, 300)'TLOGS NOLOGP NOLOGS CONSPEC 
     &    BFPFLG CUPFLG ERRFLAG'
  508   FORMAT (A)
  		  WRITE (LUDBG, 320)TLOGS,NOLOGP,NOLOGS,CONSPEC,BFPFLG,CUPFLG,
     +    ERRFLAG
  520   FORMAT(1X, I2, 2X, F5.1, 2X, F5.1, 5X, A,1X, I5, I5, 1X,I5)
       ENDIF
      
      IF (DEBUG%MODEL) THEN
       
        WRITE  (LUDBG, 600)'VOL(1) VOL(2) VOL(4) ', VOL(1),VOL(2),VOL(4)
  600   FORMAT (A, 2x, F8.4, F8.4, F8.4)
  
      ENDIF
      
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 20) ' <--Exit VOLINIT'
   20    FORMAT (A//)   
   		END IF

!--------manual debugging code-----------------------------------------     

!      IF(SPCODE .EQ. 204) THEN
!         CLOSE(LUDBG)
!      ENDIF
!----------------------------------------------------------------------

 4000 RETURN
      END SUBROUTINE VOLINIT
!----------------------------------------------------------------------
! This subroutine volinitnew enable the use of new National-scale volume and biomass equation
      !2023/03/21 Changed the subroutine name to volinitnvb and reset CTYPE for non-NVB Eq
      SUBROUTINE VOLINITNVB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,CR,CULL,DECAYCD,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST,BRKHT,BRKHTD,FIASPCD,DRYBIO,
     +    GRNBIO,MRULEFLG,MERRULES,CULLMSTOP)
      USE CHARMOD
      USE MRULES_MOD
      USE VOLINPUT_MOD
      IMPLICIT NONE
      INTEGER REGN,HTREF,FCLASS,I3,I7,I15,I20,I21,HTTFLL,BA,SI,IDIST
      INTEGER CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG,HTLOG,TLOGS
      CHARACTER*11 VOLEQ,NVBEQN
      CHARACTER*10 V_EQN
      CHARACTER*2 FORST,PROD,DIST,PROD2
      CHARACTER*1 LIVE,CTYPE,HTTYPE
      CHARACTER*4 CONSPEC
      REAL MTOPP,MTOPS,STUMP,DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD
      REAL UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2,DBTBH,BTR,NOLOGP,NOLOGS
      REAL VOL(15),LOGVOL(7,20),LOGDIA(21,3),LOGLEN(20),BOLHT(21)
      REAL NVBVOL(15),NVBLOGVOL(7,20),NVBLOGDIA(21,3),NVBLOGLEN(20)
      REAL NVBBOLHT(21),CR,CULL,NVBNOLOGP,NVBNOLOGS
      INTEGER FIASPCD,MRULEFLG,DECAYCD,ERRFLAG2,I,J,NVBTLOGS
      REAL BRKHT,BRKHTD,DRYBIO(15),GRNBIO(15),Vfactor,Rwood
      CHARACTER*3 SPCD
      TYPE(MERCHRULES)::MERRULES
      INTEGER SPGRPCD,SFTHRD,STEMS,EcoProv
      REAL WDSG, CF, SPGRNWF, SPDRYWF, MC,SPREGNWF,DeadWF,BIOMS(8)
      REAL NVBHT1PRD,NVBHT2PRD, DecayProp,CV15,CULLMSTOP,FOLIAGE,BrchRem
      REAL WtPrim,WtPrimWd,WtPrimBk,WtTW,WtTWwd,WtTWbk,WtTip,WtMstemNVB
      REAL WtTipWd,WtTipBk,WTStem,WtStemWd,WtStemBk,WF,WF2,WtMerchStem
      REAL Rprim,WtMstmNVBdry,WtTipDiff,WtTipDiffDry,DenProp,DeadCF

      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 = 21
      ERRFLAG = 0
      VOL = 0
      DRYBIO = 0
      GRNBIO = 0
      LOGVOL = 0
      LOGDIA = 0
      LOGLEN = 0
      BOLHT = 0
      TLOGS = 0
      NOLOGP = 0
      NOLOGS = 0
      NVBTLOGS = 0
      NVBNOLOGP = 0
      NVBNOLOGS = 0
      NVBHT1PRD = HT1PRD
      NVBHT2PRD = HT2PRD
      Vfactor = 1
      ERRFLAG2 = 0
      IF(IDIST.EQ.0) IDIST = 1
      WRITE (DIST, '(I2)') IDIST
      IF(FORST(2:2).LT.'0'.OR.FORST(2:2).GT.'9') THEN
        FORST(2:2) = FORST(1:1)
	    FORST(1:1) = '0'
        IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF
      !Check LIVE input to be L or D
      IF(LIVE.NE.'D') LIVE = 'L'
      !MRULEFLG = 1 indicates using modified merch rules for log segmentation
      IF(MRULEFLG.GT.0)THEN
         MRULEMOD = 'Y'
          NEWCOR = MERRULES%COR
          NEWEVOD = MERRULES%EVOD
          NEWOPT = MERRULES%OPT
          NEWMAXLEN = MERRULES%MAXLEN
          NEWMINLEN = MERRULES%MINLEN
          NEWMERCHL = MERRULES%MERCHL
          NEWMINLENT = MERRULES%MINLENT
          NEWMTOPP = MERRULES%MTOPP
          NEWMTOPS = MERRULES%MTOPS
          NEWSTUMP = MERRULES%STUMP
          NEWTRIM = MERRULES%TRIM
          NEWBTR = MERRULES%BTR
          NEWDBTBH = MERRULES%DBTBH
          NEWMINBFD = MERRULES%MINBFD 
      ENDIF
      !Reset the species code for R10 second grow species(2024/10/07)
      IF(FIASPCD.EQ.2042) FIASPCD = 42
      IF(FIASPCD.EQ.2098) FIASPCD = 98
      IF(FIASPCD.EQ.2242) FIASPCD = 242
      IF(FIASPCD.EQ.2263) FIASPCD = 263
      IF(FIASPCD.EQ.204) FIASPCD = 202
      !Calculate biomass for trees with only DBH (no HT) using Jenkins (2021/11/01)
      IF((DBHOB.GE.0.1.OR.DRCOB.GE.0.1).AND.
     + (HTTOT.LT.1.AND.HT1PRD.LT.1.AND.HT2PRD.LT.1.AND.
     + UPSHT1.LT.1.AND.UPSHT2.LT.1.).AND.CTYPE.EQ.'B')THEN
          IF(DRCOB.GE.1.AND.DBHOB.LT.1) DBHOB = DRCOB
          BIOMS = 0
          CALL JENKINS(FIASPCD,DBHOB,BIOMS)
          !Set the BIOMS component to variable DRYBIO
          DRYBIO(1) = BIOMS(1) - BIOMS(4)
          DRYBIO(6) = BIOMS(2)
          DRYBIO(7) = BIOMS(3)
          DRYBIO(12) = BIOMS(6)
          DRYBIO(13) = BIOMS(4)
          DRYBIO(14) = BIOMS(7)
          IF(LIVE.EQ.'D') DRYBIO(13) = 0
          !Get weight factor to calculate green weight, also the carbon fraction
          CALL NVB_RefSpcData(FIASPCD,SPGRPCD,WDSG,SFTHRD,CF,
     &        ERRFLAG,SPGRNWF,SPDRYWF)
          DRYBIO(15) = DRYBIO(1)*CF
          CALL GetRegnWF(REGN,FORST,FIASPCD,SPGRNWF,DeadWF,PROD)
          IF(LIVE.EQ.'L')THEN
              MC = (SPGRNWF-SPDRYWF)/SPDRYWF
          ELSE
              MC = (DeadWF-SPDRYWF)/SPDRYWF
          ENDIF
          GRNBIO = DRYBIO*(1+MC)
          RETURN
      ENDIF    
      !End Jenkins biomass calculation for trees with DBH only
      
      IF(VOLEQ(1:3).EQ.'NVB')THEN
          CALL NVBC(REGN,FORST,DIST,VOLEQ,DBHOB,HTTOT,MTOPP,MTOPS,
     + HT1PRD,HT2PRD,STUMP,PROD,BRKHT,BRKHTD,LIVE,CR,CULL,DECAYCD,
     + LOGLEN,LOGDIA,LOGVOL,BOLHT,
     + TLOGS,NOLOGP,NOLOGS,VOL,DRYBIO,GRNBIO,ERRFLAG,FIASPCD,CTYPE)
      ELSE
          V_EQN = VOLEQ(1:10)
          !When using FIA equation number like CU000001 or BD000001, the FIASPCD is passed in from CONSPEC variable
          !IF(VOLEQ(1:2).EQ.'CU'.OR.VOLEQ(1:2).EQ.'BD') THEN
          IF(FIASPCD.GT.9)THEN
              IF(FIASPCD.GT.999)THEN
                  WRITE(CONSPEC, '(I4)') FIASPCD
              ELSEIF(FIASPCD.GT.99)THEN
                  WRITE(CONSPEC, '(I3)') FIASPCD
              ELSEIF(FIASPCD.GT.9)THEN
                  WRITE(CONSPEC, '(I2)') FIASPCD
              ENDIF
          ENDIF
          CALL VOLINIT(REGN,FORST,V_EQN,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
          IF(ERRFLAG.GT.0) RETURN
          IF(FIASPCD.EQ.0) READ (V_EQN(8:10),'(I3)') FIASPCD
          IF(FIASPCD.EQ.204) FIASPCD = 202
          CALL NVB_DefaultEq(REGN,FORST,DIST,FIASPCD,NVBEQN,ERRFLAG)
          IF(ERRFLAG.GT.0) THEN
              !Check if woodland species and calculate biomass based FIA equation
              CALL NVB_RefSpcData(FIASPCD,SPGRPCD,WDSG,SFTHRD,CF,
     &            ERRFLAG,SPGRNWF,SPDRYWF)
              IF(SPGRPCD.EQ.10)THEN
                  ERRFLAG = 0
                  STEMS = FCLASS
                  IF(DRCOB.LT.0.1.AND.DBHOB.GE.0.1) DRCOB=DBHOB
                  !FIA adjust missing top VOL using CULLMSTOP (20240426)
                  CV15 = VOL(1)
                  IF(CULLMSTOP.GT.0) CV15 = CV15*(1-CULLMSTOP/100)
                  IF(REGN.EQ.5.OR.REGN.EQ.6)THEN
                      !For woodland species in pacific NW calculated as CVT*WDDEN
                      DRYBIO(1) = CV15*WDSG
                  ELSE
                      !For woodland species in  RockyMountain region   
                    CALL WOODLAND_BIO(FIASPCD, DRCOB,HTTOT,STEMS,CV15,
     &              DRYBIO,ERRFLAG)
                  ENDIF
                      
                  !Calculate foliage using Jenkins equation. This is to match FIA foliage calculation
                  !Calculate foliage for LIVE tree only    
                  BIOMS = 0
                  CALL JENKINS(FIASPCD,DRCOB,BIOMS)
                  FOLIAGE = BIOMS(4)
                  IF(LIVE.EQ.'D') FOLIAGE = 0
                  !Adjust foliage for missing top tree
                  IF(BRKHT.GT.0.AND.BRKHT.LT.HTTOT)THEN
                      CALL NVB_EcoProv(REGN,FORST,DIST,EcoProv)
                      IF(LIVE.EQ.'L'.AND.CR.EQ.0) CR = 1
                      CALL NVB_BrchRem(EcoProv,FIASPCD,BRKHT,HTTOT,CR,
     &                  BrchRem)
                      FOLIAGE = FOLIAGE*BrchRem
                  ENDIF
                  !Reset the foliage result
                  !DRYBIO(13) = FOLIAGE
                  !calc carbon
                  DRYBIO(15) = DRYBIO(1)*CF
                  !Adhust CULL for VOL and DRYBIO (20240428)
                  VOL(1) = VOL(1)*(1-(CULL + CULLMSTOP)/100)
                  VOL(4) = VOL(4)*(1-(CULL + CULLMSTOP)/100)
                  IF(LIVE.EQ.'L'.AND.CULL.GT.0)THEN
                    DecayProp = 0.92
                    IF(FIASPCD.GE.300) DecayProp = 0.54
                    DRYBIO = DRYBIO*(1.0 - CULL/100 * (1.0 - DecayProp))  
                  !ENDIF
                  !Adjust wood denProp besed on DECAYCD (2025/05/08)
                  !IF(DECAYCD.GT.0)THEN
                  ELSEIF(LIVE.EQ.'D')THEN
                      IF(DECAYCD.EQ.0) DECAYCD = 3
                      CALL DecayDenProp(SFTHRD,DECAYCD,DenProp,DeadCF)
                      DRYBIO = DRYBIO*DenProp
                      DRYBIO(15) = DRYBIO(1)*DeadCF
                  ENDIF
                  !Reset the foliage result
                  DRYBIO(13) = FOLIAGE
                  !calc green weight
                  CALL GetRegnWF(REGN,FORST,FIASPCD,SPGRNWF,DeadWF,PROD)
                  IF(LIVE.EQ.'L')THEN
                      MC = (SPGRNWF-SPDRYWF)/SPDRYWF
                  ELSE
                      MC = (DeadWF-SPDRYWF)/SPDRYWF
                  ENDIF
                  GRNBIO = DRYBIO*(1+MC)
              ENDIF
              IF(ERRFLAG.GT.0) ERRFLAG = 0
              RETURN
          ENDIF
          !NVBVOL = 0
          !NVBLOGVOL = 0
          !NVBLOGLEN = 0
          !NVBLOGDIA = 0
          !NVBBOLHT = 0
          
          DO 102,I=1,20
             DO 103, J=1,7
               NVBLOGVOL(J,I) = 0.0
  103        CONTINUE        
             NVBLOGDIA(I,1) = 0.0
             NVBLOGDIA(I,2) = 0.0
             NVBLOGDIA(I,3) = 0.0
             NVBLOGLEN(I) = 0.0
             NVBBOLHT(I) = 0.0
  102     CONTINUE        
       
          DO 104, I=1,15
             NVBVOL(I) = 0.0
  104     CONTINUE
          
          !Set CTYPE to B to make the calc same as FIA except the MTOPP and MTOPS
          IF(CTYPE.NE.'I') CTYPE = 'B'
          CALL NVBC(REGN,FORST,DIST,NVBEQN,DBHOB,HTTOT,MTOPP,MTOPS,
     +    NVBHT1PRD,NVBHT2PRD,STUMP,PROD,BRKHT,BRKHTD,
     +     LIVE,CR,CULL,DECAYCD,NVBLOGLEN,NVBLOGDIA,
     +    NVBLOGVOL,NVBBOLHT,NVBTLOGS,NVBNOLOGP,NVBNOLOGS,NVBVOL,
     +    DRYBIO,GRNBIO,ERRFLAG2,FIASPCD,CTYPE)
          
          !Recalculate GRNBIO using freen weight factor and VOL from cruise VOLEQ(20241114)
          CALL GetRegnWF(REGN,FORST,FIASPCD,SPGRNWF,DeadWF,PROD)
          !get the secondary weight factor
          PROD2='20'
          WF2 = 0
          CALL GetRegnWF(REGN,FORST,FIASPCD,WF2,DeadWF,PROD2)
          IF(LIVE.EQ.'L')THEN
              WF = SPGRNWF
          ELSE
              WF = DeadWF
              WF2 = WF
          ENDIF
          !calculate moisture content
          IF(DRYBIO(1).GT.0) THEN
              MC = GRNBIO(1)/DRYBIO(1) - 1.0
          ELSE
              CALL NVB_RefSpcData(FIASPCD,SPGRPCD,WDSG,SFTHRD,CF,
     &        ERRFLAG,SPGRNWF,SPDRYWF)
              MC = (WF - SPDRYWF)/SPDRYWF
          ENDIF
          WtPrim = 0
          WtPrimWd = 0
          WtPrimBk = 0
          WtTW = 0
          WtTWwd = 0
          WtTWbk = 0
          WtTip = 0
          WtTipWd = 0
          WtTipBk = 0
          WtStem = 0
          WtStemWd = 0
          WtStemBk = 0
          IF(WF2.LT.1) WF2 = WF
          WtPrim = VOL(4)*WF
          WtTW = VOL(7)*WF2
          WtMerchStem = WtPrim + WtTW
          Rprim = 1
          IF(WtMerchStem.GT.0) Rprim = WtPrim/WtMerchStem
          WtMstemNVB = GRNBIO(6)+GRNBIO(7)+GRNBIO(8)+GRNBIO(9)
          Vfactor = 1
          IF(WtMstemNVB.GT.0.AND.WtMerchStem.GT.0) THEN
              Vfactor=WtMerchStem/WtMstemNVB
              Rwood = (GRNBIO(6)+GRNBIO(8))/WtMstemNVB
          ELSE
              WtStem = VOL(1)*WF
              IF((GRNBIO(2)+GRNBIO(3)).GT.0) THEN
                  Vfactor = WtStem/(GRNBIO(2)+GRNBIO(3))
                  Rwood = GRNBIO(2)/(GRNBIO(2)+GRNBIO(3))
              ELSE
                  Vfactor = 1
                  Rwood = 1
              ENDIF
          ENDIF
          IF(Vfactor.LE.0) Vfactor = 1
          DRYBIO = DRYBIO*Vfactor
          GRNBIO = GRNBIO*Vfactor
          !Reset GRNBIO stem component
          WtMstmNVBdry = DRYBIO(6)+DRYBIO(7)+DRYBIO(8)+DRYBIO(9)
          IF(VOL(4).GT.0) THEN
              GRNBIO(6) = WtPrim*Rwood
              GRNBIO(7) = WtPrim*(1.0-Rwood)
              DRYBIO(6) = Rprim*WtMstmNVBdry*Rwood
              DRYBIO(7) = Rprim*WtMstmNVBdry*(1.0-Rwood)
          ELSE
              GRNBIO(6) = 0
              GRNBIO(7) = 0
              DRYBIO(6) = 0
              DRYBIO(7) = 0
          ENDIF
          IF(VOL(7).GT.0) THEN
              GRNBIO(8) = WtTW*Rwood
              GRNBIO(9) = WtTW*(1.0-Rwood)
              DRYBIO(8) = (1.0-Rprim)*WtMstmNVBdry*Rwood
              DRYBIO(9) = (1.0-Rprim)*WtMstmNVBdry*(1.0-Rwood)
          ELSE
              GRNBIO(8) = 0
              GRNBIO(9) = 0
              DRYBIO(8) = 0
              DRYBIO(9) = 0
          ENDIF
          IF(VOL(15).GT.0) THEN
              WtTip = VOL(15)*WF2
              WtTipDiff = WtTip-GRNBIO(10)-GRNBIO(11)
              WtTipDiffDry = WtTipDiff/(1.0+MC)
              GRNBIO(10) = WtTip*Rwood
              GRNBIO(11) = WtTip*(1.0-Rwood)
              DRYBIO(10) = GRNBIO(10)/(1.0+MC)
              DRYBIO(11) = GRNBIO(11)/(1.0+MC)
          ENDIF
          !Adjust AGB and main stem total
          GRNBIO(2) = GRNBIO(4)+GRNBIO(6)+GRNBIO(8)+GRNBIO(10)
          GRNBIO(3) = GRNBIO(5)+GRNBIO(7)+GRNBIO(9)+GRNBIO(11)
          DRYBIO(2) = DRYBIO(4)+DRYBIO(6)+DRYBIO(8)+DRYBIO(10)
          DRYBIO(3) = DRYBIO(5)+DRYBIO(7)+DRYBIO(9)+DRYBIO(11)
          GRNBIO(1) = GRNBIO(2)+GRNBIO(3)+GRNBIO(12)
          DRYBIO(1) = DRYBIO(2)+DRYBIO(3)+DRYBIO(12)
      ENDIF
      RETURN
      END
!----------------------------------------------------------------------
      !Set the log weight in LOGVOL for CTYPE = Cruise
      !Set the log weight to LOGVOL(7,I) which is Intl BDFT for other CTYPE
      SUBROUTINE CruiseLogWt(LOGVOL,NOLOGP,NOLOGS,WTFAC,WTFAC2)
      IMPLICIT NONE
      REAL LOGVOL(7,20),WTFAC,WTFAC2
      REAL NOLOGP,NOLOGS
      INTEGER NLOGP,NLOGS,I,J
      NLOGP = CEILING(NOLOGP)
      NLOGS = CEILING(NOLOGS)
      IF(NLOGP.GE.1) THEN
          DO I = 1, NLOGP
              LOGVOL(7,I) = LOGVOL(4,I)*WTFAC
          ENDDO
      ENDIF
      IF(NLOGS.GE.1)THEN
          DO J = 1+NLOGP, NLOGP+NLOGS
              LOGVOL(7,J) = LOGVOL(4,J)*WTFAC2
          ENDDO
      ENDIF
      RETURN    
      END