!== last modified  3-29-2004
      SUBROUTINE R2OLDV(VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG,PROD,
     & MTOPP)
      IMPLICIT NONE
C--  THIS SUBROUTINE DETERMINES THE VOLUME OF A TREE
C--  USING REGION 2 D*D*H VOLUME DETERMINATION EQUATIONS.
! YW 2018/09/05 Added International boardfoot volume and sawlog portion cubic volume calculation
! for 200DVEW122, 200DVEW746, 200DVEW108, 200DVEW093 and 212DVEW122. This calculation
! is from FIA volume codes. 
! YW 2018/09/06 Added 201DEVW746 AND MYERS(1964) RM-6 TOP=8 (Scribner and international)
! YW 2018/09/12 Changed the FCLASS to be: 1 = sngle, others = multistem
!               This is to make it consistent with R3 DVE equation.
! YW 2022/08/05 Added new equation (223DVEW122) for Black Hills non-saw product (DBH<9)
!               also add the input variable STUMP to the subroutine
! YW 2022/11/22 Moved the codes for equation 223DVEW122 to its own subroution
C**************************************************************

      CHARACTER*10 VOLEQ
      INTEGER ERRFLAG,MSTEM,FCLASS,I
      CHARACTER*2 PROD
      REAL MTOPP,TOPWOOD,CV6,SPF,DIB,X1,X2,X3
      REAL DBHOB,DRC,D2H,HTTOT,VOL(15),grsbdt,GCUFT,TCUFT,trevol

      TREVOL = 0.0
      grsbdt=0.0
      GCUFT=0.0
      TCUFT=0.0
      ERRFLAG = 0
!     Changed the FCLASS to be: 1 = single, others = multistem
!     2018/09/12
      IF(FCLASS .EQ. 1) THEN
         MSTEM = 1
      ELSE
         MSTEM = 0
      ENDIF

C--   IF DBHOB OR HTTOT EQUALS ZERO THEN DON'T CALCULATE THE VOLUME

      IF (DBHOB .LE. 0.0 .AND. DRC.LE.0.0) THEN
         ERRFLAG = 3
         GO TO 1000
      ENDIF
      IF( HTTOT .LE. 0.0)THEN
         ERRFLAG = 4
         GO TO 1000
      ENDIF

      DO 100, I=1,15
         VOL(I) = 0
  100 CONTINUE
C--   CALCULATE THE DIAMETER SQUARED TIMES HEIGHT

      D2H = (DBHOB**2)*(HTTOT)

C--***********  ASPEN-RM232-TOTAL PG3  TOTAL CUBIC

      IF (VOLEQ(8:10).EQ.'746' .AND. VOLEQ(1:3).EQ.'200') THEN

         IF ( D2H .LE. 12470. ) THEN
            TCUFT = 0.002219 * D2H
         ELSE
            TCUFT = 0.001896 * D2H + 4.0267
         ENDIF

C--**************  ASPEN-RM232-TOP=6   BOARD FOOT
         IF(DBHOB.GT.7)THEN
             IF ( D2H .LE. 2500. ) THEN
                grsbdt = 8
             ELSEIF ( D2H .LE. 8850. ) THEN
                grsbdt = 0.011389 * D2H - 20.5112
             ELSE
                grsbdt = 0.010344 * D2H - 11.2615
             ENDIF
! INTERNATIONAL BOARDFOOT VOLUME (YW 2018/0905)
            IF(D2H.LE.11460) THEN
              VOL(10)= .013472*D2H-25.5968
            ELSE
              VOL(10) = .011989*D2H-8.6015
            ENDIF
         ENDIF
C--***********  ASPEN-RM232-TOP=4

         IF ( D2H .LE. 11800. ) THEN
            GCUFT = 0.002195 * D2H - 0.9076
         ELSE
            GCUFT = 0.001837 * D2H + 3.3075
         ENDIF
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.11.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .96
           ELSE
             SPF =.92-(.22*((42.0-DBHOB)/33.0)**1.4)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF
           
! --Aspen
!Peterson, Geraldine. 1961. Volume Tables for Aspen in Colorado. Forest Service, U.S. Department of Agriculture, Rocky Mountain
!Forest Range Experiment Station. Research Notes, No. 63. 4p.
!volume to 4" top DIB
      ELSEIF (VOLEQ(8:10).EQ.'746' .AND. VOLEQ(1:3).EQ.'201') THEN
         DIB = (0.8954*DBHOB)+0.3168
         X1 = LOG10(DIB-4.0)
         X2 = LOG10(HTTOT-4.5)
         X3 = X1*X2
         GCUFT = (10**((0.0827*X1)+(0.4045*X2)+(.6593*X3)-0.4721))+0.3
C--***********  ASPEN-RM232-TOTAL PG3

      ELSEIF (VOLEQ(8:10).EQ.'746' .AND. VOLEQ(1:3).EQ.'210') THEN

         IF ( D2H .LE. 12470. ) THEN
            GCUFT = 0.002219 * D2H
         ELSE
            GCUFT = 0.001896 * D2H + 4.0267
         ENDIF
C******************************************************************
C--*************  LODGEPOLE PINE-RM6-TOTAL PG5  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'108' .AND. VOLEQ(1:3).EQ.'200' ) THEN

         IF ( D2H .LE. 7000.) THEN
            TCUFT = 0.002777 * D2H + 0.027967
         ELSE
            TCUFT = 0.002332 * D2H + 3.446454
         ENDIF

C--*************  LODGEPOLE PINE-RM157-TOP=6   BOARD FOOT
         IF(DBHOB.GT.7)THEN

            IF ( D2H .LE. 22800. ) THEN
               grsbdt = 0.01202 * D2H - 6.00933
            ELSE
               grsbdt = 0.01263 * D2H - 19.76641
            ENDIF
! ADD INTERNATIONAL BOARDFOOT VOLUME FROM MYERS(1964) RM-6 TOP=8           
            IF(MTOPP.GE.8.0)THEN
              IF(D2H.LE.15000.0)THEN
                grsbdt = .012535*D2H-20.19057
                VOL(10) = 0.015097*D2H - 26.28782
              ELSE
                grsbdt = .012893*D2H -29.24566
                VOL(10) = 0.013766*D2H - 6.30999
              ENDIF
            ENDIF
         ENDIF
C--*************  LODGEPOLE PINE-RM6-TOP=4

         IF ( D2H .LE. 7000.) THEN
            GCUFT = 0.002798 * D2H - 1.04578
         ELSE
            GCUFT = 0.002256 * D2H + 2.836222
         ENDIF
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.9.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .95
           ELSE
             SPF =.98-(.10*((42.0-DBHOB)/33.0)**4.0)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF

C--*************  LODGEPOLE PINE-RM6-TOTAL PG5

      ELSEIF (VOLEQ(8:10).EQ.'108' .AND. VOLEQ(1:3).EQ.'210') THEN

         IF ( D2H .LE. 7000.) THEN
            GCUFT = 0.002777 * D2H + 0.027967
         ELSE
            GCUFT = 0.002332 * D2H + 3.446454
         ENDIF

C******************************************************************
C--**************  PONDEROSA PINE PROSSER BLACK HILLS TOTAL  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'203') THEN

         IF ( D2H .LT. 6000. ) THEN
            TCUFT = 0.0024506 * D2H + .3470564
         ELSE
            TCUFT = 0.0022325 * D2H + 3.2829984
         ENDIF

C--*************  PONDEROSA PINE PROSSER BLACK HILLS-TOP=6   BOARD FOOT
         IF(DBHOB.GT.7)THEN

            IF ( D2H .LT. 16000. ) THEN
               grsbdt = .0132429 * D2H - 26.0553842
            ELSE
               grsbdt = 0.0127305 * D2H - 15.5653183
            ENDIF
         ENDIF
C--**************  PONDEROSA PINE BLACK HILLS MYERS TOP=4

         IF ( D2H .LE. 6700. ) THEN
            GCUFT = 0.002297 * D2H - 1.032297
         ELSE
            GCUFT = 0.002407 * D2H - 2.257724
         ENDIF
C--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOT PG5

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'213') THEN

         IF ( D2H .LE. 6000. ) THEN
            GCUFT = 0.002213 * D2H + 0.030288
         ELSE
            GCUFT = 0.002474 * D2H - 1.557103
         ENDIF

C******************************************************************
C--**************  PONDEROSA PINE FRONT RANGE-RM218-TOTAL PG4  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'200') THEN

         TCUFT = 0.00226 * D2H

C--**************  PONDEROSA PINE FRONT RANGE-RM218-TOP=6   BOARD FOOT
         IF(DBHOB.GT.7)THEN

            IF ( D2H .LE. 2830. ) THEN
               grsbdt = 8
            ELSE
               grsbdt = 0.01149 * D2H - 24.5404
            ENDIF
!           ADDED CALCULATION OF INTERNATIONAL BD YW 2018/09/04            
            IF ( D2H .LE. 2535. ) THEN
              VOL(10) = 9.0
            ELSE
              VOL(10) = .01286*D2H-23.5932
            ENDIF
         ENDIF
C--**************  PONDEROSA PINE FRONT RANGE-RM218-TOP-4

         GCUFT = 0.00216 * D2H - 0.44670
         
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.9.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .93
           ELSE
             SPF =.95-(.26*((42.0-DBHOB)/33.0)**7.0)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF

C--**************  PONDEROSA PINE FRONT RANGE-RM218-TOTAL PG4

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'210') THEN

         GCUFT = 0.00226 * D2H


C******************************************************************
C--**************  ENGELMAN SPRUCE-RM95-TOTAL PG5  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'093' .AND. VOLEQ(1:3).EQ.'200') THEN

         IF ( D2H .LE. 22500. ) THEN
            TCUFT = 0.00239 * D2H + 0.06439
         ELSE
            TCUFT = 0.00193 * D2H + 10.41663
         ENDIF

C--**************  ENGELMANN SPRUCE-RM95-TOP=6   BOARD FOOT

         IF(DBHOB.GT.7)THEN

            IF ( D2H .LE. 12200. ) THEN
               grsbdt = 0.01097 * D2H - 15.14466
            ELSE
               grsbdt = 0.01202 * D2H - 27.91343
            ENDIF
            IF(D2H.LE.27300.0) THEN
              VOL(10)= .01391*D2H-25.61022
            ELSE
              VOL(10)= .01235*D2H+17.02079
            ENDIF
         ENDIF

C--**************  ENGELMAN SPRUCE-RM95-TOP=4

         IF ( D2H .LE. 27900. ) THEN
            GCUFT = 0.00232 * D2H - 0.83010
         ELSE
            GCUFT = 0.00182 * D2H + 13.11320
         ENDIF
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.9.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .98
           ELSE
             SPF =.96-(.15*((42.0-DBHOB)/33.0)**3.0)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF

C--**************  ENGELMAN SPRUCE-RM95-TOTAL PG5

      ELSEIF (VOLEQ(8:10).EQ.'093' .AND. VOLEQ(1:3).EQ.'210') THEN

         IF ( D2H .LE. 22500. ) THEN
            GCUFT = 0.00239 * D2H + 0.06439
         ELSE
            GCUFT = 0.00193 * D2H + 10.41663
         ENDIF

C******************************************************************
C--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOT PG5  TOTAL CUBIC

      ELSEIF (VOLEQ(8:10).EQ.'122' .AND. VOLEQ(1:3).EQ.'212') THEN

         IF ( D2H .LE. 6000. ) THEN
            TCUFT = 0.002213 * D2H + 0.030288
         ELSE
            TCUFT = 0.002474 * D2H - 1.557103
         ENDIF

C--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOP=8   BOARD FOOT
         IF(DBHOB.GT.7)THEN

            IF ( D2H .LE. 16000. ) THEN
               grsbdt = 0.012331 * D2H - 34.167170
            ELSE
               grsbdt = 0.016318 * D2H - 99.212720
            ENDIF
            IF(D2H.LE.13000.0) THEN
              VOL(10)= .015011*D2H-44.36046
            ELSE
              VOL(10)= .016991*D2H-68.7502
            ENDIF        
         ENDIF
C--**************  PONDEROSA PINE BLACK HILLS MYERS-RM8-TOP=4

         IF ( D2H .LE. 6700. ) THEN
             GCUFT = 0.002297 * D2H - 1.032297
         ELSE
             GCUFT = 0.002407 * D2H - 2.257724
         ENDIF
! SAWLOG PORTION CUBIC VOLUME (YW 2018/09/05)
         IF(PROD.EQ.'01'.AND.MTOPP.GE.6.0)THEN
           IF(DBHOB.LT.9.0) THEN
             SPF = 0.0
           ELSEIF (DBHOB.GT.42.0) THEN
             SPF = .93
           ELSE
             SPF =.95-(.26*((42.0-DBHOB)/33.0)**7.0)
           ENDIF
           CV6 = SPF*GCUFT
           IF(CV6.GT.0.0)THEN
             TOPWOOD = GCUFT-CV6
             VOL(7) = TOPWOOD
             GCUFT = CV6
           ENDIF
         ENDIF

C******************************************************************
C--**************  Oneseed Juniper Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'069')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.19321+0.136101*D2H**(1./3.)+0.038187*MSTEM)**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Rocky Mountain Juniper Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'066')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (0.02434+0.119106*D2H**(1./3.))**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Utah Juniper Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'065')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.08728+0.135420*D2H**(1./3.)-0.019587*MSTEM)**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Gambel Oak Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'814')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.13600 + 0.145743*D2H**(1./3.))**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Bur Oak Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'823')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (0.12853 + 0.105885*D2H**(1./3.))**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Pinyon Pine
      ELSEIF (VOLEQ(8:10).EQ.'106')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.20296 + 0.150283*D2H**(1./3.)+0.054178*MSTEM)**3.
        GCUFT = TCUFT

C******************************************************************
C--**************  Mountain Mahogany Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'475')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.13363 + 0.128222*D2H**(1./3.)+0.080208*MSTEM)**3.
        !Set to 0.1 for tree with DIA < 3 to match FIA (20240426)
        IF(DRC.LT.3.AND.DBHOB.LT.3) TCUFT = 0.1
        GCUFT = TCUFT

C******************************************************************
C--**************  Other Hardwoods Chojnacky INT-339
      ELSEIF (VOLEQ(8:10).EQ.'998')THEN
c     look for drc, if not use dbhob
        IF(DRC .GT. 0) D2H = DRC*DRC*HTTOT

        TCUFT = (-0.13822 + 0.121850*D2H**(1./3.))**3.
        GCUFT = TCUFT

C--       (ENDIF FOR VOLEQU EQUAL TO A VALID EQUATION)
      ELSE
         ERRFLAG = 1
      ENDIF

      IF (TCUFT .LT. 0.0) THEN
           TCUFT = 0.0
      ENDIF
      VOL(1) = TCUFT

      IF (grsbdt .LT. 0.0) THEN
            grsbdt = 0.0
      ENDIF
      VOL(2) = grsbdt

      IF (GCUFT .LT. 0.0) THEN
          GCUFT = 0.0
      ENDIF
      VOL(4) = GCUFT

 1000 CONTINUE

      RETURN
      END
C**************************************************************
C Vol Equation 223DVEW122 for R2 Black Hills Ponderosa pine Non-saw prod
      SUBROUTINE BH_NonSawPP(DBHOB,HTTOT,STUMP,MTOPP,MTOPS,
     + HT1PRD,HT2PRD,VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,LOGST,
     + NOLOGP,NOLOGS,SPFLG,PROD,ERRFLG)
      IMPLICIT NONE
      REAL DBHOB,HTTOT,STUMP,MTOPP,MTOPS,HT1PRD,HT2PRD
      REAL VOL(15),LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),BOLHT(21)
      REAL NOLOGP,NOLOGS,LOGLENT(20)
      INTEGER LOGST,SPFLG,ERRFLG,I
      CHARACTER*2 PROD
      REAL TCUFT
! MRULE VARIABLES
      CHARACTER*10 VOLEQ
      CHARACTER*1 COR 
      CHARACTER*2 FORST                 
      INTEGER EVOD,OPT,REGN,NUMSEG
      REAL MAXLEN,MINLEN,MERCHL,TRIM
      REAL MINLENT,MINBFD,BTR,DBTBH,LMERCH
      REAL a,b,HTLOW,HT2,DIB,DIBL
      
      IF(DBHOB.LT.1.0)THEN
          ERRFLG = 3
          RETURN
      ENDIF
      IF(HTTOT.LT.4.5)THEN
          ERRFLG = 4
          RETURN
      ENDIF
      LOGST = 0
      NOLOGP = 0
      NOLOGS = 0
      VOL = 0.0
      LOGLEN = 0.0
      LOGVOL = 0.0
      LOGDIA = 0.0
      BOLHT = 0.0
      LOGLENT = 0.0
      CALL BH_PPTCU(DBHOB,HTTOT,TCUFT)
      VOL(1) = TCUFT
      
      IF(MTOPP.LT.0.1) MTOPP = 4.0
      IF(MTOPS.LT.0.1) MTOPS = 2.0
      REGN = 2
      FORST = '03'
      VOLEQ = '223DVEW122'
      BTR = 0.0
      DBTBH = 0.0
      HT2 = STUMP
      NUMSEG = 0
      CALL MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,
     >    MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,
     >    MINBFD,PROD)
      CALL GetRatioCoef(a,b)
      IF(HT1PRD.LT.0.1)THEN
        HTLOW = 0.0
        CALL GetHT2TOPD2(TCUFT,a,b,HTTOT,MTOPP,HT1PRD,HTLOW)
      ENDIF
      LMERCH = HT1PRD - STUMP
      IF(LMERCH.GE.MERCHL)THEN
        !Added max number of logs (20) check (20250626)
        IF((LMERCH/(MAXLEN+TRIM)).GT.20)THEN
          ERRFLG = 12
          RETURN
        ENDIF
        CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)  
        CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG,LOGLEN)
        LOGST = 0
        HT2 = 4.5
        CALL GetHTDIB2(TCUFT,a,b,HTTOT,HT2,DIB)
        LOGDIA(1,2)= DIB
        LOGDIA(1,1)=NINT(DIB)
        BOLHT(1) = HT2
C--   USE DIB AT DBHOB FOR LARGE END BUTT LOG              
        DIBL = LOGDIA(1,1)
        HT2 = STUMP
        CALL CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,TCUFT,TRIM,HTTOT,
     +  LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR)
        LOGST = NUMSEG
        NOLOGP = NUMSEG
      ENDIF
C     Calculate topwood volume
      IF(SPFLG.EQ.1.AND.MTOPS.LT.MTOPP)THEN
        IF(HT2PRD.LT.0.1)THEN
            CALL GetHT2TOPD2(TCUFT,a,b,HTTOT,MTOPS,HT2PRD,HT2)
        ENDIF
        LMERCH = HT2PRD - HT2
        IF(LMERCH.GE.MINLENT)THEN
          NUMSEG = 0
          !Added max number of logs (20) check (20250626)
          IF((LMERCH/(MAXLEN+TRIM)).GT.20)THEN
            ERRFLG = 12
            RETURN
          ENDIF
          CALL NUMLOG(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG)  
          CALL SEGMNT(OPT,EVOD,LMERCH,MAXLEN,MINLEN,TRIM,NUMSEG,LOGLENT)
          NOLOGS = NUMSEG
          !Add secondary log length to LOGLEN
          DO 600 I = 1, NUMSEG
            LOGLEN(I+LOGST) = LOGLENT(I)
 600      CONTINUE
          CALL CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,TCUFT,TRIM,HTTOT,
     +    LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR)
          LOGST = LOGST + NUMSEG
        ENDIF        
      ENDIF
      RETURN
      END
C**************************************************************      
      SUBROUTINE BH_PPTCU(DBHOB,HTTOT,TCUFT)
      IMPLICIT NONE
      REAL DBHOB,HTTOT, TCUFT
      REAL a0,b0,c0
C         Total cubic volume equation vol=a*DBH^b*THT^c   
C         The coefficients was developed from sample trees collected in 
C         Black Hills volume and biomass study in 2022          
          a0 = 0.003573
          b0 = 1.89865505
          c0 = 0.95409984
          TCUFT = a0*DBHOB**b0*HTTOT**c0
      RETURN    
      END
C**************************************************************
      SUBROUTINE GetRatioCoef(a,b)
      IMPLICIT NONE
C     Coefficients for the ratio of volume to a given height to total vol  using the method
C     developed by Phil Radtke from Virginia Tech for Black Hills
      REAL a,b
      a = 2.187093
      b = 0.9399898
      END
C**************************************************************
      SUBROUTINE BH_PPRATIO(HTTOT,HT2,R)
      IMPLICIT NONE
      REAL HTTOT, HT2, R, X
      REAL a,b
C     Calculate the ratio of volume to a given height to total vol  using the method
C     developed by Phil Radtke from Virginia Tech for Black Hills
      CALL GetRatioCoef(a,b)
      X = HT2/HTTOT
      R = (1.0-(1.0-X)**a)**b
      RETURN
      END
C**************************************************************
C Calculate height to a given TOPD      
      SUBROUTINE GetHT2TOPD(DBHOB,HTTOT,TOPD,MHT,HTLOW)
      IMPLICIT NONE
      REAL DBHOB, HTTOT,TOPD,MHT,HTLOW
      REAL R,low,hi,diff,X,a,b,TCUFT,mid
      INTEGER loopcnt

      ! Get total cuft vol
      CALL BH_PPTCU(DBHOB,HTTOT,TCUFT)
C     Ratio coefficients developed by Phil Radtke from Virginia Tech
      CALL GetRatioCoef(a,b)
      CALL GetHT2TOPD2(TCUFT,a,b,HTTOT,TOPD,MHT,HTLOW)
      RETURN
      END
C**************************************************************
C Calculate height to a given TOPD      
      SUBROUTINE GetHT2TOPD2(TCUFT,a,b,HTTOT,TOPD,MHT,HTLOW)
      IMPLICIT NONE
      REAL HTTOT,TOPD,MHT,HTLOW
      REAL low,hi,diff,X,a,b,TCUFT,mid
      INTEGER loopcnt
      
      low = HTLOW
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
      MHT = mid      
      
      RETURN
      END

C**************************************************************
C Calculate DIB at a given height HT2      
      SUBROUTINE GetHTDIB(DBHOB,THT,HT2,DIB)
      IMPLICIT NONE
      REAL DBHOB,HT2,DIB,a,b,TCUFT,THT
      
      CALL BH_PPTCU(DBHOB,THT,TCUFT)
      CALL GetRatioCoef(a,b)
      CALL GetHTDIB2(TCUFT,a,b,THT,HT2,DIB)
      RETURN
      END
C**************************************************************
C Calculate DIB at a given height HT2      
      SUBROUTINE GetHTDIB2(TCUFT,a,b,THT,HT2,DIB)
      IMPLICIT NONE
      REAL HT2,DIB,a,b,TCUFT,THT
      
      DIB=SQRT((TCUFT/ 0.005454 / THT) * (a * b * 
     >    (1-HT2/THT)**(a-1)*(1-(1 - HT2/THT)**a)**(b-1)))
      RETURN
      END      
C**************************************************************
C Calculate LOGDIA, LOGVOL, BOLHT and VOL      
      SUBROUTINE CalcLOGVOL(LOGST,NUMSEG,DIBL,HT2,TCUFT,TRIM,THT,
     + LOGLEN,LOGDIA,LOGVOL,BOLHT,VOL,COR)
      IMPLICIT NONE
      INTEGER LOGST,NUMSEG,I
      REAL DIBL,HT2,TCUFT,TRIM,DIB,a,b,THT,DIBS,LOGCV,LENTH,LOGV,BFINT
      REAL LOGLEN(20),LOGVOL(7,20),LOGDIA(21,3),BOLHT(21),VOL(15)
      CHARACTER*1 COR
      
      CALL GetRatioCoef(a,b)
      DO 500 I=1+LOGST,NUMSEG+LOGST
          HT2=HT2+TRIM+LOGLEN(I)
          CALL GetHTDIB2(TCUFT,a,b,THT,HT2,DIB)
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
C********BOARD FOOT VOLUME EQUATION NUMBERS********

C--    201 =  ASPEN RM-232 - 6" TOP - PAGE 6
C--    202 = LODGEPOLE PINE RESEARCH NOTE RM-157 - 6" TOP
C--    203 = PONDEROSA PINE BLACK HILLS PROSSER - 6" TOP
C--    204 = PONDEROSA PINE FRONT RANGE RM-218 - 6" TOP - PAGE 7
C--    207 = ENGELMANN SPRUCE RM-95 - 6" TOP - PAGE 8
C--    212 = PONDEROSA PINE BLACK HILLS MYERS RM-8-8" TOP - PG 8
C--    FOR LIMBER PINE USE EQUATION 204
C--    FOR SOUTHWEST PONDEROSA PINE USE EQUATION 204
C--    FOR DOUGLAS FIR USE EQUATION 207
C--    FOR TRUE FIRS USE EQUATION 207

C********CUBIC FOOT VOLUME EQUATION NUMBERS********

C--    201 =  ASPEN RM-232 - 4" TOP - PAGE 4
C--    202 = LODGEPOLE PINE RM-6 - 4" TOP - PAGE 6
C--    203 = PONDEROSA PINE MYERS RM-8-4" TOP
C--    204 = PONDEROSA PINE FRONT RANGE RM-218 - 4" TOP - PAGE 5
C--    207 = ENGELMANN SPRUCE RM-95 - 4" TOP - PAGE 6
C--    212 = PONDEROSA PINE BLACK HILLS MYERS RM-8-4" TOP - PG 6
C--    FOR LIMBER PINE USE EQUATION 204
C--    FOR SOUTHWEST PONDEROSA PINE USE EQUATION 204
C--    FOR DOUGLAS FIR USE EQUATION 207
C--    FOR TRUE FIRS USE EQUATION 207

C*****CUBIC FOOT BIOMASS GROUND TO TIP VOLUME EQUATION NUMBERS
C--    221 =  ASPEN RM-232 - PAGE 3
C--    222 = LODGEPOLE PINE RESEARCH NOTE RM-6 - PAGE 5
C--    223 = PONDEROSA PINE BLACK HILLS MYERS RM-8 - PAGE 5
C--    224 = PONDEROSA PINE FRONT RANGE RM-218 - PAGE 4
C--    227 = ENGELMANN SPRUCE RM-95 - PAGE 5
C--    FOR LIMBER PINE USE EQUATION 204
C--    FOR SOUTHWEST PONDEROSA PINE USE EQUATION 204
C--    FOR DOUGLAS FIR USE EQUATION 207
C--    FOR TRUE FIRS USE EQUATION 207

C--  HTTOT - REAL -  **TREE HEIGHT IN FT. FROM A 1 FOOT STUMP**
