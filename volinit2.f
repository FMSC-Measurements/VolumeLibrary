C  This version of Volint allows the user to pass in the merrules
C  defined type.  This type allows the user to set the merch rules
C  externally.
C
C  created TDH 04/01/09
C
c  revised YW 05/16/2013 For BEH equation, if total height is entered, the HTTYPE is set to F
!REV  YW 02/27/2014 For region 6 Behre equation, if merch height is entered in feet, convert it to log ht
!REV  YW 07/30/2014 For region 6 Behre equation, if merch height is entered in feet, use log rules to convert to log ht.
C     YW 09/15/2016 Added output variable LOGDIA,LOGLEN,LOGVOL to R4vol subroutine
C     YW 08/08/2022 Added STUMP to call DVEST subroutine
C_______________________________________________________________________

      SUBROUTINE VOLINIT2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG, MERRULES,IDIST)
C_______________________________________________________________________

	USE CHARMOD
	USE DEBUG_MOD
	USE MRULES_MOD
      USE VOLINPUT_MOD
      IMPLICIT NONE

!REV  Created TDH  
!REV  Revised iii ../../..

!**********************************************************************
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      CHARACTER*10 EQNUM
      INTEGER      SPEC

!   MERCH VARIABLES 
      INTEGER      REGN,HTTFLL,BA,SI
      REAL         STUMP,MTOPP,MTOPS,THT1,MAXLEN
      INTEGER      CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
      TYPE(MERCHRULES)::MERRULES
      
!   Tree variables
      REAL 		   HTTOT,HT1PRD,HT2PRD,LEFTOV 
      REAL 		   DBHOB,DRCOB,DBTBH,BTR,CR,TRIM,VOLIB,VOLOB,HTUP
      INTEGER      FCLASS,HTLOG,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL         UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 	   HTREF
    
!   OUTPUTS
      REAL         NOLOGP,NOLOGS
      INTEGER      TLOGS,IFORST,IDIST
    
!   ARRAYS
      INTEGER      I15,I21,I20,I7,I3,I,J
      REAL 		   VOL(I15),LOGVOL(I7,I20)
      REAL		   LOGDIA(I21,I3),LOGLEN(I20),BOLHT(I21)
 !********************************************************************

      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 2) ' -->Enter VOLINIT2'
    2    FORMAT (A)   
   		END IF

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

      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
      GOTO 4000
         		
! The following codes are almost duplicated from VOLINIT1 and will be deleted 2017/03/17
! **************************************************************************************
!Check for a DBH of less than 1.  Drop out of volume if true.  10/97
      IF(DBHOB.LT.1 .AND. DRCOB.LT.1) THEN
        ERRFLAG = 3
        GOTO 4000
      ENDIF
!-----Set the default DIST to 01---------------------      
      IF(IDIST.NE.IDIST) IDIST = 1
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
c     +   MDL.EQ.'jb2') THEN     
     +   MDL.EQ.'jb2' .OR. MDL.EQ.'BEH') THEN
!************************
!    FLEWELLING MODELS  *
!    REGION 2 MODELS    *
!    REGION 5 MODELS    * 
!************************

        CALL PROFILE2 (REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,HTTYPE,
     +      HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,
     +      AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,
     +      TLOGS, NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,
     +      DRCOB,CTYPE,FCLASS,PROD,ERRFLAG, MERRULES)
      

      ELSEIF (MDL.EQ.'MAT' .OR. MDL.EQ.'mat') THEN
!**********************
!    REGION 4 MODEL  * 
!**********************

        CALL R4VOL(REGN,VOLEQ,MTOPP,HTTOT,DBHOB,HT1PRD,VOL,NOLOGP,
     +             NOLOGS, LOGDIA,LOGLEN,LOGVOL,BOLHT,
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
C              THT1 = HT1PRD
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
            IF(THT1.EQ.0) RETURN
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

      ELSEIF (VOLEQ(1:1).EQ.'8') THEN
!********************
!    REGION 8 MODEL  * 
!********************
        IF(MDL.EQ.'CLK' .OR. MDL.EQ.'clk') THEN
C ADDED TO TEST R8 CLARK PROFILE FOR LOG BOARDFOOT VOLUME   
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
     
          CALL R8CLARK(VOLEQ,FORST,STUMP,MTOPP,MTOPS,DBHOB,HT1PRD,
     +             HT2PRD,HTTOT, LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,CUTFLG,
     +                  BFPFLG,CUPFLG, CDPFLG,SPFLG,PROD,ERRFLAG,CTYPE,
     +                  UPSHT1,TLOGS,NOLOGP,NOLOGS)
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
      
          CALL R9CLARK (VOLEQ,STUMP,MTOPP,MTOPS,DBHOB,HT1PRD,HT2PRD,
     +                  HTTOT, LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,CUTFLG,
     +                  BFPFLG,CUPFLG, CDPFLG,SPFLG,PROD,ERRFLAG,CTYPE,
     +                  UPSHT1,TLOGS,NOLOGP,NOLOGS)
          
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
         IF ((VOLEQ(1:3).EQ.'A01'.OR.VOLEQ(1:3).EQ.'A02' .or.
     &        VOLEQ(1:3).EQ.'a01'.OR.VOLEQ(1:3).EQ.'a02' ) .or.
     &       (HTTOT.LE.40 .OR. DBHOB.LT.9.0)) THEN
      
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
      
         CALL DVEST (VOLEQ,DBHOB,DRCOB,HTTOT,MTOPP,FCLASS,HTLOG,
     +              HT1PRD,HT2PRD, FORST,BTR,VOL,CUTFLG,BFPFLG,CUPFLG,
     +              CDPFLG,SPFLG,PROD,HTTYPE,HTTFLL,NOLOGP,LIVE,BA,
     +              SI,CTYPE,errflag,MTOPS,STUMP)
      
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
C  calc Tip volume and save to VOL(15)
      IF(ERRFLAG.GT.0) RETURN
      IF(VOL(14).LT.0.01)THEN
        SPECIES = VOLEQ(8:10)
        READ(SPECIES,'(i3)') SPEC
        HTUP = STUMP
        CALL RAILEVOL(SPEC, DBHOB, HTUP, VOLIB, VOLOB)
        VOL(14) = VOLIB
      ENDIF
c now calc tip volume
c      IF(VOL(15).LT.0.01 .AND. VOL(4).GT.0.0)THEN
c        VOL(15) = VOL(1)-VOL(4)-VOL(7)-VOL(14)
c        IF(VOL(15).LT.0.0) VOL(15) = 0.0
c      ENDIF
                  
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 20) ' <--Exit VOLINIT2'
   20    FORMAT (A)   
   		END IF
      
 4000 RETURN
      END SUBROUTINE VOLINIT2