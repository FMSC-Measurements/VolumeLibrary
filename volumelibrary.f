      SUBROUTINE VOLUMELIBRARY(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,
     +    DBHOB,
     &    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     &    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     &    BA,SI,CTYPE,ERRFLAG,IDIST)
     
! 04-19-2016     Added IDIST as input variable
! 09/21/2016  Added biomass calculation

! Expose subroutine VOLUMELIBRARY to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLUMELIBRARY
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLUMELIBRARY
  !    !DEC$ ATTRIBUTES DECORATE,ALIAS:'_VOLUMELIBRARY@224'::VOLUMELIBRARY
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLUMELIBRARY'::VOLUMELIBRARY
      
      USE CHARMOD 
	USE DEBUG_MOD
		  
      IMPLICIT NONE
      
!     Parameters
      INTEGER         REGN
      CHARACTER*(*)   FORST, VOLEQ
      REAL            MTOPP, MTOPS, STUMP,DBHOB, DRCOB
      CHARACTER*(*)   HTTYPE
      REAL            HTTOT
      INTEGER         HTLOG
      REAL            HT1PRD, HT2PRD, UPSHT1, UPSHT2, UPSD1, UPSD2
      INTEGER         HTREF
      REAL            AVGZ1, AVGZ2
      INTEGER         FCLASS
      REAL            DBTBH, BTR
      INTEGER         I3, I7, I15, I20, I21
      REAL            LOGVOL(I7,I20), LOGDIA(I21,I3), LOGLEN(I20)
      REAL            BOLHT(I21)
      INTEGER         TLOGS
      REAL            NOLOGP,NOLOGS
      INTEGER         CUTFLG, BFPFLG, CUPFLG, CDPFLG, CUSFLG, CDSFLG
      CHARACTER*(*)   PROD
      CHARACTER*(*)   CONSPEC
      INTEGER         HTTFLL
      CHARACTER*(*)   LIVE, CTYPE
      INTEGER         ERRFLG
      CHARACTER*2     DIST, VAR
      INTEGER         IDIST
!     Local variables      
!     Variable required for call to VOLINIT      
      INTEGER         SPFLG
      REAL            VOL(15)
      INTEGER         BA, SI
      INTEGER         ERRFLAG
!     Variable for biomass      
      REAL    WF(3), BMS(8)
      INTEGER SPCD, BMSFLG
      CHARACTER*10 EQNUM
        
! 	    print *, '--> enter volume library'
!	    print *, '    regn = ',regn, 'forst = ', forst
!	    print *, '    dist = ', dist
!	    print *, '*****************************'
!	    print *, '   prod = ', prod, 'voleq = ', voleq 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 !      vol(2) = 17.3
 !      logvol(4,1) = 32.3
      IF(BMSFLG.EQ.1.AND.VOLEQ.EQ."")THEN
        VAR = '  '
        CALL VOLEQDEF(VAR,REGN,FORST,DIST,SPCD,PROD,EQNUM,ERRFLAG)
        VOLEQ = EQNUM
      ENDIF
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
 !      print *, 'vol(2) = ', vol(2)
 !      print *, 'logvol(4,1) = ', logvol(4,1)
 !    Added the following to calculat biomass (09/20/2016)
      IF (BMSFLG.EQ.1) THEN
        CALL CRZBIOMASS(REGN,FORST,SPCD,DBHOB,DRCOB, HTTOT,FCLASS,
     +  VOL,WF,BMS,ERRFLG)
      ENDIF
 4000 RETURN
      
      END SUBROUTINE VOLUMELIBRARY
C ---------------------------------------------------------------------      
      SUBROUTINE VOLLIBVB8(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  TOTCU, MERCHCU, BDFT, XINT)
!... 03-23-2015     This function is make the DLL be called from VB.NET

! Expose subroutine VOLLIBVB8 to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBVB8
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBVB8
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBVB8'::VOLLIBVB8
      !DEC$ ATTRIBUTES REFERENCE :: EQNUM
      IMPLICIT NONE
      CHARACTER*10 EQNUM
      INTEGER      REGN, ERRFLG
      REAL DBHOB,HTTOT,TOPD,TOTCU,MERCHCU,BDFT,XINT,MHT
      REAL VOL(15)
      
      CALL VOLLIBVB8INIT(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  VOL, MHT, ERRFLG) 

      TOTCU = VOL(1)
      MERCHCU = VOL(4)
      BDFT = VOL(2)
      XINT = VOL(10)

      RETURN
      
      END SUBROUTINE VOLLIBVB8

      SUBROUTINE VOLLIBVB8XHT(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  TOTCU, MERCHCU, BDFT, XINT, MHT)
!... 12-01-2015     This function is make the DLL be called from VB.NET

! Expose subroutine VOLLIBVB8 to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBVB8XHT
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBVB8XHT
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBVB8XHT'::VOLLIBVB8XHT
      !DEC$ ATTRIBUTES REFERENCE :: EQNUM
      IMPLICIT NONE
      CHARACTER*10 EQNUM
      INTEGER      REGN,ERRFLG
      REAL DBHOB,HTTOT,TOPD,TOTCU,MERCHCU,BDFT,XINT,MHT
      REAL VOL(15)
      
      CALL VOLLIBVB8INIT(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  VOL, MHT,ERRFLG) 

      TOTCU = VOL(1)
      MERCHCU = VOL(4)
      BDFT = VOL(2)
      XINT = VOL(10)
      RETURN
      
      END SUBROUTINE VOLLIBVB8XHT
      
C -------------------------------------------------------------------------      
      SUBROUTINE VOLLIBVB8INIT(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  VOL, MHT, ERRFLAG) 

      USE CHARMOD 
	USE DEBUG_MOD
		  
      IMPLICIT NONE
      
!**********************************************************************
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      CHARACTER*10 EQNUM
      INTEGER      SPEC
      INTEGER      IDIST

!   MERCH VARIABLES 
      INTEGER        REGN,HTTFLL,BA,SI
      REAL           STUMP,MTOPP,MTOPS,THT1,MAXLEN, MHT
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
      INTEGER        TLOGS,IFORST
    
!   ARRAYS
      INTEGER        I15,I21,I20,I7,I3,I,J
      REAL 					 VOL(15),LOGVOL(7,20)
      REAL				   LOGDIA(21,3),LOGLEN(20),BOLHT(21)

!     Extra variable
!      INTEGER IFORST
      REAL TOPD, TOTCU, MERCHCU, BDFT, XINT
      
!     Set default value for unused variables
      IF(IFORST.GT.99) THEN
        FORST = '01'
      ELSE 
        WRITE (FORST, '(I2)') IFORST
      ENDIF
      IF(FORST(2:2) .LT. '0') THEN 
        FORST(2:2) = FORST(1:1)
        FORST(1:1) = '0'
        IF(FORST(2:2) .LT. '0') FORST(2:2) = '0'
      ENDIF
      HT1PRD=0.0
      HT2PRD=0.0
      FCLASS=0
      DBTBH=0.0
      BTR=0.0
      PROD='01'
      HTTYPE='F'
      HTLOG=0
      STUMP=0.0
      UPSHT1=0.0
      UPSD1=0.0
      AVGZ1=0.0
      HTREF=0
      UPSHT2=0.0
      UPSD2=0.0
      AVGZ2=0.0
      CONSPEC='    '
      DRCOB=0.0
      HTTFLL=0
      BA=0
      SI=0
      CTYPE='F'
      CUTFLG=1
      CUPFLG=1
      SPFLG=1
      BFPFLG=1
      IF(TOPD.GT.0.0)THEN
        MTOPP=TOPD
        MTOPS=TOPD
      ELSE
        MTOPP=0.0
        MTOPS=0.0
      ENDIF
      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 =21

      FORST = '01'
      VOLEQ=EQNUM
      
 
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)

      MHT = HT1PRD
      RETURN
      
      END SUBROUTINE VOLLIBVB8INIT
C ------------------------------------------------------------------------------
      SUBROUTINE EZVOLLIB(VOLEQI,DBHOB,HTTOT,VOL)
C ADD THIS EAZY LIBRARY FOR USER WITH ONLY DBH AND HEIGHT
C 2017/02/08

      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::EZVOLLIB
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: EZVOLLIB
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'EZVOLLIB'::EZVOLLIB
      IMPLICIT NONE
      
      CHARACTER*(*)   VOLEQI
      CHARACTER*10 VOLEQ
      REAL DBHOB,HTTOT,TOPD, VOL(15), MHT
      INTEGER REGN,ERRFLG
      
      VOLEQ   = VOLEQI(1:10)
      IF(VOLEQ(1:1).EQ.'A'.OR.VOLEQ(1:1).EQ.'a')THEN
        REGN = 10
      ELSEIF(VOLEQ(1:1).EQ.'B'.OR.VOLEQ(1:1).EQ.'B')THEN
        REGN = 7
      ELSEIF(VOLEQ(1:1).EQ.'I'.OR.VOLEQ(1:1).EQ.'i')THEN
        REGN = 1
      ELSEIF(VOLEQ(1:1).EQ.'H'.OR.VOLEQ(1:1).EQ.'h')THEN
        REGN = 5
      ELSEIF(VOLEQ(1:1).EQ.'F'.OR.VOLEQ(1:1).EQ.'f')THEN
        REGN = 6
      ELSE
        READ(VOLEQ(1:1),'(I1)') REGN
      ENDIF
      
      TOPD = 0.0
      CALL VOLLIBVB8INIT(VOLEQ, REGN,DBHOB, HTTOT, TOPD,
     +  VOL, MHT,ERRFLG)
      
      VOLEQI = VOLEQ // char(0)
      RETURN
      END SUBROUTINE EZVOLLIB   
C *******************************************************************************
      subroutine vollib_r(VOLEQ,REGN,FORST,DIST,SPEC,DBHOB_d,HTTOT_d,
     + MTOPP_d,MTOPS_d,HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d,STUMP_d,
     + FCLASS,DBTBH_d,BTR_d,VOL_d, ERRFLAG)
C This subroutine is for R user to calculate volume from vollib      !
C YW 02/10/2017
C testing --still not working

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::vollib_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'vollib_r_'::vollib_r

	USE CHARMOD
	USE DEBUG_MOD
      USE VOLINPUT_MOD

      IMPLICIT NONE
      
      DOUBLE PRECISION DBHOB_d,HTTOT_d,MTOPP_d,MTOPS_d,STUMP_d
      DOUBLE PRECISION HT1PRD_d,HT2PRD_d,UPSHT1_d,UPSD1_d
      DOUBLE PRECISION DBTBH_d,BTR_d,VOL_d(15)
      
      CHARACTER*1  HTTYPE,LIVE,CTYPE
      CHARACTER*2  FORST,PROD
      character*4  CONSPEC
      CHARACTER*10 VOLEQ
      CHARACTER*3  MDL,SPECIES
      CHARACTER*2  DIST,VAR
   
      INTEGER      SPEC,TMPSPEC,NULEQ

!   MERCH VARIABLES 
      INTEGER      REGN,HTTFLL,BA,SI,IFORST,IDIST
      REAL         STUMP,MTOPP,MTOPS  !,THT1,MAXLEN
      INTEGER      CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,ERRFLAG
      REAL         TIPDIB,TIPLEN
      
!   Tree variables
      REAL 		HTTOT,HT1PRD,HT2PRD  !,LEFTOV 
      REAL 		DBHOB,DRCOB,DBTBH,BTR  !,CR,TRIM
      INTEGER   FCLASS,HTLOG  !,SPCODE, WHOLELOGS
    
!	3RD POINT VARIABLES
      REAL      UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 	HTREF
    
!   OUTPUTS
      REAL      NOLOGP,NOLOGS
      INTEGER   TLOGS  !,IFORST, IDIST
    
!   ARRAYS
      INTEGER   I15,I21,I20,I7,I3,I,J
      REAL 		VOL(15),LOGVOL(7,20)
      REAL		LOGDIA(21,3),LOGLEN(20),BOLHT(21)
      
      
      DBHOB = REAL(DBHOB_d)
      HTTOT = REAL(HTTOT_d)
      MTOPP = REAL(MTOPP_d)
      MTOPS = REAL(MTOPS_d)
      HT1PRD = REAL(HT1PRD_d)
      HT2PRD = REAL(HT2PRD_d)
      UPSHT1 = REAL(UPSHT1_d)
      UPSD1 = REAL(UPSD1_d)
      STUMP = REAL(STUMP_d)
      DBTBH = REAL(DBTBH_d)
      BTR = REAL(BTR_d)
      
      READ (DIST, '(I2)') IDIST
C     Set the default value for other variable
      PROD='01'
      HTTYPE='F'
      HTLOG=0
      AVGZ1=0.0
      HTREF=0
      UPSHT2=0.0
      UPSD2=0.0
      AVGZ2=0.0
      CONSPEC='    '
      DRCOB=0.0
      HTTFLL=0
      BA=0
      SI=0
      CTYPE='F'
      CUTFLG=1
      CUPFLG=1
      SPFLG=1
      BFPFLG=1
      I3 = 3
      I7 = 7
      I15 = 15
      I20 = 20
      I21 =21
      
C     Check if the VOLEQ is valid. If not valid, return error flag 1      
c      NULEQ = INDEX(VOLEQ,' ')
c      IF(LEN_TRIM(VOLEQ).EQ.0.OR.NULEQ.GT.0)THEN
c        CALL VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG) 
c      ENDIF
c      TMPSPEC = 9999
c      CALL VOLEQDEF(VAR,REGN,FORST,DIST,TMPSPEC,PROD,VOLEQ,ERRFLAG)
c      IF(TMPSPEC.NE.8888) GOTO 999
      
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
      
      VOL_d = DBLE(VOL)
      DBHOB_d = DBLE(DBHOB)
      HTTOT_d = DBLE(HTTOT)
      MTOPP_d = DBLE(MTOPP)
      MTOPS_d = DBLE(MTOPS)
      HT1PRD_d = DBLE(HT1PRD)
      HT2PRD_d = DBLE(HT2PRD)
      UPSHT1_d = DBLE(UPSHT1)
      UPSD1_d = DBLE(UPSD1)
      STUMP_d = DBLE(STUMP)
      DBTBH_d = DBLE(DBTBH)
      BTR_d = DBLE(BTR)
      
c      TOPD = 0.0
c      CALL VOLLIBVB8INIT(VOLEQ, REGN,DBHOB, HTTOT, TOPD,
c     +  VOL, MHT,ERRFLG)
c      TCU = DBLE(VOL)

999   CONTINUE
      RETURN
      end subroutine vollib_r   
      
C *********************************************************************************
      subroutine test_r(voleq,dbh,tht,tcu)
C this is a test
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::test_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'test_r_'::test_r
      IMPLICIT NONE
      
      CHARACTER*10 voleq
      double precision dbh,tht,tcu
      real dbhob, httot, totcu
      INTEGER REGN,ERR
      
      err=0
      dbhob = REAL(dbh)
      httot = REAL(tht)
      totcu = dbhob*dbhob*httot
      tcu= DBLE(totcu)
      err=10
      RETURN
      end subroutine test_r      