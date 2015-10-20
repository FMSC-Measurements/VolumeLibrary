      SUBROUTINE VOLUMELIBRARY(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,
     +    DBHOB,
     &    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     &    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     &    BA,SI,CTYPE,ERRFLAG)
     
!... last modified  04-13-2004     Main call into volume dll

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
      
      
!     Local variables      
!     Variable required for call to VOLINIT      
      INTEGER         SPFLG
      REAL            VOL(15)
      INTEGER         BA, SI
      INTEGER         ERRFLAG
        
! 	    print *, '--> enter volume library'
!	    print *, '    regn = ',regn, 'forst = ', forst
!	    print *, '    dist = ', dist
!	    print *, '*****************************'
!	    print *, '   prod = ', prod, 'voleq = ', voleq 
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
 !      vol(2) = 17.3
 !      logvol(4,1) = 32.3
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG)
 !      print *, 'vol(2) = ', vol(2)
 !      print *, 'logvol(4,1) = ', logvol(4,1)
 4000 RETURN
      
      END SUBROUTINE VOLUMELIBRARY
      
!      SUBROUTINE VOLLIBVB8(REGN,IFORST,EQNUM,DBHOB,HTTOT,TOPD,
!     + TOTCU, MERCHCU, BDFT, XINT, ERRFLAG)
      SUBROUTINE VOLLIBVB8(EQNUM, REGN,DBHOB, HTTOT, TOPD,
     +  TOTCU, MERCHCU, BDFT, XINT)
!... 03-23-2015     This function is make the DLL be called from VB.NET

! Expose subroutine VOLLIBVB8 to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBVB8
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBVB8
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBVB8'::VOLLIBVB8
      !DEC$ ATTRIBUTES REFERENCE :: EQNUM
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
     +    BA,SI,CTYPE,ERRFLAG)

c      TOTCU = NINT(VOL(1)*10.0)/10.0
c      MERCHCU = NINT(VOL(4)*10.0)/10.0
c      BDFT = NINT(VOL(2)*10.0)/10.0
c      XINT = NINT(VOL(10)*10.0)/10.0
      TOTCU = VOL(1)
      MERCHCU = VOL(4)
      BDFT = VOL(2)
      XINT = VOL(10)
      RETURN
      
      END SUBROUTINE VOLLIBVB8
            