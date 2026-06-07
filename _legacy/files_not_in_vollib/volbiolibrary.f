       SUBROUTINE VOLBIOLIB(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     &    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     &    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     &    BA,SI,CTYPE,ERRFLAG,IDIST,BIOEQ,SPEC,CR,BIOGRN,BIODRY)
     

! Expose subroutine VOLBIOLIBRARY to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLBIOLIB
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLBIOLIB
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLUMELIB'::VOLBIOLIB
      
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
      REAL WF(3), BMS(8), CR, MC, BIOGRN(8),BIODRY(8), SG(11),BIOMS(8)
      REAL RATIO,STMDRYWT,STMGRNWT
      INTEGER SPEC
      CHARACTER*12 BIOEQ
      CHARACTER*40 REF(8)
        
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
 
 !    The volume has been calculated, now calculate biomass with Cruising biomass
c      IF(ERRFLAG.EQ.0)THEN
c        IF(LEN_TRIM(BIOEQ).EQ.12) THEN
c          READ (BIOEQ(4:6),*,IOSTAT=STAT) SPCD
c          IF(STAT.NE.0) READ(VOLEQ(8:10),*,IOSTAT=STAT) SPCD
c        ENDIF

      IF(SPEC.EQ.0)THEN
        IF(LEN_TRIM(BIOEQ).EQ.12) THEN
          READ (BIOEQ(4:6),'(I3)') SPEC
        ELSE
          READ(VOLEQ(8:10),'(I3)') SPEC
        ENDIF
      ENDIF
      
!     Call Jenkin's to calculate biomass
      CALL JENKINS(SPEC, DBHOB, BIOMS)
!     The elements in BIOMS are dry weight in pounds as below:
C     1 ABOVE GROUND TOTAL
C     2 MERCH STEM WOOD
C     3 MERCH STEM BARK
C     4 FOLIAGE
C     5 ROOTS
C     6 BRANCHES
C     7 CROWN
C     8 MERCH STEM WOOD AND BARK
      
C     GET REGIONAL OR NATIONAL DEFAULT weight factor
      CALL CRZSPDFT(REGN,FORST,SPEC,WF,BIOEQ,REF)

C     Calculate merch stem green weight using cubic feet volume and weight factor
      STMGRNWT = WF(1)*(VOL(4)+VOL(7))
      
C     Get the moisture content from Miles $ Smith 2009
      IF(WF(3).EQ.0)THEN
        CALL MILSDATA(SPEC,SG)
        WF(3) = (SG(9)-SG(10))/SG(10)*100
      ENDIF
      MC = WF(3)/100
      
      STMDRYWT = STMGRNWT/(1+MC)
      
C     GET the ratio for stem calculated from weight factor and Jenkins
      IF(BIOMS(8).GT.0)THEN
        RATIO = STMDRYWT/BIOMS(8)
      ELSE
        RATIO = 1
      ENDIF

C     Apply the ratio to biomass calculated from Jenkins and also add MC to get green weight
      BIODRY = BIOMS*RATIO
      BIOGRN = BIODRY*(1+MC)  
              
      RETURN         
      END