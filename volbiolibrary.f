       SUBROUTINE VOLBIOLIBRARY(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     &    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     &    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     &    BA,SI,CTYPE,ERRFLAG,IDIST,BIOEQ,CR,BIO)
     

! Expose subroutine VOLBIOLIBRARY to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLBIOLIBRARY
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLBIOLIBRARY
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLUMELIBRARY'::VOLBIOLIBRARY
      
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
      REAL     WF(3), BMS(8), CR, MC, BIO(15), SG(11),BIOMS
      INTEGER SPCD
      CHARACTER*12 BIOEQ
        
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
 
 !    The volume has been calculated, now calculate biomass with Cruising biomass
      IF(ERRFLAG.EQ.0)THEN
        IF(LEN_TRIM(BIOEQ).EQ.12) THEN
          READ (BIOEQ(4:6),*,IOSTAT=STAT) SPCD
          IF(STAT.NE.0) READ(VOLEQ(8:10),*,IOSTAT=STAT) SPCD
        ENDIF
        CALL CRZBIOMASS(REGN,FORST,SPCD,DBHOB,DRCOB, HTTOT,FCLASS,
     +  VOL,WF,BMS,ERRFLAG)    
!       The biomass calculated from above is green biomass and saved in BMS as:
!       1 - above ground total
!       2 - live branches
!       3 - dead branches
!       4 - foliage
!       5 - primary prod (wood + bark)
!       6 - secondary prod / topwood (wood + bark)
!       7 - stem tip

!       Convert the green biomass to dry and save to BIO
        CALL MILSDATA(SPCD,SG)
        MC = (SG(9)-SG(10))/SG(10)
        
!       The biomass component saved in BIO are:
!       1 - above ground total
!       2 - merch stem total (from stump to merch topd)
!       3 - merch stem wood
!       4 - merch stem bark
!       5 - stump
!       6 - stem tip
!       7 - crown (branches + foliage)
!       8 - live branches
!       9 - dead branches
!       10 - foliage
!       11 - roots
!       12 - branches (0 - 1/4)
!       13 - branches (1/4 - 1)
!       14 - branches (1 - 3)
!       15 - branches (3+)

        BIO(1) = BMS(1)/(1+MC)
        BIO(2) = (BMS(5)+BMS(6))/(1+MC)
        BIO(3) = VOL(14)*Wf(1)/(1+MC)
        BIO(4) = BMS(7)/(1+MC)
        BIO(5) = (BMS(2)+BMS(3)+BMS(4))/(1+MC)
        BIO(6) = BMS(2)/(1+MC)
        BIO(7) = BMS(3)/(1+MC)
        BIO(8) = BMS(4)/(1+MC)
        BIO(9) = 0
        
!       If BIOEQ is provided, calculate the biomass and saved it to BIOMS
        IF(LEN_TRIM(BIOEQ).EQ.12) THEN
          CALL BiomassLibrary(BIOEQ,DBHOB,HTTOT,CR,HT1PRD, 
     +         HT2PRD,MTOPP, FCLASS, VOL, BIOMS, ERRFLG)  
        ENDIF           
      ENDIF
      RETURN         
      END