C  C# .Net entry point to the volume library
C
C
C  created TDH 03/04/10
C
c  revised 06/23/11
C  Cleaned up code and added comments
C
C  Revised TDH 10/04/10
C  Adding logic to expand the profile model tutorial
C  YW 2016/01/13 Added BTR default value for Region 3 Santa Fe forest DF and PP
C_______________________________________________________________________

      SUBROUTINE VOLLIBCS(REGN,FORSTI,VOLEQI,MTOPP,MTOPS,STUMP,
     +    DBHOB,
     &    DRCOB,HTTYPEI,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     &    VOL,LOGVOLI,LOGDIAI,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPECI,PRODI,HTTFLL,LIVEI,
     &    BA,SI,CTYPEI,ERRFLAG, INDEB, PMTFLG, MERRULES,IDIST)
C_______________________________________________________________________


! Expose subroutine VOLLIBCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::VOLLIBCS

      USE DEBUG_MOD
      USE MRULES_MOD

      IMPLICIT NONE

C**********************************************************************      
!     Parameters
      INTEGER         REGN
      CHARACTER*(*)   FORSTI, VOLEQI
      REAL            MTOPP, MTOPS, STUMP,DBHOB, DRCOB
      CHARACTER*(*)   HTTYPEI
      REAL            HTTOT
      INTEGER         HTLOG
      REAL            HT1PRD, HT2PRD, UPSHT1, UPSHT2, UPSD1, UPSD2
      INTEGER         HTREF
      REAL            AVGZ1, AVGZ2
      INTEGER         FCLASS
      REAL            DBTBH, BTR
      INTEGER         I3, I7, I15, I20, I21
      REAL            LOGVOLI(I7,I20), LOGDIAI(I21,I3), LOGLEN(I20)
      REAL            BOLHT(I21)
      INTEGER         TLOGS
      REAL            NOLOGP,NOLOGS
      INTEGER         CUTFLG, BFPFLG, CUPFLG, CDPFLG, CUSFLG, CDSFLG
      CHARACTER*(*)   PRODI
      CHARACTER*(*)   CONSPECI
      INTEGER         HTTFLL
      CHARACTER*(*)   LIVEI, CTYPEI
      INTEGER         ERRFLG, INDEB, PMTFLG
      TYPE(MERCHRULES):: MERRULES
      
      
!     Local variables      
!     Variable required for call to VOLINIT      
      INTEGER         SPFLG
      REAL            VOL(15)
      INTEGER         BA, SI
      INTEGER         ERRFLAG
      
!     Local variables
      CHARACTER(2)   FORST
      CHARACTER(10)  VOLEQ
      CHARACTER(1)   HTTYPE
      CHARACTER(4)   CONSPEC
      CHARACTER(2)   PROD
      CHARACTER(1)   LIVE
      CHARACTER(1)   CTYPE
      CHARACTER*3    MDL,SPECIES
      CHARACTER*2    DIST,VAR   
      CHARACTER*10   EQNUM
      INTEGER        SPEC
      REAL           LOGVOL(I7,I20),LOGDIA(I21,I3),DIBO 
      INTEGER        IDIST
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

      !Convert the CHAR*256 types to Char(*) so I don't have to rename 
      !all references below
     
!--------------------------------------------------------------------
      FORST   = FORSTI(1:2)
      VOLEQ   = VOLEQI(1:10)
      HTTYPE  = HTTYPEI(1:1)
      CONSPEC = CONSPECI(1:4)
      PROD    = PRODI(1:2)
      LIVE    = LIVEI(1:1)
      CTYPE   = CTYPEI(1:1)
!---------------------------------------------------
!Use array converter to reshape c arrays to fortran notation
      LOGVOL = RESHAPE(LOGVOLI, SHAPE(LOGVOL))
      LOGDIA = RESHAPE(LOGDIAI, SHAPE(LOGDIA))
      ERRFLAG = 0

! This is for manual debug only
!      IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
!	   OPEN (UNIT=LUDBG, FILE='Debug.txt', STATUS='UNKNOWN')
!	   WRITE (LUDBG,2)'Debugging VOLLIBCS'
!   2     FORMAT(A)
!         WRITE  (LUDBG, 50)'REGN FORST VOLEQ     HTTYPE CONSPEC PROD
!     & LIVE CTYPE DBHOB MTOPP STUMP  BTR DBTBH HTTOT HT1PRD'
!  50     FORMAT (A)
!         WRITE  (LUDBG, 70) REGN, FORST, VOLEQ, HTTYPE, CONSPEC, PROD, 
!     &    LIVE, CTYPE, DBHOB, MTOPP, STUMP, BTR, DBTBH,
!     &    HTTOT, HT1PRD 
!   70    FORMAT (I2, 3X, A, 3X, A,1X, A, 2X, A,8X, A,3X,A,4X,
!     &           A, 2X, F7.1, F7.1, F6.1,F5.1, F5.2, F7.1,F7.1)
!
!	   CLOSE(LUDBG)
!      ENDIF
      
      IF (PMTFLG .EQ. 1) THEN
!         CALL PMTPROFILE (FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
!     >   HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,UPSD2,
!     >   AVGZ1,AVGZ2,HTREF,DBTBH,BTR,LOGDIA,BOLHT,LOGLEN,LOGVOL,VOL,
!     >   TLOGS,NOLOGP,NOLOGS,CUTFLG,BFPFLG,CUPFLG,CDPFLG,SPFLG,DRCOB,
!     >   CTYPE,FCLASS,PROD,DIBO,ERRFLAG)
     
!         NOLOGP = DIBO
     
      ELSE IF (PMTFLG .EQ. 2) THEN
    
           CALL VOLINIT2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,3,7,15,20,21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG, MERRULES,IDIST)
     
      
      ELSE
 !     	   IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
 !     	   WRITE (LUDBG,2)'Before call VOLINIT'
 !     	   ENDIF

           CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
          
!          IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
!          WRITE (LUDBG,2)'After call VOLINIT'
!          ENDIF
       ENDIF

     
      !add null terminator required by C# strings
      FORSTI = FORST // char(0)
      VOLEQI = VOLEQ // char(0)
      HTTYPEI = HTTYPE // char(0)
      CONSPECI = CONSPEC // char(0)
      PRODI = PROD // char(0)
      LIVEI = LIVE // char(0)
      CTYPEI = CTYPE // char(0)
      !MERRULES%OPT = ' '//char(0)
     
           
      !copy the logvol and logdia data back into the subroutine 
      !paramater for return to c#
      LOGVOLI = RESHAPE(LOGVOL, SHAPE(LOGVOL))
      LOGDIAI = RESHAPE(LOGDIA, SHAPE(LOGDIA))
 
!      IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
!         WRITE  (LUDBG, 80)'After call NVEL: Vol(1)  Vol(2)  Vol(4)' 
!  80     FORMAT (A)
!         WRITE  (LUDBG, 90) VOL(1), VOL(2), VOL(4) 
!  90    FORMAT (16X, F7.1,F7.1, F7.1)

!	   CLOSE(LUDBG)
!      ENDIF

 4000 RETURN
      END SUBROUTINE VOLLIBCS
      
C_______________________________________________________________________

      SUBROUTINE VOLLIBC2(REGN,FORSTI,VOLEQI,MTOPP,MTOPS,STUMP,
     +    DBHOB,
     &    DRCOB,HTTYPEI,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     &    VOL,LOGVOLI,LOGDIAI,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPECI,PRODI,HTTFLL,LIVEI,
     &    BA,SI,CTYPEI,ERRFLAG, INDEB, PMTFLG, MERRULES)
C_______________________________________________________________________
C This function is for the user to use costom merch rules.
C The pathon script can call it with user defined merch rules.

! Expose subroutine VOLUMELIBRARY2 to users of this DLL
!
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::VOLLIBC2
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: VOLLIBC2
  !    !DEC$ ATTRIBUTES DECORATE,ALIAS:'_VOLUMELIBRARY@224'::VOLUMELIBRARY
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VOLLIBC2'::VOLLIBC2

      USE DEBUG_MOD
      USE MRULES_MOD

      IMPLICIT NONE

C**********************************************************************      
!     Parameters
      INTEGER         REGN
      CHARACTER*(*)   FORSTI, VOLEQI
      REAL            MTOPP, MTOPS, STUMP,DBHOB, DRCOB
      CHARACTER*(*)   HTTYPEI
      REAL            HTTOT
      INTEGER         HTLOG
      REAL            HT1PRD, HT2PRD, UPSHT1, UPSHT2, UPSD1, UPSD2
      INTEGER         HTREF
      REAL            AVGZ1, AVGZ2
      INTEGER         FCLASS
      REAL            DBTBH, BTR
      INTEGER         I3, I7, I15, I20, I21
      REAL            LOGVOLI(I7,I20), LOGDIAI(I21,I3), LOGLEN(I20)
      REAL            BOLHT(I21)
      INTEGER         TLOGS
      REAL            NOLOGP,NOLOGS
      INTEGER         CUTFLG, BFPFLG, CUPFLG, CDPFLG, CUSFLG, CDSFLG
      CHARACTER*(*)   PRODI
      CHARACTER*(*)   CONSPECI
      INTEGER         HTTFLL
      CHARACTER*(*)   LIVEI, CTYPEI
      INTEGER         ERRFLG, INDEB, PMTFLG
      TYPE(MERCHRULES):: MERRULES
      
!     Local variables      
!     Variable required for call to VOLINIT      
      INTEGER         SPFLG
      REAL            VOL(15)
      INTEGER         BA, SI
      INTEGER         ERRFLAG
      
!     Local variables
      CHARACTER(2)   FORST
      CHARACTER(10)  VOLEQ
      CHARACTER(1)   HTTYPE
      CHARACTER(4)   CONSPEC
      CHARACTER(2)   PROD
      CHARACTER(1)   LIVE
      CHARACTER(1)   CTYPE
      CHARACTER*3    MDL,SPECIES
      CHARACTER*2    DIST,VAR   
      CHARACTER*10   EQNUM
      INTEGER        SPEC
      REAL           LOGVOL(I7,I20),LOGDIA(I21,I3),DIBO 
      INTEGER        IDIST
      IF (INDEB.eq.1) THEN
	   OPEN (UNIT=LUDBG, FILE='Debug.txt', STATUS='UNKNOWN')
	   WRITE (LUDBG,5)'Debugging VOLLIBCS2'
   5     FORMAT(A)
         WRITE  (LUDBG, 108)'  COR EVOD OPT MAXLEN MINLEN MERCHL 
     &    MINLENT MTOPP MTOPS STUMP TRIM BTR DBTBH MINBFD'
  108    FORMAT (A)
         WRITE  (LUDBG, 110) MERRULES%COR, MERRULES%EVOD, MERRULES%OPT, 
     &    MERRULES%MAXLEN, MERRULES%MINLEN, MERRULES%MERCHL,
     &    MERRULES%MINLENT, MERRULES%MTOPP, MERRULES%MTOPS, 
     &    MERRULES%STUMP,MERRULES%TRIM, MERRULES%BTR, MERRULES%DBTBH,
     &    MERRULES%MINBFD
  110    FORMAT (2X, A, 3X, I2, 3X, I3,3X, F7.1, F7.1, F7.1,
     &           F7.1, F7.1, F7.1,F7.1, F7.1, F7.1,F7.1, F7.1)

	   CLOSE(LUDBG)
      ENDIF
c  PMTFLG = 2 will user defined rule. otherwise use default rule.
      IF (PMTFLG.NE.2) PMTFLG = 3
C      CALL VOLLIBCS(REGN,FORSTI,VOLEQI,MTOPP,MTOPS,STUMP,
C     +    DBHOB,
C     &    DRCOB,HTTYPEI,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
C     &    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
C     &    VOL,LOGVOLI,LOGDIAI,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
C     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPECI,PRODI,HTTFLL,LIVEI,
C     &    BA,SI,CTYPEI,ERRFLAG, INDEB, PMTFLG, MERRULES)
!--------------------------------------------------------------------
      FORST   = FORSTI(1:2)
      VOLEQ   = VOLEQI(1:10)
      HTTYPE  = HTTYPEI(1:1)
      CONSPEC = CONSPECI(1:4)
      PROD    = PRODI(1:2)
      LIVE    = LIVEI(1:1)
      CTYPE   = CTYPEI(1:1)
!---------------------------------------------------
!Use array converter to reshape c arrays to fortran notation
      LOGVOL = RESHAPE(LOGVOLI, SHAPE(LOGVOL))
      LOGDIA = RESHAPE(LOGDIAI, SHAPE(LOGDIA))
      ERRFLAG = 0

      IF (PMTFLG .EQ. 2) THEN
    
           CALL VOLINIT2(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,3,7,15,20,21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG, MERRULES,IDIST)
      
      ELSE
           CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
          
!          IF (INDEB.eq.1 .AND. VOLEQ .EQ. 'F06FW2W202') THEN
!          WRITE (LUDBG,2)'After call VOLINIT'
!          ENDIF
       ENDIF

     
      !add null terminator required by C# strings
      FORSTI = FORST // char(0)
      VOLEQI = VOLEQ // char(0)
      HTTYPEI = HTTYPE // char(0)
      CONSPECI = CONSPEC // char(0)
      PRODI = PROD // char(0)
      LIVEI = LIVE // char(0)
      CTYPEI = CTYPE // char(0)

      END SUBROUTINE VOLLIBC2
      
C_______________________________________________________________________

      SUBROUTINE MRULESCS(REGN,VOLEQI,PRODI,TRIM,MINLEN,MAXLEN,OPT,
     +    MERCHL)
C_______________________________________________________________________
C 04/15/2014 YW Added this function to get the regional default merch rules.

! Expose subroutine MRULESCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::MRULESCS

      IMPLICIT NONE

C**********************************************************************      
!     Parameters
      INTEGER         REGN, OPT
      CHARACTER*(*)   PRODI, VOLEQI
      REAL            TRIM, MINLEN, MAXLEN,MERCHL
      
!     Local variables
      CHARACTER*1 COR 
      CHARACTER*2 FORST, PROD                 
      CHARACTER*3 MDL                 
      character*10 VOLEQ
      INTEGER EVOD
      REAL MTOPP,MTOPS,STUMP
      REAL MINLENT,MINBFD,BTR,DBTBH,DBHOB


      VOLEQ   = VOLEQI(1:10)
      PROD    = PRODI(1:2)
      
      CALL MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,
     >   MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,
     >   PROD)
     
      VOLEQI = VOLEQ // char(0)
      PRODI = PROD // char(0)
     
      END SUBROUTINE MRULESCS
      
C_______________________________________________________________________

      SUBROUTINE MRULESCS2(REGN,VOLEQI,PRODI,TRIM,MINLEN,MAXLEN,OPT,
     +    MERCHL,MTOPP,MTOPS,STUMP,MINLENT)
C_______________________________________________________________________
C 04/15/2014 YW Added this function to get the regional default merch rules.

! Expose subroutine MRULESCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::MRULESCS2

      IMPLICIT NONE

C**********************************************************************      
!     Parameters
      INTEGER         REGN, OPT
      CHARACTER*(*)   PRODI, VOLEQI
      REAL            TRIM, MINLEN, MAXLEN,MERCHL
      
!     Local variables
      CHARACTER*1 COR 
      CHARACTER*2 FORST, PROD                 
      CHARACTER*3 MDL                 
      character*10 VOLEQ
      INTEGER EVOD
      REAL MTOPP,MTOPS,STUMP
      REAL MINLENT,MINBFD,BTR,DBTBH,DBHOB


      VOLEQ   = VOLEQI(1:10)
      PROD    = PRODI(1:2)
      
      CALL MRULES(REGN,FORST,VOLEQ,DBHOB,COR,EVOD,OPT,MAXLEN,
     >   MINLEN,MERCHL,MINLENT,MTOPP,MTOPS,STUMP,TRIM,BTR,DBTBH,MINBFD,
     >   PROD)
     
      VOLEQI = VOLEQ // char(0)
      PRODI = PROD // char(0)
     
      END SUBROUTINE MRULESCS2      
C____________________________________________________________________      
      subroutine CALCDIACS(REGN,FORSTI,VOLEQI,STUMP,DBHOB,
     &    DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,
     &    AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)
! Expose subroutine MRULESCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::CALCDIACS

      IMPLICIT NONE
      
      CHARACTER*(*)   FORSTI, VOLEQI
!     Local variables
      CHARACTER(2)   FORST
      CHARACTER(10)  VOLEQ
c      CHARACTER(2)   PROD
     
!     MERCH VARIABLES 
      INTEGER REGN,BA,SI
      INTEGER ERRFLAG
        
!     TREE VARIABLES
      REAL HTTOT,HTUP,MHT,MTOPP,UHT,CUVOL
      REAL DBHOB,DRCOB,DBTBH,BTR,STUMP,TOP6
      INTEGER FCLASS
!	  3RD POINT VARIABLES
      REAL UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER HTREF
!     OUTPUTS
      REAL DIB,DOB
!     Variables to hold flewellings coefficients
      INTEGER SETOPT(6),JSP,MFLG,NEXTRA
      REAL RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      REAL HEX(2),dex(2), ZEX(2)
      REAL mTopS,ht1Prd,ht2Prd
      CHARACTER prod*2
      INTEGER SPN
      REAL TLH
      CHARACTER*3 MDL

      TLH = 0.
      
      FORST   = FORSTI(1:2)

      VOLEQ   = VOLEQI(1:10)
      CALL CALCDIA(REGN,FORST,VOLEQ,STUMP,DBHOB,
     &    DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,
     &    AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)
     
      !add null terminator required by C# strings
1000  FORSTI = FORST // char(0)
      VOLEQI = VOLEQ // char(0)
      END SUBROUTINE CALCDIACS
      
C________________________________________________________________________
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
      SUBROUTINE CRZBIOMASSCS(REGN,FORSTI,SPCD,DBHOB,DRCOB,HTTOT,FCLASS,
     +   VOL,WF,BMS,ERRFLG)
! Expose subroutine CRZBIOMASSCS to C# users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::CRZBIOMASSCS

      IMPLICIT NONE
      INTEGER REGN, SPCD, ERRFLG, FCLASS
      REAL DBHOB, HTTOT, VOL(15), WF(3), BMS(8), WFI(3), DRCOB
      CHARACTER*(*) FORSTI
      CHARACTER(2) FORST
      
      FORST   = FORSTI(1:2)
      CALL CRZBIOMASS(REGN,FORST,SPCD,DBHOB,DRCOB,HTTOT,FCLASS,
     &      VOL,WF,BMS,ERRFLG)
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
      