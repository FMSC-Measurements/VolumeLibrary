!*******************************************************************************************
!*******************************************************************************************
!== last modified  4-4-2017
! Added calcdib_r subroutind for R program to use the CALCDIA (4/4/2017)
! Added DIB calculation for region 8 Clark equation (6/4/14)
! Added DIB calculation for Behr equation (1/28/2014)
! Added stump diameter (from ground to 4.5 ft) calculation for non profile model using Raile 1983 (YW)
! YW 2016/01/13 Added BTR default value for Region 3 Santa Fe forest DF and PP
!  CalcDia.f90 
!  FUNCTIONS/SUBROUTINES exported from VOLLIB.dll:
!	CALCDIA      - subroutine 
!
      subroutine CALCDIA(REGN,FORST,VOLEQ,STUMP,DBHOB,
     &    DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,
     &    AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)

  ! Expose subroutine CALCDIA to users of this DLL
  !
  !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::CALCDIA
  !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG :: CALCDIA
  !DEC$ ATTRIBUTES DECORATE, ALIAS:'CALCDIA'::CALCDIA
      CHARACTER*(*) FORST
      CHARACTER*(*) VOLEQ
      
      INTEGER REGN,ERRFLAG,FCLASS,HTREF
      REAL STUMP,DBHOB,DRCOB,HTTOT
      REAL UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2
      REAL DBTBH,BTR,HTUP,DIB,DOB
      CHARACTER*10 VOLEQI
      CHARACTER*2 FORSTI
      
      FORSTI = FORST(1:2)
      VOLEQI = VOLEQ(1:10)
      CALL CALCDIA2(REGN,FORSTI,VOLEQI,STUMP,DBHOB,
     &    DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,
     &    AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)
     
      RETURN
      END
C **************************************************************      
      subroutine CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,
     &    DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,
     &    AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)
      
      CHARACTER*10 VOLEQ
      CHARACTER*2 FORST
      CHARACTER*3 MDL

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
      REAL DIB,DOB, UPSHT
!         Variables to hold flewellings coefficients
      INTEGER SETOPT(6),JSP,MFLG,NEXTRA
      REAL RHFW(4),RFLW(6),TAPCOE(12),F,FMOD(3),PINV_Z(2)
      REAL HEX(2),dex(2), ZEX(2)
      REAL mTopS,ht1Prd,ht2Prd
      CHARACTER prod*2
      INTEGER SPN
      REAL TLH
      ! Variable for R4 taper
      REAL STUMPD,BUTTCF,CF0,B
      
      TLH = 0.
!     ARRAYS
! initialize profile model  
!C  heck for a DBH of less than 1.  Drop out of volume if true.  10/97
      if(dbhob.lt.1) then
        errflag = 3
        goto 1000
      endif

      IF(VOLEQ .EQ. "")THEN
         ERRFLAG = 1
         GOTO 1000
      ENDIF
      MFLG = 2
      MHT = 0
      MTOPP = 0
      MDL = VOLEQ(4:6)
      prod = '01'
      
      IF(MDL.EQ.'FW2' .OR. MDL.EQ.'fw2' .OR. MDL.EQ.'FW3' .OR.
     &   MDL.EQ.'fw3' .OR. MDL.EQ.'CZ2' .OR. MDL.EQ.'cz2' .OR.
     &   MDL.EQ.'CZ3' .OR. MDL.EQ.'cz3' .OR. MDL.EQ.'WO2' .OR.     
     &   MDL.EQ.'wo2' .OR. MDL.EQ.'F32' .OR. MDL.EQ.'f32' .OR.
     &   MDL.EQ.'F33' .OR. MDL.EQ.'f33' .OR. MDL.EQ.'JB2' .OR.
     &   MDL.EQ.'jb2' .OR. MDL.EQ.'DEM' .OR. MDL.EQ.'CUR' .OR.
     &   MDL.EQ.'dem' .OR. MDL.EQ.'cur') THEN
!************************
!    FLEWELLING MODELS  *
!    REGION 2 MODELS    *
!    REGION 5 MODELS    * 
!************************
        IF (VOLEQ(4:4).EQ.'F' .OR. VOLEQ(4:4).EQ.'f') THEN
!--   Initialize Flewelling model for this tree
          CALL FWINIT(VOLEQ,DBHOB,HTTOT,MHT,MTOPP,UPSHT1,UPSHT2,UPSD1,
     &       UPSD2,AVGZ1,AVGZ2,HTREF,DBTBH,JSP,RHFW,RFLW,
     &       TAPCOE,F,SETOPT,NEXTRA,HEX,DEX,ZEX,PINV_Z,FMOD,btr,FCLASS,
     &       ERRFLAG)
       
        ELSEIF (VOLEQ(4:6).EQ.'CZ3' .OR. VOLEQ(4:6).EQ.'cz3') THEN
!        initialize Czaplewski three point model
         IF(HTTOT.LE.4.5)THEN
  	         ERRFLAG = 4
	         GOTO 1000
         ENDIF
         UHT = HTTOT * 0.95
         if(UPSHT1.LE.0 .or. UPSD1.LE.0) THEN 
            ERRFLAG = 9
            GO TO 1000
         endif
         if(UPSHT1.LE.4.5 .or. UPSHT1.GT.UHT) then
            ERRFLAG = 10
            GO TO 1000
         endif      
         HEX(1) = UPSHT1
         DEX(1) = UPSD1
         CALL TOP6LEN(VOLEQ,HTTOT,DBHOB,DEX,HEX,STUMP,6.0,
     &                TOP6,DBTBH,errflag)
        ELSE
!C       CHECK FOR TOTAL TREE HEIGHT
        IF(HTTOT.LE.4.5)THEN
	     ERRFLAG = 4
	     GOTO 1000
        ENDIF
      ENDIF
! GET THE DIAMETERS
      CALL TAPERMODEL(VOLEQ,FORST,JSP,NEXTRA,SETOPT,DBHOB,HTTOT,
     &      DBTBH,HEX,DEX,ZEX,RHFW,RFLW,TAPCOE,F,FMOD,PINV_Z,TOP6,HTUP,
     &      MTOPP,MFLG,CUVOL,DIB,DOB,errflag)
C Added Clark profile model for region 9
      ELSEIF (MDL.EQ.'CLK' .OR. MDL.EQ.'clk') THEN
        IF (VOLEQ(1:1).EQ.'9'.OR.
     +     (VOLEQ(1:1).EQ.'8'.AND.VOLEQ(3:3).EQ.'1')) THEN
          ht2Prd = UPSHT2
          ht1Prd = UPSHT1
          mTopP = UPSD1
          mTopS = UPSD2
          errFlag = 0
c reset UPSHT1 to 0 (yw 09/24/2012)          
          UPSHT = 0
          CALL r9clarkdib (VOLEQ,STUMP,mTopP,mTopS,DBHOB,
     &                    ht1Prd,ht2Prd,HTTOT,HTUP,DIB,prod,errFlag,
     &                    UPSHT)
        ELSE
          CALL R8CLKDIB(VOLEQ, FORST, DBHOB, HTTOT, UPSHT1,HTUP,DIB, 
     &                  ERRFLAG)
        ENDIF
      ELSEIF (MDL.EQ.'BEH' .OR. MDL.EQ.'beh') THEN
C     added DIB calculation for Behr equation
         IF (FCLASS.LE.0) THEN
           CALL GETFCLASS(VOLEQ,FORST,DBHOB,FCLASS)
         ENDIF
         CALL BEHTAP(VOLEQ,DBHOB,HTTOT,TLH,HTUP,FCLASS,MTOPP,DIB)    
C     Added the calculation for R4 taper equation (03/20/2017)
      ELSEIF (MDL.EQ.'MAT' .OR. MDL.EQ.'mat') THEN  
         DIB = 0.0
         CALL R4MATTAPER(VOLEQ,DBHOB,HTTOT,STUMPD,BUTTCF,CF0,B,
     +     HTUP,DIB,ERRFLAG)      
C calculation for diameter from ground to 4.5 ft heigh for non profile model
C added on 7/22/2012 YW
C using Raile 1982
      ELSE
        IF (HTUP .LT. 4.5) THEN
          READ (VOLEQ(8:10),'(I3)') SPN
          IF (HTUP .LT. 0.0001) HTUP = 1.0
          CALL STUMPDIA(SPN, DBHOB, HTUP, DIB, DOB)
          RETURN
        ENDIF      
      ENDIF

 1000 RETURN
      end subroutine CALCDIA2
      
C *******************************************************************************
      subroutine calcdib_r(VOLEQ,REGN,FORST,DBHOB_d,HTTOT_d,
     + HTUP_d,DIB_d, ERRFLAG)
C This subroutine is for R user to calculate DIB at given height      !
C YW 04/04/2017

      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::calcdib_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'calcdib_r_'::calcdib_r

      IMPLICIT NONE
      
      DOUBLE PRECISION DBHOB_d,HTTOT_d,DIB_d,HTUP_d
      CHARACTER*2  FORST 
      CHARACTER*10 VOLEQ
      INTEGER      REGN,ERRFLAG 
      
!   Tree variables
      REAL 		HTTOT,HTUP,DIB,DOB 
      REAL 		DBHOB,DRCOB,DBTBH,BTR,STUMP  
      INTEGER   FCLASS 
    
!	3RD POINT VARIABLES
      REAL      UPSD1,UPSD2,UPSHT1,UPSHT2,AVGZ1,AVGZ2    
      INTEGER 	HTREF
      
      DBHOB = REAL(DBHOB_d)
      HTTOT = REAL(HTTOT_d)
      HTUP = REAL(HTUP_d)
C     Set the default value for other variable
      UPSHT1 = 0.0
      UPSD1 = 0.0
      STUMP = 0.0
      DBTBH = 0.0
      BTR = 0.0
      AVGZ1=0.0
      HTREF=0
      UPSHT2=0.0
      UPSD2=0.0
      AVGZ2=0.0
      DRCOB=0.0
      FCLASS = 0

      CALL CALCDIA2(REGN,FORST,VOLEQ,STUMP,DBHOB,
     &    DRCOB,HTTOT,UPSHT1,UPSHT2,UPSD1,UPSD2,HTREF,AVGZ1,
     &    AVGZ2,FCLASS,DBTBH,BTR,HTUP,DIB,DOB,ERRFLAG)
           
      DIB_d = DBLE(DIB)      

      CONTINUE
      RETURN
      end subroutine calcdib_r         