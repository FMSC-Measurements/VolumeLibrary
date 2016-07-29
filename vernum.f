      SUBROUTINE VERNUM(VERSION)
! 20151130 For R8 Clark profile use MINLEN = 2 to test BDFT
! 20151201 Added function VOLLIBVB8XHT for VB project to get merch HT
C 20160113 Added BTR default value for Region 3 Santa Fe forest DF and PP
C 20160125 Modify r9ht in r9clark.f to check NaN value for stemHt. The old check doesnot work well.
C 20160126 Added weight factor for R3 Santa Fe forest (Modified regdftdata.inc)
C 20160302 Modified r9clark.f to remove the error check for HtTot < 17.3. The calc will use small tree logic
C 20160322 Modified r8clkdib.f DIB calculation for UPSHT1 < 17.3 to avoid Nan value error. 
C          Added hardcoded weightfactor for DF in Rogue River - Siskiyou NF based on DBH
C          Added volume rounding to R8CLARK to be same as the one in r8vol rounding
C
C 20160408 Added BTR for R3 F10 WF (91.8) for testing
C 20160505 Added DIST to VOLINIT and VOLLIBCS to set VOL(2) with Scribner or International for Region 8.
C 20160517 Added biomass calculation for sapling adjustment factor; set LOGST to 0 in Profile.f for non variable log cruising
C 20160713 Modified fwinit to add 301FW2W122, 301FW2W202, 301FW2W015, and 301FW2W108 for Santa Fe NF.
!...  Contains the volume library version number
!...  This is simply the date of the latest release/version

			USE DEBUG_MOD

      IMPLICIT NONE
      
!REV  Created KC? 09/26/07
!REV  Revised TDH 04/01/09 moved to separate file

!...  Expose subroutine VERNUM to users of this DLL
!...  Commented out for local testing, remove comements for 
!...  export to dll      
      !DEC$ ATTRIBUTES STDCALL,REFERENCE,DLLEXPORT::VERNUM
!      !DEC$ ATTRIBUTES STDCALL,DLLEXPORT::VERNUM
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG ::VERNUM
!      !DEC$ ATTRIBUTES ALIAS:'_VERNUM@4' :: VERNUM
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'VERNUM'::VERNUM


!     Parameters      
      INTEGER VERSION

!---------------------------------------------------------------------
      IF (DEBUG%ANALYSIS) THEN
         WRITE  (LUDBG, 15) ' -->Enter VERNUM'
   15    FORMAT (A)   
   		END IF

      VERSION = 20160713
      RETURN
      END SUBROUTINE VERNUM
      
!***********************************************************************
      SUBROUTINE VERNUM2(VERSION)

!...  Contains the volume library version number
!...  This is simply the date of the latest release/version

	USE DEBUG_MOD

      IMPLICIT NONE
      
!REV  Created KC? 09/26/07
!REV  Revised TDH 04/01/09 moved to separate file
!REV  Revised TDH 02/24/10 removed alias, now it exports fine

!...  Expose subroutine VERNUM to users of this DLL
!...  Commented out for local testing, remove comements for 
!...  export to dll      
	!DEC$ ATTRIBUTES DLLEXPORT::VERNUM2
!      !DEC$ ATTRIBUTES ALIAS:'_VERNUM2@4' :: VERNUM2

!     Parameters      
      INTEGER VERSION

!---------------------------------------------------------------------
      IF (DEBUG%ANALYSIS) THEN
         WRITE  (LUDBG, 15) ' -->Enter VERNUM'
   15    FORMAT (A)   
   		END IF

      VERSION = 20160713
      RETURN
      END SUBROUTINE VERNUM2

!***********************************************************************
      
      SUBROUTINE VERNUM_F(VERSION)

!...  Contains the volume library version number
!...  This is simply the date of the latest release/version

      !DEC$ ATTRIBUTES DLLEXPORT::VERNUM_F

      IMPLICIT NONE
      
!REV  Created TDH 11/18/10 
!REV  Revised ... ../../..

!     Parameters      
      INTEGER VERSION

!---------------------------------------------------------------------
     
      VERSION = 20160713
      
      PRINT     '(I8)', VERSION
      RETURN
      END SUBROUTINE VERNUM_F