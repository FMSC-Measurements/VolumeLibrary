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
C 20160922 (1)Added DRC to DBH conversion and when DRC is measured, convert it to DBH bafore call Jenkins equation for biomass calc
C          (2)Added output variable LOGDIA,LOGLEN,LOGVOL to R4vol subroutine
C          (3)Updated R6 weight factor for GF, NF, PSF, DF and MH in Mt Hood, DF in Siuslaw and WF, PP and DF in Umatilla NF.
C 20160930 Correct the error in CRZSPDFT for searching the weight factor in the last of the array 
C          and made correction to LAST = HALF-1 to avoid infinit loop.
C 20161121 Removed the weight factor for douglas-fir in Siuslaw NF. They want to use the regional weight factor for DF.
C 20170209 Added EZVOLLIB subroutine to volumelibrary.f
C 20170214 Added vernum_r, getvoleq_r and vollib_r for use by R program
C 20170227 Modified crzbiomass small tree biomass calculation, i.e. when merch stem vol is calculated, the stem biomass is also calculated.
C 20170414 Removed 532WO2W*** eqn from R5_EQN list, modified CALCDIA to include R4 MAT model, added HT2TOPD to calculate stem HT to a
C          given stem DIB, modified log vol calc for R8 and R9 to also include topwood logs, moved stump and tip vol calc to volinit sunroutine.
C 20170525 Added BIOLIB subroutine to export biomass
C 20170721 Added new equation number for R8 Clark equation using R9 codes. The new R8 equation looks like 8*1CLKE*** and update R6 weight factor.
C          Modified R9clark to recognize broken top input from ht1prd and mtopp ot ht2prd and mtops
C 20170918 Added VOLLIBVB in volumelibrary.f to let VB.net to call the library with MERRULES variable.
C 20171221 Modified R9Clark for NaN error, modified Mrule for custom rule, add R8clkht to r8vol2
C 20180125 Removed R9clark broken top calculation, which cause other problem for cruise processing
c 20180202 modifying R8prep with outside bark coef
c 20180312 Modified r8prep and r9clark for R8 upsHt1
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

      VERSION = 20180312
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

      VERSION = 20180312
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
     
      VERSION = 20180312
      
      PRINT     '(I8)', VERSION
      RETURN
      END SUBROUTINE VERNUM_F
C*********************************************************************

      subroutine vernum_r(version)
C     R program need subroutine name to be all lower case
c      !DEC$ ATTRIBUTES STDCALL,REFERENCE,DLLEXPORT::vernum_r
c      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG ::vernum_r
c      !DEC$ ATTRIBUTES DECORATE, ALIAS:'vernum_r_'::vernum_r
      !DEC$ ATTRIBUTES DLLEXPORT::vernum_r
      !DEC$ ATTRIBUTES C, REFERENCE, ALIAS:'vernum_r_'::vernum_r

      integer version
      version = 20180312
      return
      end subroutine vernum_r