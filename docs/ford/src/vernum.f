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
! 20180913 Modify FCLASS for use with single/multi-stem as: =1 single, =0 multistems in the consistent way for R2 and R3.
!          Also added Hahn (NC-250) equations (R9) to the library
! 20181210 Added FIA volume equations to the library
C 20190426 Changed volinit.f to set the default stump and MTOPP for FIA equation only
C          Added equation for BIA I16BEHW000 (Behr), I00DVEW000 (Johnson formclass) and C00DVEE***.
C          Added Equation for FVS A02DVEW094 (Malone et al 2013) for statewide white spruce
C          Added equation for blackjack 301HAB0122, 302HAB0122 and yellow pine 300HAB1122
! 20190514 Added biomass equation from Affleck 2019, added more input variables to BIOLIB subroutine,
!          added UPSHT1 and UPSD1 to calcdib_r, calc_dob_r,ht2topd_r subroutines,
!          added new subroutine biomasslib_r for biomass calculation
! 20190531 Changed volinit.f to use 80 for CF to cord convertor for R3. 
!          And put back the boardfoot calculation for topwood made on 20170414
! 20190718 Temp change for R3 Mrules to run a particular cruise processing
! 20190925 Made biomass library also able to use FIA biomass equation number and also changed R8 new clark equation for prod 08 to use MTOPP as DIB
! 20191231 Merch rule changes for Region 3 and added FIA equation for paulownia from Berg etal 2019
! 20200109 Correct R3 MINLENT error (it was set to = MINLEN)
! 20200219 Correct MERCHL for prod 01, 08, 20 to 10.0
! 20200319 R6 requests to use 102.4 for cuft to Cord
! 20200520 Added getwtfactor_r for R program
! 20200729 Added vollib2_r with log info variables.
! 20201109 Modified r9clark to check HTTOT value for R8 prod 08 calculation and modified FIA_volinit
! 20201123 Updated R3 region wide weight_factor and moiture content for 122,202,15 and 119
! 20210105 Corrected R10 Larson equation for small trees (DBH<6) in R10d2h.f. This affects volume of A00DVEW094,A00DVEW375,A00DVEW747.
!          Also corrected Demar small tree volume of A01DEMW000 and A02DEMW000 in R10vol.f
! 20210219 Added fwdbt_r for R program to get Flewelling Equation double bark thikness at given ht and dob
! 20210701 Added subroutine vollibfsveg for FSVeg to call the library
! 20200719 Set FCLASS initial to 0 in vollibfia.f in order to get the species default form class.
! 20210727 Added DBTBH as input variable for vollibfsveg and also modified default for FCLASS for DVE equation in Region 2,3,4
! 20210811 (1) Set R6 default stump=1.0 in mrules.f, (2)add input variable stump, ba and si to vollibfsveg, (3) enabled r8vol to call r8_mhts for fsveg test
! 20210823 Modified Mrules to reset R3 MINBFD back to 1.0
! 20211026 Removed weight factor for R03F10 (Santa Fe NF)
! 20211108 Corrected str2int sunroutine. This affect FIA biomass calculation
! 20211117 Added BIOLIBCS in vollibcs.f for C# call wrapper
! 20211220 Set R6 default stump = 0 (it was changed to 1.0 when testing FSVeg)
! 20220414 Modified CRZBIOMASS to return green biomass when select BIOEQ is green or dry 
! 20220503 Modified VOLINIY to Set the total cubic vol to VOL(4) if MTOPP=0.1      
! 20220819 Added new equation 223DVEW122 for R2 Black Hills non-saw product (DBH<9)
!          modified subroutines include R2OLDV, DVEST, VOLINIT, VOLINIT2, GROSSVOL,
!          also added biomass equation WA2122AGT01G and WA2122CRW01G      
! 20220830 Added FVS changes (variable initialization) on some routines and changed the merch volume for R2 Black Hills 223DVEW122
!          to be sum of individual log volumes, 
! 20220929 Updated voleqdef for FVS AK variant 
! 20221025 Fix the change made on 2022/05/03 to be for R2 only    
! 20221129 (1)Created new subroutines for R2 Black Hills 223DVEW122 and also modified CalcDia2 for this equation  
!          (2)Included changes from FVS on some files    
! 20230104 Correted the errflag for checking DBHOB and HTTOT in caldib and profile and also Removed errflag from stump vol calc in volinit 
! 20230118 Changed R1 Mrules min topwood length MINLENT to 16 
! 20230124 Corrected changes in profile made on 20200219 for call SEGMNT in CUPFLG   
! 20230221* Added NSVB volume and biomass equations and VOLLIBCSNEW, VOLUMELIBRARY2 and vollibnew_r for DLL   
! 20230223 Updated R6 Wallowa-Whitman NF(16) Whitman Ranger District number to 31 and Changed R1 Prod 08 minimum merch length MERCHL to 16   
! 20230310 Corrected R3 merch rules for MINLEN, MINLENT and OPT. It was not set correctly in the library based on R3 rules.  
! 20230314 Modified NVBC to calculate sawtimber and merch volume to be as that in the paper not sum of logvol
!          Also modified volinitnew for DRYBIO and GRNBIO based on the merch volume from non-NVB equation
!20230315 Corrected error in BIOLIB and BIOLIB2 for the Excel function     
!20230403* Added R1 wight factor and changed subroutine name with _NEW to _NVB    
!20230526 Added GETWTFAC subroutine to volumelibrary.f to get default weight factor for Excel function    
!20230605 Modified vollibfsveg to reset the REGN and FORST for R1 BEH equation to get the default form class   
!20230609 Modified r8prep to set DIB17 = 0.1 to avoid divided by 0 error when DIB17 <= 0      
!20230622 Modified r9cuft calculation to handle very small number 2^-126 and r9prep call mrules and also check mtops and mtopp in mrules
!20230802* Combined BTR for DF(202) to make 300FW2W202  
!20230809 Fixed bugs found by FVS testing.  
!20230818 Added MINLENT initial values in MRULES for R8 and R9 and topwood log calc for LEFTOV > MINLEN   
!20231106 Modified nsvb.f bole biomass calc with CULL and other minor changes with test for FIADB vol and biomass      
!         Check STUMP>0 before stump vol calc
!20240410 (1)Modified VOLINITNVB to make woodland species use regional biomass equation for biomass calculation and Jenkins for foliage.  
!         (2)Added variable initial value 0 to D2 and DIB in CALCDIA2 and BLMTAP and BEHTAP to avoid error. 
!         (3)Added weight factor for R06F16(Wallowa Whitman) and DeadWeightFactor to RegDftData.inc
!         (4)Updated R03 weight factor and set R4 regional wide weight factor
!20240423 Modified VOLINITNVB to add variable NVBNOLOGP, NVBNOLOGS and NVBTLOGS     
!20240429 Modified VOLINITNVB and vollibnvb_r to add new variable CULLMSTOP for missing top CULL      
!20240513 Checked LIVE variable in VOINITNVB and NSVB subroutine   
!20240605 Added input variable PROD to CRZBIOMASS and CRZBIOMASSCS subroutines for non-saw product biomass calculation using non-saw weight factor.    
      !   Changed the default species in searching wdbkdada.inc to species 999 for invalid species instead of the TOPSPC
      !   Also changed CRZBIOMASS to check WF input before call REGNSPDFT for regional default
!20240626 Added cord volume calculation for NSVB equation.  
!20240711 Added VOLLIBCPP to volapss.f for C++ interface subroutine   
!20240805 Added vollibnvb2_2 to volumelibrary.f to allow input variable max log lenth MAXLOGLEN      
!20241007 Added GETREGNWFCS for C# to get regional default live and dead weight factor and modified voleqdef.f.    
!20241101 Modified VOLINITNVB to calculate biomass for DBH only trees using Jenkins method 
!20241118 Modified VOLINITNVB to calculate GRNBIO using VOL and weight factor for cruise VOLEQ (not NVB equation)   
!20250227 Modified nsvb.f and voiinit.f to set log weight to LOGVOL for CTYPE=Cruise and
!         Updated R6_EQN for species Incense cedar(81) to use I00FW2W073 and Grand fir(17) to use I00FW2W017 in Willamette NF    
!20250310 Fixed r9logs to make sure even logs and modified R9 MINLEN = 2
!20250325 Updated volinit.f for biomass adjustment for cruise VOLEQ (non-NSVB equation) and
!         updated R4 dead weight factor based on study data at Deer Hollow, Mt. Dutton, Navajo Basin      
!20250401 Fixed nsvb.f to make sure HTTOT>0 for calculation and added CTYPE='B' chek for DBH only tree.
!20250512 Fixed nsvb.f to calculate merch height for FIA (CTYPE='I') to be minimum 5, 
!         applied denProp based on DECAYCD to woodland species biomass, and added species checck for 63 and 65 in woodland_bio   
!20250527 Fixed volinit.f for potential divided by zero problem and modified voleqdef.f (R8_CEQN) species list to include all timber cruise species.      
!20250701 Modified nsvb.f to add default DECAYCD (3), made correction in NVB_BrchRem subroutine and 
!      checked max number logs (20) before call numlogs.f to catch error code for more than 20 logs
!20250805 Added weight factor for Juniper (60 and 64) for region 6 forest 01 and 07.
!20250822 Added check for REGN=10 in nolinit.f for CUR and DEM equation.
!20251021 Modified nsvb.f to check MTOPS < DBHOB and limit the Ht to not below stump     
!...  Contains the volume library version number
!...  This is simply the date of the latest release/version

!			USE DEBUG_MOD

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
!      IF (DEBUG%ANALYSIS) THEN
!         WRITE  (LUDBG, 15) ' -->Enter VERNUM'
!   15    FORMAT (A)   
!   		END IF

      VERSION = 20251021
      RETURN
      END SUBROUTINE VERNUM
      
!***********************************************************************
      SUBROUTINE VERNUM2(VERSION)

!...  Contains the volume library version number
!...  This is simply the date of the latest release/version

!	USE DEBUG_MOD

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
!      IF (DEBUG%ANALYSIS) THEN
!         WRITE  (LUDBG, 15) ' -->Enter VERNUM'
!   15    FORMAT (A)   
!   		END IF

      VERSION = 20251021
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
     
      VERSION = 20251021
      
      PRINT     '(I8)', VERSION
      RETURN
      END SUBROUTINE VERNUM_F
C*********************************************************************

      subroutine vernum_r(version)
C     R program need subroutine name to be all lower case
      !DEC$ ATTRIBUTES DLLEXPORT::vernum_r
      !DEC$ ATTRIBUTES C, REFERENCE, ALIAS:'vernum_r_'::vernum_r

      integer version
      version = 20251021
      return
      end subroutine vernum_r