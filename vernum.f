      SUBROUTINE VERNUM(VERSION)

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

      VERSION = 20151008
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

      VERSION = 20151008
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
     
      VERSION = 20151008
      
      PRINT     '(I8)', VERSION
      RETURN
      END SUBROUTINE VERNUM_F