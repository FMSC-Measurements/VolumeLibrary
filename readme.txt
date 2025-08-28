This directory has two ways to compile the volume library, but only one works at a time.

1) as a standalone program, useful for debugging and testing.

2) as the standard dll used by cruise processing, excel funtions, etc.

***********************************************
Compiling option 1:

To compile as a standalone you need the main function contained in volfor.*  This file
needs to be renamed volfor.f if it currently is set to something else (which it will be if
the latest compile was as a dll, since the main in volfor creates problems with a dll).  Once
this is taken care of you can compile with using the comp2 batch file (comp2.bat).  First, you 
must set intel vis fortran options using the comman ifortvars.

Additionally, depending on which version of the entry point (subroutine, e.g., vollib, volumelibrary)
you wish to use you will have to comment out the external declarations so subroutines are available
locally.  e.g.,  !    !DEC$ ATTRIBUTES DLLEXPORT::VOLUMELIBRARY
      		 !    !DEC$ ATTRIBUTES ALIAS:'_VOLUMELIBRARY@224' :: VOLUMELIBRARY

to make VOLUMELIBRARY available locally.


      