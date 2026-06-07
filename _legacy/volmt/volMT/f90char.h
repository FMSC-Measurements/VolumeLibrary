#ifndef F90CHAR_H
#define F90CHAR_H
#define _CRT_SECURE_NO_DEPRECATE
#include <string>
struct f90Char{
	 f90Char();
	 int length;
	 char str[256];
	 
	 f90Char(int alen, std::string sstr){
		 for(int i=0; i<256; i++)str[i]=' ';//pad the char array
		 strcpy (str, sstr.c_str());	
		 length = alen;
	}
};
#endif