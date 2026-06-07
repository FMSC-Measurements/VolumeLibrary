//myfuncs.cpp
#include "myfuncs.h"

std::string getStdString(CString inStr)
{
	std::string outStr = (CStringA(inStr));
  return outStr;
}



void throwErrorException(char * msg)
{
//	QMessageBox msgBox;
//  msgBox.setText(msg);
//  msgBox.exec();
}