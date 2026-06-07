#ifndef VOLMT_H
#define VOLMT_H
#include "f90char.h"
#include "Tree.h"
#include <mk4.h>
#include <mk4str.h>

const static int i3 = 3;
const static int i7 = 7;
const static int i15 = 15;
const static int i20 = 20;
const static int i21 = 21;

typedef void (*impFunc)(int*);
typedef void (*getvFunc)(int*, f90Char*, f90Char*, int*
       , f90Char*, f90Char*, int*);
typedef void (*volFunc)(int*,f90Char*, f90Char*, float *,float *,
		 float*, float*, float*, f90Char*, float*, int*, float*, float*,
		 float*, float*, float*, float*, int*, float*, float*, int*, float*,
		 float*, int&, int&, int& , int&, int&, float*, float[i7][i20],
     float[i21][i3], float*,float*, int*,float*, float*, int*, int*,
     int*, int*, int*, f90Char*, f90Char*, int*, f90Char*,
     int*, int*, f90Char*, int*, int*);
       


class VolMT
{
    
public:
  VolMT();
  void on_goButton_clicked();
  void on_getVolSerial();
  void on_getVolParallel();
  void createTrees();
  void loadTrees();
  void getCruiseInfo();
  void getVolume(Tree);
  void getDLL();
private:
  
  
  impFunc VERNUM; 
  getvFunc GETVOLEQ;
  volFunc VOLLIB09;
  HINSTANCE hinstLib;
  
  int region;
  int saleNo;
  CString cruiseNo;
  CString saleName;
  CString forest;
  CString district;
  CString purpose;
  CString remarks;
  
  CString fileName;
  
  vector<Tree> trees;

  
};

#endif
