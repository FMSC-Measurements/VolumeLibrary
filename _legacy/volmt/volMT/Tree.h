#ifndef TREE_H
#define TREE_H
#include <string>
#include <vector>
#include <atlstr.h>
#include "f90char.h"

using namespace std;

//weight factors for oregon and washington forests in pounds per cubic foot      
const static int dougFirWLte15 = 59; //species 202
const static int dougFirWGt15  = 51; //species 202
const static int whemlock      = 64; //species 263
const static int redalder      = 57; //species 351
const static int sitkaSpruce   = 54; //species 98
const static int dougFirEast   = 61; //species 202
const static int whiteFir      = 68; //species 119
const static int pondPine      = 69; //species 122
const static int lodgePine     = 63; //species 108
const static int westLarch     = 55; //species 73

typedef void (*getvFunc)(int*, f90Char*, f90Char*, int*
       , f90Char*, f90Char*, int*);

class Tree
{


  public:
          Tree();
          Tree(CString, CString, CString, float, float, float, float, float, float, CString, CString);
          Tree(char *, char * , float, float, float);
          
          int getWeight();
          float getVol();
          float getDBH();
          float getDRCOB();
          float getTotalHeight();
          float getMrchHtP();
          float getMrchHtS();
          float getTopDIBp();
          float getTopDIBs();
          float getWeightFactorPP();
          float getWeightFactorSP();
          CString getTreeNum();
          CString getProdP();
          CString getProdS();
          
          int   getTreeNumI();
          int   getSpeciesNum();
          void setTreeNum(CString tnum){treeNum = tnum;};
          CString getSpecies();
          CString getCutUnit();
          			    
  private:
  				CString treeNum;
  				CString species;
  				int speciesNum;
  				CString cutUnit;
  				float dbh;
  				float drcob;
  				float totalHeight;
  				float topDIBp;
  				float topDIBs;
  				float mrchHtP;
  				float mrchHtS;
 				  CString prodP;
 				  CString prodS;
 				  CString volumeEquation;
  				float gcCuFtVol;
  				float VOL[15];
  				float weightFactorPP;
  				float weightFactorSP;  
  									  
};
#endif