#include <string>
#include "Tree.h"
#include <fstream>

#define LOGVOL(i,j)LOGVOL[j-1][i-1]

Tree::Tree(CString cutU, CString spec, CString tNum, float dia, float ht, float topdbp, float topdbs, float mrhtp, float mrhts, CString pp, CString sp)
{
  cutUnit = cutU;
  species = spec;
  treeNum = tNum;
  dbh = dia;
  drcob = 0.0;
  totalHeight = ht;
  topDIBp = topdbp;
  topDIBs = topdbs;
  mrchHtP = mrhtp;
  mrchHtS = mrhts;
  prodP = pp;
  prodS = sp;
  //gcCuFtVol = gCFtVol;
  //do I need default values???
}

Tree::Tree(char * spec, char * tNum, float dia, float ht, float topdb)
{
  sprintf(spec,"%s",species);
  sprintf(tNum,"%s",treeNum);
  dbh = dia;
  totalHeight = ht;
  topDIBp = topdb;
//  volEqDIBS = 0.0;
}


Tree::Tree()
{
	cutUnit = "1";
	species = "125";
	treeNum = "1";
	dbh = 11.3;
	totalHeight = 45.3;
	topDIBp = 7.4;
    drcob = 0.0;
    topDIBs = 4.1;
    mrchHtP = 25.0;
    mrchHtS = 12.0;
    prodP = "01";
    prodS = "02";
}

/*
void Tree::convert2weight()
{
  switch(speciesNum) {
      case(202):
      	//test for east vs west side
         if (dbh <= 15.0)
            weight = gcCuFtVol * dougFirWLte15;
         else
            weight = gcCuFtVol * dougFirWGt15;
      break;
      case(263):
        weight = gcCuFtVol * whemlock;
      break;
      case(351):
      	weight = gcCuFtVol * redalder;
      break;
      case(98):
      	weight= gcCuFtVol * sitkaSpruce;
      break;
      case(119):
      	weight= gcCuFtVol * whiteFir;
      break;
      case(122):
      	weight= gcCuFtVol * pondPine;
      break;
      case(108):
      	weight= gcCuFtVol * lodgePine;
      break;
      case(73):
      	weight= gcCuFtVol * westLarch;
      break;
  }
}
*/
//int Tree::getWeight()
//{
//	return weight;
//}

float Tree::getVol()
{
	return gcCuFtVol;
}

float Tree::getDBH()
{
	return dbh;
}

float Tree::getDRCOB()
{
	return drcob;
}

float Tree::getTotalHeight()
{
	return totalHeight;
}

float Tree::getMrchHtP()
{
	return mrchHtP;
}

float Tree::getMrchHtS()
{
	return mrchHtS;
}

float Tree::getTopDIBp()
{
	return topDIBp;
}

float Tree::getTopDIBs()
{
	return topDIBs;
}		

CString Tree::getTreeNum()
{
	return treeNum;
}

CString Tree::getSpecies()
{
	return species;
}

// operator== for Item* because we have Item* as container elements
//bool Tree::treeNumComp(CString isNum)
//  {
//   return isNum.Compare(treeNum);
//  }
  
int Tree::getTreeNumI()
{
	return _ttoi(treeNum);
}


CString Tree::getProdP()
{
	return prodP;
}

CString Tree::getProdS()
{
	return prodS;
}



int Tree::getSpeciesNum()
{
	return speciesNum;
}


