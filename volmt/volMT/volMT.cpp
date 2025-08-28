#include <windows.h> 
#include <stdio.h> 
#include <string>
#include <iostream>
#include <ctime>
#include <omp.h>
#include "volMT.h"
#include "myfuncs.h"
#include <mk4.h>
#include <mk4str.h>

//macros - allow the programmer to access C arrays identical to Fortran
#define LOGVOL(i,j)LOGVOL[j-1][i-1]
#define LOGDIA(i,j)LOGDIA[j-1][i-1]

int main(int argc, char *argv[])
{
   VolMT *app = new VolMT();

   

   app->createTrees();

   app->on_getVolSerial();
   app->on_getVolParallel();
}
 
VolMT::VolMT()
{
	fileName = "C:\\development\\EightBird.crz";
} 



void VolMT::on_getVolParallel()
{
	//load the dll
	 this->getDLL();
   
   //get trees from cruise file then loop using openMP
   //make sure and get start/finish times tho.
   
   //this->getCruiseInfo();
   
   
   //this->loadTrees();

   double elapTime;
   clock_t beginT, endT;

   
   //beginT = float(clock())/float(CLOCKS_PER_SEC);

   //time_t start,end;
   //double dif;

   omp_set_num_threads(4);

   //time (&start);
   beginT = clock();

   #pragma omp parallel for
   for (int i = 0; i < trees.size(); i++)
   {
	   this->getVolume(trees[i]);
   }

   //time (&end);
   //dif = difftime (end,start);

   endT = clock();
//   endT = float(clock())/float(CLOCKS_PER_SEC);
   elapTime = ((double)(endT - beginT)/**1000*/)/CLOCKS_PER_SEC;
//   elapTime = beginT - endT;

   //std::cout <<"It took  " << dif <<" to get the volume parallel\n";
   //printf ("It took you %.20lf seconds to get the volume parallel.\n", dif );
   std::cout <<"It took  " << elapTime <<" to get the volume parallel \n";
   
   //release the handle to the library      
   FreeLibrary(hinstLib);            
}


void VolMT::on_getVolSerial()
{
	 //load the dll
	 this->getDLL();
   
   //get trees from cruise file then loop using openMP
   //make sure and get start/finish times tho.
   
  // this->getCruiseInfo();
   
  // this->loadTrees();

   double elapTime;
   clock_t beginT, endT;

   beginT = clock();
   //beginT = float(clock())/float(CLOCKS_PER_SEC);

   //time_t start,end;
   //double dif;

   //omp_set_num_threads(10);

   //time (&start);
   
   //#pragma omp parallel for
       
   for (int i = 0; i < trees.size(); i++)
   {
	   this->getVolume(trees[i]);
   }

   //time (&end);
  // dif = difftime (end,start);

   endT = clock();
//   endT = float(clock())/float(CLOCKS_PER_SEC);
   elapTime = ((double)(endT - beginT)/**1000*/)/CLOCKS_PER_SEC;
//   elapTime = beginT - endT;

  // std::cout <<"It took  " << dif <<" to get the volume \n";
  // printf ("It took you %.20lf seconds to get the volume.\n", dif );
   std::cout <<"It took  " << elapTime <<" to get the volume \n";
   
   //release the handle to the library      
   FreeLibrary(hinstLib);            
}

void VolMT::getDLL()
{
	//get handle to the library
	hinstLib = LoadLibrary(TEXT("VolLib.dll"));
	//check to see if library loaded, if not print error and return
	if (hinstLib == NULL) 
  { 
   	std::cout <<"error: dll nf\n"; 
	  return;
	}
	//get address of VERNUM subroutine	
	VERNUM = (impFunc)GetProcAddress(hinstLib, "_VERNUM2@4");
	//check to see if function found, if not print error and return
	if(VERNUM == NULL)
	{
		std::cout <<"error: VERNUM nf\n";
		FreeLibrary(hinstLib);
		return;
	}
	//get address of GETVOLEQ subroutine
	//ui.lineEditStatus->setText("error: due GETVOLEQ nf");
	GETVOLEQ = (getvFunc)GetProcAddress(hinstLib, "_GETVOLEQ2@28");
	//check to see if function found, if not, print error and return
	if(GETVOLEQ == NULL)
	{
		std::cout <<"error: GETVOLEQ nf\n";
		FreeLibrary(hinstLib);
		return;
	}
	//get address of VOLUMELIBRARY
	VOLLIB09 = (volFunc)GetProcAddress(hinstLib, "_VOLLIB09@196");
	//check to see if function found, if not, print error and return
	if(VOLLIB09 == NULL)
	{
		std::cout <<"error: VOLLIB09 nf\n";
		FreeLibrary(hinstLib);
		return;
	}
	
	std::cout <<"successfully loaded vollib.dll \n";
	
}

void VolMT::createTrees()
{
	forest = "09";
    region = 9;
	for (int i = 0; i<100; i++)
	{  
	  Tree myTree;
  	  trees.push_back(myTree);
	}
}

void VolMT::getCruiseInfo()
{
  //cruise table props
	c4_StringProp SaleNo("SaleNo"), CruiseNo("CruiseNo"), SaleName("SaleName"), Region("Region"), Forest("Forest"),
		            District("District"), Purpose("Purpose"), Remarks("Remarks");
  
	//have to convert cstring to ansi for c4_storage assignment
	CStringA ansiString(fileName);
	
	// Note: be careful with the lifetime of views vs. storage objects.
  // When a storage object goes away, all associated views are cleared.(char *)(LPCTSTR)
  c4_Storage storage (ansiString, false);
 
/*------------------------------------------------------------------------------------------*/
//---get cruise table information---//
  c4_View cruiseTab = storage.GetAs("CruiseTable[SaleNo:S, CruiseNo:S, SaleName:S, Region:S, Forest:S, District:S, MeasSys:S, CalYear:S, Purpose:S, CruiseType:S, Remarks:S]");
  
  saleNo = _ttoi((CString)(const char*)SaleNo(cruiseTab[0]));
  cruiseNo = (CString)(const char*)CruiseNo(cruiseTab[0]);
  saleName = (CString)(const char*)SaleName(cruiseTab[0]);
  region = _ttoi((CString)(const char*)Region(cruiseTab[0]));
  forest = (CString)(const char*)Forest(cruiseTab[0]);
  district = (CString)(const char*)District(cruiseTab[0]);
  purpose = (CString)(const char*)Purpose(cruiseTab[0]);
  remarks = (CString)(const char*)Remarks(cruiseTab[0]);

}

void VolMT::loadTrees()
{
	//have to convert cstring to ansi for c4_storage
	CStringA ansiString(fileName);
	
	// Note: be careful with the lifetime of views vs. storage objects.
  // When a storage object goes away, all associated views are cleared.(char *)(LPCTSTR)
  c4_Storage storage (ansiString, false);
  
	//tree table props
	c4_StringProp cutUnit("CutUnit"), block("Block"), key("Key"), value("Value"), tree("Tree"), spec("Sp"), pprod("ProdP"), sprod("ProdS");
	c4_FloatProp dbhIn("DBH"), totHt("TotHT"), topDIBP("TopDIBP"), topDIBS("TopDIBS"), mrchHtPP("MrchHtP"), mrchHtSP("MrchHtS");
	
	c4_View treeTab = storage.GetAs("TreeTable[Key:S,Stratum:S,CutUnit:S,Plot:S,SG:S,Tree:S,Sp:S,STM:S,SlopePct:F,ProdP:S,ProdS:S,CL:S,CM:S,TreeCnt:F,UOMP:S,UOMS:S,KPI:L,3Pcode:S,CDefP:F,HDefP:F,SDefP:F,CDefS:F,HDefS:F,SDefS:F,RecDefP:F,Initials:S,YC:S,ContrSpec:S,LD:S,TreeGrade:S,HgtFLL:F,PoleLen:I,Clear:S,CR:S,DBH:F,DRCOB:F,TotHt:F,MrchHtT:S,MrchHtLL:I,MrchHtP:F,MrchHtS:F,RefHtPct:I,FC:I,AvgZForm:D,UStemDOB:F,HgtUStem:F,DBHBTR:F,DBHDBT:F,TopDIBP:F,TopDIBS:F,DefectCode:S,Ht2Def:I,DiamDefPt:F,VoidPct:F,Remarks:S,Error:I,TotCubicVol:D,GBDFTVolPP:D,GCUFTVolPP:D,NBDFTVolPP:D,NCUFTVolPP:D,CordVolPP:D,GBDFTVolRemPP:D,GCUFTVolRemPP:D,NLogMS:D,CordVolSP:D,GBDFTVolSP:D,NBDFTVolSP:D,GCUFTVolSP:D,NCUFTVolSP:D,GCUFTVolRemSP:D,NLogTop:D,GBDFTVolRP:D,GCUFTVolRP:D,CordVolRP:D,GBDFTVolInt:D,NBDFTVolInt:D,WgtPP:D,WgtSP:D,ValuePP:D,ValueSP:D,ValueRP:D,ExpanFac:D,TreeFac:D,PntFac:D,StrAcres:F,TreeCnt3P:D,XCoord:S,YCoord:S,ZCoord:S,MetaData:S,LogRecs:I,CompoundSelection:I]");
  
  //select using "find" where trees are in this cut unit (unitNum)
	for (int idx = 0; idx < treeTab.GetSize(); idx++)
    {
      Tree myTree((CString)(const char*)cutUnit(treeTab[idx]),(CString)(const char*)spec(treeTab[idx]), (CString)(const char*)tree(treeTab[idx]), (float)dbhIn(treeTab[idx]), (float)totHt(treeTab[idx]), (float)topDIBP(treeTab[idx]),(float)topDIBS(treeTab[idx]), (float)mrchHtPP(treeTab[idx]), (float)mrchHtSP(treeTab[idx]),(CString)(const char*)pprod(treeTab[idx]),(CString)(const char*)sprod(treeTab[idx]));
  		//myTree.setVolEq(volEqs);
  		//myTree.setWeightFactors(weightEqs);
  		//add tree to the vector
  		trees.push_back(myTree);
  	}
    
  
}




void VolMT::getVolume(Tree m_tree)
{
   
// standard variables
   int REGN,HTLOG,HTREF,FCLASS,HTTFLL,ERRFLAG,TLOGS,BA,SI,SPCODE, INDEB;
   int CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG,SPFLG,VERSION;
   float DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD,STUMP;
   float UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2;
   float DBTBH,BTR,MTOPP,MTOPS,NOLOGP,NOLOGS;
   
// array defintions
   int I3 = 3;
   int I7 = 7;
   int I15 = 15;
   int I20 = 20;
   int I21 = 21;

   
   float VOL[i15];
   float LOGLEN[i20];
   float BOLHT[i21];
// 2-dimentional array definitions
   float LOGVOL[i7][i20];
   float LOGDIA[i21][i3];
// character length parameters
   const int len2=2;
   const int len3=3;
   const int len5=5;
   const int len11=11;

   //if(ui.lineEditFor->text().isEmpty()) ui.lineEditFor->setText("1");   // check for null forest number
   //if(ui.lineEditm_sDist == "") m_sDist = "1";   // check for null forest number
   
   std::string m_sDist = "1";
   std::string sCtype = "F";
      
   std::string m_sLive = "";
   std::string m_sConSpec;    
   m_sConSpec = "     ";
   
/***********************************************************************************************/
   //convert strings containing input data to f90Char structs(see f90Char.h) for passing to/from
   //fortran.  Remove null character and add a "max length" identifier
	f90Char FORST = f90Char(len3,getStdString(forest));
	f90Char DIST = f90Char(len3,"01");
	f90Char PROD = f90Char(len3,getStdString(m_tree.getProdP()));
	f90Char HTTYPE = f90Char(len2,"F");
	f90Char VOLEQ = f90Char(len11,"         ");
	f90Char LIVE = f90Char(len2,"L");
	f90Char CONSPEC = f90Char(len5, getStdString(m_tree.getSpecies()));
	f90Char CTYPE = f90Char(len2, "F");		
/**********************************************************************************************/
   
   REGN =   region;//m_iRegn;
   MTOPP =  m_tree.getTopDIBp();//m_fMtopp;
   MTOPS =  m_tree.getTopDIBs();//m_fMtops;
   STUMP =  1.0;//m_fStump;
   DBHOB =  m_tree.getDBH();//m_fDbhob;
   DRCOB =  14;//m_fDrcob;
   //merch rules
   HTTOT =  m_tree.getTotalHeight();//m_fHttot;    
   HT1PRD = m_tree.getMrchHtP();
   HT2PRD = m_tree.getMrchHtS();//m_fHt2prd
   HTLOG =  0;//m_iHtlog;
   UPSHT1 = 0;
   HTTFLL = 0;
   
   if(REGN == 8)
      UPSHT1 = m_tree.getMrchHtS();
   else if (REGN == 3)
   {
      HT1PRD = 0;
      HTTFLL = m_tree.getMrchHtS();
   }
  
   UPSHT2 = 0;//m_fUpsht2;
   UPSD1 =  0;//m_fUpsd1;
   UPSD2 =  0;//m_fUpsd2;
   HTREF =  0;//m_iHtref;
   AVGZ1 =  0;//m_fAvgz1;
   AVGZ2 =  0;//m_fAvgz2;
   FCLASS = 0;//m_iFclass;
   DBTBH =  0;//m_fDbtbh;
   BTR =    0;//m_fBtr;
   CUTFLG = 1;//m_iCutflg;
   BFPFLG = 1;//m_iBfpflg;
   CUPFLG = 1;//m_iCupflg;
   CDPFLG = 1;//m_iCdpflg;
//   CUSFLG = m_iCusflg;
//   CDSFLG = m_iCdsflg;
   SPFLG = 1;//m_iSpflg;
   
   TLOGS = 0;//m_iTlogs;
   NOLOGP = 0.0;
   NOLOGS = 0.0;
   BA = 0;
   SI = 0;
   SPCODE = _ttoi(m_tree.getSpecies());
// Debug flag
//	 INDEB = (ui.debugOutCB->isChecked()) ? 1 : 0;
//   
//   for(int nn=0;nn<20;nn++)
//      LOGLEN[nn] = m_fLoglen[nn];

   ERRFLAG = 0;
   // call to version number
   VERNUM(&VERSION);
   
   // check volume equation number
   if( strcmp ( VOLEQ.str, "         " ) == 0 ) // Equal strings

   //if(VOLEQ.str == 0)// == '         ')
   {
     GETVOLEQ(&REGN, &FORST, &DIST, &SPCODE, &PROD, &VOLEQ, &ERRFLAG);     
   }

// call to fortan dll
   VOLLIB09(&REGN,&FORST,&VOLEQ,&MTOPP,&MTOPS,&STUMP,&DBHOB,&DRCOB,
	      &HTTYPE, &HTTOT,&HTLOG,&HT1PRD,&HT2PRD,&UPSHT1,&UPSHT2,&UPSD1,&UPSD2,
		  &HTREF,&AVGZ1,&AVGZ2,&FCLASS,&DBTBH,&BTR,I3,I7,I15,I20,I21,VOL,LOGVOL,
		  LOGDIA,LOGLEN,BOLHT,&TLOGS,&NOLOGP,&NOLOGS,&CUTFLG,&BFPFLG,&CUPFLG,
		  &CDPFLG,&SPFLG,&CONSPEC,&PROD,&HTTFLL,&LIVE,&BA,&SI,&CTYPE,&ERRFLAG, &INDEB);

   //std::cout <<"total cubic = " << VOL[0] <<"\n";
   
   //throwErrorException(VOLEQ.str);
// save output
/*   ui.lineEditStatus->setText("returned from volume lib");
   ui.lineEditVolEq->setText(VOLEQ.str);
   ui.lineEditTopDIBPP->setText(QString::number(MTOPP));
   ui.lineEditMrHtPP->setText(QString::number(HT1PRD));
   ui.lineEditErrFlg->setText(QString::number(ERRFLAG));
   //m_iTlogs = TLOGS;
   //m_fNologp = NOLOGP;
   //m_fNologs = NOLOGS;
   //m_iFclass = FCLASS;
   
   ui.lineEditTotCu->setText(QString::number(VOL[0]));
   ui.lineEditBdFt->setText(QString::number(VOL[1]));
   ui.lineEditCuFt->setText(QString::number(VOL[3]));
   //ui.lineEditCuFt_2->setText(QString::number(VOL[4]));
   ui.lineEditTpwdCu->setText(QString::number(VOL[6]));	
   ui.lineEditTpwdBF->setText(QString::number(VOL[11]));
   for(int i=0;i<15;i++)
      m_fVol[i] = VOL[i];
   float tempvol = 0.0;
   for(int j=0;j<20;j++)
   {
      m_fLoglen[j] = LOGLEN[j];
      m_fLogvol_Bdft[j] = LOGVOL(0,j);
      m_fLogvol_Cuft[j] = LOGVOL(3,j);
      tempvol += m_fLogvol_Cuft[j];
   }
   ui.lineEditCuFt_2->setText(QString::number(VOL[4]));
   //for testing and debugging only
   float temp = LOGVOL(3,1);
   float temp2 =LOGVOL(2,1);
   //******************************
   for(int k=0;k<20;k++)
   {
      m_fLogsc[k] = LOGDIA(k,0);
      m_fLogdib[k] = LOGDIA(k,1);
      m_fLogdob[k] = LOGDIA(k,2);
      m_fBolht[k] = BOLHT[k];
   }
   this->update();*/
}
