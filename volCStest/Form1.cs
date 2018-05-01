/******************************************************************
 * File: Form1.cs
 * Author: T. Heithecker
 * Date: 10.08.2010
 * Purpose: This is the code behind the main form.
 * Requirements: nothing really
 * 
 * Revision Date: 06.23.2011
 * Revision: cleaned up code, add comments, added GetVolEqn
 * button and corresponding method.
 * 
 * Revision Date: 12.21.2010
 * Revision: fixed a bug where the total height wasn't being
 * updated in the pmt form after the 1st run.
 * 
 * Revision Date: 10.29.2010
 * Revision: Made the merch rules box on/off depending on checkbox
 * and autofilled it after a call to the volume library.  Finished
 * translating merrules.f to c#
 * 
 * To do: Add additionaly funcionality for pmt display of models
 * not using profile.f.  eg Clarks.  NOTE: did this fo R9Clark
 * ***************************************************************/

using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Collections;
using System.Runtime.InteropServices;
using System.Drawing.Printing;
using System.IO;


namespace volCStest
{
    

    public partial class Form1 : Form
    {
        #region class variable decalarations
        //declarations for external methods from vollib.dll
        [DllImport("vollib.dll", CallingConvention = CallingConvention.Cdecl)]//EntryPoint = "VERNUM2",
        static extern void VERNUM2(ref int a);

        [DllImport("vollib.dll")]//, CallingConvention = CallingConvention.StdCall)]
        static extern void GETVOLEQ3(ref int a, StringBuilder forstc, StringBuilder distc, ref int spec, StringBuilder prod, StringBuilder voleq, ref int err, int l1, int l2, int l3, int l4);
        [DllImport("vollib.dll")]//, CallingConvention = CallingConvention.StdCall)]
        static extern void GETVARIANTCS(ref int a, StringBuilder forstc, StringBuilder distc, StringBuilder var, int l1, int l2, int l3);

        [DllImport("vollib.dll")]//, CallingConvention = CallingConvention.StdCall)]
        static extern void R9CLARKDIB(StringBuilder voleq, ref float stump, ref float mtopp, ref float mtops, ref float dbhob, ref float ht1prd, ref float ht2prd, ref float httot, ref float dibht, ref float dib, StringBuilder prod, ref int err, ref float upsht1);

        [DllImport("vollib.dll")]//, CallingConvention = CallingConvention.StdCall)]
        static extern void PMTPROFILE(StringBuilder forst, StringBuilder voleq, ref float mtopp, ref float mtops,
         ref float stump, ref float dbhob, ref float dib, StringBuilder httype, ref float httot, ref int htlog, ref float ht1prd, ref float ht2prd,
         ref float upsht1, ref float upsht2, ref float upsd1, ref float upsd2, ref float avgz1, ref float avgz2,  ref int htref, ref float dbtbh,
         ref float btr, float[] vol,
         ref int cutflg, ref int bfpflg,
         ref int cupflg,  ref int cdpflg, ref int spflg, ref float drcob,StringBuilder ctype,  ref int fclass, StringBuilder prod, 
         ref int errflg, int ll1, int ll2, int ll3, int ll4, int ll5 );
       
        [DllImport("vollib.dll")]//, CallingConvention = CallingConvention.StdCall)]
        static extern void VOLLIBCS(ref int regn, StringBuilder forst, StringBuilder voleq, ref float mtopp, ref float mtops,
         ref float stump, ref float dbhob, ref float drcob, StringBuilder httype, ref float httot, ref int htlog, ref float ht1prd, ref float ht2prd,
         ref float upsht1, ref float upsht2, ref float upsd1, ref float upsd2, ref int htref, ref float avgz1, ref float avgz2, ref int fclass, ref float dbtbh,
         ref float btr, ref int i3, ref int i7, ref int i15, ref int i20, ref int i21, float[] vol, float[,] logvol,
         float[,] logdia, float[] loglen, float[] bohlt, ref int tlogs, ref float nologp, ref float nologs, ref int cutflg, ref int bfpflg,
         ref int cupflg, ref int cdpflg, ref int spflg, StringBuilder conspec, StringBuilder prod, ref int httfll, StringBuilder live,
         ref int ba, ref int si, StringBuilder ctype, ref int errflg, ref int indeb, ref int pmtflg, ref MRules mRules, ref int dist, int ll1, int ll2, int ll3, int ll4, int ll5, int ll6, int ll7, int charLen);

        [DllImport("vollib.dll")]//, CallingConvention = CallingConvention.StdCall)]
        static extern void CALCDIACS(ref int regn, StringBuilder forst, StringBuilder voleq, ref float stump, ref float dbhob,
         ref float drcob, ref float httot, ref float upsht1, ref float upsht2, ref float upsd1, ref float upsd2, ref int htref, ref float avgz1,
         ref float avgz2, ref int fclass, ref float dbtbh, ref float btr, ref float htup, ref float dib, ref float dob, ref int errflg,
            int ll1, int ll2);

        [DllImport("vollib.dll")]//, CallingConvention = CallingConvention.StdCall)]
        static extern void HT2TOPDCS(ref int regn, StringBuilder forst, StringBuilder voleq, ref float dbhob, ref float httot, ref float ht1prd, ref float ht2prd,
            ref float upsht1, ref float upsht2, ref float upsd1, ref float upsd2, ref float avgz1, ref float avgz2, ref int htref, ref float dbtbh,
         ref float btr, ref int fclass, ref float stmdib, ref float stmht, ref int errflg, int ll1, int ll2);

        [DllImport("vollib.dll")]//, CallingConvention = CallingConvention.StdCall)]
        static extern void MRULESCS(ref int regn, StringBuilder voleq, StringBuilder prod, ref float trim, ref float minlen, ref float maxlen, ref int opt, ref float merchl, int l1, int l2);

        [DllImport("vollib.dll")]//, CallingConvention = CallingConvention.StdCall)]
        static extern void CRZBIOMASSCS(ref int regn, StringBuilder forst, ref int spcd, ref float dbhob, ref float drcob, ref float httot, ref int fclass, float[] vol, float[] wf, float[] bms, ref int errflg, int i1);
        [DllImport("vollib.dll")]
        static extern void CRZSPDFTCS(ref int regn, StringBuilder forst, ref int spcd, float[] wf, StringBuilder agteq, StringBuilder lbreq, StringBuilder dbreq, StringBuilder foleq, StringBuilder tipeq,
            StringBuilder wf1ref, StringBuilder wf2ref, StringBuilder mcref, StringBuilder agtref, StringBuilder lbrref, StringBuilder dbrref, StringBuilder folref, StringBuilder tipref, 
            int i1, int i2, int i3,int i4, int i5, int i6,int i7, int i8, int i9,int i10, int i11, int i12,int i13,int i14);
        
        [DllImport("vollib.dll")]
        static extern void BROWNCROWNFRACTION(ref int SPCD, ref float DBH, ref float THT, ref float CR, float[] CFWT);
        [DllImport("vollib.dll")]
        static extern void BROWNTOPWOOD(ref int SPN, ref float GCUFTS, ref float WT);
        [DllImport("vollib.dll")]
        static extern void BROWNCULLLOG(ref int SPN, ref float GCUFTS, ref float WT);
        [DllImport("vollib.dll")]
        static extern void BROWNCULLCHUNK(ref int SPN, ref float GCUFT, ref float NCUFT, ref float FLIW, ref float WT);

        // standard variables
        int REGN,HTLOG,HTREF,FCLASS,HTTFLL,ERRFLAG,TLOGS,BA,SI,SPCODE,INDEB, PMTFLG, IDIST;
        int CUTFLG,BFPFLG,CUPFLG,CDPFLG,CUSFLG,CDSFLG,SPFLG,VERSION;
        float DBHOB,DRCOB,HTTOT,HT1PRD,HT2PRD,STUMP;
        float UPSHT1,UPSHT2,UPSD1,UPSD2,AVGZ1,AVGZ2;
        float DBTBH,BTR,MTOPP,MTOPS,NOLOGP,NOLOGS, DIB,DIBHT, DOB;
        float DBH,CR;
        float STEMDIB, STEMHT;
        float pmtHt;
        // test MRULESCS variable
        int OPT;
        float TRIM, MINLEN, MAXLEN, MERCHL;
        // array defintions
        int I3 = 3;
        int I7 = 7;
        int I15 = 15;
        int I20 = 20;
        int I21 = 21;
        //test EZVOLLIB
        float TCU, MCU, BDF, SCNCU;
           
        float[] VOL;
        float[] LOGLEN;
        float[] BOLHT;
        // 2-dimentional array definitions
        float[,] LOGVOL;
        float[,] LOGDIA ;

        //biomas variable
        int SPCD;
        float[] WF;
        float[] BMS;
        float[] CFWT;
        float VolS, VolN, TopWt, CullLogWt, CullChunkWt, fliw;
        StringBuilder AGTEQ = new StringBuilder(256);
        StringBuilder LBREQ = new StringBuilder(256);
        StringBuilder DBREQ = new StringBuilder(256);
        StringBuilder FOLEQ = new StringBuilder(256);
        StringBuilder TIPEQ = new StringBuilder(256);
        StringBuilder WF1REF = new StringBuilder(256);
        StringBuilder WF2REF = new StringBuilder(256);
        StringBuilder MCREF = new StringBuilder(256);
        StringBuilder AGTREF = new StringBuilder(256);
        StringBuilder LBRREF = new StringBuilder(256);
        StringBuilder DBRREF = new StringBuilder(256);
        StringBuilder FOLREF = new StringBuilder(256);
        StringBuilder TIPREF = new StringBuilder(256);
        //string[] BMSEQ = new string[8];
        //string[] AUTHOR = new string[8];

        const int strlen2 = 120;
        const int strlen3 = 400;

        // character length parameters
        const int len2 = 2;
        const int len3 = 3;
        const int len5 = 5;
        const int len11 = 11;
        const int strlen = 256;
        const int charLen = 1;

        string m_sDist;
 	      string sCtype;                      
        string m_sLive;
        string m_sConSpec;    
                     
        //strings for passing to/from fortran.  Pass fixed length strings
	    StringBuilder FORST = new StringBuilder(256);
        StringBuilder DIST = new StringBuilder(256);
        StringBuilder PROD = new StringBuilder(256);
        StringBuilder HTTYPE = new StringBuilder(256);
        StringBuilder VOLEQ = new StringBuilder(256);
        StringBuilder LIVE = new StringBuilder(256);
        StringBuilder CONSPEC = new StringBuilder(256);
        StringBuilder CTYPE = new StringBuilder(256);
        StringBuilder VAR = new StringBuilder(256);

        String MDL;
           
        //inside bark diameters for pmt
        float[,] dibs = new float[2, 300];
        float dibScalingFactor;

        //pmt form open flag
        bool pmtFormOpen;
        int pmtOpenCount;
        
        PMTForm pmtForm;

        Bitmap image; 

        MRules mRules;
        //List to store info for report
        List<string> tree1 = new List<string>();
        List<string> tree2 = new List<string>();
        string exePath = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetEntryAssembly().Location);
        string FileName = "\\VolTestReport.TXT";
        int lognum1, lognum2;
        #endregion

        public Form1()
        {
            InitializeComponent();
            
            pmtFormOpen = false;
            pmtOpenCount = 0;
            PMTFLG = 0;            
            mRules = new MRules(0,0,0,0,0,0,0,0,0);
            PrintGraphBtn.Enabled = false;
            regionTB.Text = "09";
            int Num;
            bool isInt = int.TryParse(regionTB.Text, out Num);
            if (isInt) loadVolEqList(regionTB.Text);
            
        }

        private void getVolEqB_Click(object sender, EventArgs e)
        {
            float TOPD;
            //float tcu;
            //tcu = 0;
            //VOLLIBVB8(ref tcu);

            //make sure all required fields are populated
            if (!checkFields())
                return;
            //MessageBox.Show("HtType=" + htTypeTB.Text);
            //test to see if the form has 1) been created and 2) been disposed (closed)
            if(pmtForm != null)
                //form has been opened, see if it has been disposed
                if (pmtForm.IsDisposed)
                {
                    pmtFormOpen = false;
                    pmtOpenCount = 0;
                }
                else
                    pmtFormOpen = true;

            m_sConSpec = "     ";
      
           //initialize variables for volume library dll call
           getVolPrep();

           // call to version number subroutine
           VERNUM2(ref VERSION);

           verNumTB.Text = VERSION.ToString();
           verNumLB.Text = VERSION.ToString();

           #region get and display volumes
           //re-intialized variables after previous call to vollib
           getVolPrep();
           mRulesPrep();
            //check UPSHT1 required for some equations
           if (UPSHT1 < 0.1 && FCLASS == 0)
           {
               if (VOLEQ.ToString().Substring(3, 3) == "FW3" || VOLEQ.ToString().Substring(3, 3) == "F33")
               {
                   MessageBox.Show("Form Class or UPSHT1 and UPSD1 is required for the equation: " + volEqTB.Text);
                   //upsht1TB.Focus();
                   return;
               }
           }
            if (ctypeTB.Text.Substring(0,1) == "C")
            {
                if (UPSHT1 < 0.1)
                {
                    if (VOLEQ.ToString().Substring(0, 1) == "8" && VOLEQ.ToString().Substring(3, 3) == "CLK")
                    {
                        if (VOLEQ.ToString().Substring(2, 1) != "0" && VOLEQ.ToString().Substring(2, 1) != "8")
                        {
                            MessageBox.Show("Upper stem height is required for the equation: " + volEqTB.Text);
                            upsht1TB.Focus();
                            return;
                        }
                    }
                }
                if (HT1PRD < 0.1)
                {
                    if (VOLEQ.ToString().Substring(0, 1) == "8" && VOLEQ.ToString().Substring(3, 3) == "DVE")
                    {
                        MessageBox.Show("Merch height for primary prod is required for the equation: " + volEqTB.Text);
                        mrchHtPPTB.Focus();
                        return;
                    }
                }
            }
            //STUMP = 1.0F;
           //MessageBox.Show("b4 HTTYPE=" + HTTYPE);
           //if pmtflg == 2 call profile2 with user defined merch rules 
           //else just call the regular profile model...vollibcs.f handles this.
           IDIST = int.Parse(DIST.ToString());
           VOLLIBCS(ref REGN, FORST, VOLEQ, ref MTOPP, ref MTOPS, ref STUMP, ref DBHOB, ref DRCOB,
             HTTYPE, ref HTTOT, ref HTLOG, ref HT1PRD, ref HT2PRD, ref UPSHT1, ref UPSHT2, ref UPSD1, ref UPSD2,
             ref HTREF, ref AVGZ1, ref AVGZ2, ref FCLASS, ref DBTBH, ref BTR, ref I3, ref I7, ref I15, ref I20, ref I21, VOL, LOGVOL,
             LOGDIA, LOGLEN, BOLHT, ref TLOGS, ref NOLOGP, ref NOLOGS, ref CUTFLG, ref BFPFLG, ref CUPFLG,
             ref CDPFLG, ref SPFLG, CONSPEC, PROD, ref HTTFLL, LIVE, ref BA, ref SI, CTYPE, ref ERRFLAG, ref INDEB, ref PMTFLG,
             ref mRules, ref IDIST, strlen, strlen, strlen, strlen, strlen, strlen, strlen, charLen);

            //test call MRULESCS
           MRULESCS(ref REGN, VOLEQ, PROD, ref TRIM, ref MINLEN, ref MAXLEN, ref OPT, ref MERCHL, strlen, strlen);

            //test call biomass calc for Cruise Processing
            //the following call does work!!!!
           CRZSPDFTCS(ref REGN, FORST, ref SPCD, WF, AGTEQ, LBREQ, DBREQ, FOLEQ, TIPEQ,
               WF1REF, WF2REF, MCREF, AGTREF, LBRREF, DBRREF, FOLREF, TIPREF,
               strlen, strlen, strlen, strlen, strlen, strlen, strlen, strlen, strlen, strlen, strlen, strlen, strlen, strlen);
           //the calculation works!!!
           DRCOB = 0.0F;
           CRZBIOMASSCS(ref REGN, FORST, ref SPCD, ref DBHOB, ref DRCOB, ref HTTOT, ref FCLASS, VOL, WF, BMS, ref ERRFLAG, strlen);
           //test Brown's function works!!!
           DBH = DBHOB;
           CR = 0.5F;
           BROWNCROWNFRACTION(ref SPCD, ref DBH, ref HTTOT, ref CR, CFWT);
           VolS = VOL[6];
           VolN = VolS - 0.2F;
           fliw = 0.5F;
           //BROWNTOPWOOD(ref SPCD, ref VolS, ref TopWt);
           //BROWNCULLLOG(ref SPCD, ref VolS, ref CullLogWt);
           VolS = 302.7F;
           VolN = 293.9F;
           //BROWNCULLCHUNK(ref SPCD, ref VolS, ref VolN, ref fliw, ref CullChunkWt);

            //test on EZVOLLIB
           //EZVOLLIB(VOLEQ, ref DBHOB, ref HTTOT, ref VOL, strlen);
            //volumes have been calculated, display them in the form and display merch rules
           panel2.Enabled = true;
           //MessageBox.Show("HTTYPE=" + HTTYPE);
           //we got the defaults now update merch rules
           updateMerRulesForm();

           //disable the user defined mer rules checkbox
           panel2.Enabled = false;
           
           //is user wants to change mer rules then allow them too
           if (mRulesCB.Checked)
               panel2.Enabled = true;
           displayVolumes();
           if (ERRFLAG != 0) return;
           if (MTOPP > 0) TOPD = MTOPP;
           else TOPD = mRules.mtopp;
           #endregion

           //check to see if user wants to view profile model
           if (pmtCB.Checked == true)
           {
               //test for valid equation numbers for the profile model tutorial
               if (isProfileModel())
               {
                   //initialize variables for call to profile model
                   if (MDL.Contains("BEH"))
                   {
                       //Added this to let it draw the stem when there is no total height
                       //01/03/2014 (yw)
                       //if (HTTOT == 0)
                       //{
                       //    if (HT2PRD > 0)  HTTOT = HT2PRD;
                       //    else if (HT1PRD > 0) HTTOT = HT1PRD;
                       //}    

                       if (pmtFormOpen && pmtOpenCount < 2)
                       {
                           dibScalingFactor = ((1.0F / DBHOB) * 10 + 1.4F);
                           pmtOpenCount = 2;
                           LOGDIA[0, 0] = DBHOB - DBTBH;
                           LOGDIA[1, 0] = DBHOB - DBTBH;
                           if (!mRulesCB.Checked)
                               pmtForm.drawBehresTree(LOGDIA, LOGLEN, LOGVOL, 1.0f, BOLHT, 600);
                           else
                              pmtForm.drawBehresTree(LOGDIA, LOGLEN, LOGVOL, STUMP, BOLHT, 600);
                           //pmtForm.drawTreeInfo(DBHOB, HTTOT, VOLEQ.ToString(), mRules.mtopp, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 600);
                           //pmtForm.drawTreeInfo(DBHOB, HTTOT, VOLEQ.ToString(), TOPD, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 600);
                           pmtForm.drawTreeInfo(DBHOB, pmtHt, VOLEQ.ToString(), TOPD, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 600);

                       }
                       //if 2 trees have been drawn then inform the user
                       else if (pmtOpenCount == 2)
                       {
                           MessageBox.Show("You can display a maximum of 2 trees for comparison.  Please close the graphic window to reset graphics.");
                       }
                       else
                       {
                           dibScalingFactor = ((1.0F / DBHOB) * 10 + 1.4F);
                           //pmtForm = new PMTForm(HTTOT, dibScalingFactor, true);
                           pmtForm = new PMTForm(pmtHt, dibScalingFactor, true);
                           this.image = pmtForm.ShowForm();
                           //pmtForm.Show();
                           pmtFormOpen = true;
                           pmtOpenCount++;
                           LOGDIA[0, 0] = DBHOB - DBTBH;
                           LOGDIA[1, 0] = DBHOB - DBTBH;
                           if (!mRulesCB.Checked)
                               pmtForm.drawBehresTree(LOGDIA, LOGLEN, LOGVOL, 1.0f, BOLHT, 0);
                           else
                               pmtForm.drawBehresTree(LOGDIA, LOGLEN, LOGVOL, STUMP, BOLHT, 0);
                           //pmtForm.drawTreeInfo(DBHOB, HTTOT, VOLEQ.ToString(), mRules.mtopp, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 0);
                           //pmtForm.drawTreeInfo(DBHOB, HTTOT, VOLEQ.ToString(), TOPD, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 0);
                           pmtForm.drawTreeInfo(DBHOB, pmtHt, VOLEQ.ToString(), TOPD, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 0);

                       }
                   }
                   else
                   {
                       //get the dibs and prepare data for displaying the pm graphic
                       pmtPrep();

                       //if the form is already open and only 1 tree is drawn, draw the 2nd
                       if (pmtFormOpen && pmtOpenCount < 2)
                       {
                               //pmtForm.drawTree(dibs, HTTOT, dibScalingFactor);
                               pmtForm.drawTree(dibs, pmtHt, dibScalingFactor);
                               pmtOpenCount = 2;
                               //pmtForm.drawTreeInfo(DBHOB, HTTOT, VOLEQ.ToString(), mRules.mtopp, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 600);
                               //pmtForm.drawTreeInfo(DBHOB, HTTOT, VOLEQ.ToString(), TOPD, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 600);
                               pmtForm.drawTreeInfo(DBHOB, pmtHt, VOLEQ.ToString(), TOPD, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 600);
                           
                       }
                       //if 2 trees have been drawn then inform the user
                       else if (pmtOpenCount == 2)
                       {
                           MessageBox.Show("You can display a maximum of 2 trees for comparison.  Please close the graphic window to reset graphics.");
                       }
                       //form is not open so open it and draw tree 1
                       else
                       {
                           //pmtForm = new PMTForm(dibs, HTTOT, dibScalingFactor);
                           pmtForm = new PMTForm(dibs, pmtHt, dibScalingFactor);
                           this.image = pmtForm.ShowForm();
                           //pmtForm.Show();
                           pmtFormOpen = true;
                           pmtOpenCount++;
                           //pmtForm.drawTreeInfo(DBHOB, HTTOT, VOLEQ.ToString(), mRules.mtopp, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 0);
                           //pmtForm.drawTreeInfo(DBHOB, HTTOT, VOLEQ.ToString(), TOPD, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 0);
                           pmtForm.drawTreeInfo(DBHOB, pmtHt, VOLEQ.ToString(), TOPD, mRules.stump, mRules.trim, mRules.maxlen, mRules.minlen, mRules.opt, 0);

                       }
                   }
               }
               else
               {
                   //invalid equation number for profile model display
                   MessageBox.Show("Cannot display Profile Model graphic for this equation.");
                   //uncheck the profile model tutorial check box
                   pmtCB.Checked = false;
               }

           }

           
           if (pmtCB.Checked)
           {
               if (REGN == 6)
               {
                   LOGDIA[0, 0] = DBHOB - DBTBH;
                   LOGDIA[1, 0] = DBHOB - DBTBH;
               }
               if (VOLEQ.ToString().Substring(3, 3) == "BEH" && !mRulesCB.Checked)
                   pmtForm.drawLogs(LOGDIA, LOGLEN, LOGVOL, 1.0f, BOLHT, 0.3f);
               else
               {
                   if (STUMP == 0) STUMP = mRules.stump;
                   pmtForm.drawLogs(LOGDIA, LOGLEN, LOGVOL, STUMP, BOLHT, mRules.trim);
               }
           }
           //if the PMT form is open, enable the print graph button
           if (pmtFormOpen) PrintGraphBtn.Enabled = true;
           else PrintGraphBtn.Enabled = false;

            //Write tree info and result to tree List
           if (pmtOpenCount < 2) AddTree1ToList();
           else if (pmtOpenCount == 2) AddTree2ToList();

        }

        //initialize variables for calling the volume library dll
        private void getVolPrep()        
        {            
            //first clear all variables in case we are doing multiple
            //runs without closing the window.
            clearStrings();            
            m_sConSpec = "     ";
            if (string.IsNullOrEmpty(districtTB.Text)) districtTB.Text = "01";
            FORST.Append(forestTB.Text);
            DIST.Append(districtTB.Text);  // ("01 ");
            PROD.Append(prodTB.Text);
            //MessageBox.Show("htType.text=" + htTypeTB.Text);
            HTTYPE.Append(htTypeTB.Text.Substring(0,1));
            VOLEQ.Append(volEqTB.Text);
            CONSPEC.Append(m_sConSpec);

            LIVE.Append("L");
            if (string.IsNullOrEmpty(ctypeTB.Text)) ctypeTB.Text = "F";
            CTYPE.Append(ctypeTB.Text.Substring(0,1));

            VOL = new float[I15];
            LOGLEN = new float[I20];
            BOLHT = new float[I21];
            // 2-dimentional array definitions
            LOGVOL = new float[I20,I7];
            LOGDIA = new float[I3,I21];           

            REGN = int.Parse(regionTB.Text);
            if (string.IsNullOrEmpty(topDibPPTB.Text)) topDibPPTB.Text = "0";
            if (string.IsNullOrEmpty(topDibSPTB.Text)) topDibSPTB.Text = "0";
            if (string.IsNullOrEmpty(mrchHtPPTB.Text)) mrchHtPPTB.Text = "0";
            if (string.IsNullOrEmpty(mrchHtSPTB.Text)) mrchHtSPTB.Text = "0";
            if (string.IsNullOrEmpty(upsht1TB.Text)) upsht1TB.Text = "0";
            if (string.IsNullOrEmpty(formClsTB.Text)) formClsTB.Text = "0";
            if (string.IsNullOrEmpty(dbhTB.Text))
            {
                MessageBox.Show("Please enter a number for DBH.");
                DBHOB = 0.0F;
                dbhTB.Text = "0";
                return;
            }
            if (string.IsNullOrEmpty(totalHtTB.Text)) totalHtTB.Text = "0";
            MTOPP = float.Parse(topDibPPTB.Text);
            MTOPS = float.Parse(topDibSPTB.Text);
            STUMP = 0.0F;
            DBHOB = float.Parse(dbhTB.Text);
            DRCOB = 0.0F;
            //merch rules
            HTTOT = float.Parse(totalHtTB.Text);
            HT1PRD = float.Parse(mrchHtPPTB.Text);
            HT2PRD = float.Parse(mrchHtSPTB.Text);
            HTLOG = 0;
            UPSHT1 = float.Parse(upsht1TB.Text);
            UPSHT2 = 0;
            HTTFLL = 0;
            if (HTTOT > 0) pmtHt = HTTOT;
            else if (UPSHT1 > 0) pmtHt = UPSHT1;
            else if (HT2PRD > 0) pmtHt = HT2PRD;
            else if (HT1PRD > 0) pmtHt = HT1PRD;

            //if (REGN == 8 && VOLEQ.ToString().Substring(2,1) != "1")
            //{
            //    if (UPSHT1 <= 0) UPSHT1 = HT2PRD;
            //    if (UPSHT1 > 0 && HT1PRD == 0) HT1PRD = UPSHT1;
            //}
             //changed the above line to test region 8 broken height
            // comment out to test region 3 profile 300FW2W122  yw 07/10/12
            //else if (REGN == 3) 
            //{
            //    HT1PRD = 0;
            // }

            DIBHT = 0;
            //this code really won't do anything now, but could be incorporated
            //into a function to return DIB at a give height
            if (upstht2TB.TextLength > 0)
                DIBHT = float.Parse(upstht2TB.Text); //spcode
            //else
            DRCOB = 0;//upper stem height 2
            //*****************************************************************

            UPSD1 = float.Parse(upsd1TB.Text);//upper stem diameter 1
            UPSD2 = 0;//upper stem diameter 2
            HTREF = int.Parse(htrefTB.Text);//reference height
            AVGZ1 = float.Parse(avgz1TB.Text);
            AVGZ2 = 0;
            if (int.Parse(formClsTB.Text) > 0) FCLASS = int.Parse(formClsTB.Text);
            else FCLASS = 0;
            //if (FCLASS == 0) FCLASS = 80;

            DBTBH = float.Parse(dbtbhTB.Text);
            //DBTBH = float.Parse("0.88");  //0;//double bark thickness at breast height
            BTR = float.Parse(btrTB.Text);//bark thickness ratio
            CUTFLG = 1;//cut flag
            //if (PROD.ToString(0, 2) == "01" || PROD.ToString(0, 2) == "14") BFPFLG = 1;//board foot flag
            //else BFPFLG = 0;
            BFPFLG = 1;
            CUPFLG = 1;//cubic foot flag;
            CDPFLG = 1;//cord volume flag;
            SPFLG = 1;//;
            PMTFLG = 0;//flag for which profile model to run
            TLOGS = 0;//total # logs;
            NOLOGP = 0.0F;//# logs primary product
            NOLOGS = 0.0F;//# logs secondary product
            BA = 0; //basal area
            SI = 0; //site index

            if (speciesTB.TextLength > 0)
                SPCODE = int.Parse(speciesTB.Text); //spcode
            else
                SPCODE = 0;
            if (SPCODE > 999) SPCODE = 999;
            // output debug flag
            INDEB = 0;// debugCB.Checked ? 1 : 0;
            if (debugCB.Checked == true)
                INDEB = 1;

            ERRFLAG = 0;//error flag
            VERSION = 0;

            //if volume equation not provided, then get it
            if (VOLEQ.Length < 10)
                GETVOLEQ3(ref REGN, FORST, DIST, ref SPCODE, PROD, VOLEQ, ref ERRFLAG, strlen, strlen, strlen, strlen);

            //GETVARIANTCS(ref REGN, FORST, DIST, VAR, strlen, strlen, strlen);
            //MessageBox.Show(VAR.ToString());

            MDL = VOLEQ.ToString().Substring(3, 3);
            //added for A00F32W*** equation (08/27/2013)
            if (MDL == "F32" || MDL == "f32") HTLOG = 32;
            
            //add test biomass calc variable
            SPCD = int.Parse(speciesTB.Text);
            WF = new float[3];
            WF[0] = 0.0F;
            WF[1] = 0.0F;
            WF[2] = 0.0F;
            BMS = new float[8];
            CFWT = new float[7];
        }

        //prepare variables for calling the profile model just to get dibs for drawing the profile
        private void pmtPrep()        
        {
            //int PMTFLG = 1;
            float segLen = 0.0F;
            float DRC = 0.0F;
            float totHt = 0.0F;
            //# points = 300
            //those points have to be scaled so that their sum = 750.
            //750/300 = 2.5
            
            //Added this to let it draw the stem when there is no total height
            //11/15/2011 (yw)
            //getVolPrep();
            HTTOT = float.Parse(totalHtTB.Text);
            HT1PRD = float.Parse(mrchHtPPTB.Text);
            HT2PRD = float.Parse(mrchHtSPTB.Text);
            if (HTTOT > 0) pmtHt = HTTOT;
            else if (HT2PRD > 0) pmtHt = HT2PRD;
            else if (HT1PRD > 0) pmtHt = HT1PRD;

            if (HTTOT == 0)
            {
                if (HT2PRD > 0)
                {
                    totHt = HT2PRD;
                }
                else if (HT1PRD > 0)
                {
                    totHt = HT1PRD;
                }
                else totHt = UPSHT1;
            }
            else totHt = HTTOT;

            segLen = totHt / 300;

            //drcob is used as the height where you want dib
            DRCOB = 0F;

            //this scales the dibs so huge trees don't take up the whole screen
            //and small trees aren't super tiny/thin
            dibScalingFactor = ((1.0F / DBHOB) * 10 + 1.4F);

            //for each point(height) get the DIB and draw it
            for (int i = 0; i < 300; i++)
            {                
                //for now I am just using DRCOB to pass in the height
                DRCOB += segLen;

                if (REGN == 9)
                {
                    //R9CLARKDIB is a modified version of r9clark.f that only cals dib at a given height
                    R9CLARKDIB(VOLEQ, ref STUMP, ref MTOPP, ref MTOPS, ref DBHOB, ref HT1PRD, ref HT2PRD, ref HTTOT, ref DRCOB, ref NOLOGP, PROD, ref ERRFLAG, ref UPSHT1);
                }
                else if (VOLEQ.ToString().Contains("BEH"))
                {
                    DIBHT = HTTOT - DRCOB;
                    NOLOGP = r6behDIB(DIBHT);                   
                }
                else //if (VOLEQ.ToString().Contains("MAT"))
                {
                    CALCDIACS(ref REGN,FORST,VOLEQ,ref STUMP,ref DBHOB,ref DRC,ref HTTOT,ref UPSHT1,ref UPSHT2,ref UPSD1,ref UPSD2,ref HTREF,ref AVGZ1,ref AVGZ2,
                        ref FCLASS, ref DBTBH, ref BTR, ref DRCOB, ref NOLOGP, ref DOB, ref ERRFLAG, strlen, strlen);
                }
                //else
                //{
                //    //pmtprofile is a trimmed down version of profile.f that just returns dib
                //    PMTPROFILE(FORST, VOLEQ, ref MTOPP, ref MTOPS, ref STUMP, ref DBHOB, ref NOLOGP,
                //       HTTYPE, ref HTTOT, ref HTLOG, ref HT1PRD, ref HT2PRD, ref UPSHT1, ref UPSHT2, ref UPSD1, ref UPSD2,
                //       ref AVGZ1, ref AVGZ2, ref HTREF, ref DBTBH, ref BTR, VOL,
                //       ref CUTFLG, ref BFPFLG, ref CUPFLG,
                //       ref CDPFLG, ref SPFLG, ref DRCOB, CTYPE, ref FCLASS, PROD, ref ERRFLAG,
                //       strlen, strlen, strlen, strlen, strlen);

                //    //and dib is returned in NOLOGP

                //}
                dibs[0, i] = NOLOGP;
                dibs[1, i] = DRCOB;                
            }
            //adjust the scaling factor if too big or too small
            if (DBHOB * dibScalingFactor > 65)
                while (DBHOB * dibScalingFactor > 65)
                    dibScalingFactor -= .05F;
            else if (DBHOB * dibScalingFactor < 20)
                while (DBHOB * dibScalingFactor < 13)
                    dibScalingFactor += .05F;
        }

        private void clearStrings()
        {
            FORST.Length = 0;
            DIST.Length = 0;
            PROD.Length = 0;
            HTTYPE.Length = 0;
            VOLEQ.Length = 0;
            LIVE.Length = 0;
            CONSPEC.Length = 0;
            CTYPE.Length = 0;
        }

        private void displayVolumes()
        //populate volume fields on the form
        {
            double bfToCuRatio;
            string[] errmsg = new string[14]{"No volume equation match",
                                            "No form class",
                                            "DBH less than one",
                                            "Tree height less than 4.5",
                                            "D2H is out of bounds",
                                            "No species match",
                                            "Illegal primary product log height (Ht1prd)",
                                            "Illegal secondary product log height (Ht2prd)",
                                            "Upper stem measurements required",
                                            "Illegal upper stem height (UPSHT1)",
                                            "Unable to fit profile given dbh, merch ht and top dia",
                                            "Tree has more than 20 logs",
                                            "Illegal top diameter",
                                            "The VOLEQ does not have a bark thickness equation"};
            int i = 0;
            versionTB.Text = VERSION.ToString();
            volEqTB.Text = VOLEQ.ToString();
            errrorFlagTB.Text = ERRFLAG.ToString();
            totCuVolTB.Text = Math.Round(VOL[0],1).ToString();
            grossCuVolTB.Text = Math.Round(VOL[3],1).ToString();
            boardFtVolTB.Text = Math.Round(VOL[1]).ToString();
            topwoodCuTB.Text = Math.Round(VOL[6],1).ToString();
            stemtipCuVolTB.Text = Math.Round(VOL[14],1).ToString();
            stumpwoodCuTB.Text = Math.Round(VOL[13],1).ToString();
            XintTB.Text = VOL[9].ToString();
            netCuVolTB.Text = VOL[4].ToString();
            diboTB.Text = DIB.ToString();
            merchCordTB.Text = VOL[5].ToString();
            if (VOL[1] > 0 && VOL[3] > 0)
            {
                bfToCuRatio = VOL[1] / VOL[3];
                bfToCuRatio = Math.Round(bfToCuRatio, 1);
                bfToCuRatioTB.Text = bfToCuRatio.ToString();
            }
            else
            {
                bfToCuRatioTB.Clear();
            }
            if (ERRFLAG > 0)
            {
                i = ERRFLAG - 1;
                errmsgTB.Text = errmsg[i];
                errmsgTB.ForeColor = Color.Red;
                errmsgTB.Visible = true;
            }
            else errmsgTB.Visible = false;

        }

        private void mRulesPrep()
        {
            //check to see if user wants to enter/mod merch rules
            if (mRulesCB.Checked)
                PMTFLG = 2;

            //set the default merch rules
            setMerchDefaults();
            
            //user wants to modify mer rules so get values from form
            if (PMTFLG == 2)
            {
                switch (optCB.SelectedIndex)
                {
                    case 0:
                        mRules.opt = 11;
                        break;
                    case 1:
                        mRules.opt = 12;
                        break;
                    case 2:
                        mRules.opt = 13;
                        break;
                    case 3:
                        mRules.opt = 14;
                        break;
                    case 4:
                        mRules.opt = 21;
                        break;
                    case 5:
                        mRules.opt = 22;
                        break;
                    case 6:
                        mRules.opt = 23;
                        break;
                    case 7:
                        mRules.opt = 24;
                        break;
                }
                //get the rest of the merch rules from user entered text boxes
                mRules.mtopp = float.Parse(topDIBTB.Text);
                mRules.maxlen = float.Parse(maxLenTB.Text);
                mRules.minlen = float.Parse(minLenTB.Text);
                mRules.minlent = float.Parse(minLenTTB.Text);
                mRules.stump = float.Parse(stumpTB.Text);
                mRules.trim = float.Parse(trimTB.Text);   

            }
        }

        private void setDefaults_Click(object sender, EventArgs e)
        {
            getVolPrep();
            setMerchDefaults();
            updateMerRulesForm();
        }                        
        
        private void updateMerRulesForm()
        {
            if (PROD.ToString(0,2) == "01") 
            {
                topDIBTB.Text = mRules.mtopp.ToString();
            }
            else
            {
                topDIBTB.Text = mRules.mtops.ToString();
            }
            maxLenTB.Text = mRules.maxlen.ToString();
            minLenTB.Text = mRules.minlen.ToString();
            minLenTTB.Text = mRules.minlent.ToString();
            stumpTB.Text = mRules.stump.ToString();
            trimTB.Text = mRules.trim.ToString();
            switch (mRules.opt)
            {
                case 11:
                    optCB.SelectedIndex = 0;
                    break;
                case 12:
                    optCB.SelectedIndex = 1;
                    break;
                case 13:
                    optCB.SelectedIndex = 2;
                    break;
                case 14:
                    optCB.SelectedIndex = 3;
                    break;
                case 21:
                    optCB.SelectedIndex = 4;
                    break;
                case 22:
                    optCB.SelectedIndex = 5;
                    break;
                case 23:
                    optCB.SelectedIndex = 6;
                    break;
                case 24:
                    optCB.SelectedIndex = 7;
                    break;
            }
            
        }

        private void setMerchDefaults()
        {
            string MDL = VOLEQ.ToString(3,3);
            int num;
            bool isNum =int.TryParse(VOLEQ.ToString(7, 3),out num);
            int spp;
            string mProd = PROD.ToString(0, 2);
            if (isNum)
            {
                spp = int.Parse(VOLEQ.ToString(7, 3));
            }
            else
            {
                MessageBox.Show("Invalid equation");
                return;
            }

            if(BTR > 0.0 && DBTBH <= 0)
                DBTBH = DBHOB - (DBHOB * BTR/100);

            if (REGN == 1)
            {
                if (MDL == "FW2" || MDL == "fw2" || MDL == "FW3" || MDL == "fw3")
                {
                    mRules.cor = 'Y';
                    mRules.evod = 2;

                    mRules.maxlen = 16;
                    mRules.minlen = 2;
                    mRules.minlent = 8;
                    mRules.opt = 22;

                    //if (mRules.stump <= 0) 
                        mRules.stump = 1;

                    //if (mRules.mtopp <= 0) 
                        mRules.mtopp = 5.6F;
                    //if (mRules.mtops <= 0) 
                        mRules.mtops = 4;
                    mRules.trim = .5F;
                    //MIN SAWTIMBER LENGTH
                    mRules.merchl = 8;
                    // min dbh tree for sawtimber
                    mRules.minbfd = 1;
                }
                else
                {
                    mRules.cor = 'Y';
                    mRules.evod = 2;
                    mRules.maxlen = 20;
                    mRules.minlen = 10;
                    mRules.minlent = 2;
                    mRules.opt = 12;
                    //if (mRules.stump <= 0) 
                        mRules.stump = 1;
                    //if (mRules.mtopp <= 0) 
                        mRules.mtopp = 5.6F;
                    //if (mRules.mtops <= 0) 
                        mRules.mtops = 4;
                    mRules.trim = .5F;
                    // MIN SAWTIMBER LENGTH
                    mRules.merchl = 10;
                    //min dbh tree for sawtimber
                    mRules.minbfd = 1;
                }
            }
            else if (REGN == 2)
            {
                mRules.cor = 'Y';
                mRules.evod = 2;
                mRules.maxlen = 16;
                mRules.minlen = 2;
                mRules.minlent = 2;
                mRules.opt = 22;
                //if (mRules.stump <= 0) 
                    mRules.stump = 1;
                //if (mRules.mtopp <= 0) 
                    mRules.mtopp = 6;
                //if (mRules.mtops <= 0) 
                    mRules.mtops = 4;
                mRules.trim = 0.5F;
                // MIN SAWTIMBER LENGTH
                mRules.merchl = 8;
                //min dbh tree for sawtimber
                mRules.minbfd = 1;
            }
            else if (REGN == 3)
            {
                mRules.cor = 'Y';
                mRules.evod = 2;
                mRules.maxlen = 16;
                //changed min log length for region 3 (03/25/14)
                if (mProd == "01") 
                {
                    mRules.minlen = 8;
                }
                else
                {
                    mRules.minlen = 10;
                }
                mRules.minlent = 10;
                mRules.opt = 22;
                //changed to 23 for test only YW (7/11/12)
                //mRules.opt = 23;
                //if (mRules.stump <= 0) 
                    mRules.stump = 1;
                //if (mRules.mtopp <= 0) 
                    mRules.mtopp = 6;
                //if (mRules.mtops <= 0) 
                    mRules.mtops = 4;
                mRules.trim = 0.5F;
                // MIN SAWTIMBER LENGTH
                mRules.merchl = 8;
                //min dbh tree for sawtimber
                //       mRules.minbfd = 7
                mRules.minbfd = 1;
            }
            else if (REGN == 4)
            {

                mRules.cor = 'Y';
                mRules.evod = 2;
                mRules.maxlen = 16;
                mRules.minlen = 2;
                mRules.minlent = 2;
                mRules.opt = 22;
                //if (mRules.stump <= 0) 
                    mRules.stump = 1;
                //if (mRules.mtopp <= 0) 
                    mRules.mtopp = 6;
                //if (mRules.mtops <= 0) 
                    mRules.mtops = 4;
                mRules.trim = .5F;
                // MIN SAWTIMBER LENGTH
                mRules.merchl = 8;
                //min dbh tree for sawtimber
                //        mRules.minbfd = 7
                mRules.minbfd = 1;
            }
            else if (REGN == 5)
            {
                mRules.cor = 'Y';
                mRules.evod = 2;
                mRules.maxlen = 16;
                mRules.minlen = 2;
                mRules.minlent = 2;
                mRules.opt = 22;
                //if (mRules.stump <= 0) 
                    mRules.stump = 1;
                //if (mRules.mtopp <= 0) 
                    mRules.mtopp = 6;
                //if (mRules.mtops <= 0) 
                    mRules.mtops = 4;
                mRules.trim = .5f;
                mRules.trim = 0;
                // MIN SAWTIMBER LENGTH
                mRules.merchl = 8;
                mRules.merchl = 4;
                //min dbh tree for sawtimber
                //        mRules.minbfd = 7
                mRules.minbfd = 1;
            }
            else if (REGN == 6 || REGN == 11)
            {
                mRules.cor = 'N';
                mRules.evod = 2;
                mRules.maxlen = 16;
                mRules.minlen = 2;
                mRules.minlent = 2;
                mRules.opt = 23;
                //if (mRules.stump <= 0) 
                    mRules.stump = 0;
                //if (mRules.mtopp <= 0) 
                    mRules.mtopp = 2;
                //if (mRules.mtops <= 0) 
                    mRules.mtops = 2;
                mRules.trim = .5F;
                // MIN SAWTIMBER LENGTH
                mRules.merchl = 8;
                //min dbh tree for sawtimber
                //          mRules.minbfd = 6
                mRules.minbfd = 1;
            }
            else if (REGN == 7)
            {
                mRules.cor = 'N';
                mRules.evod = 2;
                mRules.maxlen = 16;
                mRules.minlen = 2;
                mRules.minlent = 2;
                mRules.opt = 23;
                //if (mRules.stump <= 0) 
                    mRules.stump = 1;
                //if (mRules.mtopp <= 0) 
                    mRules.mtopp = (float)(Math.Truncate((0.184F * DBHOB) + 2.24F));
                //if (mRules.mtops <= 0) 
                    mRules.mtops = 2;
                mRules.trim = .5f;
                // MIN SAWTIMBER LENGTH
                mRules.merchl = 8;
                //min dbh tree for sawtimber
                //        mRules.minbfd = 6
                mRules.minbfd = 1;
            }
            else if (REGN == 8)
            {
                if (MDL == "CLK" || MDL == "NEW")
                {
                    mRules.cor = 'Y';
                    mRules.evod = 2;

                    mRules.maxlen = 8;  // 16; changed to 8 based on r9clark.f
                    mRules.minlen = 2; // 2; changed to 4 based on r9clark.f
                    mRules.minlent = 8;
                    mRules.opt = 22;
                    if (spp < 300)
                    {
                        mRules.mtopp = 7.0F;
                    }
                    else
                    {
                        mRules.mtopp = 9.0F;
                    }
                    mRules.mtops = 4.0F;
                    mRules.trim = .5F;
                    if (mProd == "01")
                    {
                        mRules.stump = 1.0F;
                    }
                    else
                    {
                        mRules.stump = .5F;
                    }

                }
            }
            else if (REGN == 9)
            {
                if (MDL == "CLK")
                {
                    mRules.cor = 'Y';
                    mRules.evod = 2;

                    mRules.maxlen = 8;  // 16; changed to 8 based on r9clark.f
                    mRules.minlen = 4; // 2; changed to 4 based on r9clark.f
                    mRules.minlent = 8;
                    mRules.opt = 22;
                    if (spp < 300)
                    {
                        mRules.mtopp = 7.6F;
                    }
                    else
                    {
                        mRules.mtopp = 9.6F;
                    }
                    mRules.mtops = 4.0F;
                    mRules.trim = .3F;
                    if (mProd == "01")
                    {
                        mRules.stump = 1.0F;
                    }
                    else
                    {
                        mRules.stump = .5F;
                    }

                }
            }
            else if (REGN == 10)
            {
                mRules.cor = 'Y';
                mRules.evod = 2;
                mRules.maxlen = 16;
                mRules.minlen = 8;
                mRules.minlent = 8;
                mRules.opt = 23;
                //if (mRules.stump <= 0) 
                    mRules.stump = 1;
                //if (mRules.mtopp <= 0) 
                    mRules.mtopp = 6;
                //if (mRules.mtops <= 0) 
                    mRules.mtops = 4;
                mRules.trim = .5F;
                // MIN SAWTIMBER LENGTH
                mRules.merchl = 8;
                //min dbh tree for sawtimber
                //        mRules.minbfd = 6
                mRules.minbfd = 1;
            }
            //default merch rules
            else
            {
                mRules.cor = 'Y';
                mRules.evod = 2;
                mRules.maxlen = 16;
                mRules.minlen = 2;
                mRules.minlent = 2;
                mRules.opt = 22;
                //if (mRules.stump <= 0) 
                    mRules.stump = 1;
                //if (mRules.mtopp <= 0) 
                    mRules.mtopp = 6;
                //if (mRules.mtops <= 0) 
                    mRules.mtops = 4;
                mRules.trim = .5F;
                // MIN SAWTIMBER LENGTH
                mRules.merchl = 8;
                //min dbh tree for sawtimber
                mRules.minbfd = 1;
            }
        }

        private void mRulesCB_CheckedChanged(object sender, EventArgs e)
        {
            if (mRulesCB.Checked)
            {
                if (string.IsNullOrEmpty(volEqTB.Text))
                {
                    MessageBox.Show("You cannot modify merch rules without a volume equation.");
                    mRulesCB.Checked = false;
                }
                else
                {
                    string volMDL = volEqTB.Text.ToString().ToUpper().Substring(3,2);
                    string volREGN = volEqTB.Text.ToString().ToUpper().Substring(0, 1);
                    if (volMDL == "FW" || volMDL == "WO" || volMDL == "CZ" || volMDL == "JB" || volMDL == "BE" || (volMDL == "CL")) // && volREGN == "8"))
                    //if (volMDL == "FW" || volMDL == "WO" || volMDL == "CZ" || volMDL == "JB" || volMDL == "BE")
                    {
                        panel2.Enabled = true;
                        if (string.IsNullOrEmpty(maxLenTB.Text))
                        {
                            getVolPrep();
                            setMerchDefaults();
                            updateMerRulesForm();
                        }
                    }
                    else
                    {
                        MessageBox.Show("The merch rules for " + volEqTB.Text.ToString() + " cannot be modified.");
                        mRulesCB.Checked = false;
                    }

                }

            }
            else
                panel2.Enabled = false;
            

        }
         
        private bool checkFields()
        {
            bool allGood = true;
            if (string.IsNullOrEmpty(regionTB.Text))
            {
                MessageBox.Show("Region cannot be blank");
                allGood = false;
            }
            if(string.IsNullOrEmpty(forestTB.Text))
            {
                MessageBox.Show("Invalid forest #");
                allGood = false;
            }

            if (string.IsNullOrEmpty(formClsTB.Text))
            {
                MessageBox.Show("Invalid form class #");
                allGood = false;
            }

            if (string.IsNullOrEmpty(totalHtTB.Text))
            {
                MessageBox.Show("Invalid form total height");
                allGood = false;
            }

            return allGood;
        }

        //getDIB for give height
        private void GetDibBN_Click(object sender, EventArgs e)
        {
            int i2 = 2;
            int i10 = 10;            
            if (upstht2TB.TextLength > 0)
                DIBHT = float.Parse(upstht2TB.Text);
            
            //make sure all required fields are populated
            if (!checkFields())
                return;

            //initialize variables for volume library dll call
            getVolPrep();
            PMTFLG = 1;

            if (VOLEQ.Length < 10)
            {
                GETVOLEQ3(ref REGN, FORST, DIST, ref SPCODE, PROD, VOLEQ, ref ERRFLAG, strlen, strlen, strlen, strlen);
                volEqTB.Text = VOLEQ.ToString();
            }

            //check for profile model volume equation number
            if (!checkVolEq())
            {
                MessageBox.Show("cannot return dib for this volume equation");
                return;
            }

            if (REGN == 9 && HTTOT == 0)
            {
                UPSHT1 = HT1PRD;
                UPSD1 = MTOPP;
                UPSHT2 = HT2PRD;
                UPSD2 = MTOPS;
            }
            //    R9CLARKDIB(VOLEQ, ref STUMP, ref MTOPP, ref MTOPS, ref DBHOB, ref HT1PRD, ref HT2PRD, ref HTTOT, ref DIBHT, ref DIB, PROD, ref ERRFLAG, ref UPSHT1);
            
            //else if (VOLEQ.ToString().Contains("BEH") || (REGN == 8 && VOLEQ.ToString().Contains("CLK")))
            //{
                CALCDIACS(ref REGN, FORST, VOLEQ, ref STUMP, ref DBHOB, ref DRCOB, ref HTTOT, ref UPSHT1, ref UPSHT2, ref UPSD1, ref UPSD2, ref HTREF,
                    ref AVGZ1, ref AVGZ2, ref FCLASS, ref DBTBH, ref BTR, ref DIBHT, ref DIB, ref DOB, ref ERRFLAG, strlen, strlen);
            //}
            
            //else
            //    PMTPROFILE(FORST, VOLEQ, ref MTOPP, ref MTOPS, ref STUMP, ref DBHOB, ref DIB,
            //      HTTYPE, ref HTTOT, ref HTLOG, ref HT1PRD, ref HT2PRD, ref UPSHT1, ref UPSHT2, ref UPSD1, ref UPSD2,
            //      ref AVGZ1, ref AVGZ2, ref HTREF, ref DBTBH, ref BTR, VOL,
            //      ref CUTFLG, ref BFPFLG, ref CUPFLG,
            //      ref CDPFLG, ref SPFLG, ref DIBHT, CTYPE, ref FCLASS, PROD, ref ERRFLAG,
            //      strlen, strlen, strlen, strlen, strlen);

            volEqTB.Text = VOLEQ.ToString();
            diboTB.Text = DIB.ToString();

        }

        //for now this checks to see if you can calculate DIB only for the given voleq
        private bool checkVolEq(){
            if (VOLEQ.ToString().Contains("FW") ||
                VOLEQ.ToString().Contains("F3") ||
                   VOLEQ.ToString().Contains("CZ") ||
                   VOLEQ.ToString().Contains("DEM") ||
                    VOLEQ.ToString().Contains("WO2") ||
                   VOLEQ.ToString().Contains("MAT")||
                   VOLEQ.ToString().Contains("CLK")||
                VOLEQ.ToString().Contains("BEH"))
                return true;
            else
                return false;
        }

        
        //check the middle (MDL) of the equation number to see if it is valid for
        //displaying the profile model graphic
        private bool isProfileModel()
        {
            if (MDL.Contains("FW") ||
                VOLEQ.ToString().Contains("F3") ||
                  MDL.Contains("CZ") ||
                  MDL.Contains("DEM") ||
                  MDL.Contains("WO2") ||
                  //(REGN == 9 && MDL.Contains("CLK")) ||
                  MDL.Contains("CLK") ||
                  MDL.Contains("MAT") ||
                  MDL.Contains("BEH"))//NEED TO SET THIS TO R6 ONLY
                return true;
            else
                return false;
        }

        //method for calculating dib at htdib using behre's hyperbola
        //and regino 6 specs.  Converted from FORTRAN code R6DIBS.f
        private float r6behDIB(float htdib)
        {        
            string volEqNum;
            //int zone;
            //float fc_ht;
            float d17;
            float h17;
            float dbhib;

            //get the volume equation number (1st 3 digits)
            volEqNum = VOLEQ.ToString().Substring(0,3);

            // DETERMINE THE ZONE
            if (volEqNum == "616" || volEqNum == "628")
            {
                //zone = 1;
                //fc_ht = 17.3f;
                h17 = 17.3f;
            }
            else if (volEqNum == "632")
            {
                //zone = 2;
                //fc_ht = 33.6f;
                h17 = 33.6f;
            }
            else
            {
                ERRFLAG = 1;
                return 0;
            }
              
            d17 = FCLASS / 100.0f * DBHOB;
            //h17 = 17.3f;
           // if(zone == 2) h17 = 33.6f;
            dbhib = DBHOB - DBTBH;
            if(dbhib <= 0 || dbhib > DBHOB) dbhib = DBHOB;

            float HTUP;
            float A = 0.62f;
            float B = 1.0f - A;
            if (HTTOT <= 1.0)
                HTUP = HT1PRD - h17;
            else
                HTUP = HTTOT - h17;
            //FIND DIAMETER FOR given height            
            float HRATIO = htdib/HTUP;

            if (HRATIO <= 0) return 0;            
            float DR = HRATIO / (A*HRATIO+B);
            return DR * d17;
        }

        //base model for calculating dib at a height using behres...NOTE: not currently used.
        private float behresDIB(float htdib)
        {
            float D17, DIBCOR, HBUTT, A;

            A = 0.62F;

            HBUTT = HTTOT - 17.8F;

            D17 = FCLASS * DBHOB / 100;

            DIBCOR = D17 * ((htdib / HBUTT) / (A * (htdib / HBUTT) + (1 - A)));

            return DIBCOR;
        }

        private void getVolumeEqnBN_Click(object sender, EventArgs e)
        {
            if (checkVolEqnRequiredFields())
            {
                //getVolPrep();
                REGN = int.Parse(regionTB.Text);
                FORST.Length = 0;
                FORST.Append(forestTB.Text);
                DIST.Length = 0;
                DIST.Append(districtTB.Text);
                PROD.Length = 0;
                PROD.Append(prodTB.Text);
                SPCODE = int.Parse(speciesTB.Text);
                if (SPCODE > 999) SPCODE = 999;
                GETVOLEQ3(ref REGN, FORST, DIST, ref SPCODE, PROD, VOLEQ, ref ERRFLAG, strlen, strlen, strlen, strlen);
                volEqTB.Text = VOLEQ.ToString();
            }
            else
                volEqTB.Text = "";
            
        }

        private bool checkVolEqnRequiredFields()
        {
            bool allGood = true;
            if (string.IsNullOrEmpty(regionTB.Text))
            {
                MessageBox.Show("Region cannot be blank");
                allGood = false;
            }
            if (string.IsNullOrEmpty(forestTB.Text))
            {
                MessageBox.Show("Invalid forest #");
                allGood = false;
            }

            if (string.IsNullOrEmpty(speciesTB.Text))
            {
                MessageBox.Show("Invalid species #");
                allGood = false;
            }

            if (string.IsNullOrEmpty(prodTB.Text))
            {
                MessageBox.Show("Invalid product code");
                allGood = false;
            }

            return allGood;
        }

        private void ClearFormBtn_Click(object sender, EventArgs e)
        {
            regionTB.Text = null;
            forestTB.Text = null;
            districtTB.Text = null;
            speciesTB.Text = null;
            prodTB.Text = "01";
            volEqTB.Text = null;
            ctypeTB.Text = "FVS Vol";
            htTypeTB.Text = "Feet";
            dbhTB.Text = null;
            totalHtTB.Text = null;
            formClsTB.Text = "0";
            mrchHtPPTB.Text = "0";
            upsht1TB.Text = "0";
            topDibPPTB.Text = "0";
            mrchHtSPTB.Text = "0";
            topDibSPTB.Text = "0";
            topDIBTB.Text = null;
            maxLenTB.Text = null;
            minLenTB.Text = null;
            minLenTTB.Text = null;
            stumpTB.Text = null;
            trimTB.Text = null;
            optCB.Text = null;
            totCuVolTB.Text = null;
            grossCuVolTB.Text = null;
            netCuVolTB.Text = null;
            topwoodCuTB.Text = null;
            stumpwoodCuTB.Text = null;
            merchCordTB.Text = null;
            boardFtVolTB.Text = null;
            bfToCuRatioTB.Text = null;
            XintTB.Text = null;
            stemtipCuVolTB.Text = null;
            errrorFlagTB.Text = null;
            dbtbhTB.Text = "0";
            btrTB.Text = "0";
            upsd1TB.Text = "0";
            htrefTB.Text = "0";
            avgz1TB.Text = "0";
            errmsgTB.Visible = false;
        }

        private void PrintGraphBtn_Click(object sender, EventArgs e)
        {
            PrintDocument pd = new PrintDocument(); 
            pd.PrintPage += new PrintPageEventHandler(Print_Page);
            pd.DefaultPageSettings.Landscape = true;
            //PrintPreviewDialog dlg = new PrintPreviewDialog(); 
            //dlg.Document = pd; 
            //dlg.ShowDialog(); 
            PrintDialog pDialog = new PrintDialog();
            pDialog.Document = pd;
            DialogResult prt = pDialog.ShowDialog();
            if (prt == DialogResult.OK)
              pd.Print();
        }
        private void Print_Page(object o, PrintPageEventArgs e) 
        {
            Image i = this.image;   
            Point p = new Point(10, 10);
            float newWidth = i.Width * 100 / i.HorizontalResolution;
            float newHeight = i.Height * 100 / i.VerticalResolution;

            float widthFactor = newWidth / e.MarginBounds.Width;
            float heightFactor = newHeight / e.MarginBounds.Height;

            if (widthFactor > 1 | heightFactor > 1)
            {
                if (widthFactor > heightFactor)
                {
                    newWidth = newWidth / widthFactor;
                    newHeight = newHeight / widthFactor;
                }
                else
                {
                    newWidth = newWidth / heightFactor;
                    newHeight = newHeight / heightFactor;
                }
            }
            e.Graphics.DrawImage(i, 130, 100, (int)newWidth, (int)newHeight);

        }
        //Add info for tree #1 to list
        private void AddTree1ToList()
        {
            //int lognum = 0;
            int opt = 0;
            tree1.Clear();
            //add input variables
            tree1.Add("Input Parameters:");
            tree1.Add("Region".PadRight(25, ' ').PadLeft(30, ' ') + regionTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Forest".PadRight(25, ' ').PadLeft(30, ' ') + forestTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("District".PadRight(25, ' ').PadLeft(30, ' ') + districtTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Species".PadRight(25, ' ').PadLeft(30, ' ') + speciesTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Prod".PadRight(25, ' ').PadLeft(30, ' ') + prodTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Vol Equation".PadRight(25, ' ').PadLeft(30, ' ') + volEqTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("DBH".PadRight(25, ' ').PadLeft(30, ' ') + dbhTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Total HT".PadRight(25, ' ').PadLeft(30, ' ') + totalHtTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("HT Type".PadRight(25, ' ').PadLeft(30, ' ') + htTypeTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Form Class".PadRight(25, ' ').PadLeft(30, ' ') + formClsTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Merch HT (PP)".PadRight(25, ' ').PadLeft(30, ' ') + mrchHtPPTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Top DIB (PP)".PadRight(25, ' ').PadLeft(30, ' ') + topDibPPTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Upper Stem HT".PadRight(25, ' ').PadLeft(30, ' ') + upsht1TB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Merch HT (SP)".PadRight(25, ' ').PadLeft(30, ' ') + mrchHtSPTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Top DIB (SP)".PadRight(25, ' ').PadLeft(30, ' ') + topDibSPTB.Text.ToString().PadLeft(10, ' '));
            //merch rules
            switch (optCB.SelectedIndex)
            {
                case 0:
                    opt = 11;
                    break;
                case 1:
                    opt = 12;
                    break;
                case 2:
                    opt = 13;
                    break;
                case 3:
                    opt = 14;
                    break;
                case 4:
                    opt = 21;
                    break;
                case 5:
                    opt = 22;
                    break;
                case 6:
                    opt = 23;
                    break;
                case 7:
                    opt = 24;
                    break;
            }
            
            tree1.Add(" ");
            tree1.Add("Merch Rules:");
            tree1.Add("Top DIB".PadRight(25, ' ').PadLeft(30, ' ') + topDIBTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Max Log Length".PadRight(25, ' ').PadLeft(30, ' ') + maxLenTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Min Log Length".PadRight(25, ' ').PadLeft(30, ' ') + minLenTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Min Log Len for Topwood".PadRight(25, ' ').PadLeft(30, ' ') + minLenTTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Stump".PadRight(25, ' ').PadLeft(30, ' ') + stumpTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Trim".PadRight(25, ' ').PadLeft(30, ' ') + trimTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Segmention Opt".PadRight(25, ' ').PadLeft(30, ' ') + opt.ToString().PadLeft(10, ' '));
            //add meaning of segmention option
            tree1.Add("        -----------------------------------------------------------------");
            tree1.Add("        11 = 16 ft log scale, presented as tree length log. (FSH 2409.11)");
            tree1.Add("        12 = 20 ft log scale, presented as tree length log. (FSH 2409.11)");
            tree1.Add("        13 = 32 ft log scale, presented as tree length log.");
            tree1.Add("        14 = 40 ft log scale, presented as tree length log.");
            tree1.Add("        21 = Nominal log length (NLL), if top log is less then half");
            tree1.Add("             of the NLL then it is combined with the next lowest log and");
            tree1.Add("             this combined piece is then resegmented according to the");
            tree1.Add("             entered merchandising parameters giving two approximately");
            tree1.Add("             equal log lengths.  If the segment length is greater then");
            tree1.Add("             or equal to half the NNL then the segment stands on its' own.");
            tree1.Add("        22 = Nominal log length (NLL), top log is combined with the next");
            tree1.Add("             lowest log and this combined piece is then resegmented");
            tree1.Add("             according to the entered merchandising parameters giving");
            tree1.Add("             two approximately equal log lengths.");
            tree1.Add("        23 = Nominal log length, top segment stands on its' own.");
            tree1.Add("        24 = Nominal log length, if the top segment is less then 1/4 of");
            tree1.Add("             NNL then the segment is droped,  it the segment is 1/4 to");
            tree1.Add("             3/4 of NNL then the segment length is set to 1/2 of NNL,");
            tree1.Add("             if the segment is greater then 3/4 of NNL then the segment");
            tree1.Add("             length is set to NNL.");
            tree1.Add("        -----------------------------------------------------------------");
            //volumes
            tree1.Add(" ");
            tree1.Add("Tree Volumes:");
            tree1.Add("Total Cubic".PadRight(25, ' ').PadLeft(30, ' ') + totCuVolTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Gross Merch Cubic".PadRight(25, ' ').PadLeft(30, ' ') + grossCuVolTB.Text.ToString().PadLeft(10, ' '));
            //tree1.Add("Net Merch Cubic".PadRight(25, ' ').PadLeft(30, ' ') + netCuVolTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Gross Boardfoot".PadRight(25, ' ').PadLeft(30, ' ') + boardFtVolTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Gross Xint Boardfoot".PadRight(25, ' ').PadLeft(30, ' ') + XintTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Topwood Cubic".PadRight(25, ' ').PadLeft(30, ' ') + topwoodCuTB.Text.ToString().PadLeft(10, ' '));
            tree1.Add("Merch Cordwood".PadRight(25, ' ').PadLeft(30, ' ') + merchCordTB.Text.ToString().PadLeft(10, ' '));
            //tree1.Add("Stump".PadRight(25, ' ').PadLeft(30, ' ') + stumpwoodCuTB.Text.ToString().PadLeft(10, ' '));
            //tree1.Add("Stem Tip".PadRight(25, ' ').PadLeft(30, ' ') + stemtipCuVolTB.Text.ToString().PadLeft(10, ' '));
            //Logs
            tree1.Add(" ");
            tree1.Add("Logs:");
            for (int i = 0; 1 < 20; i++)
            {
                if (this.LOGLEN[i] > 0)
                {
                    lognum1 = i + 1;
                    tree1.Add("Log # ".PadLeft(11, ' ') + lognum1.ToString());
                    tree1.Add("Length".PadRight(25, ' ').PadLeft(30, ' ') + this.LOGLEN[i].ToString().PadLeft(10, ' '));
                    tree1.Add("Small End Dia".PadRight(25, ' ').PadLeft(30, ' ') + Math.Round(this.LOGDIA[1,i+1],1).ToString().PadLeft(10, ' '));
                    tree1.Add("Large End Dia".PadRight(25, ' ').PadLeft(30, ' ') + Math.Round(this.LOGDIA[1,i],1).ToString().PadLeft(10, ' '));
                    tree1.Add("Cubic Vol".PadRight(25, ' ').PadLeft(30, ' ') + Math.Round(this.LOGVOL[i,3],1).ToString().PadLeft(10, ' '));
                    tree1.Add("Boardfoot".PadRight(25, ' ').PadLeft(30, ' ') + Math.Round(this.LOGVOL[i,0]).ToString().PadLeft(10, ' '));
                    
                }
                else break;
            }
        }
        //Add info for tree #2 to list
        private void AddTree2ToList()
        {
            int opt = 0;
            int k = 15;
            tree2.Clear();
            //add input variables
            tree2.Add("          ");
            tree2.Add(regionTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(forestTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(districtTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(speciesTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(prodTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(volEqTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(dbhTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(totalHtTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(htTypeTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(formClsTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(mrchHtPPTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(topDibPPTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(upsht1TB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(mrchHtSPTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(topDibSPTB.Text.ToString().PadLeft(k, ' '));
            //merch rules
            switch (optCB.SelectedIndex)
            {
                case 0:
                    opt = 11;
                    break;
                case 1:
                    opt = 12;
                    break;
                case 2:
                    opt = 13;
                    break;
                case 3:
                    opt = 14;
                    break;
                case 4:
                    opt = 21;
                    break;
                case 5:
                    opt = 22;
                    break;
                case 6:
                    opt = 23;
                    break;
                case 7:
                    opt = 24;
                    break;
            }

            tree2.Add(" ");
            tree2.Add(" ");
            tree2.Add(topDIBTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(maxLenTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(minLenTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(minLenTTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(stumpTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(trimTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(opt.ToString().PadLeft(k, ' '));
            //add empty line for segmention opt meaning
            for (int i = 0; i < 22; i++)
            {
                tree2.Add(" ");
            }
            //volumes
            tree2.Add(" ");
            tree2.Add(" ");
            tree2.Add(totCuVolTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(grossCuVolTB.Text.ToString().PadLeft(k, ' '));
            //tree2.Add(netCuVolTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(boardFtVolTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(XintTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(topwoodCuTB.Text.ToString().PadLeft(k, ' '));
            tree2.Add(merchCordTB.Text.ToString().PadLeft(k, ' '));
            //tree2.Add(stumpwoodCuTB.Text.ToString().PadLeft(k, ' '));
            //tree2.Add(stemtipCuVolTB.Text.ToString().PadLeft(k, ' '));
            //Logs
            tree2.Add(" ");
            tree2.Add(" ");
            for (int i = 0; 1 < 20; i++)
            {
                if (this.LOGLEN[i] > 0)
                {
                    lognum2 = i + 1;
                    tree2.Add(" ");
                    tree2.Add(this.LOGLEN[i].ToString().PadLeft(k, ' '));
                    tree2.Add(Math.Round(this.LOGDIA[1, i + 1], 1).ToString().PadLeft(k, ' '));
                    tree2.Add(Math.Round(this.LOGDIA[1, i], 1).ToString().PadLeft(k, ' '));
                    tree2.Add(Math.Round(this.LOGVOL[i, 3], 1).ToString().PadLeft(k, ' '));
                    tree2.Add(Math.Round(this.LOGVOL[i, 0]).ToString().PadLeft(k, ' '));
                    //if tree #2 has more logs, add empty record to tree #1
                    if (lognum2 > lognum1)
                    {
                        tree1.Add("Log # ".PadLeft(11, ' ') + lognum2.ToString());
                        tree1.Add("Length".PadRight(35, ' ').PadLeft(40, ' '));
                        tree1.Add("Small End Dia".PadRight(35, ' ').PadLeft(40, ' '));
                        tree1.Add("Large End Dia".PadRight(35, ' ').PadLeft(40, ' '));
                        tree1.Add("Cubic Vol".PadRight(35, ' ').PadLeft(40, ' '));
                        tree1.Add("Boardfoot".PadRight(35, ' ').PadLeft(40, ' '));
                    }
                }
                else break;
            }

        }
        //write tree1 and tree2 to report
        private void BuildReport()
        {
            using (var writer = new StreamWriter(exePath+FileName))
            {
                writer.WriteLine("Volume Tester Result".PadLeft(40, ' ') + "(VolLib Ver#:" + verNumTB.Text.ToString() + ")");
                //writer.WriteLine("(VolLib Ver#:".PadLeft(32, ' ') + verNumTB.Text.ToString() + ")");

                if (pmtOpenCount < 2)
                {
                    //only need to write one tree to report
                    for (int i = 0; i < tree1.Count; i++)
                    {
                        writer.WriteLine(tree1[i]);
                    }
                }
                else
                {
                    //write two trees
                    writer.WriteLine(" ");
                    writer.WriteLine("Tree #1".PadLeft(40, ' ') + "Tree #2".PadLeft(15, ' '));
                    for (int i = 0; i < tree1.Count; i++)
                    {
                        if (i < tree2.Count) writer.WriteLine(tree1[i] + tree2[i]);
                        else writer.WriteLine(tree1[i]);
                    }
                }
            }
        }

        private void PrintReportBtn_Click(object sender, EventArgs e)
        {
            BuildReport();
            //MessageBox.Show("test Result = " + exePath + FileName);
            System.Diagnostics.Process.Start(exePath + FileName);

        }

        //list of available volume equation for each region
        private string[] VolEqList(int regn)
        {
            string[] volEqAry = new string[] { };
            /*
            string[] R1VolEq = {"I00FW2W012","I00FW2W017","I00FW2W019","I00FW2W073","I00FW2W093",
                                "I00FW2W108","I00FW2W119","I00FW2W122","I00FW2W202","I00FW2W242",
                                "I00FW2W260","I00FW2W260","203FW2W122","616BEHW231","101DVEW375",
                                "101DVEW740","102DVEW017","102DVEW060","102DVEW090","102DVEW106",
                                "102DVEW108","200DVEW746","400DVEW475"};
            
            */
            string[] R1VolEq = {"I00FW2W012","I00FW2W017","I00FW2W019","I00FW2W073","I00FW2W093",
                                "I00FW2W108","I00FW2W119","I00FW2W122","I00FW2W202","I00FW2W242",
                                "I00FW2W263","I00FW2W264","I14FW2W017","I14FW2W019","I14FW2W093",
                                "I14FW2W122","I14FW2W202","I14FW2W263","I14FW2W264","I00FW3W012",
                                "I00FW3W017","I00FW3W019","I00FW3W073","I00FW3W093","I00FW3W108",
                                "I00FW3W119","I00FW3W122","I00FW3W202","I00FW3W242","I00FW3W263",
                                "I00FW3W264","I14FW3W017","I14FW3W019","I14FW3W093","I14FW3W122",
                                "I14FW3W202","I14FW3W263","I14FW3W264","101DVEW119","101DVEW073",
                                "101DVEW202","101DVEW017","101DVEW263","101DVEW242","101DVEW108",
                                "101DVEW093","101DVEW019","101DVEW122","101DVEW999","101DVEW375",
                                "101DVEW740","102DVEW746","102DVEW740","102DVEW017","102DVEW019",
                                "102DVEW070","102DVEW073","102DVEW090","102DVEW093","102DVEW101",
                                "102DVEW108","102DVEW119","102DVEW122","102DVEW202","102DVEW240",
                                "102DVEW242","102DVEW260","102DVEW263","102DVEW060","102DVEW106",
                                "103DVEW101","103DVEW108","103DVEW122","103DVEW202","104DVEW108",
                                "104DVEW122","105DVEW122","106DVEW122","100JB2W017","100JB2W070",
                                "100JB2W073","100JB2W108","100JB2W119","100JB2W122","100JB2W202"};
            /*
            string[] R2VolEq = {"I00FW2W019","200FW2W015","200FW2W108","200FW2W122","200FW2W202",
                                "200FW2W746","202FW2W108","203FW2W122","213FW2W122","407FW2W093",
                                "200DVEW065","200DVEW069","200DVEW475","200DVEW823","200DVEW998",
                                "300DVEW060","300DVEW106","300DVEW106","300DVEW800","300DVEW999"};
            */
            string[] R2VolEq = {"200FW2W015","200FW2W108","200FW2W122","200FW2W202","200FW2W746",
                                "203FW2W122","200FW3W015","200FW3W108","200FW3W122","200FW3W202",
                                "200FW3W746","203FW3W122","200CZ2W015","200CZ2W019","200CZ2W093",
                                "200CZ2W108","200CZ2W122","200CZ2W202","200CZ2W746","203CZ2W122",
                                "200CZ3W015","200CZ3W019","200CZ3W093","200CZ3W108","200CZ3W122",
                                "200CZ3W202","200CZ3W746","203CZ3W122","200DVEW746","200DVEW108",
                                "203DVEW122","200DVEW122","212DVEW122","200DVEW093","200DVEW069",
                                "200DVEW066","200DVEW065","200DVEW814","200DVEW823","200DVEW106",
                                "200DVEW475","200DVEW998","210DVEW746","210DVEW108","210DVEW122",
                                "213DVEW122","210DVEW093"};
            /*
            string[] R3VolEq = {"300FW2W122","300DVEW060","300DVEW093","300DVEW106","300DVEW113",
                                "300DVEW122","300DVEW310","300DVEW314","300DVEW746","300DVEW800",
                                "300DVEW999","301DVEW015","301DVEW202","302DVEW202"};
            */
            string[] R3VolEq = {"300FW2W122","300FW3W122","300DVEW060","300DVEW093","300DVEW106",
                                "300DVEW113","300DVEW122","300DVEW310","300DVEW314","300DVEW746",
                                "300DVEW800","300DVEW999","301DVEW015","302DVEW015","301DVEW202","302DVEW202"};
            /*
            string[] R4VolEq = {"I15FW2W017","I15FW2W093","I15FW2W122","I15FW2W202","407FW2W093",
                                "400MATW015","400MATW019","400MATW020","400MATW073","400MATW081",
                                "400MATW093","400MATW108","400MATW117","400MATW122","400MATW202",
                                "400MATW746","401MATW015","401MATW108","401MATW122","402MATW122",
                                "403MATW122","405MATW019","405MATW202","300DVEW060","300DVEW106",
                                "300DVEW800","400DVEW065","400DVEW066","400DVEW133","400DVEW475",
                                "400DVEW998","401DVEW065"};
            */
            string[] R4VolEq = {"I15FW2W017","I15FW2W019","I15FW2W093","I15FW2W122","I15FW2W202",
                                "I15FW2W263","I15FW2W264","I15FW3W017","I15FW3W019","I15FW3W093",
                                "I15FW3W122","I15FW3W202","I15FW3W263","I15FW3W264","400MATW015",
                                "400MATW019","400MATW020","400MATW073","400MATW081","400MATW093",
                                "400MATW108","400MATW117","400MATW122","400MATW746","400MATW202",
                                "401MATW015","401MATW108","401MATW122","401MATW202","402MATW122",
                                "403MATW122","405MATW202","405MATW019","407MATW093","400DVEW060",
                                "400DVEW066","400DVEW065","400DVEW133","400DVEW106","400DVEW475","400DVEW998"};
            /*
            string[] R5VolEq = {"500WO2W015","500WO2W020","500WO2W081","500WO2W108","500WO2W116",
                                "500WO2W117","500WO2W122","500WO2W202","500WO2W211","H00SN2W301",
                                "H00SN2W671","H01SN2W510","500DVEW060","500DVEW212","500DVEW312",
                                "500DVEW351","500DVEW361","500DVEW431","500DVEW631","500DVEW801",
                                "500DVEW805","500DVEW807","500DVEW811","500DVEW815","500DVEW818",
                                "500DVEW821","500DVEW839","500DVEW981"};
            */
            string[] R5VolEq = {"500WO2W015","500WO2W020","500WO2W081","500WO2W108","500WO2W116",
                                "500WO2W117","500WO2W122","500WO2W202","500WO2W211","H00SN2W301",
                                "H00SN2W510","H00SN2W671","H01SN2W510","500DVEW060","500DVEW212",
                                "500DVEW312","500DVEW351","500DVEW361","500DVEW431","500DVEW631",
                                "500DVEW801","500DVEW805","500DVEW807","500DVEW811","500DVEW815",
                                "500DVEW818","500DVEW821","500DVEW839","500DVEW981","500TRFW312",
                                "500TRFW351","500TRFW361","500TRFW431","500TRFW475","500TRFW631",
                                "500TRFW801","500TRFW805","500TRFW807","500TRFW811","500TRFW815",
                                "500TRFW818","500TRFW821","500TRFW839","500TRFW981","516TRFW015",
                                "516TRFW021","516TRFW060","516TRFW081","516TRFW108","516TRFW117",
                                "516TRFW122","516TRFW202","516TRFW211","532TRFW015","532TRFW021",
                                "532TRFW060","532TRFW081","532TRFW117","532TRFW122","532TRFW211"};
            /*
            string[] R6VolEq = {"I00FW2W012","I00FW2W017","I00FW2W019","I00FW2W073","I00FW2W093",
                                "I00FW2W108","I00FW2W119","I00FW2W122","I00FW2W202","I00FW2W242",
                                "I00FW2W260","I00FW2W263","I11FW2W012","I11FW2W017","I11FW2W019",
                                "I11FW2W073","I11FW2W093","I11FW2W108","I11FW2W119","I11FW2W122",
                                "I11FW2W202","I11FW2W242","I11FW2W260","I11FW2W263","I12FW2W012",
                                "I12FW2W017","I12FW2W019","I12FW2W073","I12FW2W093","I12FW2W108",
                                "I12FW2W119","I12FW2W122","I12FW2W202","I12FW2W242","I12FW2W260",
                                "I12FW2W263","I13FW2W012","I13FW2W017","I13FW2W019","I13FW2W073",
                                "I13FW2W093","I13FW2W108","I13FW2W119","I13FW2W122","I13FW2W202",
                                "I13FW2W242","I13FW2W260","I13FW2W263","I14FW2W012","I14FW2W017",
                                "I14FW2W019","I14FW2W073","I14FW2W093","I14FW2W108","I14FW2W119",
                                "I14FW2W122","I14FW2W202","I14FW2W242","I14FW2W260","I14FW2W263",
                                "I21FW2W012","I21FW2W017","I21FW2W019","I21FW2W073","I21FW2W093",
                                "I21FW2W108","I21FW2W119","I21FW2W122","I21FW2W202","I21FW2W242",
                                "I21FW2W260","I22FW2W012","I22FW2W017","I22FW2W019","I22FW2W073",
                                "I22FW2W093","I22FW2W108","I22FW2W119","I22FW2W122","I22FW2W202",
                                "I22FW2W242","I22FW2W260","I23FW2W012","I23FW2W017","I23FW2W019",
                                "I23FW2W073","I23FW2W093","I23FW2W108","I23FW2W119","I23FW2W122",
                                "I23FW2W202","I23FW2W242","I23FW2W260","F00FW2W202","F00FW2W242",
                                "F00FW2W263","F01FW2W202","F01FW2W242","F01FW2W263","F02FW2W202",
                                "F02FW2W242","F02FW2W263","F03FW2W202","F03FW2W242","F03FW2W263",
                                "F04FW2W202","F04FW2W242","F04FW2W263","F05FW2W202","F05FW2W242",
                                "F05FW2W263","F06FW2W202","F06FW2W242","F06FW2W263","F07FW2W202",
                                "F07FW2W242","F07FW2W263","F08FW2W202","F08FW2W242","F08FW2W263",
                                "B00BEHW011","B00BEHW015","B00BEHW017","B00BEHW021","B00BEHW022",
                                "B00BEHW041","B00BEHW042","B00BEHW073","B00BEHW081","B00BEHW093",
                                "B00BEHW098","B00BEHW108","B00BEHW116","B00BEHW117","B00BEHW119",
                                "B00BEHW122","B00BEHW211","B00BEHW231","B00BEHW242","B00BEHW260",
                                "B00BEHW312","B00BEHW351","B00BEHW361","B00BEHW431","B00BEHW542",
                                "B00BEHW631","B00BEHW747","B00BEHW800","B00BEHW998","B00BEHW999",
                                "B01BEHW202","B02BEHW202","B03BEHW202","601DVEW015","601DVEW205",
                                "601DVEW263","602DVEW015","602DVEW108","602DVEW122","602DVEW204"};
            */
            string[] R6VolEq = {"I00FW2W012","I00FW2W017","I00FW2W019","I00FW2W073","I00FW2W093",
                                "I00FW2W108","I00FW2W119","I00FW2W122","I00FW2W202","I00FW2W242",
                                "I00FW2W263","I00FW2W264","I11FW2W017","I11FW2W019","I11FW2W093",
                                "I11FW2W122","I11FW2W202","I11FW2W263","I11FW2W264","I12FW2W017",
                                "I12FW2W019","I12FW2W093","I12FW2W122","I12FW2W202","I12FW2W263",
                                "I12FW2W264","I13FW2W017","I13FW2W019","I13FW2W093","I13FW2W122",
                                "I13FW2W202","I13FW2W263","I13FW2W264","F00FW2W202","F00FW2W242",
                                "F00FW2W263","F01FW2W202","F01FW2W242","F01FW2W263","F02FW2W202",
                                "F02FW2W242","F02FW2W263","F03FW2W202","F03FW2W242","F03FW2W263",
                                "F04FW2W202","F04FW2W242","F04FW2W263","F05FW2W202","F05FW2W242",
                                "F05FW2W263","F06FW2W202","F06FW2W242","F06FW2W263","F07FW2W202",
                                "F07FW2W242","F07FW2W263","F08FW2W202","F08FW2W242","F08FW2W263",
                                "I00FW3W012","I00FW3W017","I00FW3W019","I00FW3W073","I00FW3W093",
                                "I00FW3W108","I00FW3W119","I00FW3W122","I00FW3W202","I00FW3W242",
                                "I00FW3W263","I00FW3W264","I11FW3W017","I11FW3W019","I11FW3W093",
                                "I11FW3W122","I11FW3W202","I11FW3W263","I11FW3W264","I12FW3W017",
                                "I12FW3W019","I12FW3W093","I12FW3W122","I12FW3W202","I12FW3W263",
                                "I12FW3W264","I13FW3W017","I13FW3W019","I13FW3W093","I13FW3W122",
                                "I13FW3W202","I13FW3W263","I13FW3W264","F00FW3W202","F00FW3W242",
                                "F00FW3W263","F01FW3W202","F01FW3W242","F01FW3W263","F02FW3W202",
                                "F02FW3W242","F02FW3W263","F03FW3W202","F03FW3W242","F03FW3W263",
                                "F04FW3W202","F04FW3W242","F04FW3W263","F05FW3W202","F05FW3W242",
                                "F05FW3W263","F06FW3W202","F06FW3W242","F06FW3W263","F07FW3W202",
                                "F07FW3W242","F07FW3W263","F08FW3W202","F08FW3W242","F08FW3W263",
                                "616BEHW000","632BEHW000","628BEHW000","B00BEHW011","B00BEHW015",
                                "B00BEHW017","B00BEHW021","B00BEHW022","B00BEHW081","B00BEHW093",
                                "B00BEHW098","B00BEHW108","B00BEHW116","B00BEHW117","B00BEHW119",
                                "B00BEHW122","B00BEHW231","B00BEHW260","B00BEHW263","B00BEHW312",
                                "B00BEHW351","B00BEHW361","B00BEHW431","B00BEHW542","B00BEHW631",
                                "B00BEHW747","B00BEHW800","B00BEHW998","B01BEHW202","B02BEHW202",
                                "B03BEHW202","601DVEW205","601DVEW263","601DVEW015","602DVEW204",
                                "602DVEW015","602DVEW108","600DVEW122","616TRFW019","616TRFW073",
                                "616TRFW094","616TRFW098","616TRFW108","616TRFW122","616TRFW202",
                                "616TRFW242","616TRFW263","616TRFW264","616TRFW351","616TRFW746",
                                "616TRFW747","616TRFW998","632TRFW011","632TRFW073","632TRFW098",
                                "632TRFW108","632TRFW122","632TRFW202","632TRFW242","632TRFW263","632TRFW264"};
            /*
            string[] R10VolEq = {"A00F32W042","A00F32W098","A00F32W242","A00F32W260","A02F32W098",
                                "A02F32W260","A01DEMW000","A16DEMW098","A16CURW260","A32CURW000",
                                "A00DVEW094"};
            */
            string[] R10VolEq = {"A00F32W042","A00F32W098","A00F32W242","A00F32W260","A00F32W263",
                                "A00F32W264","A00FW2W042","A00FW2W098","A00FW2W242","A00FW2W260",
                                "A00FW2W263","A00FW2W264","A02F32W098","A02F32W260","A02F32W263",
                                "A02F32W264","A02FW2W098","A02FW2W260","A02FW2W263","A02FW2W264",
                                "A00F33W042","A00F33W098","A00F33W242","A00F33W260","A00F33W263",
                                "A00F33W264","A00FW3W042","A00FW3W098","A00FW3W242","A00FW3W260",
                                "A00FW3W263","A00FW3W264","A02F33W098","A02F33W260","A02F33W263",
                                "A02F33W264","A02FW3W098","A02FW3W260","A02FW3W263","A02FW3W264",
                                "A01DEMW000","A02DEMW000","A16DEMW042","A16DEMW098","A16DEMW242",
                                "A16DEMW351","A32DEMW042","A32DEMW098","A32DEMW242","A32DEMW351",
                                "A00DVEW094","A00DVEW095","A00DVEW108","A00DVEW310","A00DVEW351",
                                "A00DVEW660","A00DVEW375","A00DVEW746","A00DVEW747","A00DVEW920",
                                "A01DVEW094","A01DVEW095","A01DVEW375","A01DVEW746","A01DVEW920","A01DEVW747"};
            int[] R9VolSp = {12,68,90,94,95,100,105,110,125,129,
                            241,261,299,310,316,317,318,370,371,375,
                            400,407,531,540,541,543,621,740,741,742,
                            743,746,752,760,762,800,802,806,809,823,
                            833,837,950,951,970,972,998};
            int[] R8C1Sp = { 100, 107, 111, 121, 128, 131, 221, 222, 300, 316, 500, 611, 621, 653, 694, 800, 802, 827, 831 };
            int[] R8C2Sp = { 100, 110, 121, 131, 132, 300, 316, 400, 500, 611, 621, 731, 800, 802, 806, 812, 827, 832, 837, 970 };
            int[] R8C3Sp = { 100, 110, 126, 129, 132, 261, 300, 316, 330, 370, 400, 500, 531, 541, 621, 693, 800, 802, 806, 812, 832, 833, 837, 901, 950 };
            int[] R8C4Sp = { 100, 110, 131, 132, 300, 400, 500, 611, 621, 800, 802, 806, 812, 832, 835 };
            int[] R8C5Sp = { 100, 110, 111, 115, 121, 131, 300, 316, 400, 500, 531, 544, 611, 621, 652, 653, 693, 731, 800, 802, 812, 813, 827, 831, 835, 970 };
            int[] R8C6Sp = { 100, 110, 131, 400, 500, 693, 800, 802, 812, 833, 835, 837 };
            int[] R8C7Sp = { 300, 400, 404, 460, 500, 544, 611, 621, 800, 802, 812, 822, 828, 835, 837, 970 };
            int[] R8C9Sp = { 100, 107, 110, 111, 115, 121, 126, 128, 129, 131, 
                               132, 221, 222, 261, 300, 316, 330, 370, 400, 404, 
                               460, 500, 531, 541, 544, 611, 621, 652, 653, 693, 
                               694, 731, 800, 802, 806, 812, 813, 822, 827, 828, 
                               828, 831, 832, 833, 835, 837, 901, 950, 970 };
            int[] R8BeqSp = { 100, 110, 111, 115, 121, 126, 128, 129, 131, 132, 
                                221, 222, 261, 300, 316, 330, 370, 400, 460, 500, 
                                531, 541, 544, 611, 621, 652, 653, 693, 694, 731, 
                                800, 802, 806, 812, 813, 822, 827, 832, 833, 835, 
                                901, 970 };

            string[] volEqAryTmp = new string[47];
            if (regn == 1) volEqAry = R1VolEq;
            else if (regn == 2) volEqAry = R2VolEq;
            else if (regn == 3) volEqAry = R3VolEq;
            else if (regn == 4) volEqAry = R4VolEq;
            else if (regn == 5) volEqAry = R5VolEq;
            else if (regn == 6 || regn == 7 || regn == 11) volEqAry = R6VolEq;
            else if (regn == 8)
            {
                if (checkVolEqnRequiredFields())
                {
                    REGN = int.Parse(regionTB.Text);
                    FORST.Length = 0;
                    FORST.Append(forestTB.Text);
                    DIST.Length = 0;
                    DIST.Append(districtTB.Text);
                    PROD.Length = 0;
                    PROD.Append(prodTB.Text);
                    //SPCODE = int.Parse(speciesTB.Text);                
                    int testSp = 100;
                    string volEqStr1, volEqStr2;
                    int geocode;
                    List<string> list = new List<string>();
                    List<int> listSp = new List<int>();
                    int top79;
                    if (prodTB.Text == "01")
                    {
                        //get the DVE equation first
                        GETVOLEQ3(ref REGN, FORST, DIST, ref testSp, PROD, VOLEQ, ref ERRFLAG, strlen, strlen, strlen, strlen);
                        volEqStr1 = VOLEQ.ToString().Substring(0,7);
                        for (int i = 0; i < R8BeqSp.Length; i++)
                        {
                            list.Add(volEqStr1 + R8BeqSp[i]);
                        }
                        //get Clark sawtimber equation 7/9 equation
                        PROD.Length = 0;
                        PROD.Append("02");
                        GETVOLEQ3(ref REGN, FORST, DIST, ref testSp, PROD, VOLEQ, ref ERRFLAG, strlen, strlen, strlen, strlen);
                        geocode = Convert.ToInt32(VOLEQ.ToString().Substring(1, 1));
                        if (geocode == 1) listSp.AddRange(R8C1Sp);
                        else if (geocode == 2) listSp.AddRange(R8C2Sp);
                        else if (geocode == 3) listSp.AddRange(R8C3Sp);
                        else if (geocode == 4) listSp.AddRange(R8C4Sp);
                        else if (geocode == 5) listSp.AddRange(R8C5Sp);
                        else if (geocode == 6) listSp.AddRange(R8C6Sp);
                        else if (geocode == 7) listSp.AddRange(R8C7Sp);
                        //add HT7/9 equation for the geoarea
                        foreach (int i in listSp)
                        {
                            if (i < 300) top79 = 7;
                            else top79 = 9;
                            list.Add("8" + geocode.ToString() + top79.ToString()+ "CLKE" + i.ToString());
                        }
                        //add HT7/9 equation for geoarea 9
                        for (int i = 0; i < R8C9Sp.Length; i++)
                        {
                            if (R8C9Sp[i] < 300) top79 = 7;
                            else top79 = 9;
                            list.Add("89" + top79.ToString() + "CLKE" + R8C9Sp[i]);
                        }

                    }
                    else if (prodTB.Text == "02")
                    {
                        //get the Clark equation first
                        GETVOLEQ3(ref REGN, FORST, DIST, ref testSp, PROD, VOLEQ, ref ERRFLAG, strlen, strlen, strlen, strlen);
                        geocode = Convert.ToInt32(VOLEQ.ToString().Substring(1, 1));
                        if (geocode == 1) listSp.AddRange(R8C1Sp);
                        else if (geocode == 2) listSp.AddRange(R8C2Sp);
                        else if (geocode == 3) listSp.AddRange(R8C3Sp);
                        else if (geocode == 4) listSp.AddRange(R8C4Sp);
                        else if (geocode == 5) listSp.AddRange(R8C5Sp);
                        else if (geocode == 6) listSp.AddRange(R8C6Sp);
                        else if (geocode == 7) listSp.AddRange(R8C7Sp);
                        //add HT4 equation for the geoarea
                        foreach (int i in listSp)
                        {
                            list.Add("8" + geocode.ToString() + "4CLKE" + i.ToString());
                        }
                        //add HT4 equation for geoarea 9
                        for (int i = 0; i < R8C9Sp.Length; i++)
                        {
                            list.Add("894" + "CLKE" + R8C9Sp[i]);
                        }
                        //add total height equation for the geoarea
                        foreach (int i in listSp)
                        {
                            list.Add("8" + geocode.ToString() + "0CLKE" + i.ToString());
                        }
                        //add total height equation for geoarea 9
                        for (int i = 0; i < R8C9Sp.Length; i++)
                        {
                            list.Add("890" + "CLKE" + R8C9Sp[i]);
                        }

                    }
                    else if (prodTB.Text == "08")
                    {
                        PROD.Length = 0;
                        PROD.Append("02");
                        GETVOLEQ3(ref REGN, FORST, DIST, ref testSp, PROD, VOLEQ, ref ERRFLAG, strlen, strlen, strlen, strlen);
                        geocode = Convert.ToInt32(VOLEQ.ToString().Substring(1, 1));
                        if (geocode == 1) listSp.AddRange(R8C1Sp);
                        else if (geocode == 2) listSp.AddRange(R8C2Sp);
                        else if (geocode == 3) listSp.AddRange(R8C3Sp);
                        else if (geocode == 4) listSp.AddRange(R8C4Sp);
                        else if (geocode == 5) listSp.AddRange(R8C5Sp);
                        else if (geocode == 6) listSp.AddRange(R8C6Sp);
                        else if (geocode == 7) listSp.AddRange(R8C7Sp);
                        //add total height equation for the geoarea
                        foreach (int i in listSp)
                        {
                            list.Add("8" + geocode.ToString() + "8CLKE" + i.ToString());
                        }
                        //add total height equation for geoarea 9
                        for (int i = 0; i < R8C9Sp.Length; i++)
                        {
                            list.Add("898" + "CLKE" + R8C9Sp[i]);
                        }

                    }
                    volEqAry = list.ToArray();
                }
            }
            else if (regn == 9)
            {
                for (int i = 0; i < R9VolSp.Length; i++)
                {
                    volEqAryTmp[i] = "900CLKE" + R9VolSp[i].ToString().PadLeft(3, '0');
                }
                volEqAry = volEqAryTmp;
            }
            else if (regn == 10) volEqAry = R10VolEq;


            return volEqAry;
        }
        //Load volume equation to list based on selected region
        private void loadVolEqList(string regn)
        {
            int Num;
            bool isInt = int.TryParse(regn, out Num);
            if (isInt)
            {
                string[] volEq = VolEqList(Convert.ToInt32(regn));
                volEqTB.Items.Clear();
                for (int i = 0; i < volEq.Length; i++)
                {
                    volEqTB.Items.Add(volEq[i]);
                }
            }
            else
                volEqTB.Items.Clear();
        }

        private void regionTB_SelectedIndexChanged(object sender, EventArgs e)
        {
            loadVolEqList(regionTB.Text);
        }

        private void forestTB_TextChanged(object sender, EventArgs e)
        {
            if (regionTB.Text == "08") loadVolEqList(regionTB.Text);
        }

        private void districtTB_TextChanged(object sender, EventArgs e)
        {
            if (regionTB.Text == "08") loadVolEqList(regionTB.Text);
        }

        private void prodTB_TextChanged(object sender, EventArgs e)
        {
            if (regionTB.Text == "08") loadVolEqList(regionTB.Text);
        }

        private void forestTB_Validating(object sender, CancelEventArgs e)
        {
            if (!isTextInteger(forestTB.Text))
            {
                // Cancel the event and select the text to be corrected by the user.
                e.Cancel = true;
                forestTB.Select(0, forestTB.Text.Length);
                // Set the ErrorProvider error with the text to display.  
                MessageBox.Show("An integer is required for this field.");
            }
        }
        //This is to check if a text item is an integer
        private bool isTextInteger(string text)
        {
            bool isNum;
            int Num;
            isNum = int.TryParse(text, out Num);
            return isNum;
        }

        private void districtTB_Validating(object sender, CancelEventArgs e)
        {
            if (!isTextInteger(districtTB.Text))
            {
                // Cancel the event and select the text to be corrected by the user.
                e.Cancel = true;
                districtTB.Select(0, districtTB.Text.Length);
                // Set the ErrorProvider error with the text to display.  
                MessageBox.Show("An integer is required for this field.");
            }

        }

        private void speciesTB_Validating(object sender, CancelEventArgs e)
        {
            if (!isTextInteger(speciesTB.Text))
            {
                // Cancel the event and select the text to be corrected by the user.
                e.Cancel = true;
                speciesTB.Select(0, speciesTB.Text.Length);
                // Set the ErrorProvider error with the text to display.  
                MessageBox.Show("A valid FIA species number is required for this field.");
            }

        }

        //button to view biomass species defaults and calculted biomass data
        private void button1_Click(object sender, EventArgs e)
        {
            string biomassFile = "\\BiomassInfo.TXT";
            using (var writer = new StreamWriter(exePath + biomassFile))
            {
                writer.WriteLine("Biomass Result".PadLeft(40, ' ') + "(VolLib Ver#:" + verNumTB.Text.ToString() + ")");
                writer.WriteLine(" ");
                writer.WriteLine("Species Regional/Forest Defaults: Region " + REGN.ToString() + " Forest " + FORST + " Species " + SPCD.ToString());
                writer.WriteLine("COMPONENT".PadRight(25, ' ') + "VALUE".PadRight(15, ' ') + "Reference");
                writer.WriteLine("Weightfactor (primary)".PadRight(25, ' ') + WF[0].ToString().PadRight(15, ' ') + WF1REF);
                writer.WriteLine("Weightfactor (secondary)".PadRight(25, ' ') + WF[1].ToString().PadRight(15, ' ') + WF2REF);
                writer.WriteLine("Moisture content (%)".PadRight(25, ' ') + WF[2].ToString().PadRight(15, ' ') + MCREF);
                writer.WriteLine("Above ground total".PadRight(25, ' ') + AGTEQ.ToString().PadRight(15, ' ') + AGTREF);
                writer.WriteLine("Live branches".PadRight(25, ' ') + LBREQ.ToString().PadRight(15, ' ') + LBRREF);
                writer.WriteLine("Dead branches".PadRight(25, ' ') + DBREQ.ToString().PadRight(15, ' ') + DBRREF);
                writer.WriteLine("Foliages".PadRight(25, ' ') + FOLEQ.ToString().PadRight(15, ' ') + FOLREF);
                writer.WriteLine("Stem tip".PadRight(25, ' ') + TIPEQ.ToString().PadRight(15, ' ') + TIPREF);
                writer.WriteLine(" ");
                writer.WriteLine("Green biomass(lb) for tree DBH " + DBHOB.ToString() + " Total HT " + HTTOT.ToString());
                writer.WriteLine("COMPONENT".PadRight(25, ' ') + "Biomass(lb)");
                writer.WriteLine("Above ground total".PadRight(25, ' ') + BMS[0].ToString());
                writer.WriteLine("Primary prod".PadRight(25, ' ') + BMS[4].ToString());
                writer.WriteLine("Secondary prod".PadRight(25, ' ') + BMS[5].ToString());
                writer.WriteLine("Live branches".PadRight(25, ' ') + BMS[1].ToString());
                writer.WriteLine("Dead branches".PadRight(25, ' ') + BMS[2].ToString());
                writer.WriteLine("Foliages".PadRight(25, ' ') + BMS[3].ToString());
                writer.WriteLine("Stem tip".PadRight(25, ' ') + BMS[6].ToString());
                writer.WriteLine(" ");
                writer.WriteLine("Brown Crown Fraction:");
                writer.WriteLine("Needles: " + CFWT[0].ToString());
                writer.WriteLine("0 to 1/4: " + CFWT[1].ToString());
                writer.WriteLine("1/4 to 1: " + CFWT[2].ToString());
                writer.WriteLine("1 to 3: " + CFWT[3].ToString());
                writer.WriteLine("3 and up: " + CFWT[4].ToString());
                writer.WriteLine("Live crown: " + CFWT[5].ToString());
                writer.WriteLine("Dead crown: " + CFWT[6].ToString());
                writer.WriteLine(" ");
                writer.WriteLine("Brown Topwood: " + TopWt.ToString());
                writer.WriteLine("Brown CullLog: " + CullLogWt.ToString());
                writer.WriteLine("Brown CullChunk: " + CullChunkWt.ToString());
            }
            System.Diagnostics.Process.Start(exePath + biomassFile);
        }

        private void btnGetHT_Click(object sender, EventArgs e)
        {
            
            STEMDIB = 0.0F;
            STEMHT = 0.0F;
            if (txStemDIB.TextLength > 0)
                STEMDIB = float.Parse(txStemDIB.Text);

            //make sure all required fields are populated
            if (!checkFields())
                return;

            //initialize variables for volume library dll call
            getVolPrep();
            //PMTFLG = 1;

            if (VOLEQ.Length < 10)
            {
                GETVOLEQ3(ref REGN, FORST, DIST, ref SPCODE, PROD, VOLEQ, ref ERRFLAG, strlen, strlen, strlen, strlen);
                volEqTB.Text = VOLEQ.ToString();
            }
            //check for profile model volume equation number
            if (!checkVolEq())
            {
                MessageBox.Show("cannot return height to the dib for this volume equation");
                return;
            }
            if (MTOPP > 0)
            {
                UPSD1 = MTOPP;
                UPSHT1 = HT1PRD;
            }
            if (MTOPS > 0)
            {
                UPSD2 = MTOPS;
                UPSHT2 = HT2PRD;
            }
            HT2TOPDCS(ref REGN, FORST, VOLEQ, ref DBHOB, ref HTTOT, ref HT1PRD, ref HT2PRD, ref UPSHT1, ref UPSHT2, ref UPSD1, ref UPSD2,
                ref AVGZ1, ref AVGZ2, ref HTREF, ref DBTBH, ref BTR, ref FCLASS, ref STEMDIB, ref STEMHT, ref ERRFLAG, strlen, strlen);

            //volEqTB.Text = VOLEQ.ToString();
            txStemHT.Text = STEMHT.ToString();
        }

    }
               
       
}