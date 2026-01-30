using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FMSC.VolumeLibrary.Interop
{
    public class VolumeLibraryDelegateTypes
    {
        //TODO do we need to define calling convention?
        // apparently the default calling convention for fortran is cdecl, see: https://www.cita.utoronto.ca/~merz/intel_f10b/main_for/mergedProjects/bldaps_for/common/bldaps_call_cproc.htm
        // to set the calling convention use the UnmanagedFunctionPointer attribute see: https://learn.microsoft.com/en-us/dotnet/api/system.runtime.interopservices.unmanagedfunctionpointerattribute?view=net-10.0
        // however there might be a method of casting a delegate pointer that might be more useful see: https://developers.redhat.com/blog/2021/04/27/some-more-c-9#


        public delegate void VERNUM2(out int a);

        public delegate void GETVOLEQ3(ref int region, StringBuilder forest, StringBuilder district,
            ref int spec, StringBuilder prod, StringBuilder voleq, out int err,
            int l1, int l2, int l3, int l4);

        public delegate void GETNVBEQ(ref int region, StringBuilder forest, StringBuilder district, ref int spec, StringBuilder voleq,
            out int err, int l1, int l2, int l4);

        public delegate void VOLLIBCSNVB(ref int regn,
                    StringBuilder forst,
                    StringBuilder voleq,
                    ref float mtopp,
                    ref float mtops,

                    ref float stump,
                    ref float dbhob,
                    ref float drcob,
                    StringBuilder httype,
                    ref float httot,

                    ref int htlog,
                    ref float ht1prd,
                    ref float ht2prd,
                    ref float upsht1,
                    ref float upsht2,

                    ref float upsd1,
                    ref float upsd2,
                    ref int htref,
                    ref float avgz1,
                    ref float avgz2,

                    ref int fclass,
                    ref float dbtbh,
                    ref float btr,
                    float[] vol,
                    float[,] logvol,

                    float[,] logdia,
                    float[] loglen,
                    float[] bohlt,
                    ref int tlogs,
                    ref float nologp,

                    ref float nologs,
                    ref int cutflg,
                    ref int bfpflg,
                    ref int cupflg,
                    ref int cdpflg,

                    ref int spflg,
                    StringBuilder conspec,
                    StringBuilder prod,
                    ref int httfll,
                    StringBuilder live,

                    ref int ba,
                    ref int si,
                    StringBuilder ctype,
                    ref int errflg,
                    ref int pmtflg,

                    ref MRules mRules,
                    ref int dist,

                    ref float brkht,
                    ref float brkhtd,
                    ref int fiaspcd,
                    float[] drybio,
                    float[] grnbio,

                    ref float cr,
                    ref float cull,
                    ref int decaycd,

                    int ll1,
                    int ll2,
                    int ll3,
                    int ll4,
                    int ll5,
                    int ll6,
                    int ll7,
                    int charLen);

        public delegate void CRZBIOMASSCS(ref int regn,
                    StringBuilder forst,
                    ref int spcd,
                    ref float dbhob,
                    ref float drcob,
                    ref float httot,
                    ref int fclass,
                    float[] vol,
                    float[] wf,
                    float[] bms,
                    ref int errflg,
                    StringBuilder prod,
                    int i1,
                    int i2);

        public delegate void CRZSPDFTCS(ref int regn, StringBuilder forst, ref int spcd, float[] wf, StringBuilder agteq, StringBuilder lbreq,
    StringBuilder dbreq, StringBuilder foleq, StringBuilder tipeq, StringBuilder wf1ref, StringBuilder wf2ref, StringBuilder mcref,
    StringBuilder agtref, StringBuilder lbrref, StringBuilder dbrref, StringBuilder folref, StringBuilder tipref,
    int i1, int i2, int i3, int i4, int i5, int i6, int i7, int i8, int i9, int i10, int i11, int i12, int i13, int i14);

        public delegate void GETREGNWFCS(ref int regin, StringBuilder forest, ref int fiaCode, StringBuilder prod, out float greenWf, out float deadWf, int i1, int i2);

        public delegate void BROWNCROWNFRACTION(ref int SPCD, ref float DBH, ref float THT, ref float CR, float[] CFWT);

        public delegate void BROWNTOPWOOD(ref int SPN, ref float GCUFTS, ref float WT);

        public delegate void BROWNCULLLOG(ref int SPN, ref float GCUFTS, ref float WT);

        public delegate void BROWNCULLCHUNK(ref int SPN, ref float GCUFT, ref float NCUFT, ref float FLIW, ref float WT);

        public delegate void MRULESCS(ref int regn, StringBuilder voleq, StringBuilder prod, out float trim,
                                    out float minlen, out float maxlen, out int opt, out float merchl,
                                    int l1, int l2);
    }
}
