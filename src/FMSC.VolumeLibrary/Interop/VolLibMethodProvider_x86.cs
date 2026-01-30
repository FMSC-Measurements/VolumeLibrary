using System.Runtime.InteropServices;
using System.Text;
using FMSC.VolumeLibrary.Logging;
using Microsoft.Extensions.Logging;

namespace FMSC.VolumeLibrary.Interop
{
    public class VolLibMethodProvider_x86 : IVolLibMethodProvider
    {
        ILogger Logger { get; } = LoggerResolver.CreateLogger<VolLibMethodProvider_x86>();

        VolumeLibraryNativeMethods IVolLibMethodProvider.GetNativeMethods()
        {
            Logger.LogInformation("Resolving Native Methods");
            return VolLibMethodProvider_x64.GetNativeMethods();
        }

        private const string DLL_NAME = "win-x86\\vollib.dll";

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        public static extern void VERNUM2(out int a);

        [DllImport(DLL_NAME)]//, CallingConvention = CallingConvention.StdCall)]
        private static extern void GETVOLEQ3(ref int region, StringBuilder forest, StringBuilder district, ref int spec, StringBuilder prod, StringBuilder voleq, out int err, int l1, int l2, int l3, int l4);

        [DllImport(DLL_NAME)]//, CallingConvention = CallingConvention.StdCall)]
        private static extern void GETNVBEQ(ref int region, StringBuilder forest, StringBuilder district, ref int spec, StringBuilder voleq, out int err, int l1, int l2, int l4);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern void VOLLIBCSNVB(ref int regn,
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

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        public static extern void CRZBIOMASSCS(ref int regn,
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

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern void CRZSPDFTCS(ref int regn, StringBuilder forst, ref int spcd, float[] wf, StringBuilder agteq, StringBuilder lbreq,
    StringBuilder dbreq, StringBuilder foleq, StringBuilder tipeq, StringBuilder wf1ref, StringBuilder wf2ref, StringBuilder mcref,
    StringBuilder agtref, StringBuilder lbrref, StringBuilder dbrref, StringBuilder folref, StringBuilder tipref,
    int i1, int i2, int i3, int i4, int i5, int i6, int i7, int i8, int i9, int i10, int i11, int i12, int i13, int i14);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern void GETREGNWFCS(ref int regin, StringBuilder forest, ref int fiaCode, StringBuilder prod, out float greenWf, out float deadWf, int i1, int i2);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern void BROWNCROWNFRACTION(ref int SPCD, ref float DBH, ref float THT, ref float CR, float[] CFWT);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern void BROWNTOPWOOD(ref int SPN, ref float GCUFTS, ref float WT);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern void BROWNCULLLOG(ref int SPN, ref float GCUFTS, ref float WT);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern void BROWNCULLCHUNK(ref int SPN, ref float GCUFT, ref float NCUFT, ref float FLIW, ref float WT);

        [DllImport(DLL_NAME, CallingConvention = CallingConvention.Cdecl)]
        private static extern void MRULESCS(ref int regn, StringBuilder voleq, StringBuilder prod, out float trim,
                                    out float minlen, out float maxlen, out int opt, out float merchl,
                                    int l1, int l2);

        private static VolumeLibraryNativeMethods _nativeMethods;
        public static VolumeLibraryNativeMethods GetNativeMethods()
        {
            return _nativeMethods ??= new VolumeLibraryNativeMethods()
            {
                BROWNCROWNFRACTION = BROWNCROWNFRACTION,
                BROWNCULLCHUNK = BROWNCULLCHUNK,
                BROWNCULLLOG = BROWNCULLLOG,
                BROWNTOPWOOD = BROWNTOPWOOD,
                CRZBIOMASSCS = CRZBIOMASSCS,
                CRZSPDFTCS = CRZSPDFTCS,
                GETNVBEQ = GETNVBEQ,
                GETREGNWFCS = GETREGNWFCS,
                GETVOLEQ3 = GETVOLEQ3,
                MRULESCS = MRULESCS,
                VERNUM2 = VERNUM2,
                VOLLIBCSNVB = VOLLIBCSNVB,
            };
        }
    }
}