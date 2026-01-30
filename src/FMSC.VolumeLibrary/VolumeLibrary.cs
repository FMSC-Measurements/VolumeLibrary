using FMSC.VolumeLibrary.Interop;
using FMSC.VolumeLibrary.Logging;
using Microsoft.Extensions.Logging;
using System.Text;

namespace FMSC.VolumeLibrary
{
    public class VolumeLibrary : IVolumeLibrary
    {
        private static readonly IReadOnlyCollection<int> R5_Prod20_WF_FIAcodes = new int[] { 122, 116, 117, 015, 020, 202, 081, 108 };
        public static int CROWN_FACTOR_WEIGHT_ARRAY_LENGTH = 7;

        public const int CRZSPDFTCS_STRINGLENGTH = 256;
        public const int STRING_BUFFER_SIZE = 256;
        public const int CHARLEN = 1;

        public const int VOLLIBNVB_VOL_SIZE = 15;
        public const int VOLLIBNVB_LOGLEN_SIZE = 20;
        public const int VOLLIBNVB_BOLHT_SIZE = 21;
        public const int VOLLIBNVB_LOGVOL_SIZE_X = 20;
        public const int VOLLIBNVB_LOGVOL_SIZE_Y = 7;
        public const int VOLLIBNVB_LOGDIA_SIZE_X = 3;
        public const int VOLLIBNVB_LOGDIA_SIZE_Y = 21;

        public const int VOLLIBNVB_BIO_SIZE = 15;
        public const int DRYBIO_ARRAY_SIZE = VOLLIBNVB_BIO_SIZE;
        public const int GRNBIO_ARRAY_SIZE = VOLLIBNVB_BIO_SIZE;

        public const int I3 = 3;
        public const int I7 = 7;
        public const int I15 = 15;
        public const int I20 = 20;
        public const int I21 = 21;

        public const int CRZBIOMASSCS_BMS_SIZE = 8;


        protected ILogger Log { get; }
        VolumeLibraryNativeMethods NativeMethods { get; }

        public VolumeLibrary(ILogger<VolumeLibrary> logger = null) : 
            this(ResolveNativeMethods(), logger)
        {

        }

        public VolumeLibrary(VolumeLibraryNativeMethods nativeMethods, ILogger<VolumeLibrary> logger = null)
        {
            Log = logger ?? LoggerResolver.CreateLogger<VolumeLibrary>();
            NativeMethods = nativeMethods ;
        }

        private static VolumeLibraryNativeMethods ResolveNativeMethods()
        {
#if NET8_0_OR_GREATER
            var nativeProvider = new VolLibMethodProvider_netcore();
            return nativeProvider.GetNativeMethods();
#else

            if (Environment.Is64BitProcess)
            {
                return VolLibMethodProvider_x64.GetNativeMethods();
            }
            else
            {
                return VolLibMethodProvider_x86.GetNativeMethods();
            }
#endif
        }

        public float[] LookupWeightFactorsCRZSPDFT(int region, string forest, string product, int fiaCode)
        {
            var wf = LookupWeightFactorsCRZSPDFTRaw(region, forest, fiaCode);

            if (wf[1] > 0)
            {
                if ((region == 5 && product == "20" && R5_Prod20_WF_FIAcodes.Any(x => x == fiaCode))
                   || (region == 1 && product != "01"))
                {
                    wf[0] = wf[1];
                }
            }

            return wf;
        }

        public int GetVersionNumber()
        {
            NativeMethods.VERNUM2(out var i);
            return i;
        }

        public VolLibNVBoutput CalculateVolumeNVB(
                int regn, int forst, string voleq, double mtopp, double mtops,
                double stump, double dbhob, double drcob, string httype, double httot,
                int htlog, double ht1prd, double ht2prd, double upsht1, double upsht2,
                double upsd1, double upsd2, int htref, double avgz1, double avgz2,
                int fclass, double dbtbh, double btr, int cutflg, int bfpflg, int cupflg, int cdpflg,
                int spflg, string conspec, string prod, int httfll, string live,
                int ba, int si, string ctype, int pmtflg,
                MRules mRules, int idist,
                double brkht, double brkhtd, int fiaspcd,
                double cr, double cull, int decaycd)
        {
            var sForest = forst.ToString().PadLeft(2, '0');

            return CalculateVolumeNVB(regn, sForest, voleq, (float)mtopp, (float)mtops,
                                (float)stump, (float)dbhob, (float)drcob, httype, (float)httot,
                                htlog, (float)ht1prd, (float)ht2prd, (float)upsht1, (float)upsht2,
                                (float)upsd1, (float)upsd2, htref, (float)avgz1, (float)avgz2,
                                fclass, (float)dbtbh, (float)btr, cutflg, bfpflg, cupflg, cdpflg,
                                spflg, conspec, prod, httfll, live,
                                ba, si, ctype, pmtflg,
                                mRules, idist,
                                (float)brkht, (float)brkhtd, fiaspcd,
                                (float)cr, (float)cull, decaycd);
        }

        public VolLibNVBoutput CalculateVolumeNVB(
                int regn, string forst, string voleq, float mtopp, float mtops,
                float stump, float dbhob, float drcob, string httype, float httot,
                int htlog, float ht1prd, float ht2prd, float upsht1, float upsht2,
                float upsd1, float upsd2, int htref, float avgz1, float avgz2,
                int fclass, float dbtbh, float btr, int cutflg, int bfpflg, int cupflg, int cdpflg,
                int spflg, string conspec, string prod, int httfll, string live,
                int ba, int si, string ctype, int pmtflg,
                MRules mRules, int idist,
                float brkht, float brkhtd, int fiaspcd,
                float cr, float cull, int decaycd)
        {
            CalculateVolumeNVB(regn, forst, voleq, mtopp, mtops,
                                stump, dbhob, drcob, httype, httot,
                                htlog, ht1prd, ht2prd, upsht1, upsht2,
                                upsd1, upsd2, htref, avgz1, avgz2,
                                fclass, dbtbh, btr, out var vol, out var logvol,
                                out var logdia, out var loglen, out var bolht, out var tlogs, out var nologp,
                                out var nologs, cutflg, bfpflg, cupflg, cdpflg,
                                spflg, conspec, prod, httfll, live,
                                ba, si, ctype, out var errflg, pmtflg,
                                mRules, idist,
                                brkht, brkhtd, fiaspcd, out var drybio, out var grnbio,
                                cr, cull, decaycd);

            var volumes = new Volumes(vol);
            var logVolumes = new LogVolume[VOLLIBNVB_LOGVOL_SIZE_X];
            for (var i = 0; i < VOLLIBNVB_LOGVOL_SIZE_X; i++)
            {
                logVolumes[i] = new LogVolume().FromArray(logvol, i);
            }

            var logDiameters = new LogDiameter[VOLLIBNVB_LOGDIA_SIZE_Y];
            for (var i = 0; i < VOLLIBNVB_LOGDIA_SIZE_Y; i++)
            {
                logDiameters[i] = new LogDiameter().FromArray(logdia, i);
            }

            var greenBio = new VolLibNVBCalculatedBiomass().FromArray(grnbio);
            var dryBio = new VolLibNVBCalculatedBiomass().FromArray(drybio);

            var output = new VolLibNVBoutput
            {
                Volumes = volumes,
                LogVolumes = logVolumes,
                LogDiameters = logDiameters,
                LogLengths = loglen,
                BottomOfLogHeights = bolht,
                TotalLogs = tlogs,
                NoLogsPrimary = nologp,
                NoLogsSecondary = nologs,
                GreenBio = greenBio,
                DryBio = dryBio,
                ErrorCode = errflg,
            };

            return output;
        }

        public void CalculateVolumeNVB(
            int regn, string forst, string voleq, float mtopp, float mtops,
            float stump, float dbhob, float drcob, string httype, float httot,
            int htlog, float ht1prd, float ht2prd, float upsht1, float upsht2,
            float upsd1, float upsd2, int htref, float avgz1, float avgz2,
            int fclass, float dbtbh, float btr, out float[] vol, out float[,] logvol,
            out float[,] logdia, out float[] loglen, out float[] bolht, out int tlogs, out float nologp,
            out float nologs, int cutflg, int bfpflg, int cupflg, int cdpflg,
            int spflg, string conspec, string prod, int httfll, string live,
            int ba, int si, string ctype, out int errflg, int pmtflg,
            MRules mRules, int idist,
            float brkht, float brkhtd, int fiaspcd, out float[] drybio, out float[] grnbio,
            float cr, float cull, int decaycd)
        {
            tlogs = 0;
            nologp = 0f;
            nologs = 0f;
            errflg = 0;

            vol = new float[VOLLIBNVB_VOL_SIZE];
            logvol = new float[VOLLIBNVB_LOGVOL_SIZE_X, VOLLIBNVB_LOGVOL_SIZE_Y];
            logdia = new float[VOLLIBNVB_LOGDIA_SIZE_X, VOLLIBNVB_LOGDIA_SIZE_Y];
            loglen = new float[VOLLIBNVB_LOGLEN_SIZE];
            bolht = new float[VOLLIBNVB_BOLHT_SIZE];
            drybio = new float[DRYBIO_ARRAY_SIZE];
            grnbio = new float[GRNBIO_ARRAY_SIZE];

            StringBuilder FORST = new StringBuilder(STRING_BUFFER_SIZE).Append(forst);
            StringBuilder VOLEQ = new StringBuilder(STRING_BUFFER_SIZE).Append(voleq);
            StringBuilder CTYPE = new StringBuilder(STRING_BUFFER_SIZE).Append(ctype);

            StringBuilder CONSPEC = new StringBuilder(STRING_BUFFER_SIZE).Append(conspec);
            StringBuilder PROD = new StringBuilder(STRING_BUFFER_SIZE).Append(prod);
            StringBuilder LIVE = new StringBuilder(STRING_BUFFER_SIZE).Append(live);
            StringBuilder HTTYPE = new StringBuilder(STRING_BUFFER_SIZE).Append(httype);

            Log?.LogTrace("VOLLIBCSNVB Prams \r\n" +
                "{regn}, {forst}, {voleq}, {mtopp}, {mtops}, " +
                "{stump}, {dbhob}, {drcob}, {httype}, {httot}, " +
                "{htlog}, {ht1prd}, {ht2prd}, {upsht1}, {upsht2}, " +
                "{upsd1}, {upsd2}, {htref}, {avgz1}, {avgz2}, " +
                "{fclass}, {dbtbh}, {btr}, " +
                "{cutflg}, {bfpflg}, {cupflg}, {cdpflg}, " +
                "{spflg}, {conspec}, {prod}, {httfll}, {live}, " +
                "{ba}, {si}, {ctype}, {pmtflg}, " +
                //"{mRules}, {idist}, " +
                "{brkht}, {brkhtd}, {fiaspcd}, " +
                "{cr}, {cull}, {decaycd}",
                regn, forst, voleq, mtopp, mtops,
                stump, dbhob, drcob, httype, httot,
                htlog, ht1prd, ht2prd, upsht1, upsht2,
                upsd1, upsd2, htref, avgz1, avgz2,
                fclass, dbtbh, btr,
                cutflg, bfpflg, cupflg, cdpflg,
                spflg, conspec, prod, httfll, live,
                ba, si, ctype, pmtflg,
                //mRules, idist,
                brkht, brkhtd, fiaspcd,
                cr, cull, decaycd);

            NativeMethods.VOLLIBCSNVB(ref regn, FORST, VOLEQ, ref mtopp, ref mtops,
               ref stump, ref dbhob, ref drcob, HTTYPE, ref httot,
               ref htlog, ref ht1prd, ref ht2prd, ref upsht1, ref upsht2,
               ref upsd1, ref upsd2, ref htref, ref avgz1, ref avgz2,
               ref fclass, ref dbtbh, ref btr, vol, logvol,
               logdia, loglen, bolht, ref tlogs, ref nologp,
               ref nologs, ref cutflg, ref bfpflg, ref cupflg, ref cdpflg,
               ref spflg, CONSPEC, PROD, ref httfll, LIVE,
               ref ba, ref si, CTYPE, ref errflg, ref pmtflg,
               ref mRules, ref idist,
               ref brkht, ref brkhtd, ref fiaspcd, drybio, grnbio,
               ref cr, ref cull, ref decaycd,
               STRING_BUFFER_SIZE, STRING_BUFFER_SIZE, STRING_BUFFER_SIZE, STRING_BUFFER_SIZE,
                    STRING_BUFFER_SIZE, STRING_BUFFER_SIZE, STRING_BUFFER_SIZE, CHARLEN);

            Log?.LogTrace("VOLLIBCSNVB Calculated Volumes\r\n{Volumes}", string.Join(", ", vol));

            if (errflg > 0)
            {
                Log?.LogInformation($"VOLLIBCSNVB Error Flag {errflg} - " + ErrorCodeHelper.GetErrorMessage(errflg));
            }
        }




        public CrzBiomassResult CalculateBiomass(int regn, string forst, int spcd, float dbhob, float drcob, float httot, int fclass, float[] vol, float[] wf, out int errflg, string prod)
        {
            var calculatedBiomass = new float[CRZBIOMASSCS_BMS_SIZE];
            StringBuilder FORST = new StringBuilder(STRING_BUFFER_SIZE).Append(forst);
            StringBuilder PROD = new StringBuilder(STRING_BUFFER_SIZE).Append(prod);
            errflg = 0;

            NativeMethods.CRZBIOMASSCS(ref regn, FORST, ref spcd, ref dbhob, ref drcob, ref httot, ref fclass, vol, wf,
                                    calculatedBiomass, ref errflg, PROD, STRING_BUFFER_SIZE, STRING_BUFFER_SIZE);

            if (errflg > 0)
            {
                Log?.LogInformation($"CRZBIOMASSCS Error Flag {errflg} - " + ErrorCodeHelper.GetErrorMessage(errflg));
            }

            return new CrzBiomassResult().FromArray(calculatedBiomass);
        }

        public string LookupVolumeEquation(int region, string forest, string district, int fiaCode, string product, out int error)
        {
            var sbForest = new StringBuilder(STRING_BUFFER_SIZE).Append(forest);
            var sbDistrict = new StringBuilder(STRING_BUFFER_SIZE).Append(district);
            var sbProd = new StringBuilder(STRING_BUFFER_SIZE).Append(product);

            var sbVolEq = new StringBuilder(STRING_BUFFER_SIZE);

            NativeMethods.GETVOLEQ3(ref region, sbForest, sbDistrict, ref fiaCode, sbProd, sbVolEq, out error,
                STRING_BUFFER_SIZE, STRING_BUFFER_SIZE, STRING_BUFFER_SIZE, STRING_BUFFER_SIZE);

            return sbVolEq.ToString();
        }


        public string LookupVolumeEquationNVB(int region, string forest, string district, int fiaCode, out int error)
        {
            var sbForest = new StringBuilder(STRING_BUFFER_SIZE).Append(forest);
            var sbDistrict = new StringBuilder(STRING_BUFFER_SIZE).Append(district);

            var sbVolEq = new StringBuilder(STRING_BUFFER_SIZE);

            NativeMethods.GETNVBEQ(ref region, sbForest, sbDistrict, ref fiaCode, sbVolEq, out error,
                STRING_BUFFER_SIZE, STRING_BUFFER_SIZE, STRING_BUFFER_SIZE);

            return sbVolEq.ToString();
        }

        public float[] LookupWeightFactorsCRZSPDFTRaw(int region, string forest, int fiaCode)
        {
            float[] WF = new float[3];
            var FORST = new StringBuilder(CRZSPDFTCS_STRINGLENGTH).Append(forest);
            var AGTEQ = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var LBREQ = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var DBREQ = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var FOLEQ = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var TIPEQ = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var WF1REF = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var WF2REF = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var MCREF = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var AGTREF = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var LBRREF = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var DBRREF = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var FOLREF = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            var TIPREF = new StringBuilder(CRZSPDFTCS_STRINGLENGTH);
            int REGN = region;
            int SPCD = fiaCode;
            NativeMethods.CRZSPDFTCS(ref REGN,
                       FORST,
                       ref fiaCode,
                       WF,
                       AGTEQ,
                       LBREQ,
                       DBREQ,
                       FOLEQ,
                       TIPEQ,
                       WF1REF,
                       WF2REF,
                       MCREF,
                       AGTREF,
                       LBRREF,
                       DBRREF,
                       FOLREF,
                       TIPREF,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH,
                       CRZSPDFTCS_STRINGLENGTH);
            // note: fiaCode is both an input and an output variable

            return WF;
        }

        public void LookupWeightFactorsNVB(int regin, string forest, int fiaCode, string prod, out float greenWf, out float deadWf)
        {
            var FORST = new StringBuilder(STRING_BUFFER_SIZE).Append(forest);
            var PROD = new StringBuilder(STRING_BUFFER_SIZE).Append(prod);

            NativeMethods.GETREGNWFCS(ref regin, FORST, ref fiaCode, PROD, out greenWf, out deadWf, STRING_BUFFER_SIZE, STRING_BUFFER_SIZE);
        }

        public float LookupWeightFactorsNVB(int regin, string forest, int fiaCode, string prod, string liveDead)
        {
            LookupWeightFactorsNVB(regin, forest, fiaCode, prod, out float greenWf, out float deadWf);
            return liveDead.Trim().ToUpper() switch
            {
                "L" => greenWf,
                "D" => deadWf,
                _ => throw new ArgumentException("Invalid Tree Live Dead Value", nameof(liveDead))
            };
        }

        public void BrownCrownFraction(int fiaCode, float DBH, float THT, float CR, float[] crownFractionWGT)
        {
            NativeMethods.BROWNCROWNFRACTION(ref fiaCode, ref DBH, ref THT, ref CR, crownFractionWGT);
        }

        public CrownFractionWeight BrownCrownFraction(int fiaCode, float DBH, float THT, float CR)
        {
            var crownFractionWGT = new float[CROWN_FACTOR_WEIGHT_ARRAY_LENGTH];
            NativeMethods.BROWNCROWNFRACTION(ref fiaCode, ref DBH, ref THT, ref CR, crownFractionWGT);
            return new CrownFractionWeight().FromArray(crownFractionWGT);
        }

        public void BrownTopwood(int fiaCode, float grsVol, out float topwoodWGT)
        {
            topwoodWGT = 0;
            NativeMethods.BROWNTOPWOOD(ref fiaCode, ref grsVol, ref topwoodWGT);
        }

        public void BrownCullLog(int fiaCode, float GCUFTS, out float cullLogWGT)
        {
            cullLogWGT = 0;
            NativeMethods.BROWNCULLLOG(ref fiaCode, ref GCUFTS, ref cullLogWGT);
        }

        public void BrownCullChunk(int fiaCode, float GCUFT, float NCUFT, float FLIW, out float cullChunkWGT)
        {
            cullChunkWGT = 0;
            NativeMethods.BROWNCULLCHUNK(ref fiaCode, ref GCUFT, ref NCUFT, ref FLIW, ref cullChunkWGT);
        }

        public MRules GetMRules(int region, string volEq, string product)
        {
            StringBuilder VOLEQ = new StringBuilder(STRING_BUFFER_SIZE).Append(volEq);
            StringBuilder PROD = new StringBuilder(STRING_BUFFER_SIZE).Append(product);

            NativeMethods.MRULESCS(ref region, VOLEQ, PROD, out float trim,
                                    out float minlen, out float maxlen, out int opt, out float merchl,
                                    STRING_BUFFER_SIZE, STRING_BUFFER_SIZE);

            return new MRules()
            {
                trim = trim,
                minlen = minlen,
                maxlen = maxlen,
                opt = opt,
                merchl = merchl
            };
        }
    }
}
