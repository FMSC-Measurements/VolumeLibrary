namespace FMSC.VolumeLibrary
{
    public interface IVolumeLibrary
    {
        int GetVersionNumber();

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
            double cr, double cull, int decaycd);

        VolLibNVBoutput CalculateVolumeNVB(
            int regn, string forst, string voleq, float mtopp, float mtops,
            float stump, float dbhob, float drcob, string httype, float httot,
            int htlog, float ht1prd, float ht2prd, float upsht1, float upsht2,
            float upsd1, float upsd2, int htref, float avgz1, float avgz2,
            int fclass, float dbtbh, float btr, int cutflg, int bfpflg, int cupflg, int cdpflg,
            int spflg, string conspec, string prod, int httfll, string live,
            int ba, int si, string ctype, int pmtflg,
            MRules mRules, int idist,
            float brkht, float brkhtd, int fiaspcd,
            float cr, float cull, int decaycd);

        void CalculateVolumeNVB(
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
            float cr, float cull, int decaycd);

        CrzBiomassResult CalculateBiomass(
            int regn, string forst, int spcd, float dbhob, float drcob,
            float httot, int fclass, float[] vol, float[] wf,
            out int errflg, string prod);

        string LookupVolumeEquation(int region, string forest, string district, int fiaCode, string product, out int error);

        string LookupVolumeEquationNVB(int region, string forest, string district, int fiaCode, out int error);

        float[] LookupWeightFactorsCRZSPDFT(int region, string forest, string product, int fiaCode);

        float[] LookupWeightFactorsCRZSPDFTRaw(int region, string forest, int fiaCode);

        void LookupWeightFactorsNVB(int regin, string forest, int fiaCode, string prod, out float greenWf, out float deadWf);

        float LookupWeightFactorsNVB(int regin, string forest, int fiaCode, string prod, string liveDead);

        [Obsolete]
        void BrownCrownFraction(int fiaCode, float DBH, float THT, float CR, float[] crownFractionWGT);

        CrownFractionWeight BrownCrownFraction(int fiaCode, float DBH, float THT, float CR);

        void BrownTopwood(int fiaCode, float grsVol, out float topwoodWGT);

        void BrownCullLog(int fiaCode, float GCUFTS, out float cullLogWGT);

        void BrownCullChunk(int fiaCode, float GCUFT, float NCUFT, float FLIW, out float cullChunkWGT);

        MRules GetMRules(int region, string volEq, string product);
    }
}