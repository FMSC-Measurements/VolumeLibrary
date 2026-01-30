namespace FMSC.VolumeLibrary
{
    public struct MRules
    {
#pragma warning disable S1104 // Fields should not have public accessibility
#pragma warning disable VSSpell001 // Spell Check
        public int evod;
        public int opt;
        public float maxlen;
        public float minlen;
        public float minlent;
        public float merchl;
        public float mtopp;
        public float mtops;
        public float stump;
        public float trim;
        public float btr;
        public float dbhtbh;
        public float minbfd;
        public char cor;
#pragma warning restore VSSpell001 // Spell Check
#pragma warning restore S1104 // Fields should not have public accessibility

        public MRules(int evod, int op)
        {
            this.evod = evod;
            opt = op;
            maxlen = 0;
            minlen = 0;
            merchl = 0;
            mtopp = 0;
            mtops = 0;
            stump = 0;
            trim = 0;

            cor = 'Y';
            btr = default;
            dbhtbh = default;
            minbfd = default;
            minlent = default;
        }

        //public MRules(VolumeEquationDO ved)
        //{
        //    evod = (int)ved.EvenOddSegment;
        //    opt = (int)ved.SegmentationLogic;
        //    maxlen = ved.MaxLogLengthPrimary;
        //    minlen = ved.MinLogLengthPrimary;
        //    merchl = ved.MinMerchLength;
        //    mtopp = ved.TopDIBPrimary;
        //    mtops = ved.TopDIBSecondary;
        //    stump = ved.StumpHeight;
        //    trim = ved.Trim;

        //    cor = 'Y';
        //    btr = default;
        //    dbhtbh = default;
        //    minbfd = default;
        //    minlent = default;
        //}

        public MRules(int ev, float mxln, float mnln, float merln, float mtpp, float mtps, int op, float stmp, float trm)
        {
            evod = ev;
            maxlen = mxln;
            minlen = mnln;
            merchl = merln;
            mtopp = mtpp;
            mtops = mtps;
            opt = op;
            stump = stmp;
            trim = trm;

            cor = 'Y';
            minlent = 0;
            btr = 0;
            dbhtbh = 0;
            minbfd = 0;
        }   //  end MRules

        //  MERCHANDIZING VARIABLES
        //***************************
        //  REGION - INTEGER - Region number used to set Regional Merchandizing Rules
        //  COR - CHARACTER - Flag to indicate Scribner table or Scribner factor volumes.
        //                  "Y" = table volumes, "N" = factor volumes
        //  EVOD - INTEGER - allow even or oadd segment lengths
        //                  segment options 11-14 allow odd lengths by definition
        //                  1 = odd segment lengths allowed
        //                  2 = only even segment lengths will be allowed
        //  MAXLEN - REAL - Maximum segment length
        //  MINLEN - REAL - Minimum segment length
        //  MERCHL - REAL - Minimum length of primary product a tree must have
        //                  must be merchantable
        //  ** TOP DIB TO USE**
        //  MTOPP - REAL - BDFT, CUFT and Cord Wood merch top for primary product
        //  MTOPS - REAL - CUFT and Cord Wood merch top for secondary product

        //  OPT - INTEGER - Specifies whcih segmentation option to use for
        //          merchandizing tree bole.  Option codes are as follows:
        //          11 = 16 ft log scale, presented as tree length log.  (FSH 2409.11)
        //          12 = 20 ft log scale, presented as tree length log.  (FSH 2409.11)
        //          13 = 32 ft log scale, presented as tree length log.
        //          14 = 40 ft log scale, presented as tree length log.
        //          21 = Nominal log length (NLL).  If top log is less than half
        //              of the NLL then it is combined with the next lowest log and
        //              this combined piece is then resegmented according to the
        //              entered merchandizing parameters giving two approximately
        //              equal log lengths.  If the segment length is greater than
        //              or equal to half the NLL then the segment stands on its' own.
        //          22 = Nominal log length (NLL).  top log is combined with the next
        //              lowest log and this combined piece is then resegmented
        //              according to the entered merchandizing parameters giving
        //              two approximately equal log lengths.
        //          23 = Nominal log length.  top segment stands on its' own.
        //          24 = Nominal log length.  if the top segment is less than 1/4 of
        //              NLL then the segment is dropped.  If the segment is 1/4 to
        //              3/4 of NLL then the segment length is set to 1/2 of NLL.
        //              if the segment is greater than 3/4 of NLL then the segment
        //              length is set to NLL.

        //  STUMP - REAL - height of stump in feet or fractions thereof
        //  TRIM - REAL - trim length for each segment in feet or fractions thereof.
    }   //  end MRules
}