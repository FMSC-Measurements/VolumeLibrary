namespace FMSC.VolumeLibrary
{
    public class CrzBiomassResult
    {
        public CrzBiomassResult FromArray(float[] values)
        {
            // NOTE: there is an 8 element array in the original code, but as far as I can tell, only 7 elements are used
            if (values.Length != VolumeLibrary.CRZBIOMASSCS_BMS_SIZE)
            {
                throw new ArgumentException("values must have 8 elements");
            }

            AboveGroundTotal = values[0];
            BranchesAndTop = values[1];
            DeadBranches = values[2];
            Foliage = values[3];
            PrimaryProduct = values[4];
            SecondaryProduct = values[5];
            StemTip = values[6];

            return this;
        }

        public float AboveGroundTotal { get; set; }
        public float BranchesAndTop { get; set; }
        public float DeadBranches { get; set; }
        public float Foliage { get; set; }
        public float PrimaryProduct { get; set; }
        public float SecondaryProduct { get; set; }
        public float StemTip { get; set; }

        public void AdjustForPercentRemoved(float percentRemoved)
        {
            if (percentRemoved > 0)
            {
                percentRemoved = percentRemoved / 100f;

                AboveGroundTotal *= percentRemoved;
                BranchesAndTop *= percentRemoved;
                DeadBranches *= percentRemoved;
                Foliage *= percentRemoved;
                PrimaryProduct *= percentRemoved;
                SecondaryProduct *= percentRemoved;
                StemTip *= percentRemoved;
            }
        }

        //public void CopyValuesToTreeCalculatedValues(TreeCalculatedValuesDO tcv)
        //{
        //    tcv.BiomassMainStemPrimary = PrimaryProduct;
        //    tcv.BiomassMainStemSecondary = SecondaryProduct;
        //    tcv.Biomasstotalstem = AboveGroundTotal;
        //    tcv.Biomasslivebranches = BranchesAndTop;
        //    tcv.Biomassdeadbranches = DeadBranches;
        //    tcv.Biomassfoliage = Foliage;
        //    tcv.BiomassTip = StemTip;
        //}
    }
}