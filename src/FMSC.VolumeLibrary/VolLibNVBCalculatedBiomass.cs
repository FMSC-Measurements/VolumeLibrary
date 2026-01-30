namespace FMSC.VolumeLibrary
{
    public class VolLibNVBCalculatedBiomass
    {
        //public float this[int i]

        public int Length { get; set; }

        public float AboveGroundTotal { get; set; }

        public float TotalStemWood { get; set; }

        public float TotalStemBark { get; set; }

        public float StumpWood { get; set; }

        public float StumpBark { get; set; }

        public float SawWood { get; set; }

        public float SawBark { get; set; }

        public float TopwoodWood { get; set; }

        public float TopwoodBark { get; set; }

        public float TipWood { get; set; }

        public float TipBark { get; set; }

        public float Branches { get; set; }

        public float Foliage { get; set; }

        public float TopAndLimb { get; set; }

        public float CarbonContent { get; set; }

        public VolLibNVBCalculatedBiomass FromArray(float[] values)
        {
            if (values.Length != 15)
            {
                throw new ArgumentException("values must have 15 elements");
            }
            AboveGroundTotal = values[0];
            TotalStemWood = values[1];
            TotalStemBark = values[2];
            StumpWood = values[3];
            StumpBark = values[4];
            SawWood = values[5];
            SawBark = values[6];
            TopwoodWood = values[7];
            TopwoodBark = values[8];
            TipWood = values[9];
            TipBark = values[10];
            Branches = values[11];
            Foliage = values[12];
            TopAndLimb = values[13];
            CarbonContent = values[14];

            return this;
        }
    }
}