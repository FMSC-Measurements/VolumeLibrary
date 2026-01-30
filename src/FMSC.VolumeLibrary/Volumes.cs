using System;

namespace FMSC.VolumeLibrary
{
    public class Volumes
    {
        public Volumes()
        { }

        public Volumes(float[] values)
        {
            FromArray(values);
        }

        public float this[int i]
        {
            get
            {
                switch (i)
                {
                    case 0: return TotalCubic;
                    case 1: return GrossBdFt;
                    case 2: return NetBdFt;
                    case 3: return GrossCuFt;
                    case 4: return NetCuFt;
                    case 5: return MerchantableCords;
                    case 6: return GrossSecondaryCuFt;
                    case 7: return NetSecondaryCuFt;
                    case 8: return SecondaryCords;
                    case 9: return GrossInernationalBdFt;
                    case 10: return NetInternationalBdFt;
                    case 11: return GrossSecondaryBdFt;
                    case 12: return NetSecondaryBdFt;
                    case 13: return Stump;
                    case 14: return Tip;
                    default: throw new ArgumentOutOfRangeException(nameof(i));
                }
            }
            set
            {
                switch (i)
                {
                    case 0: TotalCubic = value; break;
                    case 1: GrossBdFt = value; break;
                    case 2: NetBdFt = value; break;
                    case 3: GrossCuFt = value; break;
                    case 4: NetCuFt = value; break;
                    case 5: MerchantableCords = value; break;
                    case 6: GrossSecondaryCuFt = value; break;
                    case 7: NetSecondaryCuFt = value; break;
                    case 8: SecondaryCords = value; break;
                    case 9: GrossInernationalBdFt = value; break;
                    case 10: NetInternationalBdFt = value; break;
                    case 11: GrossSecondaryBdFt = value; break;
                    case 12: NetSecondaryBdFt = value; break;
                    case 13: Stump = value; break;
                    case 14: Tip = value; break;
                    default: throw new ArgumentOutOfRangeException(nameof(i));
                }
            }
        }

        public float TotalCubic { get; set; }
        public float GrossBdFt { get; set; }
        public float NetBdFt { get; set; }
        public float GrossCuFt { get; set; }
        public float NetCuFt { get; set; }
        public float MerchantableCords { get; set; }
        public float GrossSecondaryCuFt { get; set; }
        public float NetSecondaryCuFt { get; set; }
        public float SecondaryCords { get; set; }
        public float GrossInernationalBdFt { get; set; }
        public float NetInternationalBdFt { get; set; }
        public float GrossSecondaryBdFt { get; set; }
        public float NetSecondaryBdFt { get; set; }
        public float Stump { get; set; }
        public float Tip { get; set; }

        public Volumes FromArray(float[] values)
        {
            if (values.Length != 15)
            {
                throw new ArgumentException("values must have 15 elements");
            }
            TotalCubic = values[0];
            GrossBdFt = values[1];
            NetBdFt = values[2];
            GrossCuFt = values[3];
            NetCuFt = values[4];
            MerchantableCords = values[5];
            GrossSecondaryCuFt = values[6];
            NetSecondaryCuFt = values[7];
            SecondaryCords = values[8];
            GrossInernationalBdFt = values[9];
            NetInternationalBdFt = values[10];
            GrossSecondaryBdFt = values[11];
            NetSecondaryBdFt = values[12];
            Stump = values[13];
            Tip = values[14];

            return this;
        }
    }
}