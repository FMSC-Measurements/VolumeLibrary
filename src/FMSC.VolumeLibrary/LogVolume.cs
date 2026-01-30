namespace FMSC.VolumeLibrary
{
    public class LogVolume
    {
        public LogVolume()
        { }

        public float this[int i]
        {
            get
            {
                switch (i)
                {
                    case 0: return GrossBoardFoot;
                    case 1: return GrossRemovedBoardFoot;
                    case 2: return NetBoardFoot;
                    case 3: return GrossCubicFoot;
                    case 4: return GrossRemovedCubicFoot;
                    case 5: return NetCubicFoot;
                    case 6: return Extra;
                    default: throw new ArgumentOutOfRangeException(nameof(i));
                }
            }
            set
            {
                switch (i)
                {
                    case 0: GrossBoardFoot = value; break;
                    case 1: GrossRemovedBoardFoot = value; break;
                    case 2: NetBoardFoot = value; break;
                    case 3: GrossCubicFoot = value; break;
                    case 4: GrossRemovedCubicFoot = value; break;
                    case 5: NetCubicFoot = value; break;
                    case 6: Extra = value; break;
                    default: throw new ArgumentOutOfRangeException(nameof(i));
                }
            }
        }

        public float GrossBoardFoot { get; set; }
        public float GrossRemovedBoardFoot { get; set; }
        public float NetBoardFoot { get; set; }
        public float GrossCubicFoot { get; set; }
        public float GrossRemovedCubicFoot { get; set; }
        public float NetCubicFoot { get; set; }

        // When Ctype is 'C' this returns log weight
        // otherwise GrossBoardFootInternational
        public float Extra { get; set; }

        public LogVolume FromArray(float[,] values, int row)
        {
            if (values.GetLength(1) != VolumeLibrary.VOLLIBNVB_LOGVOL_SIZE_Y)
            {
                throw new ArgumentException("values should have 7 elements in second dimension");
            }
            GrossBoardFoot = values[row, 0];
            GrossRemovedBoardFoot = values[row, 1];
            NetBoardFoot = values[row, 2];
            GrossCubicFoot = values[row, 3];
            GrossRemovedCubicFoot = values[row, 4];
            NetCubicFoot = values[row, 5];
            Extra = values[row, 6];

            return this;
        }
    }
}