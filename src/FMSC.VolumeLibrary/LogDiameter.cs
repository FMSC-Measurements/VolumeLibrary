namespace FMSC.VolumeLibrary
{
    public class LogDiameter
    {
        public LogDiameter()
        { }

        public float this[int i]
        {
            get
            {
                switch (i)
                {
                    case 0: return ScallingDIB;
                    case 1: return DIB;
                    case 2: return DOB;
                    default: throw new ArgumentOutOfRangeException(nameof(i));
                }
            }
        }

        public float ScallingDIB { get; set; }
        public float DIB { get; set; }
        public float DOB { get; set; }

        public LogDiameter FromArray(float[,] values, int column)
        {
            if (values.GetLength(0) != VolumeLibrary.VOLLIBNVB_LOGDIA_SIZE_X)
            {
                throw new ArgumentException("values should have 3 elements in first dimension");
            }
            ScallingDIB = values[0, column];
            DIB = values[1, column];
            DOB = values[2, column];

            return this;
        }
    }
}