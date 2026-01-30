namespace FMSC.VolumeLibrary
{
    public class CrownFractionWeight
    {
        public CrownFractionWeight() { }

        public CrownFractionWeight(float[] volLibArray)
        {
            FromArray(volLibArray);
        }

        public CrownFractionWeight FromArray(float[] volLibArray)
        {
            Needles = volLibArray[0];
            QuarterInch = volLibArray[1];
            OneInch = volLibArray[2];
            ThreeInch = volLibArray[3];
            ThreePlus = volLibArray[4];

            return this;
        }

        public float Needles { get; set; }
        public float QuarterInch { get; set; }
        public float OneInch { get; set; }
        public float ThreeInch { get; set; }
        public float ThreePlus { get; set; }
    }
}
