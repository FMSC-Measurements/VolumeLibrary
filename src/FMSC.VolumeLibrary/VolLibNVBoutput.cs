namespace FMSC.VolumeLibrary
{
    public class VolLibNVBoutput
    {
        public Volumes Volumes { get; set; }
        public LogVolume[] LogVolumes { get; set; }
        public LogDiameter[] LogDiameters { get; set; }
        public float[] LogLengths { get; set; }
        public float[] BottomOfLogHeights { get; set; }
        public int TotalLogs { get; set; }
        public float NoLogsPrimary { get; set; }
        public float NoLogsSecondary { get; set; }
        public VolLibNVBCalculatedBiomass GreenBio { get; set; }
        public VolLibNVBCalculatedBiomass DryBio { get; set; }

        public int ErrorCode { get; set; } = 0;
    }
}