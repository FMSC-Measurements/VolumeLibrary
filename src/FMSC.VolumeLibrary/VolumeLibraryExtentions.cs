using System.Text;

namespace FMSC.VolumeLibrary
{
    public static class VolumeLibraryExtentions
    {
        public static string GetVersionNumberString(this IVolumeLibrary volumeLibrary)
        {
            return VolLibVersionNumberToString(volumeLibrary.GetVersionNumber());
        }

        public static string VolLibVersionNumberToString(int versionNumber)
        {
            try
            {
                //  Convert to a string to reformat date
                string sTemp = versionNumber.ToString();
                StringBuilder sDate = new StringBuilder();
                sDate.Append(sTemp.Substring(4, 2));
                sDate.Append(".");
                sDate.Append(sTemp.Substring(6, 2));
                sDate.Append(".");
                sDate.Append(sTemp.Substring(0, 4));

                return sDate.ToString();
            }
            catch
            {
                return "0.0.0.0";
            }
        }
    }
}