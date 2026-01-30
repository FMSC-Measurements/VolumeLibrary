using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using FMSC.VolumeLibrary.Interop;

namespace FMSC.VolumeLibrary.Test
{
    public class VolumeLibrary_Test
    {
        [Fact]
        public void GetVersion()
        {
            var volumeLibrary = new VolumeLibrary();
            var verNum = volumeLibrary.GetVersionNumber();

            Assert.True(verNum > 0);
        }
    }
}
