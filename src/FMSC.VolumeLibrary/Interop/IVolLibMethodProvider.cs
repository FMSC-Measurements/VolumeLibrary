using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FMSC.VolumeLibrary.Interop
{
    internal interface IVolLibMethodProvider
    {
        VolumeLibraryNativeMethods GetNativeMethods();
    }
}
