using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using FMSC.VolumeLibrary.Logging;
using Microsoft.Extensions.Logging;


namespace FMSC.VolumeLibrary.Interop;

#if !NETFRAMEWORK
internal class VolLibMethodProvider_netcore : IVolLibMethodProvider
{
    ILogger Log { get; } = LoggerResolver.CreateLogger<VolLibMethodProvider_netcore>();

    public VolumeLibraryNativeMethods GetNativeMethods()
    {
        // Determine platform and runtime identifier
        string runtimeIdentifier = GetRuntimeIdentifier();

        // Construct the path to the native library
        string executableDirectory = AppContext.BaseDirectory;
        string libraryPath = Path.Combine(executableDirectory, runtimeIdentifier, GetLibraryFileName());

        // Load the native library
        IntPtr nativeLibraryHandle = NativeLibrary.Load(libraryPath);

        if (nativeLibraryHandle == IntPtr.Zero)
        {
            Log.LogWarning($"Failed to load native library from: {libraryPath}");
            throw new DllNotFoundException($"Unable to load native library from: {libraryPath}");
        }

        Log.LogInformation($"Successfully loaded native library from: {libraryPath}");

        // Create and populate VolumeLibraryNativeMethods with delegates
        var nativeMethods = new VolumeLibraryNativeMethods()
        {
            BROWNCROWNFRACTION = LoadDelegate<VolumeLibraryDelegateTypes.BROWNCROWNFRACTION>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.BROWNCROWNFRACTION)),
            BROWNCULLCHUNK = LoadDelegate<VolumeLibraryDelegateTypes.BROWNCULLCHUNK>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.BROWNCULLCHUNK)),
            BROWNCULLLOG = LoadDelegate<VolumeLibraryDelegateTypes.BROWNCULLLOG>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.BROWNCULLLOG)),
            BROWNTOPWOOD = LoadDelegate<VolumeLibraryDelegateTypes.BROWNTOPWOOD>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.BROWNTOPWOOD)),
            CRZBIOMASSCS = LoadDelegate<VolumeLibraryDelegateTypes.CRZBIOMASSCS>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.CRZBIOMASSCS)),
            CRZSPDFTCS = LoadDelegate<VolumeLibraryDelegateTypes.CRZSPDFTCS>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.CRZSPDFTCS)),
            GETNVBEQ = LoadDelegate<VolumeLibraryDelegateTypes.GETNVBEQ>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.GETNVBEQ)),
            GETREGNWFCS = LoadDelegate<VolumeLibraryDelegateTypes.GETREGNWFCS>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.GETREGNWFCS)),
            GETVOLEQ3 = LoadDelegate<VolumeLibraryDelegateTypes.GETVOLEQ3>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.GETVOLEQ3)),
            MRULESCS = LoadDelegate<VolumeLibraryDelegateTypes.MRULESCS>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.MRULESCS)),
            VERNUM2 = LoadDelegate<VolumeLibraryDelegateTypes.VERNUM2>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.VERNUM2)),
            VOLLIBCSNVB = LoadDelegate<VolumeLibraryDelegateTypes.VOLLIBCSNVB>(nativeLibraryHandle, nameof(VolumeLibraryDelegateTypes.VOLLIBCSNVB)),
        };

        return nativeMethods;
    }

    /// <summary>
    /// Gets the runtime identifier for the current platform.
    /// </summary>
    private static string GetRuntimeIdentifier()
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            return RuntimeInformation.ProcessArchitecture == Architecture.X64 ? "win-x64" : "win-x86";
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux))
        {
            return RuntimeInformation.ProcessArchitecture == Architecture.X64 ? "linux-x64" : "linux-x86";
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
        {
            return RuntimeInformation.ProcessArchitecture == Architecture.X64 ? "osx-x64" : "osx-arm64";
        }
        else if (OperatingSystem.IsAndroid())
        {
            return RuntimeInformation.ProcessArchitecture == Architecture.Arm64 ? "arm64" : "arm";
        }

            throw new PlatformNotSupportedException($"VolumeLibrary does not support platform");
    }

    /// <summary>
    /// Gets the native library file name based on the current platform.
    /// </summary>
    private static string GetLibraryFileName()
    {
        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            return "vollib.dll";
        }
        else if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux) || RuntimeInformation.IsOSPlatform(OSPlatform.OSX) || OperatingSystem.IsAndroid())
        {
            return "vollib.so";
        }

        throw new PlatformNotSupportedException($"Platform {RuntimeInformation.OSDescription} is not supported");
    }

    /// <summary>
    /// Loads a delegate from the native library.
    /// </summary>
    private static T LoadDelegate<T>(IntPtr nativeLibraryHandle, string functionName) where T : Delegate
    {
        IntPtr functionPointer = NativeLibrary.GetExport(nativeLibraryHandle, functionName);

        if (functionPointer == IntPtr.Zero)
        {
            throw new EntryPointNotFoundException($"Function '{functionName}' not found in native library");
        }

        return Marshal.GetDelegateForFunctionPointer<T>(functionPointer);
    }
}
#endif

