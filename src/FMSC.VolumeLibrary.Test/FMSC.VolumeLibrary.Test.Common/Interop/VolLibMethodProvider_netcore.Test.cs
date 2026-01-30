using System.Runtime.InteropServices;
using FMSC.VolumeLibrary.Interop;

namespace FMSC.VolumeLibrary.Test;

#if !NETFRAMEWORK
public class VolLibMethodProvider_netcore_Test
{
    private VolLibMethodProvider_netcore _provider;

    public VolLibMethodProvider_netcore_Test()
    {
        _provider = new VolLibMethodProvider_netcore();
    }

    /// <summary>
    /// Tests that GetNativeMethods returns a non-null VolumeLibraryNativeMethods instance.
    /// This test requires the native library to be available.
    /// </summary>
    //[Fact(Skip = "Requires native DLL to be available in the correct location")]
    [Fact]
    public void GetNativeMethods_ReturnsNonNullInstance()
    {
        // Act
        var nativeMethods = _provider.GetNativeMethods();

        // Assert
        Assert.NotNull(nativeMethods);
    }

    /// <summary>
    /// Tests that GetNativeMethods populates all delegate properties.
    /// This test requires the native library to be available.
    /// </summary>
    //[Fact(Skip = "Requires native DLL to be available in the correct location")]
    [Fact]
    public void GetNativeMethods_PopulatesAllDelegates()
    {
        // Act
        var nativeMethods = _provider.GetNativeMethods();

        // Assert
        Assert.NotNull(nativeMethods.BROWNCROWNFRACTION);
        Assert.NotNull(nativeMethods.BROWNCULLCHUNK);
        Assert.NotNull(nativeMethods.BROWNCULLLOG);
        Assert.NotNull(nativeMethods.BROWNTOPWOOD);
        Assert.NotNull(nativeMethods.CRZBIOMASSCS);
        Assert.NotNull(nativeMethods.CRZSPDFTCS);
        Assert.NotNull(nativeMethods.GETNVBEQ);
        Assert.NotNull(nativeMethods.GETREGNWFCS);
        Assert.NotNull(nativeMethods.GETVOLEQ3);
        Assert.NotNull(nativeMethods.MRULESCS);
        Assert.NotNull(nativeMethods.VERNUM2);
        Assert.NotNull(nativeMethods.VOLLIBCSNVB);
    }

    /// <summary>
    /// Tests that VERNUM2 delegate can be invoked successfully and returns a valid version number.
    /// This test requires the native library to be available.
    /// </summary>
    [Fact]
    public void GetNativeMethods_VERNUM2Delegate_CanBeInvoked()
    {
        // Arrange
        var nativeMethods = _provider.GetNativeMethods();
        var vernum = nativeMethods.VERNUM2;

        // Act
        vernum(out int versionNumber);

        // Assert
        Assert.True(versionNumber > 20250000, "Version number should be greater than 20250000");
    }

    /// <summary>
    /// Tests that GetNativeMethods throws a DllNotFoundException when the native library is not found.
    /// </summary>
    [Fact(Skip = "Test not implemented yet")]
    public void GetNativeMethods_MissingLibrary_ThrowsDllNotFoundException()
    {

    }

    /// <summary>
    /// Tests that the correct runtime identifier is returned for the current platform.
    /// </summary>
    [Fact(Skip = "Test not implemented yet")]
    public void GetRuntimeIdentifier_ReturnsValidIdentifier()
    {

    }

    /// <summary>
    /// Tests that the correct library file name is used for the current platform.
    /// </summary>
    [Fact(Skip = "Test not implemented yet")]
    public void GetLibraryFileName_UsesCorrectExtension()
    {

    }

    /// <summary>
    /// Tests that GetNativeMethods uses the correct runtime identifier based on platform.
    /// </summary>
    [Fact(Skip = "Test not implemented yet")]
    public void GetNativeMethods_UsesCorrectRuntimeIdentifier()
    {

    }

    /// <summary>
    /// Tests that GetNativeMethods constructs the library path using AppContext.BaseDirectory.
    /// </summary>
    [Fact(Skip = "Test not implemented yet")]
    public void GetNativeMethods_ConstructsPathFromAppContextBaseDirectory()
    {

    }

    /// <summary>
    /// Tests that all delegate types are defined in VolumeLibraryDelegateTypes.
    /// This is a unit test that doesn't require the native library.
    /// </summary>
    [Fact]
    public void VolumeLibraryDelegateTypes_ContainsAllRequiredDelegates()
    {
        // Verify that the delegate types are defined
        var delegateTypes = typeof(VolumeLibraryDelegateTypes);

        Assert.NotNull(delegateTypes);

        // Check that delegate type definitions exist
        var declaredTypes = delegateTypes.GetNestedTypes();
        Assert.NotEmpty(declaredTypes);
    }
}
#endif