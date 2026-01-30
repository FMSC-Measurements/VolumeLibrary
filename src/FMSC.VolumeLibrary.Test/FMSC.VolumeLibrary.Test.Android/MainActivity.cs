

using Android.App;
using Android.OS;

namespace FMSC.VolumeLibrary.Test.Android;

[Activity(Label = "@string/app_name", MainLauncher = true)]
public class MainActivity : Activity
{
    protected override void OnCreate(Bundle? savedInstanceState)
    {
        base.OnCreate(savedInstanceState);

        // Set our view from the "main" layout resource
        SetContentView(Resource.Layout.activity_main);

        //VolumeLibrary test = new ();
        //var verNum = test.GetVersionNumber();
        //System.Diagnostics.Debug.WriteLine("success");
    }

    protected override void OnResume()
    {
        base.OnResume();

        VolumeLibrary test = new();
        var verNum = test.GetVersionNumber();
        System.Diagnostics.Debug.WriteLine("success");
    }

}