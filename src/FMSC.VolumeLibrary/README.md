## About
This is a dotnet library that interops with the native VolumeLibrary dll. 
It supports windows and android.



## Configure Logging
This library uses `Microsoft.Extensions.Logging` for logging. By default logs are output to `System.Diagnostics.Debug`. 
If you would like to provide your own logging implementation you can do so by providing your own Service Provider.
For example in your application startup 

```
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using AppLifetime.Example;
using FMSC.VolumeLibrary.Logging

HostApplicationBuilder builder = Host.CreateApplicationBuilder(args);

var host = builder.Build();
LoggingResolver.RegisterServiceProvider(builder.Services);

```