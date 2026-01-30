using Microsoft.Extensions.Logging;
using System.Diagnostics;

#nullable enable

namespace FMSC.VolumeLibrary.Logging;

/// <summary>
/// Default logger that outputs to System.Diagnostics.<see cref="System.Diagnostics.Debug"/>
/// Use <see cref="MinLogLevel"/> to configure the default minimum <see cref="LogLevel"/>
/// </summary>
public class DefaultLogger : ILogger
{
    public static LogLevel MinLogLevel { get; set; } = LogLevel.Information;

    public string CategoryName { get; }

    //public DefaultLogger()
    //    : this(LogLevel.Information)
    //{ }

    internal DefaultLogger(string? category = null)
    {
        CategoryName = category ?? typeof(DefaultLogger).Name;
    }

    IDisposable ILogger.BeginScope<TState>(TState state)
    {
        return NullScope.Instance;
    }

    public bool IsEnabled(LogLevel logLevel)
    {
        return logLevel >= MinLogLevel;
    }

    public void Log<TState>(LogLevel logLevel, EventId eventId, TState state, Exception? exception, Func<TState, Exception?, string> formatter)
    {
        if (!IsEnabled(logLevel))
        {
            return;
        }

        var message = formatter(state, exception);

        var logLevelString = GetLogLevelString(logLevel).PadRight(5);

        if (exception != null)
        {
            message = "Exception-" + message;
        }

        var eventName = eventId.Name
            ?? "";

        Debug.WriteLine($"[{logLevelString}]{eventName}-{message}", CategoryName);
    }

    private static string GetLogLevelString(LogLevel logLevel)
    {
        switch (logLevel)
        {
            case LogLevel.Trace:
                return "Trace";

            case LogLevel.Debug:
                return "Debug";

            case LogLevel.Information:
                return "Info";

            case LogLevel.Warning:
                return "Warn";

            case LogLevel.Error:
                return "Error";

            case LogLevel.Critical:
                return "Crit";

            default:
                throw new ArgumentOutOfRangeException(nameof(logLevel));
        }
    }
}

[System.Diagnostics.CodeAnalysis.SuppressMessage("Critical Code Smell", "S3881:Implement Dispose Pattern", Justification = "nothing to dispose")]
internal class NullScope : IDisposable
{
    public static IDisposable Instance { get; } = new NullScope();

    public void Dispose()
    { /*do nothing*/ }
}
