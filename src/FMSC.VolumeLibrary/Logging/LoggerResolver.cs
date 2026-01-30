using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;

namespace FMSC.VolumeLibrary.Logging
{
    /// <summary>
    /// Creates Logger instances. Users can register an IServiceProvider using RegisterServiceProvider, 
    /// otherwise it will create an instance of <see cref="DefaultLogger"/>
    /// </summary>
    public static class LoggerResolver
    {
        internal static ILoggerFactory DefaultLoggerFactory { get; set; } = new DefaultLoggerFactory();

        private static IServiceScope? Scope { get; set; }
        private static IServiceProvider? ServiceProvider => Scope?.ServiceProvider;

        public static void RegisterServiceProvider(IServiceProvider services)
        {
            Scope = services.CreateScope();
        }

        internal static ILogger<T> CreateLogger<T>()
        {
            var loggerFactory = ServiceProvider?.GetService<ILoggerFactory>() ?? DefaultLoggerFactory;
            return loggerFactory!.CreateLogger<T>();
        }

        internal static ILogger CreateLogger(Type type)
        {
            var loggerFactory = ServiceProvider?.GetService<ILoggerFactory>() ?? DefaultLoggerFactory;
            return loggerFactory!.CreateLogger(type);
        }

        internal static ILogger CreateLogger(string categoryName)
        {
            var loggerFactory = ServiceProvider?.GetService<ILoggerFactory>() ?? DefaultLoggerFactory;
            return loggerFactory!.CreateLogger(categoryName);
        }
    }

    [System.Diagnostics.CodeAnalysis.SuppressMessage("Major Code Smell", "S3881:Implement Dispose Pattern", Justification = "nothing to dispose")]
    internal class DefaultLoggerFactory : ILoggerFactory
    {

        public void AddProvider(ILoggerProvider provider)
        {
            throw new NotSupportedException();
        }

        public ILogger CreateLogger(string categoryName)
        {
            return new DefaultLogger(categoryName);
        }

        public void Dispose()
        {
            /* do nothing */
        }
    }
}
