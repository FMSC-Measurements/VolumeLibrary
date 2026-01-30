## Logging

### Use Warning to log exceptions
When logging exceptions, either handeled or not handeled use logging level warning to log them. Do not use logging level Error or Critical. 
When logging handeled exceptions use logging level Information if the exception is expected, otherwise if the exception is due to potential misuse by the consumer use Warning. 