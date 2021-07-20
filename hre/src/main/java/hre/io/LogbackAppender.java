package hre.io;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.AppenderBase;

import static hre.lang.System.*;

public class LogbackAppender extends AppenderBase<ILoggingEvent> {
    @Override
    protected void append(ILoggingEvent e) {
        switch(e.getLevel().toInt()) {
            case Level.ERROR_INT:
                // Additional info logged by viper on an error, will be returned to VerCors as a Fail/Abort.
                Warning("[viper] %s", e.getMessage());
                break;
            case Level.WARN_INT:
                Warning("[viper] %s", e.getMessage());
                break;
            case Level.INFO_INT:
                Output("[viper] %s", e.getMessage());
                break;
            case Level.DEBUG_INT:
                Debug("[viper] %s", e.getMessage());
                break;
            case Level.TRACE_INT:
                Debug("[viper] %s", e.getMessage());
                break;
        }
    }
}
