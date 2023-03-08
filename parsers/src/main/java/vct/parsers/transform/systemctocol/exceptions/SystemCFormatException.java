package vct.parsers.transform.systemctocol.exceptions;

import vct.result.VerificationError;

/**
 * Exception used for errors that are caused by the SystemC design not being well-formed. This might be the case if
 * ports are not connected or if event notifications are called on non-event variables.
 */
public class SystemCFormatException extends VerificationError.SystemError {

    private final String text;

    public SystemCFormatException(String text) {
        this.text = text;
    }

    @Override
    public String text() {
        return text;
    }
}
