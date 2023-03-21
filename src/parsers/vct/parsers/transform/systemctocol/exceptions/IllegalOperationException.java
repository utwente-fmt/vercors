package vct.parsers.transform.systemctocol.exceptions;

import vct.result.VerificationError;

/**
 * Exception used for operations that should not be possible. This might occur if the user attempts to convert an
 * expression into a statement or vice versa.
 */
public class IllegalOperationException extends VerificationError.SystemError {

    private final String text;

    public IllegalOperationException(String text) {
        this.text = text;
    }

    @Override
    public String text() {
        return text;
    }
}
