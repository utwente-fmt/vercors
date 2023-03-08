package vct.parsers.transform.systemctocol.exceptions;

import vct.result.VerificationError;

/**
 * Exception used to mark a feature that is not yet supported.
 */
public class UnsupportedException extends VerificationError.UserError {

    private final String text;

    public UnsupportedException(String text) {
        this.text = text;
    }

    @Override
    public String text() {
        return "Unsupported object: " + text;
    }

    @Override
    public String code() {
        return "Unsupported SystemC Construct";
    }
}
