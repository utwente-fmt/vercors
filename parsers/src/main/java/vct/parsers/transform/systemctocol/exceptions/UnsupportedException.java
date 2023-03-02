package vct.parsers.transform.systemctocol.exceptions;

import vct.result.VerificationError;

/**
 * Exception used to mark a feature that is not yet supported.
 */
public class UnsupportedException extends VerificationError.UserError {

    private final String text;

    private final Object sc_obj;

    public UnsupportedException(String text, Object sc_obj) {
        this.text = text;
        this.sc_obj = sc_obj;
    }

    @Override
    public String text() {
        return "Unsupported object: " + text;
    }

    @Override
    public String code() {
        return sc_obj.toString();
    }
}
