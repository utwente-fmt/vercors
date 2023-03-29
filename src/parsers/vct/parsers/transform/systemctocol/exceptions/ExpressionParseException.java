package vct.parsers.transform.systemctocol.exceptions;

import vct.result.VerificationError;

/**
 * Exception used for parse errors of expressions. This might occur if expressions encountered in the program are not
 * well-formed or if crucial expressions (function call parameters, branching conditions etc.) could not be transformed
 * to any valid COL expression.
 */
public class ExpressionParseException extends VerificationError.UserError {

    private final String text;

    public ExpressionParseException(String text) {
        this.text = text;
    }

    @Override
    public String text() {
        return text;
    }

    @Override
    public String code() {
        return "SystemC Parse Failure";
    }
}
