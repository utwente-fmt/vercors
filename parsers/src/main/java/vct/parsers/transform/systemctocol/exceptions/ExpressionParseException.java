package vct.parsers.transform.systemctocol.exceptions;

import de.tub.pes.syscir.sc_model.expressions.Expression;
import vct.result.VerificationError;

/**
 * Exception used for parse errors of expressions. This might occur if expressions encountered in the program are not
 * well-formed or if crucial expressions (function call parameters, branching conditions etc.) could not be transformed
 * to any valid COL expression.
 */
public class ExpressionParseException extends VerificationError.UserError {

    private final String text;

    private final Expression expr;

    public ExpressionParseException(String text, Expression expr) {
        this.text = text;
        this.expr = expr;
    }

    @Override
    public String text() {
        return text;
    }

    @Override
    public String code() {
        return expr.toStringNoSem();
    }
}
