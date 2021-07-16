package vct.logging;

import hre.ast.Origin;
import viper.api.ViperError;

import java.util.ArrayList;
import java.util.List;

/**
 * Represents verifications errors as log messages.
 *
 * @author Stefan Blom
 */
public class VerCorsError extends AbstractMessage {

    public final ErrorCode code;
    public final SubCode sub;
    public final Origin main;
    public final List<Origin> aux = new ArrayList<Origin>();

    public VerCorsError(ErrorCode code, SubCode sub, Origin origin, List<Origin> aux) {
        this.code = code;
        this.sub = sub;
        this.main = origin;
        this.aux.addAll(aux);
        fatal = true;
    }

    public static VerCorsError viper_error(ViperError<Origin> e) {
        ErrorCode code;
        SubCode sub;
        String err[] = e.getError(0).split(":");
        switch (err[0]) {
            case "postcondition.violated":
                code = ErrorCode.PostConditionFailed;
                break;
            case "exhale.failed":
                code = ErrorCode.ExhaleFailed;
                break;
            case "assert.failed":
                code = ErrorCode.AssertFailed;
                break;
            case "assignment.failed":
                code = ErrorCode.AssignmentFailed;
                break;
            case "invariant.not.established":
                code = ErrorCode.InvariantNotEstablished;
                break;
            case "invariant.not.preserved":
                code = ErrorCode.InvariantNotPreserved;
                break;
            case "predicate.not.wellformed":
            case "not.wellformed":
                code = ErrorCode.NotWellFormed;
                break;
            case "application.precondition":
                code = ErrorCode.ApplicationPreCondition;
                break;
            case "call.precondition":
                code = ErrorCode.CallPreCondition;
                break;
            case "method.precondition.unsound":
                code = ErrorCode.MethodPreConditionUnsound;
                break;
            default:
                hre.lang.System.Warning("unspecified error %s", err[0]);
                code = ErrorCode.UnspecifiedError;
                break;
        }
        switch (err[1]) {
            case "assertion.false":
                sub = SubCode.AssertionFalse;
                break;
            case "division.by.zero":
                sub = SubCode.DivisionByZero;
                break;
            case "insufficient.permission":
                sub = SubCode.InsufficientPermission;
                break;
            case "receiver.not.injective":
                sub = SubCode.InsufficientPermission;
                break;
            case "method.precondition.false":
                sub = SubCode.MethodPreConditionFalse;
                break;
            default:
                hre.lang.System.Warning("unspecified cause %s", err[1]);
                sub = SubCode.UnspecifiedCause;
                break;
        }
        Origin main = e.getOrigin(0);
        ArrayList<Origin> aux = new ArrayList<Origin>();
        for (int i = 1; i < e.getExtraCount(); i++) {
            aux.add(e.getOrigin(i));
        }
        return new VerCorsError(code, sub, main, aux);
    }

    @Override
    public void accept(MessageVisitor visitor) {
        visitor.visit(this);
    }

    /**
     * The verification problem found.
     *
     * @author Stefan Blom
     */
    public static enum ErrorCode {
        AssertFailed,
        ExhaleFailed,
        InvariantNotEstablished,
        InvariantNotPreserved,
        InvariantBroken,
        PostConditionFailed,
        MagicWandUnproven,
        MagicWandPreCondition,
        NotWellFormed,
        ApplicationPreCondition,
        CallPreCondition,
        AssignmentFailed,
        MethodPreConditionUnsound,
        UnspecifiedError,
        TypeError
    }

    /**
     * The reason for the verification problem.
     *
     * @author Stefan Blom
     */
    public static enum SubCode {
        AssertionFalse,
        DivisionByZero,
        InsufficientPermission,
        MethodPreConditionFalse,
        ExtendsThrowable,
        UnspecifiedCause,
        UnusedCatch,
        UnlistedExceptionType;
    }

}
