package vct.col.rewrite;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.util.AbstractRewriter;
import vct.col.util.FeatureScanner;

/**
 * Introduces the exc variable into each method. Assuming flattened code.
 * TODO: Only introduce if it throws or if throwing happens in the method.
 */
public class IntroExcVar extends AbstractRewriter {
    public static final String excVar = "sys__exc";

    /**
     * True if the current method is exceptional, i.e. makes use of the sys__exc variable
     */
    boolean inExceptionalMethod;

    int counter;

    public IntroExcVar(ProgramUnit source) {
        super(source);
    }

    public void visit(Method method) {
        inExceptionalMethod = ((method.getContract() != null) && method.canThrowSpec()) || usesExceptionalControlFlow(method);

        super.visit(method);

        Method resultMethod = (Method) result;

        // Only consider plain methods or constructors
        if (!(resultMethod.kind == Method.Kind.Plain || resultMethod.kind == Method.Kind.Constructor)) {
            return;
        }

        FeatureScanner scanner = new FeatureScanner();
        resultMethod.accept(scanner);

        if (canThrow(resultMethod)) {
            // Add out parameter
            resultMethod.prependArg(result.getOrigin(), excVar, create.class_type(objectClass), true);

            // Set initial value to null if method has a body
            if (resultMethod.getBody() != null) {
                BlockStatement body = (BlockStatement) resultMethod.getBody();
                body.prepend(create.assignment(create.local_name(excVar), create.reserved_name(ASTReserved.Null)));
            }
        } else if (scanner.usesFinally() || scanner.usesCatch()) {
            // Add local variable and init as null
            BlockStatement body = (BlockStatement) resultMethod.getBody();
            body.prepend(create.field_decl(excVar, create.class_type(objectClass), create.reserved_name(ASTReserved.Null)));
        }
    }

    /**
     * True if n uses exceptional control flow internally. This is not about exceptions escaping methods.
     */
    public static boolean usesExceptionalControlFlow(ASTNode n) {
        FeatureScanner scanner = new FeatureScanner();
        n.accept(scanner);
        return scanner.usesFinally()
                || scanner.usesCatch()
                || scanner.usesSpecial(ASTSpecial.Kind.Throw)
                || scanner.usesThrowingMethodCalls();
    }

    public void visit(TryCatchBlock tryCatchBlock) {
        super.visit(tryCatchBlock);

        TryCatchBlock resultTryCatch = (TryCatchBlock) result;

        for (CatchClause catchClause : resultTryCatch.catches()) {
            // Note that prepend is used here, which means the statements are added in reverse order

            // Set the exc variable to null, since the exception is now being handled by the local catch clause
            catchClause.block().prepend(create.assignment(
                    create.local_name(excVar),
                    create.reserved_name(ASTReserved.Null)
            ));

            // TODO (Bob): Once we have subtyping turn this into regular assignment, since this is a hack
            // Suprised it even works w.r.t. typechecking (could this indicate a bug in the type checker?)
            // Assign the global exc variable to the local formal parameter of the catch block
            // This way of any assertions or permissions on the exc variable were given earlier, they can also be used here
            catchClause.block().prepend(create.special(ASTSpecial.Kind.Assume,
                    create.expression(StandardOperator.EQ, create.local_name(catchClause.name()), create.local_name(excVar))));
        }
    }

    public void visit(MethodInvokation methodInvokation) {
        super.visit(methodInvokation);

        // Only consider constructors and plain methods
        Method.Kind methodKind = methodInvokation.getDefinition().kind;
        if (!(methodKind == Method.Kind.Constructor || methodKind == Method.Kind.Plain)) {
            return;
        }

        if (canThrow(methodInvokation)) {
            // Add exception parameter
            MethodInvokation resultInvokation = (MethodInvokation) result;
            resultInvokation.prependArg(create.local_name(excVar));
        }
    }

    public void visit(LoopStatement oldLoopStatement) {
        super.visit(oldLoopStatement);

        if (!inExceptionalMethod) {
            return;
        }

        // If the current method is exceptional, a while loop must promise it does not
        // change the sys__exc variable.
        // We know this is true, because:
        // - If the while loop terminates normally, no exception is thrown,
        //   and hence sys__exc remains the same
        // - If the while loop does not terminate normally, an exception is thrown, which means control
        //   flow escapes the while loop. Therefore the loop invariant is not maintained.
        // This ensures while loops can be used within finally, as otherwise it is unclear
        // if sys__exc is unchanged when the loop exits normally.

        // Store the old excVar value
        String oldExcVar = "_old_" + excVar + "_" + counter++;
        currentBlock.append(create.field_decl(oldExcVar, create.class_type(ClassType.javaLangObjectName())));
        currentBlock.append(create.assignment(create.local_name(oldExcVar), create.local_name(excVar)));

        // Get the contract of the while loop
        currentBlock.append(result);
        LoopStatement ls = (LoopStatement) result;
        // Ensure there is the empty contract if contract is null on this while loop
        ls.fixate();
        Contract lsc = ls.getContract();
        result = null;

        // Copy over the contract while adding the extra invariant
        Contract c = new Contract(
                lsc.given,
                lsc.yields,
                lsc.modifies,
                lsc.accesses,
                create.expression(StandardOperator.Star,
                        lsc.invariant,
                        create.expression(StandardOperator.EQ,
                                create.local_name(oldExcVar),
                                create.local_name(excVar))),
                lsc.pre_condition,
                lsc.post_condition,
                lsc.signals);

        ls.setContract(c);
    }
}
