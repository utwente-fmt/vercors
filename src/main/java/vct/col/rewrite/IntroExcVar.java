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
    public final String objectClass = "java_DOT_lang_DOT_Object";

    public IntroExcVar(ProgramUnit source) {
        super(source);
    }

    public static boolean canThrow(Method method) {
        // TODO (Bob): When the new parsers are merged (and the AST is updated correspondingly) extend this by checking the throws as well
        // TODO (Bob): Move to method class!
        return method.getContract().signals.length > 0 || method.throwy.length > 0;
    }

    public static boolean canThrow(MethodInvokation methodInvokation) {
        return methodInvokation.getDefinition().getContract().signals.length > 0;
    }

    public void visit(Method method) {
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

            // Set initial value to null
            BlockStatement body = (BlockStatement) resultMethod.getBody();
            body.prepend(create.assignment(create.local_name(excVar), create.reserved_name(ASTReserved.Null)));
        } else if (scanner.usesFinally() || scanner.usesCatch()) {
            // Add local variable and init as null
            BlockStatement body = (BlockStatement) resultMethod.getBody();
            body.prepend(create.field_decl(excVar, create.class_type(objectClass), create.reserved_name(ASTReserved.Null)));
        }
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
}
