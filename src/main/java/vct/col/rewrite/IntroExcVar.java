package vct.col.rewrite;

import scala.collection.immutable.Stream;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.util.FeatureScanner;
import vct.util.ClassName;

import java.util.ArrayList;
import java.util.Arrays;

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
        return method.getContract().signals.length > 0; // || method.hasAttribute(THROWS); // ???
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
            ArrayList<DeclarationStatement> args = new ArrayList<>(Arrays.asList(resultMethod.getArgs()));
            DeclarationStatement exceptionOutArgument = create.field_decl(excVar, create.class_type(objectClass));
            exceptionOutArgument.setFlag(ASTFlags.OUT_ARG, true);
            args.add(0, exceptionOutArgument);
            resultMethod.setArgs(args.toArray(new DeclarationStatement[args.size()]));
        } else if (scanner.usesFinallyClause() || scanner.usesCatchClause()) {
            // Add local variable
            BlockStatement body = (BlockStatement) resultMethod.getBody();
            body.prepend(create.field_decl(excVar, create.class_type(objectClass)));
        }
    }

    public void visit(TryCatchBlock tryCatchBlock) {
        super.visit(tryCatchBlock);

        TryCatchBlock resultTryCatch = (TryCatchBlock) result;

        for (CatchClause catchClause : resultTryCatch.catches()) {
            // Note that prepend is used here, which means the statements are added in reverse order

            // Set the exc variable to null, since the exception is now being handled by the local catch clause
            catchClause.block().prepend(create.assignment(
                    create.local_name("sys__exc"),
                    create.reserved_name(ASTReserved.Null)
            ));

            // TODO (Bob): Once we have subtyping turn this into regular assignment, since this is a hack
            // Suprised it even works w.r.t. typechecking
            // Assign the global exc variable to the local formal parameter of the catch block
            // This way of any assertions or permissions on the exc variable were given earlier, they can also be used here
            catchClause.block().prepend(create.special(ASTSpecial.Kind.Assume,
                    create.expression(StandardOperator.EQ, create.local_name(catchClause.decl().name()), create.local_name("sys__exc"))));
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
            resultInvokation.addArg(0, create.local_name(excVar));
        }
    }
}
