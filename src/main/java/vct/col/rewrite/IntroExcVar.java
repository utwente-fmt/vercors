package vct.col.rewrite;

import jdk.nashorn.internal.ir.Block;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.ClassType;
import vct.util.ClassName;

/**
 * Introduces the exc variable into each method.
 * TODO: Only introduce if it throws or if throwing happens in the method.
 */
public class IntroExcVar extends AbstractRewriter {
    public IntroExcVar(ProgramUnit source) {
        super(source);
    }

    public void visit(Method method) {
        super.visit(method);
        Method resultMethod = (Method) result;

        // Only consider plain methods or constructors
        if (!(resultMethod.kind == Method.Kind.Plain || resultMethod.kind == Method.Kind.Constructor)) {
            return;
        }

        BlockStatement body = (BlockStatement) resultMethod.getBody();
        if (body != null) {
            body.prepend(create.field_decl("__exc", create.class_type(new String[]{"java", "lang", "Object"})));
        }
    }

    public void visit(TryCatchBlock tryCatchBlock) {
        super.visit(tryCatchBlock);

        TryCatchBlock resultTryCatch = (TryCatchBlock) result;

        for (CatchClause catchClause : resultTryCatch.catches()) {
            // TODO (Bob): Once we have subtyping turn this into regular assignment, since this is a hack
            catchClause.block().prepend(create.special(ASTSpecial.Kind.Assume,
                    create.expression(StandardOperator.EQ, create.local_name(catchClause.decl().name()), create.local_name("__exc"))));
        }
    }
}
