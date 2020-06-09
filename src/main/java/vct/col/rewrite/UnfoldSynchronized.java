package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;

public class UnfoldSynchronized extends AbstractRewriter {
    int counter = 0;

    public UnfoldSynchronized(ProgramUnit source) {
        super(source);
    }

    /**
     * Type needs to be supplied since rewriting a node discards type information.
     */
    public BlockStatement synchronizedToTryFinally(Type exprType, ASTNode expr, BlockStatement body) {
        // Create expr sync variable
        NameExpression exprName = create.local_name("__sync_" + counter++);
        DeclarationStatement exprDeclaration = create.field_decl(exprName.getName(), exprType, expr);

        ASTSpecial lockStatement = create.special(ASTSpecial.Kind.Lock, exprName);
        ASTSpecial unlockStatement = create.special(ASTSpecial.Kind.Unlock, exprName);

        TryCatchBlock tryCatchBlock = create.try_catch(body, create.block(unlockStatement));

        return create.block(exprDeclaration, lockStatement, tryCatchBlock);
    }

    public void visit(Synchronized synchronizedBlock) {
        result = synchronizedToTryFinally(
                synchronizedBlock.expr().getType(),
                rewrite(synchronizedBlock.expr()),
                rewrite(synchronizedBlock.body())
        );
    }

    public void visit(Method method) {
        super.visit(method);

        if (method.isSynchronized()) {
            // Get the type of the class that owns the method
            ASTClass containingClass = (ASTClass) method.getParent();
            Type classType = create.class_type(containingClass.getFullName());

            // Wrap method body in try-finally with lock
            Method resultMethod = (Method) result;
            BlockStatement resultBody = (BlockStatement) resultMethod.getBody();
            resultBody.clearParent();
            BlockStatement wrappedBody = synchronizedToTryFinally(classType, create.reserved_name(ASTReserved.This), resultBody);
            resultMethod.setBody(wrappedBody);
        }
    }

    public void visit(NameExpression nameExpression) {
        // Remove the Synchronized keyword from the method since these are all moved inside
        if (nameExpression.isReserved(ASTReserved.Synchronized)) {
            result = null;
        } else {
            super.visit(nameExpression);
        }
    }
}
