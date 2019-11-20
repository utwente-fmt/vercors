package vct.col.rewrite;

import hre.ast.BranchOrigin;
import hre.ast.Origin;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;

import java.util.HashSet;
import java.util.Set;

import static hre.lang.System.Output;

public class AbruptRewriter extends AbstractRewriter {
    public AbruptRewriter(ProgramUnit source) {
        super(source);
    }

    // TODO (Bob): Should this be a stack of sets? As methods can be nested at some point.
    private Set<NameExpression> breakLabels = new HashSet<>();
    private Set<NameExpression> continueLabels = new HashSet<>();

    public void visit(Method method) {
        breakLabels.clear();
        continueLabels.clear();
        super.visit(method);
    }

    public void visit(LoopStatement loopStatement) {
        for (int i = 0; i < loopStatement.labels(); i++) {
            NameExpression label = loopStatement.getLabel(i);
        }
        super.visit(loopStatement);
    }

    public void visit(ASTSpecial special) {
        switch (special.kind) {
            default:
                super.visit(special);
                break;
            case Break:
                visitBreak(special);
                break;
            case Continue:
                visitContinue(special);
                break;
        }
    }

    private void visitContinue(ASTSpecial continueStatement) {
        Debug("Checking continue");
        NameExpression label = (NameExpression) continueStatement.args[0];
        Debug("Label: %s", label);
        Debug("Label exists: %s", variables.containsKey(label.getName()));

        // TODO (Bob): Create jump here
    }

    public void visitBreak(ASTSpecial breakStatement) {
        Debug("Checking break");
        NameExpression label = (NameExpression) breakStatement.args[0];
        Debug("Label: %s", label);
        Debug("Label exists: %s", variables.containsKey(label.getName()));
    }

    public void visit(TryCatchBlock tryCatchBlock) {
        Warning("Trycatchblock not yet implemented");
        super.visit(tryCatchBlock);
    }

}
