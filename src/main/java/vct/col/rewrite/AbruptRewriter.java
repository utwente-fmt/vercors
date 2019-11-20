package vct.col.rewrite;

import hre.ast.BranchOrigin;
import hre.ast.Origin;
import org.antlr.v4.codegen.model.Loop;
import scala.reflect.internal.Trees;
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

    public String breakLabel(NameExpression label) {
        return "__break_" + label.getName();
    }

    public String continueLabel(NameExpression label) {
        return "__continue_" + label.getName();
    }

    @Override
    public void post_visit(ASTNode node) {
        if (node.labels() > 0) {
            NameExpression breakLabel = create.unresolved_name(breakLabel(node.getLabel(0)));
            // Apparently this is how the "empty labeled statement" (label: ;) is parsed and encoded in COL.
            ASTNode breakTarget = create.comment(";").labeled(breakLabel.getName());

            // Only create break target if code actually breaks to label
            if (breakLabels.contains(breakLabel)) {
                // If we're rewriting a while loop, just add the label to the end of the body. This is necessary because
                // post_visit does some "fixate" business, which is skipped if we just make result = block.
                if (result instanceof LoopStatement) {
                    LoopStatement loopStatement = (LoopStatement) result;
                    BlockStatement blockStatement = (BlockStatement) loopStatement.getBody();
                    blockStatement.add(breakTarget);
                    Debug("Appending %s at the end of while loop", breakTarget);
                } else {
                    result = create.block(result, breakTarget);
                    Debug("Creating block containing node and %s", breakTarget);
                }
            }
        }

        super.post_visit(node);
    }

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
        NameExpression label = (NameExpression) continueStatement.args[0];
        NameExpression newLabel = create.unresolved_name(continueLabel(label));
        Debug("Turning continue into goto %s", newLabel.getName());
        result = create.special(ASTSpecial.Kind.Goto, newLabel);
        continueLabels.add(newLabel);
    }

    public void visitBreak(ASTSpecial breakStatement) {
        NameExpression label = (NameExpression) breakStatement.args[0];
        NameExpression newLabel = create.unresolved_name(breakLabel(label));
        Debug("Turning break into goto %s", newLabel.getName());
        result = create.special(ASTSpecial.Kind.Goto, newLabel);
        breakLabels.add(newLabel);
    }

    public void visit(TryCatchBlock tryCatchBlock) {
        Warning("Trycatchblock not yet implemented");
        super.visit(tryCatchBlock);
    }

}
