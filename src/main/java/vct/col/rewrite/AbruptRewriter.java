package vct.col.rewrite;

import org.antlr.v4.codegen.model.Loop;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;

import java.util.HashSet;
import java.util.Set;

public class AbruptRewriter extends AbstractRewriter {
    public AbruptRewriter(ProgramUnit source) {
        super(source);
    }

    // TODO (Bob): Should this be a stack of sets? As methods can be nested at some point.
    private Set<NameExpression> breakLabels = new HashSet<>();
    private Set<NameExpression> continueLabels = new HashSet<>();

    public NameExpression generateBreakLabel(NameExpression label) {
        return create.unresolved_name("__break_" + label.getName());
    }

    public NameExpression generateContinueLabel(NameExpression label) {
        return create.unresolved_name("__continue_" + label.getName());
    }

    @Override
    public void post_visit(ASTNode node) {
        if (!(result instanceof LoopStatement)) {
            for (NameExpression label : node.getLabels()) {
                NameExpression breakLabel = generateBreakLabel(label);
                // Apparently this is how the "empty labeled statement" (label: ;) is parsed and encoded in COL.
                ASTNode breakTarget = create.comment(";").labeled(breakLabel.getName());

                // Only create break target if code actually breaks to label
                // While loops are labeled properly in their own visit
                if (breakLabels.contains(breakLabel) && !(result instanceof LoopStatement)) {
                    result = create.block(result, breakTarget);
                    Debug("Creating block containing node and %s", breakTarget);
                }

                breakLabels.remove(breakLabel);
            }
        }

        super.post_visit(node);
    }

    public void visit(Method method) {
        super.visit(method);
        if (breakLabels.size() + continueLabels.size() != 0) {
            Warning("Some break or continue labels were not deleted, even though they should be. This indicates a logic error.");
        }
    }

    public void visit(LoopStatement loopStatement) {
        super.visit(loopStatement);

        BlockStatement breakTargets = create.block();

        for (NameExpression label : loopStatement.getLabels()) {
            // The targets are where to jump if "break/continue labelX" is called. Naturally the targets are not the same
            // location as the label itself, hence we need separate target labels.

            NameExpression continueLabel = generateContinueLabel(label);
            ASTNode continueTarget = create.comment(";").labeled(continueLabel.getName());
            // Only create continue target if code actually continues to label
            if (continueLabels.contains(continueLabel)) {
                BlockStatement blockStatement = (BlockStatement) ((LoopStatement) result).getBody();
                blockStatement.prepend(continueTarget);
                Debug("Pepending %s to while loop", continueTarget);
            }

            NameExpression breakLabel = generateBreakLabel(label);
            ASTNode breakTarget = create.comment(";").labeled(breakLabel.getName());
            // Only create break target if code actually breaks to label
            if (breakLabels.contains(breakLabel)) {
                breakTargets.add(breakTarget);
                Debug("Appending %s to while loop", breakTarget);
            }

            breakLabels.remove(breakLabel);
            continueLabels.remove(continueLabel);
        }

        // If there were one or more break targets, replace the resulting while loop with a block containing the while
        // loop, followed by all the break targets
        if (breakTargets.size() > 0) {
            ((LoopStatement) result).fixate();
            breakTargets.prepend(result);
            result = breakTargets;
        }
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
        NameExpression newLabel = generateContinueLabel(label);
        Debug("Turning continue into goto %s", newLabel.getName());
        result = create.special(ASTSpecial.Kind.Goto, newLabel);
        continueLabels.add(newLabel);
    }

    public void visitBreak(ASTSpecial breakStatement) {
        NameExpression label = (NameExpression) breakStatement.args[0];
        NameExpression newLabel = generateBreakLabel(label);
        Debug("Turning break into goto %s", newLabel.getName());
        result = create.special(ASTSpecial.Kind.Goto, newLabel);
        breakLabels.add(newLabel);
    }

    public void visit(TryCatchBlock tryCatchBlock) {
        Warning("Trycatchblock not yet implemented");
        super.visit(tryCatchBlock);
    }

}
