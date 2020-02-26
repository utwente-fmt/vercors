package vct.col.rewrite;

import com.google.common.collect.Iterables;
import org.antlr.v4.codegen.model.Loop;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.Switch;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.ReturnStatement;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

public class BreakToGoto extends AbstractRewriter {
    public BreakToGoto(ProgramUnit source) {
        super(source);
    }

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
            BlockStatement block = create.block();
            ArrayList<NameExpression> originalLabels = new ArrayList<>();

            for (NameExpression label : node.getLabels()) {
                NameExpression breakLabel = generateBreakLabel(label);
                ASTNode breakTarget = create.label_decl(breakLabel);

                // Only create break target if code actually breaks to label
                // While loops are labeled properly in their own visit
                if (breakLabels.contains(breakLabel)) {
                    block.add(breakTarget);
                    // Save it for later
                    originalLabels.add(label);
                    Debug("Creating block containing node and %s", breakTarget);
                }

                breakLabels.remove(breakLabel);
            }

            // Only replace the statement with a block if the labels are actually used
            if (block.size() > 0) {
                // Add the original label back to the replicated statement
                for (NameExpression label : originalLabels) {
                    result.addLabel(label);
                }
                block.prepend(result);
                result = block;
                // Clear the labels so they won't be copied in the post_visit call to the new generated block
                node.clearLabels();
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
            ASTNode continueTarget = create.label_decl(continueLabel);
            // Only create continue target if code actually continues to label
            if (continueLabels.contains(continueLabel)) {
                BlockStatement blockStatement = (BlockStatement) ((LoopStatement) result).getBody();
                blockStatement.prepend(continueTarget);
                Debug("Pepending %s to while loop", continueTarget);
            }

            NameExpression breakLabel = generateBreakLabel(label);
            ASTNode breakTarget = create.label_decl(breakLabel);
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
            // This is needed because the post_visit step usually does fixate, but if we put our while loop inside a block
            // then the post_visit won't call fixate on the members, so we have to do it ourselves...?
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
        Warning("Loop invariant checking not implemented!");
        result = create.jump(newLabel);
        continueLabels.add(newLabel);
    }

    public void visitBreak(ASTSpecial breakStatement) {
        NameExpression label = (NameExpression) breakStatement.args[0];
        NameExpression newLabel = generateBreakLabel(label);
        Debug("Turning break into goto %s", newLabel.getName());
        Warning("Loop invariant checking not implemented!");
        result = create.jump(newLabel);
        breakLabels.add(newLabel);
    }

    public void visit(ReturnStatement returnStatement) {
        super.visit(returnStatement);
        Warning("Return statement not implemented");
    }
}
