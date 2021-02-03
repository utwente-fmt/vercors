package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.Switch;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

import java.util.ArrayList;
import java.util.List;

public class SpecifyImplicitLabels extends AbstractRewriter {
    int counter = 0;

    List<String> labelStack = new ArrayList<>();

    public SpecifyImplicitLabels(ProgramUnit source) {
        super(source);
    }

    String generateLabel(String prefix) {
        return prefix + "_" + (counter++);
    }

    NameExpression currentLabel() {
        return create.label(labelStack.get(labelStack.size() - 1));
    }

    public void enterLabelScope(String prefix, ASTNode statement) {
        // Reuse an existing label or create a new one
        String currentLabel;
        if (statement.labels() > 0) {
            currentLabel = statement.getLabel(0).getName();
            if (currentLabel == null) {
                Abort("Null label is not allowed");
            }
        } else {
            currentLabel = generateLabel(prefix);
        }

        labelStack.add(currentLabel);
    }

    public void exitLabelScope(ASTNode node) {
        String previousCurrentLabel = labelStack.remove(labelStack.size() - 1);

        // Add currentlabel if the original node has no labels yet
        // Since they will be copied over in the post visit step
        if (node.labels() == 0) {
            result.addLabel(create.label(previousCurrentLabel));
        }
    }

    public void visit(LoopStatement loopStatement) {
        enterLabelScope("loop", loopStatement);
        super.visit(loopStatement);
        exitLabelScope(loopStatement);
    }

    public void visit(Switch switchStatement) {
        enterLabelScope("switch", switchStatement);
        super.visit(switchStatement);
        exitLabelScope(switchStatement);
    }

    public void visit(ASTSpecial special) {
        // If it is a break/continue without a label add the current label. Otherwise just copy over the thing.
        boolean isAbrupt = special.kind == ASTSpecial.Kind.Break || special.kind == ASTSpecial.Kind.Continue;
        if (!isAbrupt) {
            super.visit(special);
            return;
        }

        if (special.args.length == 0) {
            if (!labelStack.isEmpty()) {
                // If there is no break target but there is a label in scope, add it to the break/continue
                result = create.special(special.kind, currentLabel());
            } else {
                // If there is no break target but also no switch/while in scope, that's an error
                Abort("Break without target is only allowed within while or switch statements");
            }
        } else {
            // There already is a break target, so we can just skip it
            super.visit(special);
        }
    }
}
