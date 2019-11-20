package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.Switch;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ProgramUnit;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import static hre.lang.System.Output;

// TODO (Bob): Possibly remove labels if they're not referred to by breaks, continues...? Then VerCors can (possibly) assume a statement does not need a try-catch if there's no label

public class SpecifyImplicitLabels extends AbstractRewriter {
    int counter = 0;

    List<NameExpression> labelStack = new ArrayList<>();

    public SpecifyImplicitLabels(ProgramUnit source) {
        super(source);
    }

    String generateLabel(String prefix) {
        return "__" + prefix + "_" + (counter++);
    }

    public void enterLabelScope(String prefix, ASTNode statement) {
        // Reuse an existing label or create a new one
        NameExpression currentLabel;
        if (statement.labels() > 0) {
            currentLabel = statement.getLabel(0);
        } else {
            currentLabel = create.label(generateLabel(prefix));
        }

        labelStack.add(currentLabel);
    }

    public void exitLabelScope() {
        NameExpression currentLabel = labelStack.remove(labelStack.size() - 1);

        // Add currentlabel if the node has no labels yet
        if (result.labels() == 0) {
            result.addLabel(currentLabel);
        }
    }

    public void visit(LoopStatement loopStatement) {
        enterLabelScope("loop", loopStatement);
        super.visit(loopStatement);
        exitLabelScope();
    }

    public void visit(Switch switchStatement) {
        enterLabelScope("switch", switchStatement);
        super.visit(switchStatement);
        exitLabelScope();
    }

    public void visit(ASTSpecial special) {
        // If it is a break/continue without a label add the current label. Otherwise just copy over the thing.
        boolean isAbrupt = special.kind == ASTSpecial.Kind.Break || special.kind == ASTSpecial.Kind.Continue;
        if (!isAbrupt) {
            super.visit(special);
            return;
        }

        // If there is no break target
        if (special.args.length == 0 && labelStack.size() > 0) {
            NameExpression currentLabel = labelStack.get(labelStack.size() - 1);
            if (currentLabel == null) Abort("Null label is not allowed");
            result = create.special(special.kind, new ASTNode[]{currentLabel});
        } else if (special.args.length == 0 && labelStack.size() == 0) {
            // If there is no break target but also no switch/while in scope, that's an error
            Abort("Break without target is only allowed within while or switch statements");
        } else {
            // There already is a break target, so we can just skip it
            super.visit(special);
        }
    }
}
