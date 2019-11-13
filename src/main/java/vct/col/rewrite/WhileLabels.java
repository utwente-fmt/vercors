package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ProgramUnit;

import java.util.ArrayList;
import java.util.List;

import static hre.lang.System.Output;

public class WhileLabels extends AbstractRewriter {
    int counter = 0;

    List<NameExpression> labelStack = new ArrayList<>();

    public WhileLabels(ProgramUnit source) {
        super(source);
    }

    String generateWhileLabel() {
        return "__while_" + (counter++);
    }

    public void visit(LoopStatement loopStatement) {
        // Reuse an existing label or create a new one
        NameExpression currentLabel;
        if (loopStatement.labels() > 0) {
            currentLabel = loopStatement.getLabel(0);
        } else {
            currentLabel = create.label(generateWhileLabel());
        }

        labelStack.add(currentLabel);

        super.visit(loopStatement);

        // Add currentlabel if the node has no labels yet
        if (result.labels() == 0) {
            result.addLabel(currentLabel);
        }

        labelStack.remove(labelStack.size() - 1);
    }

    public void visit(ASTSpecial special) {
        // If it is a break/continue without a label, add the current label. Otherwise just copy over the thing
        boolean isAbrupt = special.kind == ASTSpecial.Kind.Break || special.kind == ASTSpecial.Kind.Continue;
        if (isAbrupt && special.args.length == 0) {
            NameExpression currentLabel = labelStack.get(labelStack.size() - 1);
            result = new ASTSpecial(special.kind, new ASTNode[]{ currentLabel });
        } else {
            super.visit(special);
        }
    }
}
