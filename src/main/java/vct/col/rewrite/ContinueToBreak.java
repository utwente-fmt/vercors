package vct.col.rewrite;

import org.antlr.v4.codegen.model.Loop;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.Switch;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ContinueToBreak extends AbstractRewriter {
    Set<NameExpression> continueLabels = new HashSet<>();

    public ContinueToBreak(ProgramUnit source) {
        super(source);
    }

    public void visit(LoopStatement loopStatement) {
        super.visit(loopStatement);

        ArrayList<NameExpression> usedLabels = new ArrayList<>();
        for (NameExpression loopLabel : loopStatement.getLabels()) {
            if (continueLabels.contains(loopLabel)) {
                usedLabels.add(loopLabel);
                continueLabels.remove(loopLabel);
            }
        }

        if (usedLabels.size() > 0) {
            LoopStatement resultLoop = (LoopStatement) result;
            ASTNode inner = resultLoop.getBody();
            ASTNode loopBody = create.block(inner);
            for (NameExpression usedLabel : usedLabels) {
                inner.addLabel(create.label(usedLabel.getName() + "_continue"));
            }
            resultLoop.setBody(loopBody);
        }
    }

    public void visit(Method method){
        super.visit(method);
        if (continueLabels.size() != 0) {
            Warning("Not all continue labels were used. This indicates a logic error");
        }
    }

    public void visit(ASTSpecial special) {
        // If it is a break/continue without a label add the current label. Otherwise just copy over the thing.
        if(special.kind != ASTSpecial.Kind.Continue) {
            super.visit(special);
            return;
        }

        NameExpression labelTarget = (NameExpression) special.args[0];
        continueLabels.add(labelTarget);
        result = create.special(ASTSpecial.Kind.Break, create.unresolved_name(labelTarget.getName() + "_continue"));
    }
}
