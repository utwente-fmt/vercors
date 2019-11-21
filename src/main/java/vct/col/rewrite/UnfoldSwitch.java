package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

public class UnfoldSwitch extends AbstractRewriter {
    int counter = 0;

    public UnfoldSwitch(ProgramUnit source) {
        super(source);
    }

    public void visit(Switch switchStatement) {
        super.visit(switchStatement);

        BlockStatement block = create.block();
        String switch_id = switchStatement.getLabel(0).getName();

        // Put the case expr in a var s.t. it can be referenced in the if chain
        String exprName = switch_id + "_v_" + counter++;
        block.add(create.field_decl(exprName, switchStatement.expr.getType(), switchStatement.expr));

        // Create if chain jumping to all the numbered arms
        IfStatement ifChain = null;
        int case_id = 0;
        for (Switch.Case switchCase : switchStatement.cases) {
            ASTNode ifGuard = null;
            // Aggregate all the case exprs into one big or expression
            for (ASTNode caseExpr : switchCase.cases) {
                ASTNode comparison;
                if (caseExpr == null) { // This way, when a default clause is among the other clauses, it is always taken
                    comparison = create.constant(true);
                } else {
                    comparison = eq(name(exprName), caseExpr);
                }

                if (ifGuard == null) {
                    ifGuard = comparison;
                } else {
                    ifGuard = or(ifGuard, comparison);
                }
            }

            // Add if to the chain that jumps to the case statements
            NameExpression armLabel = create.label(switch_id + "_case_" + case_id);
            IfStatement nextIf = create.ifthenelse(ifGuard, create.jump(armLabel));
            if (ifChain == null) {
                ifChain = nextIf;
                // Because we put the innermost if in ifChain, add the ifChain here so the top-level one is added to the block
                block.add(ifChain);
            } else {
                ifChain.addClause(IfStatement.elseGuard(), nextIf);
                ifChain = nextIf;
            }

            // Since we already added the root of the if chain, we can start adding the statements of all the cases after it
            block.add(create.label_decl(armLabel));
            for (ASTNode caseStatement : switchCase.stats) {
                block.add(caseStatement);
            }

            case_id += 1;
        }

        result = block;
    }
}
