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

    public NameExpression generateCaseIDLabel(String switchID, int caseID) {
        return generateCaseIDLabel(switchID, caseID + "");
    }

    public NameExpression generateCaseIDLabel(String switchID, String suffix) {
        return create.label(switchID + "_case_" + suffix);
    }

    public void visit(Switch switchStatement) {
        super.visit(switchStatement);

        BlockStatement mainBlock = create.block();
        BlockStatement caseStatementsBlock = create.block();
        String switchID = switchStatement.getLabel(0).getName();

        // Put the case expr in a var s.t. it can be referenced in the if chain
        String exprName = switchID + "_v_" + counter++;
        mainBlock.add(create.field_decl(exprName, switchStatement.expr.getType(), switchStatement.expr));

        // Create if chain jumping to all the numbered arms
        IfStatement ifChain = null;
        boolean encounteredDefault = false;
        for (int caseID = 0; caseID < switchStatement.cases.length; caseID++) {
            Switch.Case switchCase = switchStatement.cases[caseID];
            ASTNode ifGuard = null;
            // Aggregate all the case exprs into one big or expression
            for (ASTNode caseExpr : switchCase.cases) {
                ASTNode comparison;
                if (caseExpr == null) {
                    encounteredDefault = true;
                    caseStatementsBlock.add(create.label_decl(generateCaseIDLabel(switchID, "default")));
                    continue;
                } else {
                    comparison = eq(name(exprName), caseExpr);
                }

                if (ifGuard == null) {
                    ifGuard = comparison;
                } else {
                    ifGuard = or(ifGuard, comparison);
                }
            }
            // TODO (Bob): Use create.fold here instead of manually building the if guard

            // Add if to the chain that jumps to the case statements
            NameExpression caseIDLabel = generateCaseIDLabel(switchID, caseID);
            // If there was only a default case there is no if guard, so we add no if.
            // The if for default will be added at the end, after all the other are added
            if (ifGuard != null) {
                IfStatement nextIf = create.ifthenelse(ifGuard, create.jump(caseIDLabel));
                if (ifChain == null) {
                    ifChain = nextIf;
                    mainBlock.add(ifChain);
                } else {
                    ifChain.addClause(IfStatement.elseGuard(), nextIf);
                    ifChain = nextIf;
                }
            }

            caseStatementsBlock.add(create.label_decl(caseIDLabel));
            for (ASTNode caseStatement : switchCase.stats) {
                caseStatementsBlock.add(caseStatement);
            }
        }

        if (encounteredDefault) {
            ASTNode defaultJump = create.jump(generateCaseIDLabel(switchID, "default"));
            if (ifChain == null) {
                // No other cases, just the default case
                mainBlock.add(defaultJump);
            } else {
                // Other cases, add the default case as a last case
                ifChain.addClause(IfStatement.elseGuard(), defaultJump);
            }
        }

        // Add all the statements for the cases at the end, when it's sure that the if-chain is added
        mainBlock.add(caseStatementsBlock);

        result = mainBlock;
    }
}
