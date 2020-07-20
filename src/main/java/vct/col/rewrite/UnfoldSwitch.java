package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

import java.util.ArrayList;
import java.util.Objects;
import java.util.stream.Collectors;

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
        BlockStatement mainBlock = create.block();
        BlockStatement caseStatementsBlock = create.block();
        String switchID = Objects.requireNonNull(switchStatement.getLabel(0).getName());

        // Put the case expr in a var s.t. it can be referenced in the if chain
        String exprName = switchID + "_" + counter++;
        mainBlock.add(create.field_decl(exprName, switchStatement.expr.getType(), switchStatement.expr));

        String defaultCaseLabel = generateCaseIDLabel(switchID, "default").getName();

        // Create if chain jumping to all the numbered arms
        ArrayList<IfStatement> ifChain = new ArrayList<>();
        for (int caseID = 0; caseID < switchStatement.cases.length; caseID++) {
            Switch.Case switchCase = switchStatement.cases[caseID];

            if (switchCase.cases.contains(null)) {
                caseStatementsBlock.add(create.labelDecl(defaultCaseLabel));
            }

            // Fold all "switchValue == labelValue" expressions with "||", skipping default case labels (null)
            ASTNode ifGuard = create.fold(StandardOperator.Or,
                    switchCase.cases.stream()
                        .filter(Objects::nonNull)
                        .map(caseExpr -> eq(name(exprName), caseExpr))
                        .collect(Collectors.toList()));

            // If there was only a default case there is no if guard, and hence it will be the default value "false"
            // (Because of folding or, "||"), and no if will be added then.
            if (!ifGuard.equals(false)) {
                // Add if to the chain that jumps to the case statements
                NameExpression caseIDLabel = generateCaseIDLabel(switchID, caseID);
                IfStatement nextIf = create.ifthenelse(ifGuard, create.gotoStatement(caseIDLabel));
                ifChain.add(nextIf);
                caseStatementsBlock.add(create.labelDecl(caseIDLabel));
            }

            // TODO (Bob): This breaks for nested switches, since we do not call rewrite on the case statements
            // Possible improvement: refactor cases to be their own respective AST nodes? instead of doing them manually like this
            //      But it seems to work for now.
            for (ASTNode caseStatement : switchCase.stats) {
                caseStatementsBlock.add(caseStatement);
            }
        }

        // Jump to default is always the last action, or the only one if there are no value labels
        ASTNode totalIfChain;
        ASTNode defaultJump = create.gotoStatement(defaultCaseLabel);
        if (!ifChain.isEmpty())  {
            ifChain.get(ifChain.size() - 1).addClause(IfStatement.elseGuard(), defaultJump);
            totalIfChain = create.fold(ifChain);
        } else {
            totalIfChain = defaultJump;
        }

        if (!switchStatement.hasDefaultCaseLabel()) {
            // If the switch does not have a default label, the default label was not added anywhere in the caseStatementsBlock.
            // Instead we place it at the end, so the default case does not go to any cases.
            caseStatementsBlock.add(create.labelDecl(defaultCaseLabel));
        }

        // Add all the ifs, and then all the case statements the ifs jump to
        mainBlock.add(totalIfChain);
        mainBlock.add(caseStatementsBlock);

        result = mainBlock;
    }
}
