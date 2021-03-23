package vct.col.rewrite;

import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.IfStatement;
import vct.col.ast.stmt.composite.Switch;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

import java.util.ArrayList;
import java.util.List;
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
        String switchID = Objects.requireNonNull(switchStatement.getLabel(0).getName());
        BlockStatement caseStatements = create.block();

        // Put the case expr in a var s.t. it can be referenced in the if chain
        String exprName = switchID + "_" + counter++;
        currentBlock.add(create.field_decl(exprName, switchStatement.expr.getType(), switchStatement.expr));

        String defaultCaseLabel = generateCaseIDLabel(switchID, "default").getName();

        // Create if chain jumping to all the numbered arms
        // And: collect statements of cases into individual blocks
        ArrayList<IfStatement> ifChain = new ArrayList<>();
        for (int caseID = 0; caseID < switchStatement.cases.length; caseID++) {
            Switch.Case switchCase = switchStatement.cases[caseID];

            BlockStatement stashedBlock = currentBlock;
            currentBlock = create.block();

            if (switchCase.cases.contains(null)) {
                currentBlock.add(create.labelDecl(defaultCaseLabel));
            }

            // Collect all labels into equality expressions
            List<ASTNode> ifGuards = switchCase.cases.stream()
                        .filter(Objects::nonNull)
                        .map(caseExpr -> eq(name(exprName), caseExpr))
                        .collect(Collectors.toList());

            // If there were other labels besides default
            // add if to the chain that jumps to the case statements
            if (ifGuards.size() > 0) {
                NameExpression caseIDLabel = generateCaseIDLabel(switchID, caseID);
                IfStatement nextIf = create.ifthenelse(create.fold(StandardOperator.Or, ifGuards), create.gotoStatement(caseIDLabel));
                ifChain.add(nextIf);
                currentBlock.add(create.labelDecl(caseIDLabel));
            }

            /* Possible improvement: refactor cases to be their own respective AST nodes? instead of
               doing them manually like this. But it seems to work for now. */
            for (ASTNode caseStatement : switchCase.stats) {
                ASTNode resultCaseStatement = rewrite(caseStatement);
                if (resultCaseStatement != null) {
                    currentBlock.add(resultCaseStatement);
                }
            }

            caseStatements.add(currentBlock);
            currentBlock = stashedBlock;
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
            caseStatements.add(create.labelDecl(defaultCaseLabel));
        }

        // Add all the ifs, and then all the case statements the ifs jump to
        // By putting them all within one containing block, all jumps from ifs to casestatements happen within the block
        currentBlock.add(create.block(totalIfChain, caseStatements).labeled(switchID));

        result = null;
    }
}
