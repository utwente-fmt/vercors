package vct.col.rewrite;

import hre.ast.BranchOrigin;
import hre.ast.Origin;
import scala.reflect.internal.Trees;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.Type;
import vct.col.util.OriginWrapper;

public class SatCheckRewriter extends AbstractRewriter {
    static int i = 0;

    public SatCheckRewriter(ProgramUnit source) {
        super(source);
    }

    @Override
    public void visit(Method m) {
        // Find out if contract is intentionally false
        // Since if it is marked as false intentionally we do not have to check it
        Contract contract = m.getContract();
        boolean intentional_false = contract != null && contract.pre_condition.isConstant(false);;

        // If the contract is not intentionally false and the method is not abstract,
        // create a proof obligation that shows the contract is satisfiable
        if (m.getBody() != null && !intentional_false && (m.kind == Method.Kind.Plain || m.kind == Method.Kind.Constructor)) {
            // Create { assert false; } block
            ASTNode my_assert = create.special(ASTSpecial.Kind.Assert, create.constant(false));
            BranchOrigin my_branch = new BranchOrigin("contract satisfiability check", null);
            OriginWrapper.wrap(null, my_assert, my_branch);
            BlockStatement blockStatement = create.block(my_assert);

            // Create an extra method containing it
            Method assert_method = create.method_kind(
                    m.getKind(),
                    rewrite(m.getReturnType()),
                    rewrite(m.getContract()),
                    "__contract_unsatisfiable__" + m.name(),
                    rewrite(m.getArgs()),
                    m.usesVarArgs(),
                    blockStatement
            );

            currentTargetClass.add(assert_method);
        }

        // After visit, result should be equal to the rewritten version of m, i.e. nicely deep cloned
        super.visit(m);
    }

}
