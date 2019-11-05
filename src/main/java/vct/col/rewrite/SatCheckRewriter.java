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
    public static class AssertOrigin extends BranchOrigin {
        public AssertOrigin(String branch, Origin base){
            super(branch, base);
        }
    }

    public SatCheckRewriter(ProgramUnit source) {
        super(source);
    }

    @Override
    public void visit(Method m) {
        // TODO: Are there any enter/exit methods or something I should call?

        // Find out if contract is intentionally false
        // Since if it is marked as false intentionally we do not have to check it
        Contract contract = m.getContract();
        boolean intentional_false = contract != null && contract.pre_condition.isConstant(false);;

        // If the contract is not intentionally false and the method is not abstract,
        // create a proof obligation that shows the contract is satisfiable
        if (m.getBody() != null && !intentional_false && (m.kind == Method.Kind.Plain || m.kind == Method.Kind.Constructor)) {
            // Create { assert false; } block
            ASTNode my_assert = create.special(ASTSpecial.Kind.Assert, create.constant(false));
            AssertOrigin my_branch = new AssertOrigin("contract satisfiability check", m.getOrigin());
            // TODO: I use the method's origin so I think this is correct?
            my_assert.clearOrigin();
            my_assert.setOrigin(my_branch);
//            OriginWrapper.wrap(null, my_assert, my_branch); // TODO: Previous code was doing this. Why?
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

            blockStatement.setParent(assert_method);

            currentTargetClass.add(assert_method);
        }

        // After visit, result should be equal to the rewritten version of m, i.e. nicely deep cloned
        super.visit(m);
    }

}
