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
        super.visit(m);

        // If there is no contract it cannot be false
        // If there is no body it's an abstract method, and hence the user is allowed to create unsoundness
        Contract contract = m.getContract();
        if (contract == null || m.getBody() == null) {
            return;
        }

        // Do not sat-check if there is a false literal in the contract
        boolean intentional_false = contract.pre_condition.isConstant(false);

        if (!intentional_false && (m.kind == Method.Kind.Plain || m.kind == Method.Kind.Constructor)) {
            ASTNode my_assert = create.special(ASTSpecial.Kind.Assert, create.constant(false));

            // We want to use contract origin, but that is null at the moment (probably because of a bug)
            // So until then we just use the method origin.
            Origin origin = m.getOrigin();
            if (contract.getOrigin() != null) {
                origin = contract.getOrigin();
            }
            // This way we tag the assert we produced. These are then collected in SilverStatementMap.
            // By collecting them later we allow other passes to delete them or modify them
            // In SilverBackend we check if they indeed all error. The ones that do not error, we report as error
            AssertOrigin my_branch = new AssertOrigin("contract satisfiability check", origin);

            my_assert.clearOrigin();
            my_assert.setOrigin(my_branch);

            BlockStatement blockStatement = create.block(my_assert);

            // Create method that will serve as a proof obligation for the satisfiability of the contract
            Method assert_method = create.method_kind(
                    m.getKind(),
                    rewrite(m.getReturnType()),
                    rewrite(m.getContract()),
                    "__contract_unsatisfiable__" + m.name(),
                    rewrite(m.getArgs()),
                    m.usesVarArgs(),
                    blockStatement
            );
            assert_method.setStatic(m.isStatic());

            blockStatement.setParent(assert_method);

            target().add(assert_method);
        }
    }

}
