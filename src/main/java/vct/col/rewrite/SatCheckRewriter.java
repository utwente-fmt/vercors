package vct.col.rewrite;

import hre.ast.AssertOrigin;
import hre.ast.Origin;
import hre.ast.RestOfContractOrigin;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.AbstractRewriter;

public class SatCheckRewriter extends AbstractRewriter {

    public SatCheckRewriter(ProgramUnit source) {
        super(source);
    }

    public void visit(ASTClass cls) {
        for(Method method : cls.dynamicMethods()) {
            if(method.getReturnType().isPrimitive(PrimitiveSort.Process)) {
                result = copy_rw.rewrite(cls);
                return;
            }
        }

        super.visit(cls);
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
            RestOfContractOrigin restOfContractOrigin = new RestOfContractOrigin("origin indicating the rest of the contract. should be ignored if there is an error", origin);
            AssertOrigin my_branch = new AssertOrigin("contract satisfiability check", origin, restOfContractOrigin);
            my_assert.clearOrigin();
            my_assert.setOrigin(my_branch);

            BlockStatement blockStatement = create.block();

            // Inhale precondition, invariant...
            // Switched to inhales here (instead of just removing post condition from the contract) because I thought
            // it would be easier to add special tracking origins. After implementing it it probably wouldn't have been
            // hard to just leave it in the contract (setting a RestOfContractOrigin on the contract probably would've been
            // enough?) but this seems to work well too. I guess this is less backend agnostic, so if we ever
            // ditch viper this will have to change.
            // And we also tag this inhale so we can detect errors caused by the contract
            // and if the contract errors we shouldn't expect the assert error to show up
            ASTSpecial contract_inhale = create.special(ASTSpecial.Kind.Inhale,
                    create.expression(
                            StandardOperator.Star,
                            rewrite(m.getContract().invariant),
                            rewrite(m.getContract().pre_condition)
                    ));
            contract_inhale.clearOrigin();
            contract_inhale.setOrigin(restOfContractOrigin);
            blockStatement.add(contract_inhale);

            // Assert statement that's supposed to be unable to be verified, indicating the precondition
            // is satisfiable
            blockStatement.add(my_assert);

            // Inhale false, to ensure that even though we skim the contract the postcondition really is not checked
            ASTSpecial my_inhale = create.special(ASTSpecial.Kind.Inhale, create.constant(false));
            blockStatement.add(my_inhale);

            // Create method that will serve as a proof obligation for the satisfiability of the contract
            Method assert_method = create.method_kind(
                    m.getKind(),
                    rewrite(m.getReturnType()),
                    rewrite(skimContract(m.getContract())),
                    "__contract_unsatisfiable__" + m.name(),
                    rewrite(m.getArgs()),
                    m.usesVarArgs(),
                    blockStatement
            );

            blockStatement.setParent(assert_method);

            currentTargetClass.add(assert_method);
        }
    }

    /*
     * Skims the contract down to the parts needed to check if the function can even
     * be called. Invariant gets added to only the precondition.
     */
    Contract skimContract(Contract contract) {
        return new Contract(
                contract.given,
                contract.yields,
                contract.modifies,
                contract.accesses,
                Contract.default_true,
                Contract.default_true,
                Contract.default_true, // The postcondition itself
                new DeclarationStatement[0]        // And the signals clauses, since they encode to postconditions as well
        );
    }

}
