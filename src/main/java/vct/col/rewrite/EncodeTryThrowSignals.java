package vct.col.rewrite;

import com.google.common.collect.Lists;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.CatchClause;
import vct.col.ast.stmt.composite.IfStatement;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;

import java.util.ArrayList;
import java.util.HashMap;

import static vct.col.rewrite.AddTypeADT.type_adt;
import static vct.col.rewrite.IntroExcVar.excVar;

public class EncodeTryThrowSignals extends AbstractRewriter {

    public EncodeTryThrowSignals(ProgramUnit source) {
        super(source);
    }

    // Object here because CatchClause is not an ASTNode. But if this ever changes
    // it should probably be an ASTNode.
    HashMap<Object, String> entryLabels = new HashMap<>();
    HashMap<Object, String> postLabels = new HashMap<>();

    int counter;

    /**
     * Holds the most recently encountered label. Since we traverse in a post-order, this variable
     * always holds the label of the next finally or catch clause that a throw should jump to.
     */
    String nearestHandlerLabel = null;

    /**
     * Holds the name of the variable in the current signals clause. If not in a signals clause, should be null.
     * Used to replace the variable when it is encountered with the excVar variable.
     */
    String currentExceptionVarName = null;

    public String generateLabel(String prefix, String id) {
        return prefix + "_" + id + "_" + counter++;
    }

    public String generateLabel(String prefix) {
        return prefix + "_" + counter++;
    }

    /**
     * Generates entry labels for all catches and the finally clause of tryCatchBlock.
     * Generates post labels (i.e. labels supposed to be located directly after a statement) for tryCatchBlock.
     * Labels are saved in entryLabels and postLabels and keyed to the input AST elements.
     * They are saved in a hashmap because the counter needed to keep the labels unique is stateful.
     * @param tryCatchBlock
     */
    public void generateLabels(TryCatchBlock tryCatchBlock) {
        for (CatchClause catchClause : tryCatchBlock.catches()) {
            String label = generateLabel("catch", catchClause.decl().getType().toString());
            entryLabels.put(catchClause, label);
        }

        if (tryCatchBlock.after() != null) {
            String label = generateLabel("finally");
            entryLabels.put(tryCatchBlock.after(), label);
        }

        String label = generateLabel("try", "end");
        postLabels.put(tryCatchBlock, label);
    }

    /**
     * Returns the catch clause after catchClause, or null if it is the last
     */
    public CatchClause nextCatch(TryCatchBlock tryCatchBlock, CatchClause currentCatchClause) {
        boolean encounteredCurrentClause = false;

        for (CatchClause catchClause : tryCatchBlock.catches()) {
            if (encounteredCurrentClause) {
                return catchClause;
            } else if (catchClause == currentCatchClause) {
                encounteredCurrentClause = true;
            }
        }

        return null;
    }

    public void visit(TryCatchBlock tryCatchBlock) {
        generateLabels(tryCatchBlock);

        ArrayList<CatchClause> catchClauses = Lists.newArrayList(tryCatchBlock.catches());

        String oldNearestHandlerLabel = null;
        oldNearestHandlerLabel = nearestHandlerLabel;
        if (catchClauses.size() > 0) {
            nearestHandlerLabel = entryLabels.get(catchClauses.get(0));
        } else {
            // By definition if there are no catches there should be a finally clause. (disregarding try-with-resources)
            nearestHandlerLabel = entryLabels.get(tryCatchBlock.after());
        }

        visitTryBody(tryCatchBlock);

        nearestHandlerLabel = oldNearestHandlerLabel;

        for (CatchClause cc : catchClauses) {
            visitCatch(tryCatchBlock, cc);
        }

        visitFinally(tryCatchBlock);

        currentBlock.add(create.label_decl(postLabels.get(tryCatchBlock)));

        result = null;
    }

    public void visitTryBody(TryCatchBlock tryCatchBlock) {
        BlockStatement newMain = rewrite(tryCatchBlock.main());
        if (tryCatchBlock.after() != null) {
            newMain.add(create.jump(entryLabels.get(tryCatchBlock.after())));
        } else {
            newMain.add(create.jump(postLabels.get(tryCatchBlock)));
        }
        currentBlock.add(newMain);
    }

    public void visitCatch(TryCatchBlock tryCatchBlock, CatchClause catchClause) {
        // Since the declaration statement identifier cannot leak to the outside scope,
        // we do a pre_visit here, so a post_visit at the end will remove the name
        // from the scope
        pre_visit(catchClause.block());

        BlockStatement totalBlock = currentBlock;
        currentBlock = create.block();

        /* Generate the following code at the beginning of the clause:
            if (!(sys__exc instanceof type_of_catch_clause)) {
                jump next_catch_clause  // (or finally_clause if there are no more catch clauses to try)
                                        // (or nearestHandlerLabel if there is also no finally)
            }
         */
        String fallbackHandler;
        CatchClause nextCatchClause = nextCatch(tryCatchBlock, catchClause);
        if (nextCatchClause != null) {
            fallbackHandler = entryLabels.get(nextCatchClause);
        } else if (tryCatchBlock.after() != null) {
            fallbackHandler = entryLabels.get(tryCatchBlock.after());
        } else {
            fallbackHandler = nearestHandlerLabel;
        }
        if (nearestHandlerLabel == null) {
            // TODO: What about a top-level handler? or just set it at method entry?
            Abort("Nearesthandlerlabel was null, even though there should have been a handler!");
        }

        currentBlock.add(create.label_decl(entryLabels.get(catchClause)));

        currentBlock.add(create.ifthenelse(
                create.expression(StandardOperator.Not,
                    create.invokation(create.class_type(type_adt), null,"instanceof",
                            create.expression(StandardOperator.TypeOf,create.local_name(excVar)),
                            create.invokation(create.class_type(type_adt),null,"class_" + catchClause.decl().type())
                            )
                    ),
                create.jump(fallbackHandler)
                ));

        DeclarationStatement argument = rewrite(catchClause.decl());
        currentBlock.add(argument);

        currentBlock.append(rewrite(catchClause.block()));

        String targetLabel;
        if (tryCatchBlock.after() != null) {
            targetLabel = entryLabels.get(tryCatchBlock.after());
        } else {
            targetLabel = postLabels.get(tryCatchBlock);
        }
        currentBlock.add(create.jump(targetLabel));

        totalBlock.add(currentBlock);
        currentBlock = totalBlock;

        post_visit(catchClause.block());
    }

    public void visitFinally(TryCatchBlock tryCatchBlock) {
        if (tryCatchBlock.after() == null) {
            return;
        }

        BlockStatement totalBlock = currentBlock;
        currentBlock = create.block();

        currentBlock.add(create.label_decl(entryLabels.get(tryCatchBlock.after())));
        currentBlock.append(rewrite(tryCatchBlock.after()));
        currentBlock.add(create.jump(postLabels.get(tryCatchBlock)));

        totalBlock.add(currentBlock);
        currentBlock = totalBlock;
    }

    public void visit(ASTSpecial special) {
        if (special.kind != ASTSpecial.Kind.Throw) {
            super.visit(special);
            return;
        }

        currentBlock.add(create.assignment(create.local_name(excVar), rewrite(special.args[0])));
        currentBlock.add(create.jump(nearestHandlerLabel));
        result = null;
    }

    public void visit(Method method) {
        if (!(method.getKind() == Method.Kind.Constructor || method.getKind() == Method.Kind.Plain)) {
            super.visit(method);
            return;
        }

        if (method.getBody() == null) {
            super.visit(method);
        } else {
            if (nearestHandlerLabel != null) {
                // This might not be true once we can nest methods (or classes). If that's the case adapt it or delete it
                Abort("Nearesthandlerlabel was not null, even though we are entering a fresh method!");
            }

            // TODO (Bob): What about overloading? This should be handled in another phase, but currently I am not sure...
            String unhandledExceptionHandler = generateLabel("method_end", method.getName());
            nearestHandlerLabel = unhandledExceptionHandler;

            super.visit(method);

            nearestHandlerLabel = null;

            Method resultMethod = (Method) result;
            BlockStatement methodBody = (BlockStatement) resultMethod.getBody();
            methodBody.add(create.label_decl(unhandledExceptionHandler));
        }

        // Encode signals & adjust ensures
        Method resultMethod = (Method) result;
        Contract contract = resultMethod.getContract();

        if (contract.signals.length > 0) {
            ASTNode newPostCondition = create.expression(StandardOperator.Implies,
                    create.expression(StandardOperator.EQ, create.local_name(excVar), create.reserved_name(ASTReserved.Null)),
                    contract.post_condition
                    );

            ASTNode flattenedSignals = flattenSignals(contract.signals);

            ASTNode excVarTypeConstrain = create.expression(StandardOperator.Implies,
                    create.expression(StandardOperator.NEQ, create.local_name(excVar), create.reserved_name(ASTReserved.Null)),
                    constrainExcPostcondition(contract.signals)
                    );

            resultMethod.setContract(new Contract(
                    contract.given,
                    contract.yields,
                    contract.modifies,
                    contract.accesses,
                    contract.invariant,
                    contract.pre_condition,
                    create.expression(StandardOperator.Star,
                            newPostCondition,
                            create.expression(StandardOperator.Star, flattenedSignals, excVarTypeConstrain)
                    )
            ));
        }
    }

    /**
     * Creates an expression that is true if the type of excVar is an instance of one of the signal's clauses types.
     * I.e. excVar instanceof signalsType0 || excVar instanceof signalsType1 || .. || excVar instanceof signalsTypeN-1
     * It uses the instanceof encoding in AddTypeADT
     */
    private ASTNode constrainExcPostcondition(SignalsClause[] signals) {
        if (signals.length == 0) {
            return create.constant(true);
        }

        ASTNode constraint = AddTypeADT.expr_instanceof(create, copy_rw, create.local_name(excVar), (ClassType) signals[0].type());

        for (int i = 1; i < signals.length; i++) {
            constraint = create.expression(StandardOperator.Or,
                    constraint,
                    AddTypeADT.expr_instanceof(create, copy_rw, create.local_name(excVar), (ClassType) signals[0].type()));
        }

        return constraint;
    }

    public void visit(NameExpression e){
        if (currentExceptionVarName != null && e.getName().equals(currentExceptionVarName)) {
            result=create.local_name(excVar);
        } else {
            super.visit(e);
        }
    }

    private ASTNode flattenSignals(SignalsClause[] signals) {
        if (signals.length == 0) {
            return create.constant(true);
        }

        ASTNode flattenedSignals = flattenSignals(signals[0]);

        for (int i = 1; i < signals.length; i++) {
            flattenedSignals = create.expression(StandardOperator.Star, flattenedSignals, flattenSignals(signals[i]));
        }

        return flattenedSignals;
    }

    private ASTNode flattenSignals(SignalsClause signals) {
        currentExceptionVarName = signals.name();

        ASTNode condition = rewrite(signals.condition());

        currentExceptionVarName = null;

        return create.expression(StandardOperator.Implies,
                AddTypeADT.expr_instanceof(create, copy_rw, create.local_name(excVar), (ClassType) signals.type()),
                condition
                );
    }

    public void visit(MethodInvokation invokation) {
        super.visit(invokation);

        Method.Kind methodKind = invokation.getDefinition().getKind();
        if (!(methodKind == Method.Kind.Plain || methodKind == Method.Kind.Constructor)) {
            return;
        }

        if (!invokation.getDefinition().getReturnType().isVoid()) {
            Abort("Non-void method invokation can only be used as expression of assignment");
        }

        // If there's no contract we do not need to account for exceptions being thrown
        /* TODO (Bob): Signals is leading, throws needs to be compiled away in the java phase! Also below!
         */
        SignalsClause[] signals = invokation.getDefinition().getContract().signals;
        if (signals == null || signals.length == 0) {
            return;
        }

        // Non-void method invokation that is a statement: just add the invokation and append an if statement
        currentBlock.add(result);
        result = null;
        currentBlock.add(createExceptionCheck(nearestHandlerLabel));
    }

    public void visit(AssignmentStatement assignment) {
        // First rewrite the assignment statement, careful not to trigger the visit(MethodInvokation) we defined above
        if (assignment.expression() instanceof MethodInvokation) {
            MethodInvokation invokation = (MethodInvokation) assignment.expression();
            super.visit(invokation);
            MethodInvokation resultInvokation = (MethodInvokation) result;

            ASTNode resultLocation = assignment.location().apply(this);

            result = create.assignment(resultLocation, resultInvokation);

            // Then, if exceptions are involved, insert a check that possibly jumps to a handler
            // TODO (Bob): Use can throw here or smth
            if (invokation.getDefinition().getContract().signals.length > 0) {
                currentBlock.add(result);
                result = null;
                currentBlock.add(createExceptionCheck(nearestHandlerLabel));
            }
        } else {
            super.visit(assignment);
        }
    }

    public IfStatement createExceptionCheck(String handlerLabel) {
        return create.ifthenelse(
                    create.expression(StandardOperator.NEQ,
                            create.local_name(excVar),
                            create.reserved_name(ASTReserved.Null)
                    ),
                    create.jump(handlerLabel)
            );
    }
}
