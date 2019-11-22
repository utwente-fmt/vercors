package vct.col.rewrite;

import org.antlr.v4.codegen.model.Loop;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ContractBuilder;
import viper.silver.ast.Declaration;

import javax.naming.Name;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

public class BreakContinueReturnToExceptions extends AbstractRewriter {
    private Set<String> breakLabels = new HashSet<>();
    private boolean encounteredReturn = false;
    private Set<String> exceptionTypes = new HashSet<>();

    boolean createdReturnExceptionType = false;

    public BreakContinueReturnToExceptions(ProgramUnit source) {
        super(source);
    }

    public ClassType getExceptionType(String prefix, String id) {
        String name = "__" + prefix + "_" + id + "_ex";

        if (!exceptionTypes.contains(name)) {
            target().add(create.new_class(
                    name,
                    null,
                    create.class_type(new String[]{"java", "lang", "Exception"}
                    )));
            exceptionTypes.add(name);
        }

        return create.class_type(name);
    }

    public void post_visit(ASTNode node) {
        // Wrap statement in try-catch if one of the labels is broken to
        // Collect all used break labels within statement
        ArrayList<NameExpression> usedLabels = new ArrayList<>();
        for (NameExpression label : node.getLabels()) {
            if (breakLabels.contains(label.getName())) {
                usedLabels.add(label);
                breakLabels.remove(label.getName());
            }
        }

        if (usedLabels.size() > 0) {
            // There were breaks involved! Add catch clauses
            TryCatchBlock tryCatchBlock = create.try_catch(create.block(result), null);
            for (NameExpression label : usedLabels) {
                tryCatchBlock.addCatchClause(create.field_decl("e", getExceptionType("break", label.getName())), create.block());
            }

            if (result instanceof LoopStatement) {
                ((LoopStatement) result).fixate();
            }

            result = tryCatchBlock;
        }

        super.post_visit(node);
    }

    public void visit(Method method) {
        switch(method.kind){
            case Predicate:
            case Pure:
                result=copy_rw.rewrite(method);
                return;
            default:
                break;
        }

        super.visit(method);
        Method resultMethod = (Method) result;

        if (encounteredReturn) {
            // Wrap body in try-catch
            TryCatchBlock tryCatchBlock = create.try_catch(create.block(resultMethod.getBody()), null);
            tryCatchBlock.addCatchClause(create.field_decl("e", getExceptionType("return", "")), create.block());
            resultMethod.setBody(tryCatchBlock);

            encounteredReturn = false;
        }

        if (!method.getReturnType().isVoid()) {
            // Add sys__result param on the front
            ArrayList<DeclarationStatement> args = new ArrayList<DeclarationStatement>(Arrays.asList(resultMethod.getArgs()));

            DeclarationStatement sys__result = new DeclarationStatement("sys__result",rewrite(method.getReturnType()));
            sys__result.setOrigin(method);
            sys__result.setFlag(ASTFlags.OUT_ARG, true);
            args.add(0, sys__result);

            resultMethod.setArgs(args.toArray(new DeclarationStatement[0]));
        }

        if (breakLabels.size() != 0) {
            Warning("Some break labels were not deleted, even though they should be. This indicates a logic error.");
        }
    }

    public void visit(ASTSpecial special) {
        switch (special.kind) {
            default:
                super.visit(special);
                break;
            case Break:
                visitBreak(special);
                break;
            case Continue:
                Abort("Not supported");
                break;
        }
    }

    public void visitBreak(ASTSpecial breakStatement) {
        String id = ((NameExpression) breakStatement.args[0]).getName();
        breakLabels.add(id);
        result = create.special(ASTSpecial.Kind.Throw, create.new_object(getExceptionType("break", id)));
    }

    @Override
    public void visit(ReturnStatement returnStatement) {
        encounteredReturn = true;

        ASTNode expr = returnStatement.getExpression();
        BlockStatement block = create.block();
        if (expr != null){
            block.add(create.assignment(create.local_name("sys__result"),rewrite(expr)));
        }
        for(ASTNode n : returnStatement.get_after()){
            block.add(rewrite(n));
        }

        ASTSpecial returnThrow = create.special(ASTSpecial.Kind.Throw, create.new_object(getExceptionType("return", "")));
        block.add(returnThrow);

        result = block ;
    }

    public void visit(NameExpression e){
        if (e.isReserved(ASTReserved.Result)){
            result=create.unresolved_name("sys__result");
        } else {
            super.visit(e);
        }
    }

    public void visit(MethodInvokation e){
        switch(e.getDefinition().kind){
            case Predicate:
            case Pure:
                super.visit(e);
                return;
        }

        // Non-void case should be handled by
        if (!e.getDefinition().getReturnType().isVoid()){
            Fail("unexpected invokation of non-void method %s at %s",e.method,e.getOrigin());
        }

        int N=e.getArity();
        ASTNode args[]=new ASTNode[N+1];
        args[0]=create.local_name("sys__thrown");
        for(int i=0;i<N;i++){
            args[i+1]=rewrite(e.getArg(i));
        }
        MethodInvokation res=create.invokation(rewrite(e.object), rewrite(e.dispatch) , e.method , args );
        for(NameExpression lbl:e.getLabels()){
            Debug("VOIDCALLS: copying label %s",lbl);
            res.addLabel(rewrite(lbl));
        }
        res.set_before(rewrite(e.get_before()));
        res.set_after(rewrite(e.get_after()));
        result=res;
    }

    public void visit(DeclarationStatement declarationStatement) {
        if (!declarationStatement.init().isEmpty()) {
            Abort("Declaration statement with init was not unfolded");
        }

        super.visit(declarationStatement);
    }

    public void visit(AssignmentStatement assign){
        if (assign.expression() instanceof MethodInvokation){
            MethodInvokation invokation = (MethodInvokation) super.visit(assign.expression);
            AssignmentStatement resultAssign = (AssignmentStatement) result;

            MethodInvokation invocation = (MethodInvokation) assign.expression();
            if (invocation.getDefinition().kind == Method.Kind.Plain) {
                MethodInvokation resultInvocation = (MethodInvokation) resultAssign.expression();
                ArrayList<ASTNode> args = new ArrayList<ASTNode>(Arrays.asList(resultInvocation.getArgs()));
                args.add(0, resultAssign.location());
                resultInvocation.setArgs(args.toArray(new ASTNode[0]));

                result = invocation;
            }
        } else {
            super.visit(assign);
        }
    }
}
