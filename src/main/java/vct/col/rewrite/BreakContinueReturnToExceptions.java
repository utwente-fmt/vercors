package vct.col.rewrite;

import org.antlr.v4.codegen.model.Loop;
import scala.reflect.internal.Trees;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import vct.col.ast.util.ContractBuilder;
import vct.util.ClassName;
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

    public BreakContinueReturnToExceptions(ProgramUnit source) {
        super(source);
    }

    public ClassType getExceptionType(String prefix, String id) {
        return getExceptionType(prefix, id, new PrimitiveType(PrimitiveSort.Void));
    }

    public ClassType getExceptionType(String prefix, String id, Type arg) {
        String name = "__" + prefix + "_" + id + "_ex";

        if (!exceptionTypes.contains(name)) {
            ASTClass exceptionClass = create.new_class(
                    name,
                    null,
                    // Uncomment to turn on inheritance of exceptions
                    // create.class_type(new String[]{"java", "lang", "Exception"})
                    null
                    );

            if (arg != null && !arg.isVoid()) {
                // TODO (Bob): Add contract that adds ensures \result == arg;
                // TODO (Bob): Permissions
                Method exceptionConstructor = create.method_kind(
                        Method.Kind.Constructor,
                        create.primitive_type(PrimitiveSort.Void),
                        new ContractBuilder().getContract(),
                        name,
                        new DeclarationStatement[] {
                                create.field_decl("result", arg)
                        },
                        null
                );
                exceptionClass.add(exceptionConstructor);
            } else {
                create.addZeroConstructor(exceptionClass);
            }

            target().add(exceptionClass);
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
        super.visit(method);
        Method resultMethod = (Method) result;

        if (encounteredReturn) {
            TryCatchBlock tryCatchBlock = create.try_catch(create.block(resultMethod.getBody()), null);

            ClassType exceptionType = getExceptionType("return", method.getName());
            ClassName exceptionClassName = new ClassName(exceptionType.getNameFull());
            ASTNode getReturnValueExpr = create.get_field(exceptionClassName, create.field_name("e"), "value");
            ReturnStatement returnStatement = create.return_statement(getReturnValueExpr);
            BlockStatement returnBlock = create.block(create.return_statement());
            tryCatchBlock.addCatchClause(create.field_decl("e", exceptionType), create.block(returnStatement));
            resultMethod.setBody(create.block(tryCatchBlock));

            encounteredReturn = false;
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

        // TODO (Bob): Account for overloading?
        ASTSpecial returnThrow = null;
        if (expr != null){
            ClassType exceptionType = getExceptionType("return", current_method().getName(), current_method().getReturnType());
            returnThrow = create.special(ASTSpecial.Kind.Throw, create.new_object(exceptionType, rewrite(expr)));
        } else {
            ClassType exceptionType = getExceptionType("return", current_method().getName());
            returnThrow = create.special(ASTSpecial.Kind.Throw, create.new_object(exceptionType));
        }
       result = returnThrow;

        // TODO: Add this in the catch clause?
        // for(ASTNode n : returnStatement.get_after()){
        //     block.add(rewrite(n));
        // }
    }
}
