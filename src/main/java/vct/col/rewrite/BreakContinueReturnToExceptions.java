package vct.col.rewrite;

import org.antlr.v4.codegen.model.Loop;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.TryCatchBlock;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.ClassType;

import javax.naming.Name;
import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

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
        super.visit(method);
        if (encounteredReturn) {
            // Wrap body in try-catch
            Method resultMethod = (Method) result;
            TryCatchBlock tryCatchBlock = create.try_catch(create.block(resultMethod.getBody()), null);
            tryCatchBlock.addCatchClause(create.field_decl("e", getExceptionType("result", "")), create.block());
            resultMethod.setBody(tryCatchBlock);

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
        if (!createdReturnExceptionType) {
            getExceptionType("result", "");
            createdReturnExceptionType = true;
            encounteredReturn = true;
        }

        Warning("Not implemented return yet!");
        super.visit(returnStatement);
    }
}
