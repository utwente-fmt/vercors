package vct.col.rewrite;

import hre.util.MultiNameSpace;
import vct.col.ast.expr.BindingExpression;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.composite.IfStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

import java.util.Iterator;

public class SilverReorder extends AbstractRewriter {

    MultiNameSpace<String, String> locals = new MultiNameSpace<String, String>();
    private int count = 0;
    private BlockStatement main_block = null;

    public SilverReorder(ProgramUnit source) {
        super(source);
    }

    @Override
    public void visit(IfStatement s) {
        Debug("rewriting if");
        BlockStatement tmp = main_block;
        if (tmp == null) {
            main_block = currentBlock;
        }
        super.visit(s);
        main_block = tmp;
    }


    @Override
    public void visit(NameExpression n) {
        switch (n.getKind()) {
            case Local:
            case Argument: {
                Debug("lookup %s", n.getName());
                Iterator<String> names = locals.lookup(n.getName());
                if (!names.hasNext()) {
                    Debug("not found");
                    result = create.local_name(n.getName());
                } else {
                    String res = names.next();
                    Debug("found %s", res);
                    result = create.local_name(res);
                }
                break;
            }
            case Label: {
                Debug("lookup %s", n.getName());
                Iterator<String> names = locals.lookup(n.getName());
                if (!names.hasNext()) {
                    Debug("not found");
                    result = create.label(n.getName());
                } else {
                    String res = names.next();
                    Debug("found %s", res);
                    Warning("mapping label %s to local", n.getName());
                    result = create.local_name(res);
                }
                break;
            }
            case Unresolved: {
                Debug("lookup %s", n.getName());
                Iterator<String> names = locals.lookup(n.getName());
                if (!names.hasNext()) {
                    Debug("not found");
                    result = create.unresolved_name(n.getName());
                } else {
                    String res = names.next();
                    Debug("found %s", res);
                    result = create.unresolved_name(res);
                }
                break;
            }
            default:
                super.visit(n);
                break;
        }
    }

    @Override
    public void visit(DeclarationStatement d) {
        if (currentBlock != null) {
            String name = d.name();
            count++;
            DeclarationStatement res = create.field_decl(
                    name + "__" + count,
                    rewrite(d.getType()),
                    rewrite(d.initJava()));
            Debug("mapping %s", name);
            locals.add(name, name + "__" + count);
            if (main_block != null) {
                Debug("moving decl %s", d.name());
                main_block.prepend(res);
                result = null;
            } else {
                result = res;
            }
        } else {
            super.visit(d);
        }
    }

    @Override
    public void visit(BindingExpression e) {
        BlockStatement tmp = main_block;
        main_block = null;
        locals.enter();
        super.visit(e);
        locals.leave();
        main_block = tmp;
    }

    @Override
    public void visit(LoopStatement S) {
        locals.enter();
        BlockStatement tmp = main_block;
        if (tmp == null) {
            main_block = currentBlock;
        }
        super.visit(S);
        main_block = tmp;
        locals.leave();
    }

    @Override
    public void visit(Method m) {
        locals.enter();
        super.visit(m);
        locals.leave();
    }

    public void visit(BlockStatement block) {
        locals.enter();
        BlockStatement tmp = main_block;
        if (tmp == null) {
            main_block = currentBlock;
        }
        super.visit(block);
        main_block = tmp;
        locals.leave();
    }

}
