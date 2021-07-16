package vct.parsers.rewrite;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.AbstractRewriter;

import java.util.HashMap;

/**
 * Rewrite a Java AST, produced by parsing, to conform to the COL AST standard.
 */
public class JavaPostProcessor extends AbstractRewriter {

    private int wildcard_count = 0;

    public JavaPostProcessor(ProgramUnit source) {
        super(source);
    }

    @Override
    public void visit(ASTSpecial s) {
        switch (s.kind) {
            case ActionHeader:
                Fail("cannot create block around action", s.getOrigin());
                break;
            default:
                super.visit(s);
                break;
        }
    }

    @Override
    public void visit(ClassType t) {
        String[] name = t.getNameFull();
        if (name.length == 1 && name[0].equals("String")) {
            result = create.primitive_type(PrimitiveSort.String);
        } else {
            super.visit(t);
        }
    }

    @Override
    public void visit(ASTClass c) {
        super.visit(c);
        ASTClass decl = (ASTClass) result;
        int N = 0;
        for (Method m : decl.dynamicMethods()) {
            if (m.kind == Method.Kind.Constructor) N++;
        }
        if (N == 0 && c.kind != ASTClass.ClassKind.Interface) create.addZeroConstructor(decl);
    }

    @Override
    public void visit(BlockStatement b) {
        if (b.size() > 0 && b.get(0).isSpecial(Kind.ActionHeader)) {
            ASTSpecial decl = (ASTSpecial) b.get(0);
            ASTNode history = rewrite(decl.args[0]);
            ASTNode fraction = rewrite(decl.args[1]);
            ASTNode process = rewrite(decl.args[2]);
            ASTNode action = rewrite(decl.args[3]);
            HashMap<String, ASTNode> map = new HashMap<String, ASTNode>();
            for (int i = 4; i < decl.args.length; i += 2) {
                String field = decl.args[i].toString();
                ASTNode frac = rewrite(decl.args[i + 1]);
                map.put(field, frac);
            }
            BlockStatement block = create.block();
            int N = b.size();
            for (int i = 1; i < N; i++) {
                block.add(rewrite(b.get(i)));
            }
            result = create.action_block(history, fraction, process, action, map, block);
        } else {
            super.visit(b);
        }
    }
}
