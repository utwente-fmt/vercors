package vct.col.ast.util;

import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;

import java.util.Map;

public class MultiSubstitution extends AbstractRewriter {

    Map<String, Type> type_map;

    public MultiSubstitution(ProgramUnit source, Map<String, Type> type_map) {
        super(source);
        this.type_map = type_map;
    }

    public void visit(Method m) {
        result = create.method_decl(
                rewrite(m.getReturnType()),
                copy_rw.rewrite(m.getContract()),
                m.getName(),
                copy_rw.rewrite(m.getArgs()),
                copy_rw.rewrite(m.getBody())
        );
    }

    public void visit(ClassType t) {
        String name[] = t.getNameFull();
        if (name.length == 1) {
            Type res = type_map.get(name[0]);
            if (res != null) {
                result = res;
                return;
            }
        }
        super.visit(t);
    }
}
