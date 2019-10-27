package vct.col.rewrite;

import java.util.HashSet;

import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.ProgramUnit;

public class ReorderAssignments extends AbstractRewriter {
  private HashSet<String> argumentNames = new HashSet<String>();

  public ReorderAssignments(ProgramUnit source) {
    super(source);
  }

  public void visit(Method method) {
      for(DeclarationStatement decl : method.getArgs()) {
          argumentNames.add(decl.name());
      }

      super.visit(method);

      argumentNames.clear();
  }

  public void visit(BlockStatement s){
    int N=s.getLength();
    BlockStatement res=create.block();
    // copy declarations
    HashSet<String> names=new HashSet<String>();
    for(int i=0;i<N;i++){
      if (s.getStatement(i) instanceof DeclarationStatement){
        DeclarationStatement decl=(DeclarationStatement)s.getStatement(i);
        String name = decl.name();

        if(argumentNames.contains(name)) {
            Abort("Variable %s is already declared as an argument in this method", name);
        }

        if (names.contains(name)){
          Abort("variable %s was declared twice",name);
        }
        names.add(name);
        res.addStatement(decl.apply(this));
      }
    }
    // copy non-declarations
    for(int i=0;i<N;i++){
      if (!(s.getStatement(i) instanceof DeclarationStatement)){
        res.addStatement(s.getStatement(i).apply(this));
      }
    }
    result=res;
  }

}
