package vct.col.util;

import java.util.HashSet;
import java.util.Hashtable;

import vct.col.ast.expr.*;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.composite.ForEachLoop;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.composite.ParallelBlock;
import vct.col.ast.util.RecursiveVisitor;
import vct.col.ast.type.Type;
import vct.col.ast.stmt.composite.VectorBlock;


public class NameScanner extends RecursiveVisitor<Object> {

  private Hashtable<String,Type> vars;
  
  private HashSet<DeclarationStatement> safe_decls=new HashSet<DeclarationStatement>();
  
  public NameScanner(Hashtable<String,Type> vars) {
    super(null, null);
    this.vars=vars;
  }
  
  public void visit(NameExpression e){
    switch(e.getKind()){
      case Reserved: return;
      case Label: return;
      case Field:
      case Local:
      case Argument:
        String name=e.getName();
        Type t=e.getType();
        if (vars.containsKey(name)){
          if (t!=null && !t.equals(vars.get(name))) {
            Fail("type mismatch %s != %s",t,vars.get(name));
          }
        } else {
          if (t==null){
            Abort("type of %s is null",name);
          }
          vars.put(name,t);
          t.accept(this);
        }
        return;
      case Unresolved: {
        switch (e.getName()) {
          case "tcount":
          case "gsize":
          case "tid":
          case "gid":
          case "lid":
          case "threadIdx":
          case "blockIdx":
          case "blockDim":
            vars.put(e.getName(), e.getType());
            return;
        }
      }
      default:
        Abort("missing case %s %s in name scanner",e.getKind(),e.getName());
    }
  }

  public void visit(OperatorExpression e) {
    if(e.operator() == StandardOperator.StructDeref || e.operator() == StandardOperator.StructSelect) {
      e.first().accept(this);
      // TODO: implement struct field checking; skipping dereferenced field here
    } else {
      super.visit(e);
    }
  }
  
  public void visit(DeclarationStatement d){
    if (!safe_decls.contains(d)){
      Abort("missing case in free variable detection");
    }
    super.visit(d);
  }

  public static boolean occurCheck(ASTNode invariant, String var_name) {
    Hashtable<String, Type> vars=new Hashtable<String, Type>();
    invariant.accept(new NameScanner(vars));
    return vars.containsKey(var_name);
  }

  public void visit(LoopStatement s){
    ASTNode init=s.getInitBlock();
    if (init instanceof BlockStatement){
      BlockStatement block=(BlockStatement)init;
      if (block.getLength()==1){
        init=block.get(0);
      }
    }
    if (init instanceof DeclarationStatement){
      DeclarationStatement decl=(DeclarationStatement)init;
      Type old=vars.get(decl.name());
      safe_decls.add(decl);
      super.visit(s);
      vars.remove(decl.name());
      if(old!=null){
        vars.put(decl.name(), old);
      }
    } else {
      super.visit(s);
    }
  }
  
  public void visit(BindingExpression e){
    if (e.getDeclCount()==1){
      DeclarationStatement decl=e.getDeclaration(0);
      Type old=vars.get(decl.name());
      safe_decls.add(decl);
      super.visit(e);
      vars.remove(decl.name());
      if(old!=null){
        vars.put(decl.name(), old);
      }      
    } else {
      Abort("missing case in free variable detection");
    }
  }
  public void visit(ForEachLoop s){
    Type old[]=new Type[s.decls.length];
    for(int i=0;i<s.decls.length;i++){
      old[i] = vars.get(s.decls[i].name());
      safe_decls.add(s.decls[i]);
    }
    super.visit(s);
    if (s.get_before()!=null) s.get_before().accept(this);
    if (s.get_after()!=null) s.get_after().accept(this);
    auto_before_after=false;
    for(int i=0;i<s.decls.length;i++){
      vars.remove(s.decls[i].name());
      if(old[i]!=null){
        vars.put(s.decls[i].name(), old[i]);
      }      
    }
  }
  
  @Override
  public void visit(ParallelBlock pb){
    Type oldi[] = new Type[pb.iterslength()];
    
    int i = 0;
    for (DeclarationStatement decl : pb.itersJava()) {
      oldi[i] = vars.get(decl.name());
      safe_decls.add(decl);
      i++;
    }
    
    super.visit(pb);
    auto_before_after = false;
    
    i = 0;
    for (DeclarationStatement decl : pb.itersJava()) {
      vars.remove(decl.name());
      if (oldi[i] != null) {
        vars.put(decl.name(), oldi[i]);
      }
      i++;
    }
  }
  @Override
  public void visit(VectorBlock pb){
    Type old = vars.get(pb.iter().name());
    safe_decls.add(pb.iter());
    super.visit(pb);
    vars.remove(pb.iter().name());
    if (old != null){
      vars.put(pb.iter().name(), old);
    }      
  }

  @Override
  public void visit(BlockStatement b){
    int N=b.size();
    Hashtable<String,Type> saved_vars=new Hashtable<String, Type>();
    HashSet<String> saved_names=new HashSet<String>();
    for(int i=0;i<N;i++){
      ASTNode s=b.get(i);
      if (s instanceof DeclarationStatement){
        DeclarationStatement decl=(DeclarationStatement)s;
        Type old=vars.get(decl.name());
        saved_names.add(decl.name());
        if (old!=null) saved_vars.put(decl.name(), old);
        safe_decls.add(decl);
      }
      s.accept(this);
    }
    for(String name : saved_names){
      vars.remove(name);
      Type old=saved_vars.get(name);
      if(old!=null){
        vars.put(name,old);
      }
    }
  }

  @Override
  public void visit(MethodInvokation invokation) {
    super.visit(invokation);
    // The before/after annotations of methods contain the given/yields mappings, so don't include them as though they
    // are free names.
    auto_before_after = false;
  }
}
