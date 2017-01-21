package vct.col.util;

import java.util.HashSet;
import java.util.Hashtable;

import vct.col.ast.ASTNode;
import vct.col.ast.BindingExpression;
import vct.col.ast.BlockStatement;
import vct.col.ast.DeclarationStatement;
import vct.col.ast.ForEachLoop;
import vct.col.ast.LoopStatement;
import vct.col.ast.NameExpression;
import vct.col.ast.ParallelBlock;
import vct.col.ast.RecursiveVisitor;
import vct.col.ast.Type;
import vct.col.ast.VectorBlock;


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
      default:
        Abort("missing case %s %s in name scanner",e.getKind(),e.getName());
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
      Type old=vars.get(decl.name);
      safe_decls.add(decl);
      super.visit(s);
      vars.remove(decl.name);
      if(old!=null){
        vars.put(decl.name,old);
      }
    } else {
      super.visit(s);
    }
  }
  
  public void visit(BindingExpression e){
    if (e.getDeclCount()==1){
      DeclarationStatement decl=e.getDeclaration(0);
      Type old=vars.get(decl.name);
      safe_decls.add(decl);
      super.visit(e);
      vars.remove(decl.name);
      if(old!=null){
        vars.put(decl.name,old);
      }      
    } else {
      Abort("missing case in free variable detection");
    }
  }
  public void visit(ForEachLoop s){
    Type old[]=new Type[s.decls.length];
    for(int i=0;i<s.decls.length;i++){
      old[i]=vars.get(s.decls[i].name);
      safe_decls.add(s.decls[i]);
    }
    super.visit(s);
    if (s.get_before()!=null) s.get_before().accept(this);
    if (s.get_after()!=null) s.get_after().accept(this);
    auto_before_after=false;
    for(int i=0;i<s.decls.length;i++){
      vars.remove(s.decls[i].name);
      if(old[i]!=null){
        vars.put(s.decls[i].name,old[i]);
      }      
    }
  }
  
  @Override
  public void visit(ParallelBlock pb){
    Type oldi[]=new Type[pb.iters.length];
    for(int i=0;i<pb.iters.length;i++){
      oldi[i]=vars.get(pb.iters[i].name);
      safe_decls.add(pb.iters[i]);
    }
    super.visit(pb);
    auto_before_after=false;
    for(int i=0;i<pb.iters.length;i++){
      vars.remove(pb.iters[i].name);
      if(oldi[i]!=null){
        vars.put(pb.iters[i].name,oldi[i]);
      }      
    }
  }
  @Override
  public void visit(VectorBlock pb){
    Type old = vars.get(pb.iter().name);
    safe_decls.add(pb.iter());
    super.visit(pb);
    vars.remove(pb.iter().name);
    if (old != null){
      vars.put(pb.iter().name,old);
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
        Type old=vars.get(decl.name);
        saved_names.add(decl.name);
        if (old!=null) saved_vars.put(decl.name, old);
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
}
