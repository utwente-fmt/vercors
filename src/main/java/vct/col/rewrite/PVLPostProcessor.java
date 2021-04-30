package vct.col.rewrite;


import hre.ast.BranchOrigin;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.stmt.decl.ASTSpecial.Kind;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.AbstractRewriter;

/**
 * Rewrite a PVL AST, produced by parsing, to conform to the COL AST standard.  
 */
public class PVLPostProcessor extends AbstractRewriter {
  
  public PVLPostProcessor(ProgramUnit source) {
    super(source);
  }
    
  @Override
  public void visit(ASTClass c){
    super.visit(c);
    if (c.kind==ClassKind.Kernel) return;
    ASTClass decl=(ASTClass)result;
    boolean withlock=false;
    for(Method m:decl.dynamicMethods()){
      Debug("%s %s", m.kind, m.name());
      if (m.kind==Method.Kind.Predicate && m.name().equals("lock_invariant")){
        if (withlock){
          m.getOrigin().report("error","second declaration of lock_invariant.");
          throw new Error();
        }
        if (m.getArity()>0){
          m.getOrigin().report("error","predicate lock_invariant cannot have arguments");
          throw new Error();
        }
        withlock=true;
      }
    }
    int N=0;
    for(Method m:decl.dynamicMethods()){
      if (m.kind==Method.Kind.Constructor) {
        if (withlock) {
          BranchOrigin branch=new BranchOrigin("Commit lock invariant during construction",m.getOrigin());
          create.enter();
          create.setOrigin(branch);
          ASTNode S=create.special(Kind.Fold,create.invokation(null,null,"lock_invariant"));
          S.setFlag(ASTFlags.GHOST,true);
          ((BlockStatement)m.getBody()).append(S);
          S=create.special(ASTSpecial.Kind.Exhale,create.invokation(null,null,"lock_invariant"));
          S.setFlag(ASTFlags.GHOST,true);
          ((BlockStatement)m.getBody()).append(S);
          create.leave();
         }
        N++;
      }
    }
    if (N==0) {
      if (withlock){
        c.getOrigin().report("error","cannot generate implicit constructor for class with lock invariant");
        throw new Error();
      } else {
        create.addZeroConstructor(decl);
      }
    }
  }

}
