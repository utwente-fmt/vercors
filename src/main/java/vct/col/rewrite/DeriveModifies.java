package vct.col.rewrite;

import java.util.HashSet;

import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.util.ASTVisitor;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.RecursiveVisitor;

/**
 * This rewriter adds modifies clauses to all method contracts.
 * 
 * @author Stefan Blom
 */ 
public class DeriveModifies extends RecursiveVisitor<Object> {

  public DeriveModifies(){
    super(null,null);
  }
  
  private boolean dirty;
  
  public void visit(Method m) {
    Contract c=m.getContract();
    if (c==null){
      c=ContractBuilder.emptyContract();
    }
    HashSet<ASTNode> old_mods=new HashSet<ASTNode>();
    if (c.modifies!=null) for(ASTNode n:c.modifies) old_mods.add(n);
    ContractBuilder builder=new ContractBuilder();
    builder.requires(c.pre_condition);
    builder.ensures(c.post_condition);
    builder.modifies();
    ASTVisitor<Object> mod=new ModificationScanner(builder);
    m.accept(mod);
    // change contract in result.
    c=builder.getContract();
    m.setContract(c);
    for(ASTNode n:c.modifies) {
      if (!old_mods.contains(n)) dirty=true;
      break;
    }
  }

  public void annotate(ASTClass program) {
    dirty=true;
    int pass=0;
    while(dirty){
      pass++;
      dirty=false;
      Debug("modifies annotation pass %d",pass);
      program.accept(this);
    }
  }

  public void annotate(ProgramUnit arg) {
    for(ASTClass cl:arg.classes()){
      annotate(cl);
    }
  }

}
