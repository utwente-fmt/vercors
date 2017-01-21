package vct.col.annotate;

import vct.col.ast.ASTClass;
import vct.col.ast.ASTNode;
import vct.col.ast.AssignmentStatement;
import vct.col.ast.Contract;
import vct.col.ast.ContractBuilder;
import vct.col.ast.Dereference;
import vct.col.ast.Method;
import vct.col.ast.MethodInvokation;
import vct.col.ast.NameExpression;
import vct.col.ast.OperatorExpression;
import vct.col.ast.RecursiveVisitor;

public class ModificationScanner extends RecursiveVisitor<Object> {

  private ContractBuilder builder;
  public ModificationScanner(ContractBuilder builder) {
    super(null,null);
    this.builder=builder;
  }

  private void modifies_loc(ASTNode n){
    if (n instanceof NameExpression){
      NameExpression name=(NameExpression)n;
      if (name.getSite() instanceof ASTClass){
        builder.modifies(n);
      }
    } else if (n instanceof Dereference){
      builder.modifies(n);
    } else {
      Abort("unimplemented location type %s",n.getClass());
    }
  }
  
  public void visit(AssignmentStatement s){
    modifies_loc(s.location());
  }
  
  public void visit(MethodInvokation e){
    Method m=e.getDefinition();
    if (m==null) Abort("definition is null at %s",e.getOrigin());
    Contract c=m.getContract();
    if (c!=null && c.modifies!=null) {
      for (ASTNode loc : c.modifies) {
        modifies_loc(loc);
      }
    }
  }
  
  public void visit(OperatorExpression e){
    switch(e.operator()){
    case PreIncr:
    case PostIncr:
    case PreDecr:
    case PostDecr:
      modifies_loc(e.arg(0));
    default:
      break;
    }
  }
  
}
