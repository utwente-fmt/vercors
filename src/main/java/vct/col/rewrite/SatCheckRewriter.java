package vct.col.rewrite;

import hre.ast.BranchOrigin;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.Type;
import vct.col.util.OriginWrapper;

public class SatCheckRewriter extends AbstractRewriter {

	public SatCheckRewriter(ProgramUnit source) {
	  super(source);
  }

  @Override
  public void visit(Method m) {
    //checkPermission(m);
    String name=m.getName();
    if (currentContractBuilder==null) currentContractBuilder=new ContractBuilder();
    DeclarationStatement args[]=rewrite(m.getArgs());
    Contract mc=m.getContract();
    boolean intentional_false=false;
    if (mc!=null){
      rewrite(mc,currentContractBuilder);
      intentional_false=mc.pre_condition.isConstant(false);
    }
    Method.Kind kind=m.kind;
    Type rt=rewrite(m.getReturnType());
    Contract c=currentContractBuilder.getContract();
    currentContractBuilder=null;
    ASTNode body=rewrite(m.getBody());
    if (body!=null && !intentional_false) switch(kind){
    case Plain:
    case Constructor:
      ASTNode refute=create.special(ASTSpecial.Kind.Refute,create.constant(false));
    	BranchOrigin branch=new BranchOrigin("Contract Unsatisfiability Check",null);
      OriginWrapper.wrap(null,refute, branch);
	    if (body instanceof BlockStatement){
	    	((BlockStatement)body).prepend(refute);
	    } else {
	    	body=create.block(refute,body);
	    }
    default:
      break;
    }
    result=create.method_kind(kind, rt, c, name, args, m.usesVarArgs(), body);
  }

}
