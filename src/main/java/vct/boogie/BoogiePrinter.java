// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.boogie;

import hre.ast.TrackingOutput;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;

/**
 * This class contains a pretty printer for Boogie programs.
 * 
 * @author sccblom
 *
 */
public class BoogiePrinter extends AbstractBoogiePrinter {

  public BoogiePrinter(TrackingOutput out){
    super(BoogieSyntax.getBoogie(),out,true);
  }

  public void visit(Method m){
    if (m.kind==Method.Kind.Constructor) {
      Debug("skipping constructor");
      return;
    }
    DeclarationStatement args[]=m.getArgs();
    int N=args.length;
    Type result_type=m.getReturnType();
    if (!result_type.equals(PrimitiveSort.Void)) Fail("illegal return type %s",result_type);
    String name=m.getName();
    out.printf("procedure %s(",name);
    String next="";
    for(int i=0;i<N;i++){
      if (args[i].isValidFlag(ASTFlags.OUT_ARG)&&args[i].getFlag(ASTFlags.OUT_ARG)) continue;
      out.printf("%s%s: ", next,args[i].name());
      args[i].getType().accept(this);
      next=",";
    }
    out.printf(") returns (");
    next="";
    for(int i=0;i<args.length;i++){
      if (args[i].isValidFlag(ASTFlags.OUT_ARG)&&args[i].getFlag(ASTFlags.OUT_ARG)) {
        out.printf("%s%s: ", next,args[i].name());
        args[i].getType().accept(this);
        next=",";
      }
    }
    out.lnprintf(")");
    Contract contract=m.getContract();
    if (contract!=null){
      visit(contract);
      post_condition=contract.post_condition;
    }
//    if (all_fields!=null){
//      out.lnprintf("modifies %s;",all_fields);
//    }
    ASTNode body=m.getBody();
    body.accept(this);
    out.lnprintf("//end procedure %s",name);
    post_condition=null;
  }

  public void visit(NameExpression e){
    String s=e.toString();
    if (s.equals("\\result")){
      out.print("__result");
    } else if (s.equals("\\old")){
      out.print("old");
    } else {
      out.print(s);
    }
  }


  public void visit(ASTClass cl){
    int N=cl.getStaticCount();
    int M=cl.getDynamicCount();
    for(int i=0;i<M;i++) {
      if (cl.getDynamic(i) instanceof Method){
        Method m=(Method)cl.getDynamic(i);
        if (m.kind==Method.Kind.Constructor){
          continue;
        }
      } else if (cl.getDynamic(i) instanceof ASTSpecial){
        ASTSpecial s=(ASTSpecial)cl.getDynamic(i);
        if (s.kind==ASTSpecial.Kind.Comment){
          continue;
        }
      }
      throw new Error("illegal non-static in Boogie input");
    }
    if (N==1 && cl.getStatic(0) instanceof ASTClass){
      visit((ASTClass)cl.getStatic(0));
    } else for(int i=0;i<N;i++){
      cl.getStatic(i).accept(this);
      out.println("");
    }
  }
}
