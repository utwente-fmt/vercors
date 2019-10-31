package vct.col.rewrite;

import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.composite.LoopStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;

/**
 * Add the global name as a field.
 * This encoding requires a lot of contract modifications.
 * It might be deleted rather than perfected in the future. 
 *
 * @author sccblom
 *
 */
public class GlobalizeStaticsField extends GlobalizeStatics {

  public GlobalizeStaticsField(ProgramUnit source) {
    super(source);
  }

  /**
   * Add the global field to every class.
   */
  public void visit(ASTClass cl){
    super.visit(cl);
    if (cl.kind==ASTClass.ClassKind.Plain){
      ((ASTClass)result).add_dynamic(create.field_decl("global",create.class_type("Global")));
    }
  }

  /**
   * Extend contracts of non-static methods.
   */
  public void visit(Method m){
    if (prefix!=null){
      super.visit(m);
    } else {
      switch(m.getKind()){
      case Plain: {
        Contract c=m.getContract();
        ContractBuilder cb=new ContractBuilder();
        cb.requires(create.expression(StandardOperator.Value,create.field_name("global")));
        cb.ensures(create.expression(StandardOperator.Value,create.field_name("global")));
        for(DeclarationStatement d: m.getArgs()){
          if (d.getType() instanceof ClassType){
            ASTNode not_null=create.expression(StandardOperator.NEQ,create.local_name(d.name()),create.reserved_name(ASTReserved.Null));
            ASTNode gname=create.dereference(create.local_name(d.name()),"global");
            ASTNode access=create.expression(StandardOperator.Value,gname);
            ASTNode same=create.expression(StandardOperator.EQ,create.field_name("global"),gname);
            cb.requires(create.expression(StandardOperator.Implies,not_null,
                create.expression(StandardOperator.Star,access,same)));
          }
        }
        if (c!=null){
          cb.requires(rewrite(c.pre_condition));
          cb.ensures(rewrite(c.post_condition));
        }
        result=create.method_decl(
            rewrite(m.getReturnType()),
            rewrite(cb.getContract()),
            m.getName(),
            rewrite(m.getArgs()),
            rewrite(m.getBody()));
        break;
      }
      default:
        super.visit(m);
      }
    }
  }

  /**
   * Extend loop-invariants of non-static methods.
   */
 public void visit(LoopStatement s){
    super.visit(s);
    if (!processing_static) {
      ASTNode same=create.expression(StandardOperator.EQ,create.field_name("global"),
          create.expression(StandardOperator.Old,create.field_name("global")));
      ((LoopStatement)result).prependInvariant(same);
      ((LoopStatement)result).prependInvariant(create.expression(StandardOperator.Value,create.field_name("global")));
    }
  }

}
