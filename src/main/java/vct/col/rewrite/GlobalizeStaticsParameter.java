package vct.col.rewrite;

import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.util.ContractBuilder;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;

import java.util.Objects;

/**
 * Use a parameter global to refer to static entries.
 * 
 * @author sccblom
 *
 */
public class GlobalizeStaticsParameter extends GlobalizeStatics {

  public GlobalizeStaticsParameter(ProgramUnit source) {
    super(source);
  }

  /**
   * Add the global argument to every non-static method.
   */
  public void visit(Method m){
    if (prefix!=null || m.isStatic()){
      super.visit(m);
    } else {
      switch(m.getKind()){
      case Constructor:
      case Plain: {
        DeclarationStatement args[]=new DeclarationStatement[m.getArity()+1];
        //TODO: parameter decl in factory!
        args[0]=create.field_decl("global",create.class_type("Global"));
        for(int i=1;i<args.length;i++){
          args[i]=rewrite(m.getArgs()[i-1]);
        }
        ContractBuilder cb=new ContractBuilder();
        cb.requires(create.expression(StandardOperator.NEQ,
            create.argument_name("global"),create.reserved_name(ASTReserved.Null)));
        rewrite(m.getContract(),cb);
        result=create.method_kind(
            m.getKind(),
            rewrite(m.getReturnType()),
            rewrite(m.signals),
            cb.getContract(),
            m.getName(),
            args,
            m.usesVarArgs(),
            rewrite(m.getBody()));
        break;
      }
      default:
        super.visit(m);
      }
    }
  }
  
  /**
   * Add the this/global argument to every invokation of a non-static method.
   */
  public void visit(MethodInvokation e){
    Method m=e.getDefinition();
    Objects.requireNonNull(m, "cannot globalize method invokaiton without method definition");
    if (m.isStatic() && !e.isInstantiation()){
      super.visit(e);
    } else {
      Method.Kind kind=Method.Kind.Predicate;
      if (e.getDefinition()!=null){
        kind=e.getDefinition().getKind();
      } else {
        Warning("assuming kind of %s is Predicate",e.method());
      }
      switch(kind){
      case Constructor:
      case Plain:{
        ASTNode args[]=new ASTNode[e.getArity()+1];
        if (processing_static){
          args[0]=create.reserved_name(ASTReserved.This);
        } else {
          args[0]=create.local_name("global");
        }
        for(int i=1;i<args.length;i++){
          args[i]=rewrite(e.getArg(i-1));
        }
        MethodInvokation res=create.invokation(
            rewrite(e.object()),
            rewrite(e.dispatch()),
            e.method(),
            args
        );
        if (e.get_before().size()>0) {
          res.set_before(rewrite(e.get_before()));
        }
        if (e.get_after().size()>0) {
          res.set_after(rewrite(e.get_after()));
        }
        result=res;
        break;
      }
      case Pure:
      case Predicate:
        super.visit(e);
        break;
      default:
        Abort("missing case: %s",kind);
        break;
      }
    }
  }
  
  @Override
  public void visit(ASTClass cl){
    if (cl.kind==ASTClass.ClassKind.Kernel){
      result=copy_rw.rewrite(cl);
    } else {
      super.visit(cl);
    }
  }
}

