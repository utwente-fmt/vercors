package vct.col.rewrite;

import hre.ast.MessageOrigin;
import vct.col.ast.ASTClass;
import vct.col.ast.ASTClass.ClassKind;
import vct.col.ast.AxiomaticDataType;
import vct.col.ast.ClassType;
import vct.col.ast.DeclarationStatement;
import vct.col.ast.Dereference;
import vct.col.ast.Method;
import vct.col.ast.MethodInvokation;
import vct.col.ast.ProgramUnit;
import vct.col.ast.StructValue;
import vct.util.ClassName;

/**
 * Base class for rewriting all static entries as a single Global class.
 * This base class will do all of the rewriting, except the creation
 * of the name global that refers to the global entries. The class
 * {@link GlobalizeStaticsParameter} adds a parameter global to all
 * non-static methods. The class {@link GlobalizeStaticsField} adds
 * a field global to every class.
 * 
 * An advantage of adding a field is that it allows non-static predicates
 * to refer to static variables without adding an argument.
 * A disadvantage is that it requires generating contracts to make it work.
 * 
 * @author sccblom
 *
 */
public abstract class GlobalizeStatics extends AbstractRewriter {
  
  public GlobalizeStatics(ProgramUnit source) {
    super(source);
    global_class=create(new MessageOrigin("filtered globals")).ast_class("Global",ClassKind.Plain,null,null,null);
    target().add(global_class);
  }

  protected ASTClass global_class;
  protected String prefix;
  protected boolean processing_static;
  
  public void visit(ASTClass cl){
    switch(cl.kind){
    case Plain:{
      int N;
      ASTClass res=create.ast_class(cl.name,ClassKind.Plain,null,null,null);
      N=cl.getStaticCount();
      prefix=new ClassName(cl.getFullName()).toString("_");
      processing_static=true;
      Debug("prefix is now %s",prefix);
      for(int i=0;i<N;i++){
        global_class.add_dynamic(cl.getStatic(i).apply(this));
      }
      prefix=null;
      processing_static=false;
      Debug("prefix is now %s",prefix);
      N=cl.getDynamicCount();
      for(int i=0;i<N;i++){
        res.add_dynamic(cl.getDynamic(i).apply(this));
      }
      result=res;
      break;      
    }
    case Record:{
      super.visit(cl);
      return;
    }
    default: Abort("missing case: %s",cl.kind);
    }
  }
  public void visit(DeclarationStatement s){
    if (prefix!=null){
      String save=prefix;
      prefix=null;
      result=create.field_decl(save+"_"+s.getName(),
           rewrite(s.getType()), 
           rewrite(s.getInit()));
      prefix=save;
    } else {
      super.visit(s);
    }
  }
  public void visit(Method m){
    if (prefix!=null){
      String save=prefix;
      prefix=null;
      result=create.method_kind(
          m.kind,
          rewrite(m.getReturnType()),
          rewrite(m.getContract()),
          save+"_"+m.getName(),
          rewrite(m.getArgs()),
          rewrite(m.getBody()));
      prefix=save;
    } else {
      super.visit(m);
    }
  }

  @Override
  public void visit(StructValue v){
    if (v.type() instanceof ClassType){
      Abort("illegal use of struct value for constructor call");
    }
    /*
    switch(e.getOperator()){
    case Build:{
      if (e.getArg(0) instanceof ClassType){
        // Constructor call encoded as Build.
        ASTNode args[]=rewrite(null,e.getArguments());
        args[0]=args[1];
        if (processing_static){
          args[1]=create.reserved_name(ASTReserved.This);
        } else {
          args[1]=create.local_name("global");
        }
        result=create.expression(StandardOperator.Build, args);
        return;
      }
    }
    }*/
    super.visit(v);
  }
  
  public void visit(MethodInvokation e){
    Method m=e.getDefinition();
    if (m==null) Abort("cannot globalize method invokaiton without method definition");
    if (m.getParent() instanceof AxiomaticDataType){
      super.visit(e);
      return;
    }
    ASTClass cl=(ASTClass)m.getParent();
    if (cl==null){
      //System.err.printf("no parent for %s%n",m.getName());
      // FIXME: it works, but not for the right reason!
      super.visit(e);
      return;
    }
    if (m.isStatic() && !e.isInstantiation()){
      MethodInvokation res;
      String prefix=new ClassName(cl.getFullName()).toString("_");
      if (processing_static){
        res=create.invokation(
          create.this_expression(create.class_type("Global")),
          rewrite(e.dispatch),
          prefix+"_"+e.method,
          rewrite(e.getArgs()));
      } else {
        res=create.invokation(
            create.local_name("global"),
            rewrite(e.dispatch),
            prefix+"_"+e.method,
            rewrite(e.getArgs()));        
      }
      if (e.get_before().size()>0) {
        res.set_before(rewrite(e.get_before()));
      }
      if (e.get_after().size()>0) {
        res.set_after(rewrite(e.get_after()));
      }
      result=res;
    } else {
      super.visit(e);
    }
  }

  public void visit(Dereference e){
    if (e.field().equals("length")){
      super.visit(e);
      return;
    }
    if (e.field().equals("item")){
      super.visit(e);
      return;
    }
    ClassType t;
    if (e.object() instanceof ClassType){
      t=(ClassType)e.object();
    } else {
      t=(ClassType)e.object().getType();
    }
    String s=t.toString();
    if (s.equals("<<label>>")){
      // witness encoding?
      super.visit(e);
      return;
    }
    ASTClass cl=source().find(t);
    DeclarationStatement decl=cl.find_field(e.field());
    if (decl.isStatic()){
      String var_name=new ClassName(t.getNameFull()).toString("_")+"_"+e.field();
      if (!processing_static){
        result=create.dereference(create.local_name("global"),var_name);
      } else {
        result=create.dereference(create.this_expression(create.class_type("Global")),var_name);
      }     
    } else {
      super.visit(e);
    }
  }

}
