package vct.col.rewrite;

import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.stmt.decl.AxiomaticDataType;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.OperatorExpression;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.util.ClassName;

/**
 * Standardize the representation of programs.
 *
 * <UL>
 * <LI> Replace assignment expressions used as statements by assignment statements.
 * <LI> Replace simple increment and decrement statements by assignments.
 * <LI> Create objects for method invokations that do not have them.
 * </UL>
 *
 * @author Stefan Blom
 *
 */
public class Standardize extends AbstractRewriter {

  public Standardize(ProgramUnit source) {
    super(source,true);
  }

  @Override
  public void visit(Method m){
    if (m.kind == Method.Kind.Pure && m.getReturnType().isPrimitive(PrimitiveSort.Resource)){
      result=create.predicate(m.getName(), rewrite(m.getBody()), rewrite(m.getArgs()));
    } else {
      super.visit(m);
    }
  }

  public void visit(MethodInvokation e){
    ASTNode object=rewrite(e.object);
    if(object==null){
      Method m=source().find_adt(e.method);
      if (m!=null){
        String adt = ((AxiomaticDataType)m.getParent()).name();
        object=create.class_type(adt);
      }
    }
    if (object==null){
      if (e.method.equals(Method.JavaConstructor)){
        object=null;
      } else if (current_class()!=null) {
        object=create.this_expression(create.class_type(current_class().getFullName()));
      }
    }
    MethodInvokation res=create.invokation(object, rewrite(e.dispatch), e.method, rewrite(e.getArgs()));
    res.set_before(rewrite(e.get_before()));
    res.set_after(rewrite(e.get_after()));
    result=res;
  }

  public void visit(NameExpression e){
    switch(e.getKind()){
      case Field:{
        Method m=current_method();
        if (m==null) {
          Fail("cannot support expressions outside of method definitions yet.");
        }
        if (m.isStatic()){
          result=create.dereference(create.class_type(current_class().getFullName()),e.getName());
        } else {
          result=create.dereference(create.reserved_name(ASTReserved.This),e.getName());
        }
        break;
      }
      case Unresolved:{
        String name=e.getName();
        ClassName cl_name=new ClassName(name);
        if (source().find(cl_name)!=null){
          result=create.class_type(name);
          break;
        }
        VariableInfo info=variables.lookup(name);
        if (info!=null) {
          Debug("unresolved %s name %s found during standardize",info.kind,name);
          e.setKind(info.kind);
        } else {
          switch (name){
          case "false":
            result=create.constant(false);
            return;
          case "true":
            result=create.constant(true);
            return;
          }
        }
        super.visit(e);
        break;
      }
      default:{
        super.visit(e);
        break;
      }
    }
  }

  @Override
  public void visit(OperatorExpression e){
    if (e.getParent() instanceof BlockStatement){
      switch(e.operator()){
      case Assign:
      {
        ASTNode var=e.arg(0).apply(this);
        ASTNode val=e.arg(1).apply(this);
        result=create.assignment(var,val);
        break;
      }
      case PostIncr:
      case PreIncr:
      {
        ASTNode arg=e.arg(0);
        if (arg instanceof NameExpression){
          ASTNode incr=create.expression(e.getOrigin(),StandardOperator.Plus,rewrite(arg),create.constant(e.getOrigin(),1));
          result=create.assignment(rewrite(arg),incr);
        } else {
          super.visit(e);
        }
        break;
      }
      case PostDecr:
      case PreDecr:
      {
        ASTNode arg=e.arg(0);
        if (arg instanceof NameExpression){
          ASTNode incr=create.expression(e.getOrigin(),StandardOperator.Minus,rewrite(arg),create.constant(e.getOrigin(),1));
          result=create.assignment(rewrite(arg),incr);
        } else {
          super.visit(e);
        }
        break;
      }
      default:
        super.visit(e);
        break;
      }
    } else {
      super.visit(e);
    }
  }
}
