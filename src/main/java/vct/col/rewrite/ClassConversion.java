package vct.col.rewrite;

import java.util.ArrayList;
import java.util.Stack;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.decl.Method.Kind;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;

/**
 * This rewriter converts a program with classes into
 * a program with records.
 * 
 * It should no longer be needed in combination with the Viper back ends.
 * 
 * @author Stefan Blom
 *
 */
public class ClassConversion extends AbstractRewriter {

  private static final String SEP="__";
      
  public ClassConversion(ProgramUnit source) {
    super(source);
  }

  private Stack<Boolean> constructor_this=new Stack<Boolean>();
  {
    constructor_this.push(false);
  }
  
  public static final String THIS="diz";
  @Override
  public void visit(NameExpression e){
    if (e.isReserved(ASTReserved.This)){
      if (constructor_this.peek()){
        if (in_requires){
          e.getOrigin().report("error","pre-condition of constructor may not refer to this");
          Fail("fatal error");
        }
        result=create.reserved_name(ASTReserved.Result);
      } else {
        result=create.unresolved_name(THIS);
      }
    } else {
      super.visit(e);
    }
  }
  @Override
  public void visit(ASTClass cl){
    ASTClass res=create.ast_class(cl.name(), ASTClass.ClassKind.Record,null, null, null);
    for(DeclarationStatement decl:cl.staticFields()){
      String name=cl.name() + SEP + decl.name();
      create.enter();
      create(decl);
      target().add(create.field_decl(name, rewrite(decl.getType()), rewrite(decl.initJava())));
      create.leave();
    }
    for(DeclarationStatement decl:cl.dynamicFields()){
      res.add(rewrite(decl));
    }
    for(Method m:cl.methods()){
      create.enter();
      create.setOrigin(m.getOrigin());
      Method.Kind kind;
      Type returns;
      if (m.kind==Method.Kind.Constructor){
        Debug("constructor %s", m.name());
        returns=create.class_type(cl.name());
        kind=Method.Kind.Plain;
      } else {
        returns=rewrite(m.getReturnType());
        kind=m.kind;
      }
      ContractBuilder cb=new ContractBuilder();
      String name = cl.name() + SEP + m.name();
      ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
      ASTNode body=m.getBody();
      if (m.kind!= Method.Kind.Constructor && !m.isStatic()){
        args.add(create.field_decl(THIS,create.class_type(cl.name())));
        ASTNode nonnull=create.expression(StandardOperator.NEQ,
            create.local_name(THIS),
            create.reserved_name(ASTReserved.Null));
        if (m.kind!=Method.Kind.Predicate){
          cb.requires(nonnull);
        } else {
          if (body != null) {
            body=create.expression(StandardOperator.Star,nonnull,body);
          }
        }
      }
      for(DeclarationStatement d:m.getArgs()){
        args.add(rewrite(d));
      }
      body=rewrite(body);
      if (m.kind==Method.Kind.Constructor){
        if (body!=null){
          body=create.block(
            create.field_decl(THIS,create.class_type(cl.name())),
            create.assignment(
                create.local_name(THIS),
                create.new_record(create.class_type(cl.name()))
            ),
            body,
            create.return_statement(create.local_name(THIS))
          );
        }
        cb.ensures(create.expression(StandardOperator.NEQ,
            create.reserved_name(ASTReserved.Result),
            create.reserved_name(ASTReserved.Null)));
      }
      boolean varArgs=m.usesVarArgs();
      if (m.kind==Method.Kind.Constructor) {
        constructor_this.push(true);
        rewrite(m.getContract(),cb);
        constructor_this.pop();
      } else {
        rewrite(m.getContract(),cb);
      }
      Method p=create.method_kind(kind, returns,cb.getContract(), name, args.toArray(new DeclarationStatement[0]), varArgs, body);
      create.leave();
      p.setStatic(true);
      target().add(p);
    }
    result=res;
  }
  
  @Override
  public void visit(StructValue v) {
    if (v.type() instanceof ClassType) {
      Abort("struct value used for constructor call");
      // If this is actually a constructor call.
      String method = v.type() + SEP + v.type();
      MethodInvokation res=create.invokation(null, null, method, rewrite(v.valuesArray()));
      res.set_before(rewrite(v.get_before()));
      res.set_after(rewrite(v.get_after()));
      result=res;
    } else {
      super.visit(v);
    }
  }
  
  @Override
  public void visit(MethodInvokation s){
    String method;
    ArrayList<ASTNode> args=new ArrayList<ASTNode>();
    Method def=s.getDefinition();
    ClassType dispatch=s.dispatch;
    ASTNode object=null;
    if (def.getParent()==null){
      method=s.method;
    } else if (s.object instanceof ClassType){
      if (s.method.equals(Method.JavaConstructor)){
        method=s.dispatch.getName()+SEP+s.dispatch.getName();
        dispatch=null;
      } else if (def.getParent() instanceof AxiomaticDataType){
        method=s.method;
        object=copy_rw.rewrite(s.object);
      } else {
        method=((ClassType)s.object).getName()+SEP+s.method;
      }
    } else if (s.object==null){
      if (s.method.equals(Method.JavaConstructor)){
        method=s.dispatch.getName()+SEP+s.dispatch.getName();
        dispatch=null;
      } else {
        method=s.method;
      }
    } else {
      method=((ClassType)s.object.getType()).getName();
      if (method.equals("<<adt>>") || def.getParent() instanceof AxiomaticDataType){
        method=s.method;
      } else {
        method+=SEP+s.method;
        if (!def.isStatic()){
          args.add(rewrite(s.object));
        }
        if (def.kind==Kind.Predicate && !s.object.isReserved(ASTReserved.This) && (!fold_unfold) ){
          //extra=create.expression(StandardOperator.NEQ,rewrite(s.object),create.reserved_name(ASTReserved.Null));
        }
      }      
    }
    for(ASTNode arg :s.getArgs()){
      args.add(rewrite(arg));
    }
    MethodInvokation res=create.invokation(object, dispatch, method, args.toArray(new ASTNode[0]));
    res.set_before(rewrite(s.get_before()));
    res.set_after(rewrite(s.get_after()));
    result=res;
  }
}
