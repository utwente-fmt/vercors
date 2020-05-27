package vct.col.rewrite;

import hre.ast.Origin;
import hre.ast.WrappingOrigin;

import java.util.ArrayList;

import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ContractBuilder;
import vct.col.util.OriginWrapper;

class IllegalThreadLocalOrigin extends WrappingOrigin {

  public final  Origin other;
  
  public IllegalThreadLocalOrigin(Origin other){
    this.other=other;
  }
  
  @Override
  public void report(String level, Iterable<String> message){
    if (level.equals("undefined.name")){
      other.report(level,"current-thread is illegal outside of thread-local contexts");   
    } else {
      other.report(level,message);
    }
  }
  
  @Override
  public void report(String level, String ... message){
    if (level.equals("undefined.name")){
      other.report(level,"current-thread is illegal outside of thread-local contexts");
    } else {
      other.report(level,message);
    }    
  }
  
  @Override
  public WrappingOrigin wrap(Origin other) {
    return new IllegalThreadLocalOrigin(other);
  }
}

public class CurrentThreadRewriter extends AbstractRewriter {

  private WrappingOrigin wrapper=new IllegalThreadLocalOrigin(null);
  
  public CurrentThreadRewriter(ProgramUnit source) {
    super(source);
  }

  static final String ctname="current_thread_id";
  
  @Override
  public void visit(NameExpression e){
    if (e.isReserved(ASTReserved.CurrentThread)){
      result=create.local_name(ctname);
      OriginWrapper.wrap(null, result,wrapper);
    } else {
      super.visit(e);
    }
  }
  
  @Override
  public void visit(Method m){
    if (affected(m)){
        Type returns=rewrite(m.getReturnType());
        ContractBuilder cb=new ContractBuilder();
        ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
        args.add(create.field_decl(ctname, create.primitive_type(PrimitiveSort.Integer)));
        for(DeclarationStatement d:m.getArgs()){
          args.add(rewrite(d));
        }
        if (m.kind!=Method.Kind.Predicate){
          cb.requires(create.expression(StandardOperator.GTE,create.local_name(ctname),create.constant(0)));
        }
        rewrite(m.getContract(),cb);
        ASTNode body=rewrite(m.getBody());
        result=create.method_kind(m.kind,returns, cb.getContract(), m.name(), args.toArray(new DeclarationStatement[0]), body);
    } else {
        super.visit(m);
    }
  }
  
  /**
   * Check is the given method is thread-local. 
   * @param m
   */
  private boolean affected(Method m){
    Type returnType = m.getReturnType();
    if (returnType.isPrimitive(PrimitiveSort.Process)) return false;
    switch(m.kind){
      case Constructor:
      case Plain:
        return true;
      default:
        break;
    }
    return m.isValidFlag(ASTFlags.THREAD_LOCAL) && m.getFlag(ASTFlags.THREAD_LOCAL);
  }
  
  @Override
  public void visit(MethodInvokation e){
    if (affected(e.getDefinition())){
      MethodInvokation res=create.invokation(
          rewrite(e.object()),
          e.dispatch(),
          e.method(),
          rewrite(create.local_name(ctname),e.getArgs())
      );
      res.set_before(rewrite(e.get_before()));
      res.set_after(rewrite(e.get_after()));
      result=res;
    } else {
      super.visit(e);
    }
  }
}
