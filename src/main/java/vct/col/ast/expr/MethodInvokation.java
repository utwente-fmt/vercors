// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.expr;

import scala.collection.Iterable;
import scala.collection.JavaConverters;
import vct.col.ast.expr.ExpressionNode;
import vct.col.util.ASTMapping;
import vct.col.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.type.ClassType;
import vct.col.ast.util.ASTVisitor;

import java.util.*;

import static hre.lang.System.*;

/**
 * This AST node stores method invokations on objects.
 * A function call is seen as invoking a static method on a class. 
 * 
 * @author sccblom
 *
 */
public class MethodInvokation extends ExpressionNode {

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
  }

  @Override
  public Iterable<String> debugTreeChildrenFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("object", "args"));
  }

  @Override
  public Iterable<String> debugTreePropertyFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("method", "dispatch"));
  }

  public final ASTNode object;
  public final String method;
  private Method definition;
  private ASTNode args[];
  public final ClassType dispatch;
  
  public MethodInvokation(ASTNode object,String method,ASTNode ... args){
    this.object=object;
    this.method=method;
    this.args=Arrays.copyOf(args,args.length);
    dispatch=null;
  }
  
  public MethodInvokation(ASTNode object,ClassType dispatch,String method,ASTNode ... args){
    this.object=object;
    this.method=method;
    this.args=Arrays.copyOf(args,args.length);
    this.dispatch=dispatch;
  }

  
  @Override
  public <T> void accept_simple(ASTVisitor<T> visitor){
    try {
      visitor.visit(this);
    } catch (Throwable t){
      if (thrown.get()!=t){
        Debug("Triggered by %s:",getOrigin());
        thrown.set(t);
      }
      throw t;
    }
  }
  
  @Override
  public <T> T accept_simple(ASTMapping<T> map){
    try {
      return map.map(this);
    } catch (Throwable t){
      if (thrown.get()!=t){
        Debug("Triggered by %s:",getOrigin());
        thrown.set(t);
     }
      throw t;
    }
  }
 

  public int getArity() { return args.length; }
    
  public ASTNode getArg(int i){ return args[i]; }

  /**
   * Collect the results of visiting the arguments.
   * @param visitor The visitor which must be accepted by the arguments.
   * @param template Any array of the correct type.
   * @return Array of results of visiting the arguments.
   */
  public <E> E[] map_args(ASTVisitor<E> visitor,E[] template) {
    int N=args.length;
    E res[]=Arrays.copyOf(template,N);
    for(int i=0;i<N;i++){
      args[i].accept(visitor);
      res[i]=visitor.getResult();
    }
    return res;
  }

  public ASTNode[] getArgs() {
    return Arrays.copyOf(args,args.length);
  }

  public void setArgs(ASTNode[] args) {
    this.args = Arrays.copyOf(args, args.length);
  }

  public void addArg(int position, ASTNode arg) {
    ArrayList<ASTNode> newArgsList = new ArrayList<ASTNode>(Arrays.asList(args));
    newArgsList.add(position, arg);
    args = newArgsList.toArray(new ASTNode[0]);
  }
  
  public void setDefinition(Method m){
    definition=m;
  }
  public Method getDefinition(){
    return definition;
  }

  /**
   * Check if this invokation is an instantiation.
   */
  public boolean isInstantiation() {
    if (definition==null){
      Abort("invokation of unknown method");
    }
    return definition.kind==Method.Kind.Constructor;
  }

}

