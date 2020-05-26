package vct.col.ast.expr;

import scala.collection.Iterable;
import scala.collection.JavaConverters;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.expr.ExpressionNode;
import vct.col.ast.type.Type;
import vct.col.ast.util.ASTMapping;
import vct.col.ast.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.util.ASTVisitor;

import java.util.ArrayList;
import java.util.Arrays;

import static hre.lang.System.Abort;
import static hre.lang.System.Debug;

public class BindingExpression extends ExpressionNode {

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
  }

  @Override
  public Iterable<String> debugTreeChildrenFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("select", "main"));
  }

  @Override
  public Iterable<String> debugTreePropertyFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("binder", "result_type"));
  }

  public final Binder binder;
  public final ASTNode select;
  public final ASTNode main;
  public final Type result_type;
  
  private ArrayList<DeclarationStatement> decls=new ArrayList<DeclarationStatement>();
  public final ASTNode triggers[][];
  
  public BindingExpression(Binder binder,Type result_type,DeclarationStatement decls[],ASTNode triggers[][],ASTNode select,ASTNode main){
    this.binder=binder;
    this.result_type=result_type;
    this.select=select;
    this.main=main;
    if (triggers==null){
      this.triggers=null;
    } else {
      this.triggers=new ASTNode[triggers.length][];
      for(int i=0;i<triggers.length;i++){
        this.triggers[i]=Arrays.copyOf(triggers[i],triggers[i].length);
      }
    }
    for(int i=0;i<decls.length;i++){
      DeclarationStatement decl=decls[i];
      if (decl ==null){
        Abort("declaration %d is null",i);
      }
      this.decls.add(decl);
    }
  }
  
  public int getDeclCount(){
    return decls.size();
  }
  
  public DeclarationStatement getDeclaration(int i){
    return decls.get(i);
  }

  public DeclarationStatement[] getDeclarations(){
    return decls.toArray(new DeclarationStatement[0]);
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
}
