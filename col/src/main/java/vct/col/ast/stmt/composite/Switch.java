package vct.col.ast.stmt.composite;

import scala.collection.Iterable;
import scala.collection.JavaConverters;
import vct.col.ast.generic.DebugNode;
import vct.col.ast.util.ASTMapping;
import vct.col.ast.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.util.ASTVisitor;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

import static hre.lang.System.Debug;

public class Switch extends ASTNode {

  @Override
  public Iterable<String> debugTreeChildrenFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("expr", "cases"));
  }

  @Override
  public Iterable<String> debugTreePropertyFields() {
    return JavaConverters.iterableAsScalaIterable(Collections.emptyList());
  }

  public static class Case implements DebugNode {
    public final ArrayList<ASTNode> cases=new ArrayList<ASTNode>();
    public final ArrayList<ASTNode> stats=new ArrayList<ASTNode>();

    @Override
    public Iterable<String> debugTreeChildrenFields() {
      return JavaConverters.iterableAsScalaIterable(Arrays.asList("cases", "stats"));
    }

    @Override
    public Iterable<String> debugTreePropertyFields() {
      return JavaConverters.iterableAsScalaIterable(Collections.emptyList());
    }
  }
  
  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
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
 
  public final ASTNode expr;
  public final Case cases[];
  
  public Switch(ASTNode expr,Case ... cases){
    this.expr=expr;
    this.cases=Arrays.copyOf(cases,cases.length);
  }

}
