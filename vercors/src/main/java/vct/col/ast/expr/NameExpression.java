// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.expr;

import scala.collection.Iterable;
import scala.collection.JavaConverters;
import vct.col.util.ASTMapping;
import vct.col.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.Hole;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.util.ASTVisitor;

import java.util.Arrays;
import java.util.Collections;

import static hre.lang.System.Debug;

/** Node that can hold every possible kind of defined name.
 * 
 * @author Stefan Blom
 *
 */
public class NameExpression extends ASTNode {

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
  }

  @Override
  public Iterable<String> debugTreeChildrenFields() {
    return JavaConverters.iterableAsScalaIterable(Collections.emptyList());
  }

  @Override
  public Iterable<String> debugTreePropertyFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("name", "kind", "reserved"));
  }

  /** The possible kinds of defined names */
  public static enum Kind {
    /** an unresolved name */
    Unresolved,
    /** argument to a function */
    Argument,
    /** local variable */
    Local,
    /** a field in a class */
    Field,
    /** a method in a class */
    Method,
    /** for the reserved names: null, this, and super. */
    Reserved,
    /** for labels, such as statement labels and predicate labels. */
    Label,
    /** for the ?x binder of VeriFast. */
    Output,
    /** name of an ADT */
    ADT;
  }
  
  /** The name that this AST node is referencing. */
  private String name;
  /** The reserved name this node contains. */
  private ASTReserved reserved;
  /** The kind of the definition being referenced. */
  private Kind kind;
  /** The site where this name was defined. */
  private ASTNode site;
  
  public NameExpression(ASTReserved name){
    reserved=name;
    kind=Kind.Reserved;
    this.name=name.toString();
  }
  
  /** Create an unresolved name expression */
  public NameExpression(String name){
    this.name=name;
    kind=Kind.Unresolved;
  }
  /** Create an specific kind of name expression */
  public NameExpression(Kind kind,ASTReserved word,String name){
    this.name=name;
    this.reserved=word;
    this.kind=kind;
  }
  
  public void setKind(Kind kind){
    if (kind==Kind.Reserved) hre.lang.System.Abort("cannot just declared a word reserved");
    this.kind=kind;
  }
  public Kind getKind(){
    return kind;
  }
  public ASTNode getSite(){
    return site;
  }
  public void setSite(ASTNode site){
    this.site=site;
  }
  public String getName() { return name; }

  
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
 
  public String toString(){ return name; }

  public boolean equals(Object o){
    if (o instanceof NameExpression){
      NameExpression other=(NameExpression)o;
      return name.equals(other.name);
    }
    return false;
  }
  
  public boolean isName(String name) {
    return this.name.equals(name);
  }

  public int hashCode(){
    if (name==null) hre.lang.System.Abort("name is null!");
    return name.hashCode();
  }
  
  public boolean isReserved(ASTReserved word) {
    if (word==null) return kind==Kind.Reserved;
    return reserved==word;
  }

  public ASTReserved reserved(){
    return reserved;
  }

  public boolean match(ASTNode ast){
    if (ast instanceof Hole){
      return ast.match(this);
    } else { 
      return equals(ast);
    }
  }
}

