package vct.col.ast.stmt.decl;

import hre.ast.MessageOrigin;

import java.util.*;

import hre.util.ScalaHelper;
import vct.col.ast.util.ASTMapping;
import vct.col.ast.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.Type;
import vct.col.ast.util.ASTVisitor;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.MultiSubstitution;

import static hre.lang.System.Debug;

/**
 * This class represents the usual shorthand for the declaration of multiple variables.
 * The class stores the base type of the operation and for each of the instances
 * it uses the name of the variable being declared as type base type of the declaration.
 * 
 * For example, <code>int x[],y;</code> is represented with base type <code>int</code>
 * and two declarations <code>x[] x</code> and <code>y y</code>.
 * 
 * @author Stefan Blom
 *
 */
public class VariableDeclaration extends ASTNode {

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
  }

  /**
   * 
   */
  public static final String COMMON_NAME="__common_type__";
  public static final Type common_type=new ClassType(COMMON_NAME);
  static {
    common_type.setOrigin(new MessageOrigin("placeholder for common type"));
  }
  
  /**
   * Base type for all declarations 
   */
  public final Type basetype;

  /**
   * Multiple variable declarations on top of the given base type.
   */
  private ArrayList<ASTDeclaration> vars=new ArrayList<ASTDeclaration>();
  
  /**
   * Create an empty list of variables.
   * 
   * @param basetype
   */
  public VariableDeclaration(Type basetype){
    this.basetype=basetype;
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
 

  /**
   * Add a relative declaration.
   * 
   * @param decl
   */
  public void add(ASTDeclaration decl){
    vars.add(decl);
  }
  
  /**
   * Iterate over the (variable) declarations.
   */
  public Iterable<ASTDeclaration> get(){
    return vars;
  }

  /**
   * Flatten the list of relative declarations to a list of declarations
   * with the full type.
   *  
   * @return
   */
  public ASTDeclaration[] flatten_decl() {
    return flatten_list().toArray(new ASTDeclaration[0]);
  }
    
  public DeclarationStatement[] flatten() {
    return flatten_list().toArray(new DeclarationStatement[0]);
  }
    
  private ArrayList<ASTDeclaration> flatten_list(){
    ArrayList<ASTDeclaration> list=new ArrayList<ASTDeclaration>();
    Map<String,Type> map=new HashMap<String, Type>();
    AbstractRewriter rw=new MultiSubstitution(null,map);
    rw.create.setOrigin(getOrigin());
    map.put(COMMON_NAME,basetype);
    for(ASTDeclaration decl:vars){
      if (decl instanceof DeclarationStatement){
        DeclarationStatement d=(DeclarationStatement)decl;
        String name=d.name();
        map.put(name,basetype);
        Type fulltype=rw.rewrite(d.getType());
        map.remove(name);
        DeclarationStatement tmp=rw.create.field_decl(name,fulltype, rw.copy_rw.rewrite(d.initJava()));
        if (isValidFlag(ASTFlags.STATIC)){
          tmp.setStatic(isStatic());
        }
        list.add(tmp);
      } else {
        Method m=(Method)decl;
        list.add(rw.rewrite(m));
      }
    }
    return list;
  }

  @Override
  public scala.collection.Iterable<String> debugTreeChildrenFields() {
    return ScalaHelper.toIterable("basetype", "vars");
  }

  @Override
  public scala.collection.Iterable<String> debugTreePropertyFields() {
    return ScalaHelper.toIterable();
  }
}
