package vct.col.ast.stmt.decl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;

import hre.lang.HREError;
import hre.util.ScalaHelper;
import scala.collection.Iterable;
import scala.collection.JavaConverters;
import vct.col.ast.util.ASTMapping;
import vct.col.ast.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.stmt.decl.ASTDeclaration;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.util.ASTVisitor;
import vct.col.ast.util.ClassName;

import static hre.lang.System.Debug;

public class NameSpace extends ASTDeclaration implements ASTSequence<NameSpace> {

  @Override
  public Iterable<String> debugTreeChildrenFields() {
    return ScalaHelper.toIterable("imports", "space");
  }

  @Override
  public Iterable<String> debugTreePropertyFields() {
    return ScalaHelper.toIterable("full_name");
  }

  public static class Import {
    
    public final boolean static_import;
    
    public final boolean all;
    
    public final String[] name;
    
    public Import(boolean si,boolean all,String ... name){
      static_import=si;
      this.all=all;
      this.name=Arrays.copyOf(name,name.length);
    }
  }
  
  public static final String NONAME = "";

  private static String check_length(String name[]){
    if (name.length==0) throw new HREError("empty name space");
    return name[name.length-1];
  }
  
  public final ArrayList<Import> imports=new ArrayList<Import>();
  
  private ClassName full_name;
  
  public NameSpace(String ... name) {
    super(check_length(name));
    if (name.length==0) throw new HREError("empty name space");
    full_name=new ClassName(name);
  }

  @Override
  public ClassName getDeclName() {
    return full_name;
  }

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    try {
      return map.map(this,arg);
    } catch (Throwable t){
      if (thrown.get()!=t){
        Debug("Triggered by %s:",getOrigin());
        thrown.set(t);
     }
      throw t;
    }
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
   * A namespace contains declarations.
   */
  private ArrayList<ASTDeclaration> space=new ArrayList<ASTDeclaration>();

  @Override
  public int size(){
    return space.size();
  }
  
  @Override
  public ASTDeclaration get(int i){
    return space.get(i);
  }

  @SuppressWarnings({ "unchecked", "rawtypes" })
  @Override
  public Iterator<ASTNode> iterator() {
    return (Iterator)space.iterator();
  }

  @Override
  public NameSpace add(ASTNode item) {
    if (item instanceof ASTDeclaration){
      space.add((ASTDeclaration)item);
    } else if(item instanceof VariableDeclaration) {
      for(DeclarationStatement d:((VariableDeclaration)item).flatten()){
        space.add(d);
      }
    } else if (item==null) {
    } else {
      hre.lang.System.Warning("cannot insert %s into name space.",item);
      //Abort("cannot insert %s into name space.",item.getClass());
    }
    return this;
  }

  public void add_import(boolean si,boolean all,String ... name) {
    imports.add(new Import(si,all,name));    
  }


}
