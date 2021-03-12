// -*- tab-width:2 ; indent-tabs-mode:nil -*-
package vct.col.ast.stmt.decl;

import hre.ast.MessageOrigin;
import hre.util.FilteredIterable;
import hre.util.Function;

import java.util.*;

import scala.Option;
import scala.collection.JavaConverters;
import vct.col.ast.stmt.decl.Method.Kind;
import vct.col.ast.util.*;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;

import static hre.lang.System.Debug;

/** This class is the main container for declarations.
 *  For Java it contains both classes and packages.
 *  For PVL it contain both classes and the top level class list
 *  and block.
 * @author sccblom
 *
 */
public class ASTClass extends ASTDeclaration implements ASTSequence<ASTClass> {
  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
  }

  @Override
  public scala.collection.Iterable<String> debugTreeChildrenFields() {
    return DeclHelper.toIterable("parameters", "super_classes", "implemented_classes", "entries");
  }

  @Override
  public scala.collection.Iterable<String> debugTreePropertyFields() {
    return DeclHelper.toIterable("kind");
  }

  /**
   * Enumeration of the kinds of classes that are considered.
   * 
   * @author sccblom
   */
  public static enum ClassKind {
    Interface,
    Abstract,
    Plain,
    Kernel,
    Record
  };
  
  /** contains the kind of class. */
  public final ClassKind kind;
  /** contains the class parameter declarations. */
  public final DeclarationStatement parameters[];
  /** contains the classes extended by this class */
  public final ClassType super_classes[];
  /** contains the interfaces(classes) implemented by this class */
  public final ClassType implemented_classes[];
  /** contains the parent of this unit */
  private ASTClass parent_class;
  /** contains the list of entries */
  private ArrayList<ASTNode> entries=new ArrayList<ASTNode>();
  
  private void getFullName(ArrayList<String> fullname){
    if (parent_class!=null) parent_class.getFullName(fullname);
    if (name() != null) fullname.add(name()); 
  }
  
  public String[] getFullName(){
    ArrayList<String> fullname=new ArrayList<String>();
    getFullName(fullname);
    return fullname.toArray(new String[0]);
  }
  
  public ClassName getDeclName(){
    return new ClassName(getFullName());
  }

  /** Create a root class from a given block statement. */
  public ASTClass(BlockStatement node) {
    super("<<main>>");
    kind=ClassKind.Plain;
    int N=node.getLength();
    for(int i=0;i<N;i++){
      ASTNode tmp=node.getStatement(i);
      tmp.setFlag(STATIC,true);
      entries.add(tmp);
    }
    super_classes=new ClassType[0];
    implemented_classes=new ClassType[0];
    parameters=new DeclarationStatement[0];
  }

  /** Create a nested class. */
  public ASTClass(String name,ASTClass parent,boolean is_static,ClassKind kind){
    super(name);
    this.kind=kind;
    this.parent_class=parent;
    if(is_static){
      parent.add_static(this);
    } else {
      parent.add_dynamic(this);
    }
    super_classes=new ClassType[0];
    implemented_classes=new ClassType[0];
    parameters=new DeclarationStatement[0];
  }
  /** Return a static child, which is created if necessary. */
  public ASTClass getStaticClass(String name,ClassKind kind){
    int N=entries.size();
    for(int i=0;i<N;i++){
      if (get(i) instanceof ASTClass){
        ASTClass cl=(ASTClass)entries.get(i);
        if (cl.name().equals(name)) {
          if (cl.isStatic()) throw new Error("class "+name+" already exists as a dynamic entry");
          return cl;
        }
      }
    }
    ASTClass res=new ASTClass(name,this,true,kind);
    res.setOrigin(new MessageOrigin("get static class"));
    add_static(res);
    return res;
  }

  /** Create a new named class from two block statements
   *  Do not forget to set the parent later! 
   */
  public ASTClass(String name,BlockStatement static_part,BlockStatement dynamic_part,ClassKind kind){
    super(name);
    this.kind=kind;
    int N=static_part.getLength();
    for(int i=0;i<N;i++){
      add_static(static_part.getStatement(i));
    }
    N=dynamic_part.getLength();
    for(int i=0;i<N;i++){
      add_dynamic(dynamic_part.getStatement(i));
    }    
    super_classes=new ClassType[0];
    implemented_classes=new ClassType[0];
    parameters=new DeclarationStatement[0];
  }

  public String getName(){
    return name();
  }
  
  public ASTClass getParentClass(){
    return parent_class;
  }
  public void setParentClass(ASTClass parent){
    this.parent_class=parent;
  }
  public void add_static(ASTNode n){
    if (n==null) return;
    entries.add(n);
    n.setParent(this);
    n.setStatic(true);
    if (n instanceof ASTClass) {
      ((ASTClass)n).setParentClass(this);
    }
  }
  public void add_dynamic(ASTNode n){
    if (n==null) return;
    n.setParent(this);
    n.setStatic(false);
    entries.add(n);
    if (n instanceof ASTClass) {
      ((ASTClass)n).setParentClass(this);
    }
  }
  
  public ASTNode getStatic(int i){
    int N=0;
    for(ASTNode n:entries){
      if (n.isStatic()) {
        if (N==i) return n;
        N++;
      }
    }
    return null;
  }
  public int getStaticCount(){
    int N=0;
    for(ASTNode n:entries){
      if (n.isStatic()) N++;
    }
    return N;
  }
  public ASTNode getDynamic(int i){
    int N=0;
    for(ASTNode n:entries){
      if (!n.isStatic()) {
        if (N==i) return n;
        N++;
      }
    }
    return null;
  }
  public int getDynamicCount(){
    int N=0;
    for(ASTNode n:entries){
      if (!n.isStatic()) N++;
    }
    return N;
  }
  
  public ASTClass(String name,ClassKind kind){
    this(name,kind,new DeclarationStatement[0],new ClassType[0],new ClassType[0]);
  }

  /** Create a new class without putting it in a hierarchy. */
  public ASTClass(String name,ClassKind kind,DeclarationStatement parameters[],ClassType bases[],ClassType supports[]){
    super(name);
    Objects.requireNonNull(bases, "super class array may not be null");
    Objects.requireNonNull(supports, "implemented array may not be null");
    this.kind=kind;
    super_classes=Arrays.copyOf(bases,bases.length);
    implemented_classes=Arrays.copyOf(supports,supports.length);
    this.parameters=Arrays.copyOf(parameters,parameters.length);
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
 

  /** Perform a lookup of a full class name in a hierarchy.
   */
  public ASTClass find(String[] name){
    return find(name,0);
  }
  
  /** 
   * Auxiliary function for class lookup.
   */
  public ASTClass find(String[] name,int pos) {
    for(ASTNode n:entries){
      if (n instanceof ASTClass){
        ASTClass c=(ASTClass)n;
        if (c.name().equals(name[pos])) {
          pos++;
          if (pos==name.length) return c;
          else return find(name,pos);
        }
      }
    }
    return null;
  }
  private Method find(List<ASTNode> list,String name, ClassType object_type, Type[] type, ExternalClassLoader loader, NameSpace ns){
    node:for(ASTNode n:list){
      if (n instanceof Method){
        Method m=(Method)n;
        if (m.getName().equals(name)){
          Debug("checking type of method %s",name);
          DeclarationStatement args[]=m.getArgs();
          boolean varargs=m.usesVarArgs();
          if (args.length>=type.length || varargs){
            int idx;
            Debug("attempting match");
            MultiSubstitution sigma=m.getSubstitution(object_type);
            for(int i=0;i<type.length;i++){
              if (varargs && i>=args.length){
                idx=args.length-1;
              } else {
                idx=i;
              }
              Type m_idx_t=sigma.rewrite(m.getArgType(idx));
              if (!m_idx_t.supertypeof(type[i], Option.apply(root()), Option.apply(loader), Option.apply(ns))){
                if (m_idx_t.isPrimitive(PrimitiveSort.Location)){
                  Type lt=(Type)m_idx_t.firstarg();
                  if (!lt.supertypeof(root(), type[i])){
                    continue node;
                  }
                } else {
                  Debug("type of argument %d does not match method (cannot pass %s as %s)",i,type[i],m_idx_t);
                  continue node;
                }
              }
            }
            if (varargs && type.length==args.length-1) return m;
            for(int i=type.length;i<args.length;i++){
              if (!args[i].isValidFlag(GHOST) || !args[i].getFlag(GHOST)) {
                Debug("omitted argument is not ghost - no match");
                continue node;
              }
            }
            return m;
          }
        }
      }
    }
    return null;
  }

  public Method find(String name, ClassType object_type, Type[] type) {
    return find(name, object_type, type, null, null);
  }

  public Method find(String name, ClassType object_type, Type[] type, ExternalClassLoader loader, NameSpace ns) {
    return find(name,object_type,type, loader, ns, true);
  }

  public Method find(String name, ClassType object_type, Type[] type, boolean recursive) {
    return find(name, object_type, type, null, null, recursive);
  }

  public Method find(String name, ClassType object_type, Type[] type, ExternalClassLoader loader, NameSpace ns, boolean recursive) {
    //TODO: support inheritance and detect duplicate definitions.
    Method m=find(entries, name, object_type, type, loader, ns);
    if (m!=null) return m;
    if (recursive){
      for(ClassType parent:this.super_classes){
        ASTClass rp = (ASTClass) parent.definitionJava(root(), loader, ns);
        Objects.requireNonNull(rp, String.format("could not find %s", parent));
        m = rp.find(name, object_type, type, loader, ns, true);
        if (m != null) return m;
      }
    }  
    return m;
  }

  private DeclarationStatement find_field(List<ASTNode> list,String name) {
    for(ASTNode n:list){
      if (n instanceof DeclarationStatement){
        DeclarationStatement d=(DeclarationStatement)n;
        if (d.name().equals(name)) {
          return d;
        } else {
          Debug("skipping field " + d.name());
        }
      }
      if (n instanceof Method) {
        Method m = (Method)n;
        Debug("skipping method "+m.getName());
      }
    }
    return null;
  }

  /**
   * Search for a field in the current class.
   */
  public DeclarationStatement find_field(String name) {
    return find_field(name,false);    
  }
  /**
   * Search for a field declaration.
   * @param name The name of the field to be found.
   * @param recursive Flag that signals inclusion of super classes in the search path.
   * @return The Declaration of the field if found and null otherwise.
   */
  public DeclarationStatement find_field(String name,boolean recursive){
    Debug("looking for field "+name);
    DeclarationStatement temp=find_field(entries,name);
    if (temp!=null) return temp;
    if (contract!=null){
      for(DeclarationStatement tmp : contract.given){
         if (tmp.name().equals(name)) return tmp;
      }
    }
    if (!recursive) return temp;
    for(ClassType parent:this.super_classes){
      temp = root().find(parent).find_field(name,true);
      if (temp != null) return temp;
    }
    return null;
  }

  /** Get an iterator for all static members. */
  public Iterable<ASTNode> staticMembers(){
    return new FilteredIterable<ASTNode,ASTNode>(entries,new Function<ASTNode,ASTNode>(){
      public ASTNode apply(ASTNode n){
        return n.isStatic()?n:null;
      }
    });
  }
  
  /** Get an iterator for all dynamic members. */
  public Iterable<ASTNode> dynamicMembers(){
    return new FilteredIterable<ASTNode,ASTNode>(entries,new Function<ASTNode,ASTNode>(){
      public ASTNode apply(ASTNode n){
        return n.isStatic()?null:n;
      }
    });
  }

  /** Get an iterator for the fields of the class. */
  public Iterable<DeclarationStatement> fields() {
    return new FilteredIterable<ASTNode,DeclarationStatement>(this,new DeclarationFilter());
  }
  
  /** Get an iterator for the static fields of the class. */
  public Iterable<DeclarationStatement> staticFields() {
    return new FilteredIterable<ASTNode,DeclarationStatement>(staticMembers(),new DeclarationFilter());
  }
  
  /** Get an iterator for the dynamic fields of the class. */
  public Iterable<DeclarationStatement> dynamicFields() {
    return new FilteredIterable<ASTNode,DeclarationStatement>(dynamicMembers(),new DeclarationFilter());
  }
  
  /** Get an iterator for the methods of the class. */
  public Iterable<Method> methods() {
    return new FilteredIterable<ASTNode,Method>(this,new MethodFilter());
  }

  /** Get an iterator for the static methods of the class. */
  public Iterable<Method> staticMethods() {
    return new FilteredIterable<ASTNode,Method>(staticMembers(),new MethodFilter());
  }

  /** Get an iterator for the dynamic methods of the class. */
  public Iterable<Method> dynamicMethods() {
    return new FilteredIterable<ASTNode,Method>(dynamicMembers(),new MethodFilter());
  }
  
  private Contract contract;
  public Contract getContract(){
    return contract;
  }
  public void setContract(Contract contract){
    this.contract=contract;
    if (contract!=null){
      for(DeclarationStatement d:contract.given){
        d.setParent(this);
      }
      for(DeclarationStatement d:contract.yields){
        d.setParent(this);
      }
    }
  }
  public Method find_predicate(String string) {
    for(Method m:staticMethods()){
      if (m.getName().equals(string)) return m;
    }
    for(Method m:dynamicMethods()){
      if (m.getName().equals(string)) return m;
    }
    return null;
  }

  @Override
  public Iterator<ASTNode> iterator() {
    return entries.iterator();
  }

  @Override
  public ASTClass add(ASTNode n) {
    if (n==null) return this;
    entries.add(n);
    if (!n.isValidFlag(STATIC)){
      Debug("static flag not set");
      n.setStatic(false);
    }
    n.setParent(this);
    if (n instanceof ASTClass) {
      ((ASTClass)n).setParentClass(this);
    }
    return this;
  }

  @Override
  public int size() {
    return entries.size();
  }
  @Override
  public ASTNode get(int i) {
    return entries.get(i);
  }
  
  public boolean has_constructor(ProgramUnit context,Type[] c_args) {
    return get_constructor(context,c_args)!=null;
  }
  public Method get_constructor(ProgramUnit context,Type[] c_args) {
    outer:for(Method m:dynamicMethods()){
      if (m.kind==Kind.Constructor && c_args.length==m.getArity()){
        for(int i=0;i<c_args.length;i++){
          if(!m.getArgType(i).supertypeof(context,c_args[i])){
            continue outer;
          }
        }
        return m;
      }
    }
    return null;
  }

  public Boolean isOverloaded(String name) {
    boolean found=false;
    for(DeclarationStatement decl:fields()){
      if (decl.name().equals(name)){
        if(found){
          return true;
        } else {
          found=true;
        }
      }
    }
    for(Method m:methods()){
      if (m.name().equals(name)){
        if(found){
          return true;
        } else {
          found=true;
        }
      }
    }
    for(ClassType parent:this.super_classes){
      ASTClass cl = root().find(parent);
      Boolean temp=cl.isOverloaded(name);
      if (temp!=null){
        if (temp || found) return true; // may detect overriding instead of overloading...
        found=true;
      }
    }
    if (found) {
      return false;
    } else {
      return null;
    }
  }
}

