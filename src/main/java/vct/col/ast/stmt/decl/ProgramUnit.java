package vct.col.ast.stmt.decl;

import hre.ast.MessageOrigin;

import java.util.*;

import scala.collection.JavaConverters;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.generic.ASTSequence;
import vct.col.ast.generic.DebugNode;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.util.ASTVisitor;
import vct.col.util.ASTFactory;
import vct.util.ClassName;
import vct.util.Configuration;
import static hre.lang.System.*;

/**
 * Class for containing a collection of classes.
 *  
 * @author sccblom
 *
 */
public class ProgramUnit implements ASTSequence<ProgramUnit>, DebugNode {
  public enum LanguageFlag {
    SeparateArrayLocations(true);

    private boolean defaultFlag;

    private LanguageFlag(boolean defaultFlag) {
      this.defaultFlag = defaultFlag;
    }

    public boolean getDefault() {
      return this.defaultFlag;
    }
  }

  public String toString(){
    return vct.util.Configuration.getDiagSyntax().print(this).toString();
  }
  
  private SpecificationFormat format=SpecificationFormat.Concurrent;
  
  public void setSpecificationFormat(SpecificationFormat format){
    this.format=format;
  }
  
  public SpecificationFormat getSpecificationFormat(){
    return format;
  }
  
  public static HashMap<ClassName, ASTClass> library=new HashMap<ClassName, ASTClass>();
  
  static {
    ASTFactory<?> create=new ASTFactory<Object>();
    create.setOrigin(new MessageOrigin("<<ProgramUnit>>"));
    ASTClass seq=create.ast_class("seq", ClassKind.Plain, null,null, null);
    Method len=create.function_decl(create.primitive_type(PrimitiveSort.Integer), null, "length", new DeclarationStatement[0], null);
    seq.add_dynamic(len);
    library.put(new ClassName("seq"),seq);
    //ASTClass var=create.ast_class("var", ClassKind.Plain, null, null);
    
  }

  public void library_add(ClassName name,ASTClass cl){
    library.put(name,cl);
  }

  private EnumMap<LanguageFlag, Boolean> languageFlags = new EnumMap<>(LanguageFlag.class);

  /**
   * A program is made up of declarations.
   */
  private ArrayList<ASTDeclaration> program=new ArrayList<ASTDeclaration>();

  public boolean hasLanguageFlag(LanguageFlag flag) {
    return languageFlags.getOrDefault(flag, flag.getDefault());
  }

  public void setLanguageFlag(LanguageFlag flag, boolean value) {
    languageFlags.put(flag, value);
  }

  public int size(){
    return program.size();
  }
  
  public ASTDeclaration get(int i){
    return program.get(i);
  }
  
  public Iterable<ASTDeclaration> get(){
    return program;
  }
  
  /**
   * Index of classes that are contained within this program unit. 
   */
  HashMap<ClassName,ASTClass> classes=new HashMap<ClassName, ASTClass>();
  
  /**
   * Index of declarations that are contained within this program unit. 
   */
  private HashMap<ClassName,ASTDeclaration> decl_map=new HashMap<ClassName,ASTDeclaration>();
  
  
  private HashMap<ClassName,Method> adt_map=new HashMap<ClassName,Method>();
  
  private HashMap<ClassName,Method> proc_map=new HashMap<ClassName,Method>();
  
  /*
  public void addClass(ClassName name,ASTClass cl){
    classes.put(name,cl);
    cl.attach(this,name);
  }
  
  public void addClass(String name[],ASTClass cl){
    addClass(new ClassName(name),cl);
  }
  
  public void addClass(ClassType type,ASTClass cl){
    addClass(type.getNameFull(),cl);
  }
  */
  
  /**
   * Create an empty program unit.
   */
  public ProgramUnit(){
    
  }

  /**
   * Create an empty program unit, but copy language flags.
   * @param source The source to copy language flags from
   */
  @SuppressWarnings("CopyConstructorMissesField")
  public ProgramUnit(ProgramUnit source) {
    if(source != null) {
      languageFlags.putAll(source.languageFlags);
    }
  }
  
  public void add(ASTDeclaration n){
    program.add(n);
    if (n instanceof NameSpace){
      NameSpace ns=(NameSpace)n;
      String prefix[];
      if (ns.name().equals(NameSpace.NONAME)) {
        prefix=new String[0];
      } else {
        prefix=ns.getDeclName().name;
      }
      for(ASTNode nn:ns){
        add(prefix,(ASTDeclaration)nn);
      }
    } else {
      add(new String[0],n);
    }
    
  }

  public void add(String prefix[],ASTDeclaration n){
    ClassName n1=n.getDeclName();
    if (n1==null){
      if (n instanceof ASTSpecial){
        
      } else {
        Debug("null named declaration");
        Debug("%s", Configuration.getDiagSyntax().print(n));
      }
    } else {
      ClassName n2=n1.prepend(prefix);
      decl_map.put(n2,n);
    }
    if (n instanceof Method){
      Method m=(Method)n;
      proc_map.put(m.getDeclName().prepend(prefix),m);
    }
    if (n instanceof ASTClass){
      ASTClass cl=(ASTClass)n;
      Debug("indexing %s as %s",cl.name(), cl.getDeclName());
      cl.attach(this,cl.getDeclName().prepend(prefix));
      classes.put(cl.getDeclName().prepend(prefix),cl);
      for(Method m : cl.staticMethods()){
        if (m.kind==Method.Kind.Predicate){
          decl_map.put(m.getDeclName().prepend(prefix),m);
        }          
      }
      for(Method m : cl.dynamicMethods()){
        if (m.kind==Method.Kind.Predicate){
          decl_map.put(m.getDeclName().prepend(prefix),m);
        }
      }
    }
    if (n instanceof AxiomaticDataType) {
      AxiomaticDataType adt=(AxiomaticDataType)n;
      for(Method m:adt.constructorsJava()){
        Debug("putting adt entry %s",m.getDeclName().toString("."));
        adt_map.put(m.getDeclName().prepend(prefix),m);
      }
      for(Method m:adt.mappingsJava()){
        adt_map.put(m.getDeclName().prepend(prefix),m);
      }
    }    
  }
 
  public Iterable<ASTClass> classes() {
    return classes.values();
  }

  public Iterable<ClassName> classNames() {
    return classes.keySet();
  }

  public <T> void accept(ASTVisitor<T> visitor) {
    for(ASTDeclaration decl:program){
      decl.accept(visitor);
    }
  }

  public ASTClass find(String ... name) {
    return find(new ClassName(name));
  }

  public ASTClass find(ClassName name) {
    ASTClass cl=classes.get(name);
    if (cl==null && name.name.length>1){
      ASTClass base=find(name.name[0]);
      if (base!=null){
        cl=base.find(name.name,1);
      }
    }
    if (cl==null){
      cl=library.get(name);
    }
    return cl;
  }

  public ASTClass find(ClassType type) {
    return find(new ClassName(type.getNameFull()));
  }

  public Method find_predicate(String[] nameFull) {
    String [] class_name=Arrays.copyOf(nameFull, nameFull.length-1);
    ASTClass cl=find(class_name);
    if (cl==null) {
      Debug("class %s not found",class_name[class_name.length-1]);
      return null;
    }
    Method m=cl.find_predicate(nameFull[nameFull.length-1]);
    if (m==null){
      Debug("predicate %s not found in class %s",nameFull[nameFull.length-1],class_name[0]);
    }
    return m;
  }

  public ASTDeclaration find_decl(String[] nameFull) {
    ClassName class_name=new ClassName(nameFull);
    ASTDeclaration res=decl_map.get(class_name);
    if (res==null){
      res=library.get(class_name);
    }
    return res;
  }
  
  public Method find_adt(String ... nameFull) {
    ClassName class_name=new ClassName(nameFull);
    return adt_map.get(class_name);
  }
  
  public Method find_procedure(String ... nameFull) {
    ClassName class_name=new ClassName(nameFull);
    return proc_map.get(class_name);
  }

  @SuppressWarnings({ "rawtypes", "unchecked" })
  @Override
  public Iterator<ASTNode> iterator() {
    return (Iterator)program.iterator();
  }

  @Override
  public ProgramUnit add(ASTNode item) {
    if (item instanceof ASTDeclaration){
      add((ASTDeclaration)item);
    } else if(item instanceof VariableDeclaration) {
      for(ASTDeclaration d:((VariableDeclaration)item).flatten_decl()){
        add(d);
      }
    } else {
      Abort("cannot insert %s into program unit.",item.getClass());
    }
    return this;
  }

  public void addFlags(ProgramUnit other) {
    for(Map.Entry<LanguageFlag, Boolean> entry : other.languageFlags.entrySet()) {
      if(this.languageFlags.containsKey(entry.getKey())) {
        if(this.languageFlags.get(entry.getKey()).booleanValue() != entry.getValue().booleanValue()) {
          Fail(String.format(
                  "Irreconcilable language flags: the flag %s was already set to %s, but was set to %s in a new entry.",
                  entry.getKey(),
                  this.languageFlags.get(entry.getKey()),
                  entry.getValue()));
        }
      } else {
        this.languageFlags.put(entry.getKey(), entry.getValue());
      }
    }
  }

  public void add(ProgramUnit unit) {
    this.addFlags(unit);

    for(ASTDeclaration decl:unit.get()){
      add(decl);
    }
  }

  public void index_classes(ASTSequence<?> seq){
    for(Object n:seq){
      if(n instanceof ASTClass){
        ASTClass cl=(ASTClass)n;
        Debug("indexing class %s",cl.getDeclName());
        classes.put(cl.getDeclName(),cl);
        index_classes(cl);
      }
    }
  }
  
  public void index_classes(){
    index_classes(this);
  }

  @Override
  public scala.collection.Iterable<String> debugTreeChildrenFields() {
    return JavaConverters.iterableAsScalaIterable(Arrays.asList("library", "program", "classes", "decl_map", "adt_map", "proc_map"));
  }

  @Override
  public scala.collection.Iterable<String> debugTreePropertyFields() {
    return JavaConverters.iterableAsScalaIterable(Collections.singletonList("format"));
  }
}
