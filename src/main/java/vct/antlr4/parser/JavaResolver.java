package vct.antlr4.parser;

import java.io.File;
import java.lang.reflect.Modifier;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.LinkedList;
import java.util.Queue;

import hre.ast.FileOrigin;
import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.lang.HREError;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.stmt.decl.Method.Kind;
import vct.col.ast.stmt.decl.NameSpace.Import;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.rewrite.AbstractRewriter;
import vct.col.util.Parser;
import vct.util.ClassName;

public class JavaResolver extends AbstractRewriter {

  private ClassLoader path=this.getClass().getClassLoader();
  
  public JavaResolver(ProgramUnit source) {
    super(source);
  }
  
  private boolean ensures_loaded(String ... name){
    {
      ASTDeclaration decl=source().find_decl(name);
      if (decl!=null){
        if (decl instanceof ASTClass) return true;
        if (decl instanceof AxiomaticDataType) return true;
      }
    }
    if (name.length==1 && variables.containsKey(name[0])){
      return true;
    }
    if (target().find(ClassName.toString(name,FQN_SEP))!=null){
      return true;
    }
    if(base_path!=null && tryFileBase(base_path.toFile(),name)){
      return true;
    }
    if(tryFileBase(new File("."),name)){
      return true;
    }
    try {
      ClassName cln=new ClassName(name);
      String cl_name=cln.toString();
      Class<?> cl=path.loadClass(cl_name);
      Debug("loading %s",cl_name);
      create.enter();
      create.setOrigin(new MessageOrigin("library class %s",cl_name));
      ASTClass res=create.new_class(ClassName.toString(name,FQN_SEP),null,null);
      // TODO (Bob): Is this the right design decision?
      // Do not add the original class name to the library, as the class is renamed anyway!
      // This way we keep a single name of the class in the program consistently
      // Whenever it breaks we'll just have to fix it with the "real" name
//      target().library_add(cln,res);
      target().library_add(new ClassName(cln.toString(FQN_SEP)),res);
      // TODO (Bob): We use getDeclaredMethods to exclude inherited methods. Inherited methods should be added by a proper inheritance pass, ideally. But maybe that's wrong, and then that stuff should be handled right here.
      // However another possibility is to also pull in the extends/implements keywords here and make vercors understand them so flattening can happen whenever we want.
      for(java.lang.reflect.Method m:cl.getDeclaredMethods()){
        // Only public methods are allowed since protected is only accesible from the same package (and we're not std) and private is not accesible altogether
        if (!Modifier.isPublic(m.getModifiers())) {
          continue;
        }
        // We skip bridge methods (i.e. methods generated to bridge the gap between a method returning float[] and a method returning Object).
        // https://stackoverflow.com/questions/1961350/problem-in-the-getdeclaredmethods-java
        if (m.isBridge()) {
          continue;
        }
        Class<?> c=m.getReturnType();
        Type returns = convert_type(c);
        Class<?> pars[]=m.getParameterTypes();
        DeclarationStatement args[]=new DeclarationStatement[pars.length];
        for(int i=0;i<pars.length;i++){
          args[i]=create.field_decl("x"+i, convert_type(pars[i]));
        }
        if (m.isVarArgs()){
          DeclarationStatement old=args[pars.length-1];
          args[pars.length-1] = create.field_decl(old.name(), (Type)old.getType().firstarg());
        }
        Method ast=create.method_kind(Method.Kind.Plain , returns, null, m.getName(),args, m.isVarArgs() , null);
        ast.setFlag(ASTFlags.STATIC,Modifier.isStatic(m.getModifiers()));
        res.add(ast);
      }
      for (java.lang.reflect.Constructor<?> m : cl.getConstructors()) {
    	  Class<?> pars[]=m.getParameterTypes();
    	  DeclarationStatement args[]=new DeclarationStatement[pars.length];
    	  for(int i=0;i<pars.length;i++){
    		  args[i]=create.field_decl("x"+i, convert_type(pars[i]));
    	  }
    	  // Use class name here to uphold convention that constructor name == class name.
          // Constructors return void, since an out parameter is added later.
    	  Method ast = create.method_kind(Kind.Constructor, create.primitive_type(PrimitiveSort.Void), null, res.getName(), args, null);
    	  res.add(ast);
      }
      for(java.lang.reflect.Field field:cl.getFields()){
        Class<?> type=field.getType();
        DeclarationStatement decl=create.field_decl(field.getName(),convert_type(type));
        decl.setFlag(ASTFlags.STATIC,Modifier.isStatic(field.getModifiers()));
        res.add(decl);
      }
      target().add(res);
      create.leave();
      return true;
    } catch (ClassNotFoundException e) {
    }
    Debug("class %s not found",ClassName.toString(name,".."));
    return false;
  }

  private boolean tryFileBase(File base,String... name) {
    File file=base;
    for(int i=0;i<name.length-1;i++){
      file= new File(file,name[i]);
    }
    file=new File(file,name[name.length-1]+".java");
    if (file.exists()){
      Parser parser=Parsers.getParser("java");
      ProgramUnit pu=parser.parse(file);
      ASTClass cls=pu.find(name);
      if (cls!=null){
        // correct file, so remove bodies.
        pu=new RemoveBodies(pu).rewriteAll();
        // insert in source and processing queue.
        for(ASTDeclaration n:pu.get()){
          queue.add(n);
          source().add(n);
        }
        return true;
      } else {
        // wrong file.
        return false;
      }
    }
    return false;
  }

  protected Type convert_type(Class<?> c) {
    Type returns=null;
    if (c.isPrimitive()){
      switch(c.toString()){
      case "void":   
        returns=create.primitive_type(PrimitiveSort.Void);
        break;
      case "boolean":   
        returns=create.primitive_type(PrimitiveSort.Boolean);
        break;
      case "byte":   
        returns=create.primitive_type(PrimitiveSort.Byte);
        break;
      case "char":   
        returns=create.primitive_type(PrimitiveSort.Char);
        break;
      case "double":   
        returns=create.primitive_type(PrimitiveSort.Double);
        break;
      case "float":   
        returns=create.primitive_type(PrimitiveSort.Float);
        break;
      case "int":   
        returns=create.primitive_type(PrimitiveSort.Integer);
        break;
      case "long":   
        returns=create.primitive_type(PrimitiveSort.Long);
        break;
      case "short":   
        returns=create.primitive_type(PrimitiveSort.Short);
        break;
      }
    } else if (c.isArray()) {
      returns=convert_type(c.getComponentType());
      returns=create.primitive_type(PrimitiveSort.Array, returns);
    } else if (c.getName().equals("java.lang.String")){
      returns=create.primitive_type(PrimitiveSort.String);
    } else {
      String name[]=c.getName().split("\\.");
      if (ensures_loaded(name)){
        returns=create.class_type(ClassName.toString(name,FQN_SEP));
      } else {
        Fail("could not load %s",c);
      }
    }
    if (returns==null){
      Fail("missing case (%s)",c);
    }
    return returns;
  }
  
  private String[] full_name(String ... name){
    if (ensures_loaded(name)){
      return name;
    }
    ClassName tmp;
    if (current_space!=null){
      tmp=new ClassName(name);
      tmp=tmp.prepend(current_space.getDeclName().name);
      if (ensures_loaded(tmp.name)){
        return tmp.name;
      }
      for(int i=current_space.imports.size()-1;i>=0;i--){
        Import imp=current_space.imports.get(i);
        if (imp.static_import) continue;
        if (imp.all){
          tmp=new ClassName(name);
          tmp=tmp.prepend(imp.name);
          if (ensures_loaded(tmp.name)){
            return tmp.name;
          }
        } else {
          String imp_name=imp.name[imp.name.length-1];
          if (name.length==1 && name[0].equals(imp_name)) {
            if (ensures_loaded(imp.name)){
              return imp.name;
            }            
          }
        }
      }
    }
    return null;
  }
  
  @Override
  public void visit(ClassType t){
    String name[]=t.getNameFull();
    if (ensures_loaded(name)){
      result=create.class_type(ClassName.toString(name,FQN_SEP),rewrite(t.argsJava()));
      return;
    }
    ClassName tmp;
    if (current_space!=null){
      tmp=new ClassName(name);
      tmp=tmp.prepend(current_space.getDeclName().name);
      if (ensures_loaded(tmp.name)){
        result=create.class_type(tmp.toString(FQN_SEP),rewrite(t.argsJava()));
        return;
      }
      for(int i=current_space.imports.size()-1;i>=0;i--){
        Import imp=current_space.imports.get(i);
        if (imp.static_import) continue;
        if (imp.all){
          tmp=new ClassName(name);
          tmp=tmp.prepend(imp.name);
          if (ensures_loaded(tmp.name)){
            result=create.class_type(tmp.toString(FQN_SEP),rewrite(t.argsJava()));
            return;
          }
        } else {
          String imp_name=imp.name[imp.name.length-1];
          if (name.length==1 && name[0].equals(imp_name)) {
            if (ensures_loaded(imp.name)){
              result=create.class_type(ClassName.toString(imp.name,FQN_SEP),rewrite(t.argsJava()));
              return;
            }            
          }
        }
      }
    }
    Fail("cannot resolve %s",new ClassName(name));
  }
  
  public static final String FQN_SEP="_DOT_";
  
  private NameSpace current_space=null;
  
  private String prefix="";
  
  private Path base_path=null;
  
  @Override
  public void visit(NameSpace ns){
    Origin o=ns.getOrigin();
    if (o instanceof FileOrigin){
      FileOrigin fo=(FileOrigin)o;
      base_path=Paths.get(fo.getName()).toAbsolutePath().getParent();
    }
    if (current_space!=null){
      throw new HREError("nested name spaces are future work");
    }
    current_space=ns;
    ns.imports.add(0,new NameSpace.Import(false,true,"java","lang"));
    prefix="";
    for(String part:ns.getDeclName().name){
      if (part==NameSpace.NONAME) continue;
      prefix+=part+FQN_SEP;
      if(base_path!=null){
        base_path=base_path.getParent();
      }
    }
    for(ASTNode item:ns){
      item=rewrite(item);
      item.setFlag(ASTFlags.STATIC,true);
      target().add(item);
    }
    current_space=null;
    prefix="";
    base_path=null;
    result=null;
  }

  @Override
  public void visit(ASTClass cl){
    ASTClass tmp=currentTargetClass;
    currentTargetClass=create.ast_class(
        prefix+cl.name(),
        cl.kind,
        rewrite(cl.parameters),
        rewrite(cl.super_classes),
        rewrite(cl.implemented_classes)
    );
    for(ASTNode node:cl){
      currentTargetClass.add(rewrite(node));
    }
    result=currentTargetClass;
    currentTargetClass=tmp;
  }
  @Override
  public void visit(NameExpression e){
    if (e.getKind()==NameExpression.Kind.Unresolved){
      String name=e.getName();
      VariableInfo info=variables.lookup(name);
      if (info!=null) {
        Debug("unresolved %s name %s found during java resolv",info.kind,name);
        e.setKind(info.kind);
      } else {
        String cname[]=full_name(name);
        if (cname!=null){
          result=create.class_type(ClassName.toString(cname,FQN_SEP));
          return;
        }
      }
    }
    super.visit(e);
  }

  @Override
  public void visit(Method m) {
    if(m.getKind() == Method.Kind.Constructor) {
      if(!m.getName().equals(current_class().getName())) {
        Fail("Constructor has a different name (%s) than the class in which it is defined (%s). Did you mean to add a return type to turn it into a method?", m.getName(), current_class().getName());
      }
    }

    super.visit(m);
  }
  
  private Queue<ASTDeclaration> queue=new LinkedList<ASTDeclaration>();
  
  @Override
  public ProgramUnit rewriteAll(){
    for(ASTDeclaration n:source().get()){
      queue.add(n);
    }
    while(!queue.isEmpty()){
      ASTDeclaration n=queue.remove();
      ASTNode tmp=rewrite(n);
      if (tmp!=null){
        target().add(tmp);
      }
    }
    target().index_classes();
    return target();
  }
}
