package vct.col.rewrite;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import hre.ast.MessageOrigin;
import hre.lang.HREError;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.type.*;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.AxiomaticDataType;
import vct.col.ast.stmt.composite.BlockStatement;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.expr.Dereference;
import vct.col.ast.stmt.decl.Method.Kind;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.util.ASTUtils;

public class JavaEncoder extends AbstractRewriter {

  public static final String GLOBALS = "globals";
  public static final String ARRAY_SUFFIX="ARRAY";
  public static final String INTERNAL = "internal";
  
  
  public JavaEncoder(ProgramUnit source) {
    super(source);
  }
  
  private String create_type_name(Type t){
    if (t.isPrimitive(PrimitiveSort.Array)) {
      return create_type_name((Type)t.firstarg())+ARRAY_SUFFIX;
    } else if (t instanceof PrimitiveType){
      PrimitiveType pt=(PrimitiveType)t;
      switch(pt.sort){
      case Sequence:
        return "Sequence$"+create_type_name((Type)t.firstarg())+"$";
      case Set:
        return "Set$"+create_type_name((Type)t.firstarg())+"$";
      case Bag:
        return "Bag$"+create_type_name((Type)t.firstarg())+"$";
        case Array:
          return create_type_name((Type)t.firstarg())+ARRAY_SUFFIX;
        default:
          return t.toString();
      }
    } else if(t instanceof ClassType){
      ClassType ct=(ClassType)t;
      return ct.getFullName("_");
    }
    throw new HREError("cannot create encoding of%s",t);
  }
  
  private String create_field_name(ASTClass cls, String name){
    if (cls.name().equals("History") || cls.name().equals("Future")){
      return name;
    }
    String res="field_";
    for(String part:cls.getFullName()){
      res+=part+"_";
    }
    res+=name;
    return res;
  }

  private String create_method_name(String prefix,ClassType ct,Method m){
    String res=prefix;
    for(String part:ct.getNameFull()){
      res+="_"+part;
    }
    res+="_"+m.name();
    for(DeclarationStatement decl:m.getArgs()){
      res=res+"__"+create_type_name(decl.getType());
    }
    if (m.usesVarArgs()){
      res+=ARRAY_SUFFIX;
    }
    return res;
  }

  private String create_method_name(Method m){
    ASTNode parent=m.getParent();
    // ADT names are supposed to be globally unique.
    if (parent instanceof AxiomaticDataType){
      return "adt_"+m.name();
    }
    ASTClass cls=(ASTClass)parent;
    String prefix;
    if (cls==null){
      prefix="procedure_";
    } else {
      String name[]=cls.getFullName();
      if (m.name().equals(cls.name()) || m.getKind() == Kind.Constructor){
        prefix="constructor_";
      } else {
        prefix="method_";
      }
      for(String part:name){
        prefix+=part+"_";
      }
    }
    String res=m.name();
    for(DeclarationStatement decl:m.getArgs()){
      res=res+"__"+create_type_name(decl.getType());
    }
    if (m.usesVarArgs()){
      res+=ARRAY_SUFFIX;
    }
    return prefix+res;
  }

  @Override
  public void visit(ASTSpecial s){
    switch(s.kind){
    case Open:{
      MethodInvokation m=(MethodInvokation)s.args[0];
      ASTNode object=rewrite(m.object);
      currentBlock.add(create.special(ASTSpecial.Kind.Assert,
        create.expression(StandardOperator.EQ,
          create.expression(StandardOperator.TypeOf, object),
          rewrite(m.dispatch)
      )));
      String method=create_method_name(get_initial_definition(m.getDefinition()));
      ArrayList<ASTNode> args=new ArrayList<ASTNode>();
      args.add(create.local_name("globals"));
      for(ASTNode n:m.getArgs()){
        args.add(rewrite(n));
      }
      ASTNode abstract_version=create.invokation(object,null, method, args);
      currentBlock.add(create.special(ASTSpecial.Kind.Exhale, abstract_version));
      currentBlock.add(create.special(ASTSpecial.Kind.Inhale, rewrite(s.args[0])));
      break;
    }
    case Close:{
      MethodInvokation m=(MethodInvokation)s.args[0];
      ASTNode object=rewrite(m.object);
      currentBlock.add(create.special(ASTSpecial.Kind.Assert,
        create.expression(StandardOperator.EQ,
          create.expression(StandardOperator.TypeOf, object),
          rewrite(m.dispatch)
      )));
      String method=create_method_name(get_initial_definition(m.getDefinition()));
      ArrayList<ASTNode> args=new ArrayList<ASTNode>();
      args.add(create.local_name("globals"));
      for(ASTNode n:m.getArgs()){
        args.add(rewrite(n));
      }
      ASTNode abstract_version=create.invokation(object,null, method, args);
      currentBlock.add(create.special(ASTSpecial.Kind.Exhale, rewrite(s.args[0])));
      currentBlock.add(create.special(ASTSpecial.Kind.Inhale, abstract_version));
      break;
    }    
    default:
      super.visit(s);
      break;
    }
  }
  
  @Override
  public void visit(DeclarationStatement decl){
    if(decl.getParent() instanceof ASTClass){
      ASTClass cls=(ASTClass)decl.getParent();
      String field=create_field_name(cls,decl.name());
      DeclarationStatement res=create.field_decl(field,
          rewrite(decl.getType()),
          rewrite(decl.initJava()));
      if (decl.isStatic()){
        globals.add(res);
      } else {
        result=res;
      }
    } else {
      super.visit(decl);
    }
  }
  
  @Override
  public void visit(Dereference d){
    if(d.field().equals("length") || d.field().equals("item")) {
      super.visit(d);
      return;
    }

    ClassType t;
    if (d.obj() instanceof ClassType){
      t=(ClassType)d.obj();
    } else {
      Type tmp=d.obj().getType();
      if (tmp.isPrimitive(PrimitiveSort.Location)){
        tmp=(Type)tmp.firstarg();
      }
      t=(ClassType)tmp;
    }
    ASTClass cls=source().find(t);
    DeclarationStatement decl=cls.find_field(d.field(),true);
    ASTNode object;
    if (decl.isStatic()){
      object=create.local_name("globals");
    } else {
      object=rewrite(d.obj());
    }
    String field=create_field_name((ASTClass)decl.getParent(),d.field());
    result=create.dereference(object,field);
  }
  
  
  private Hashtable<ClassType,List<Method>> methods=new Hashtable<ClassType, List<Method>>();
  
  @Override
  public void visit(ASTClass cl){
    Debug("class %s",cl.name());
    if (!cl.isValidFlag(ASTFlags.FINAL)){
      cl.setFlag(ASTFlags.FINAL, false);
    }
    super.visit(cl);
    if (cl.name().equals("History")||cl.name().equals("Future")) return;
    ArrayList<Method> method_list=new ArrayList<Method>();
    for(Method m:cl.dynamicMethods()){
      if(m.kind==Kind.Constructor) continue;
      if(is_direct_definition(m)) continue;
      method_list.add(m);
    }
    ClassType cl_type=new ClassType(cl.getFullName());
    if (cl.super_classes.length>0){
      ASTClass res=(ASTClass)result;
      List<Method> parent_list=methods.get(cl.super_classes[0]);
      for(Method m:parent_list){
        int N=m.getArity();
        Type arg_type[]=new Type[N];
        DeclarationStatement pars[]=m.getArgs();
        for(int i=0;i<N;i++){
          arg_type[i]=pars[i].getType();
        }
        Method tmp=cl.find(m.name(),null,arg_type,false);
        if (tmp==null){
          method_list.add(m);
          ArrayList<DeclarationStatement> parameters=gen_pars(m);
          switch(m.kind){
          case Plain:{
            Contract external_contract=rewrite(m.getContract());
            internal_mode=true;
            Contract internal_contract=rewrite(m.getContract());
            internal_mode=false;
            Type returns=rewrite(m.getReturnType());
            String external_name=create_method_name("method", cl_type ,m);
            String internal_name=create_method_name(INTERNAL, cl_type ,m);
            boolean varArgs=m.usesVarArgs();
            res.add(create.method_kind(m.kind, returns, external_contract, external_name, parameters, varArgs, null));
            BlockStatement body=create.block();
            body.add(create.comment("//TODO: unfolds of chained predicates in pre-condition"));
            body.add(create.invokation(
                create.reserved_name(ASTReserved.Super),
                null,
                create_method_name("internal", cl.super_classes[0],m),
                get_names(parameters)));
            body.add(create.comment("//TODO: folds of chained predicates in post-condition"));
            res.add(create.method_kind(m.kind, returns, internal_contract, internal_name, parameters, varArgs, body));
            break;
          }
          case Predicate:{
            String name=create_method_name("predicate", cl_type ,m);
            res.add(create.predicate(name, null, parameters));
            name=create_method_name(INTERNAL, cl_type ,m);
            ASTNode body=create.invokation(
                create.reserved_name(ASTReserved.This),
                null,
                create_method_name("predicate", cl.super_classes[0],m),
                get_names(parameters));
            res.add(create.predicate(name, body, parameters));
            break;
          }
          default:
            Abort("unexpected kind %s",m.kind);
          }
        }
      }
      result=res;
    }
    methods.put(cl_type, method_list);
  }
  
  private Method get_initial_definition(Method m){
    if (m.isStatic()) return m;
    if (!m.isValidFlag(ASTFlags.FINAL)){
      m.setFlag(ASTFlags.FINAL, false);
    }
    int N=m.getArity();
    Type arg_type[]=new Type[N];
    DeclarationStatement pars[]=m.getArgs();
    for(int i=0;i<N;i++){
      arg_type[i]=pars[i].getType();
    }
    for(;;){
      ASTClass cls=(ASTClass)m.getParent();
      if (cls.super_classes.length>0){
        cls=source().find(cls.super_classes[0]);
        Method tmp=cls.find(m.name(),null,arg_type);
        if (tmp!=null){
          m=tmp;
          continue;
        }
      }
      return m;
    }
  }

  private boolean is_direct_definition(Method m){
    if (m.isStatic()) return true;
    if (m.isValidFlag(ASTFlags.INLINE) && m.getFlag(ASTFlags.INLINE)) return true;
    if (!m.isValidFlag(ASTFlags.FINAL)){
      m.setFlag(ASTFlags.FINAL, false);
    }
    ASTClass cls;
    // anything starting in a class named Atomic.... is inlined by CSL encoding.
    // uncomment the following lines if there is a problem with that....
    // cls=(ASTClass)m.getParent();
    // if (cls.name.startsWith("Atomic")) return true;
    Method orig=m;
    int N=m.getArity();
    Type arg_type[]=new Type[N];
    DeclarationStatement pars[]=m.getArgs();
    for(int i=0;i<N;i++){
      arg_type[i]=pars[i].getType();
    }
    for(;;){
      cls=(ASTClass)m.getParent();
      if (cls.super_classes.length>0){
        cls=source().find(cls.super_classes[0]);
        Method tmp=cls.find(m.name(),null,arg_type);
        if (tmp!=null){
          m=tmp;
          continue;
        }
      }
      break;
    }
    if (m != orig) return false;
    return cls.getFlag(ASTFlags.FINAL) || m.getFlag(ASTFlags.FINAL);
  }

  private Hashtable<Method,Contract> contract_table = new Hashtable<Method,Contract>();
  
  
  private ArrayList<DeclarationStatement> gen_pars(Method m){
    ArrayList<DeclarationStatement> args=new ArrayList<DeclarationStatement>();
    if (needs_globals(m)){
      args.add(create.field_decl("globals",create.class_type(globals.name())));
    }
    int N=m.getArity();
    DeclarationStatement pars[]=m.getArgs();
    for(int i=0;i<N;i++){
      args.add(rewrite(pars[i]));
    }
    return args;
  }
  
  private ASTNode[] get_names(List<DeclarationStatement> pars){
    ASTNode [] res=new ASTNode[pars.size()];
    for(int i=0;i<res.length;i++){
      res[i]=create.local_name(pars.get(i).name());
    }
    return res;
  }
  
  @Override
  public void visit(Method m){
    if (!m.isValidFlag(ASTFlags.FINAL)){
      m.setFlag(ASTFlags.FINAL, false);
    }
    Method.Kind kind=m.kind;
    Type returns=rewrite(m.getReturnType());
    internal_mode=true;
    Contract internal_contract=rewrite(m.getContract());
    internal_mode=false;
    Contract external_contract=rewrite(m.getContract());
    String name=create_method_name(m);
    ArrayList<DeclarationStatement> args=gen_pars(m);
    int N=m.getArity();
    Type arg_type[]=new Type[N];
    DeclarationStatement pars[]=m.getArgs();
    for(int i=0;i<N;i++){
      arg_type[i]=pars[i].getType();
    }
    boolean varArgs=m.usesVarArgs();
    if (m.getParent() instanceof ASTClass){
      ASTClass cls=(ASTClass)m.getParent();
      boolean direct=is_direct_definition(m);
      String internal_name=create_method_name(INTERNAL,new ClassType(cls.getFullName()), m);
      boolean isFinal=m.isStatic()||cls.getFlag(ASTFlags.FINAL)||m.getFlag(ASTFlags.FINAL);
      if (isFinal){
        Debug("  method %s is final",m.name());
      } else {
        Debug("  method %s is not final",m.name());
      }
      boolean isInitial=true;
      Method base=m;
      if (cls.super_classes.length>0){
        Debug("    super class is %s",cls.super_classes[0]);
        ASTClass parent=source().find(cls.super_classes[0]);
        base=get_initial_definition(m);
        if (base!=m){
          isInitial=false;
          Debug("    overrides class %s",((ASTClass)base.getParent()).getDeclName());
        } else {
          Debug("    initial declaration");
        }
      } else {
        Debug("    initial declaration");
      }
      Contract initial_contract;
      if (base==m){
        //TODO: make this into abstractified initial contract.
        // or maybe rewrite contract as the instantiated one...
        initial_contract=external_contract;
        if (initial_contract!=null) contract_table.put(base,initial_contract);
      } else {
        initial_contract=contract_table.get(base);
      }
      switch(m.kind){
      case Constructor:
      case Plain:
        if (direct){
          ASTNode body=rewrite(m.getBody());
          Method res=create.method_kind(kind, returns, external_contract, name, args, varArgs, body);
          res.copyMissingFlags(m);
          currentTargetClass.add(res);         
        } else {
          currentTargetClass.add(create.method_kind(kind, returns, initial_contract, name, args, varArgs, null));
          args=copy_rw.rewrite(args);
          internal_mode=true;
          ASTNode body=rewrite(m.getBody());
          internal_mode=false;
          currentTargetClass.add(create.method_kind(kind, returns, internal_contract, internal_name, args, varArgs, body));
        }
        break;
      case Predicate:
        if (direct){
          ASTNode body=rewrite(m.getBody());
          Method res=create.method_kind(kind, returns, null, name, args, varArgs, body);
          res.copyMissingFlags(m);
          currentTargetClass.add(res);
        } else {
          currentTargetClass.add(create.method_kind(kind, returns, null, name, args, varArgs, null));
          args=copy_rw.rewrite(args);
          internal_mode=true;
          ASTNode body=rewrite(m.getBody());
          internal_mode=false;
          if (!isInitial){
            ASTNode args_names[]=new ASTNode[args.size()];
            for(int i=0;i<args_names.length;i++){
              args_names[i]=create.local_name(args.get(i).name());
            }
            ASTNode override=create.invokation(
                create.reserved_name(ASTReserved.This),
                null,create_method_name(INTERNAL,cls.super_classes[0],m), args_names);
            
            body=create.expression(StandardOperator.Star,override,body);
          }
          currentTargetClass.add(create.method_kind(kind, returns, null, internal_name, args, varArgs, body));          
        }
        break;
      default:{
        ASTNode body=rewrite(m.getBody());
        result=create.method_kind(kind, returns, external_contract, name, args, varArgs, body);
      }}
    } else {
      ASTNode body=rewrite(m.getBody());
      result=create.method_kind(kind, returns, external_contract, name, args, varArgs, body);
    }
  }
  
  /**
   * This variable shall be set to true when rewriting a contract in
   * internal mode. It must be false otherwise. 
   */
  private boolean internal_mode=false;
  
  @Override
  public void visit(MethodInvokation s){
    Method m=s.getDefinition();
    if (m==null){
      super.visit(s);
      return;
    }
    ASTNode object;
    if (m.kind==Method.Kind.Constructor){
      object=rewrite(s.dispatch);
    } else {
      object=rewrite(s.object);
    }
    ClassType dispatch=rewrite(s.dispatch);
    String method;
    Type ot=null;
    if (s.object!=null){
      ot=s.object.getType();
    }
    if (s.object!=null && s.object.isReserved(ASTReserved.Super)
        && get_initial_definition(m)==get_initial_definition(current_method())){
      method=create_method_name("internal",(ClassType)ot,m);
    } else if (dispatch!=null) {
      // Static dispatch call...
      String prefix="unknown";
      switch(m.kind){
      case Predicate:
        if (is_direct_definition(m)||!internal_mode){
          prefix="predicate";
        } else {
          prefix=INTERNAL;
        }
        break;
      case Constructor: prefix="constructor"; break;
      default:
        break;
      }
      method=create_method_name(prefix,dispatch,m);
    } else {
      // Dynamic dispatch or other call.
      if (m.getParent() instanceof ASTClass){
        m=get_initial_definition(m);
      }
      method=create_method_name(m);
    }
    ArrayList<ASTNode> args=new ArrayList<ASTNode>();
    if (needs_globals(m)){
      args.add(create.local_name("globals"));
    }
    for(ASTNode a:s.getArgs()){
      args.add(rewrite(a));
    }
    MethodInvokation res=create.invokation(object, dispatch, method, args);
    res.set_before(rewrite(s.get_before()));
    res.set_after(rewrite(s.get_after()));
    result=res;
  }

  private boolean needs_globals(Method m) {
    if (m.kind==Method.Kind.Pure){
      Contract c=m.getContract();
      if (c==null) return false;
      for(ASTNode n:ASTUtils.conjuncts(c.pre_condition,StandardOperator.Star)){
        if (!n.getType().isBoolean()) return true;
      }
      for(ASTNode n:ASTUtils.conjuncts(c.post_condition,StandardOperator.Star)){
        if (!n.getType().isBoolean()) return true;
      }
      return false;
    }
    ASTNode parent=m.getParent();
    if (parent instanceof AxiomaticDataType) return false;
    if (parent instanceof ASTClass){
      ASTClass cl=(ASTClass)parent;
      return !cl.name().equals("Future") && !cl.name().equals("History");
    }
    return true;
  }

  private ASTClass globals;
  
  @Override
  public ProgramUnit rewriteAll(){
    globals=create.ast_class("EncodedGlobalVariables",ClassKind.Plain,null,null,null);
    globals.setOrigin(new MessageOrigin("Generated code: Encoded global variables"));
    ProgramUnit res=super.rewriteOrdered();
    res.add(globals);
    return res;
  }
}
