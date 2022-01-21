package vct.col.rewrite;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import hre.ast.MessageOrigin;
import vct.col.ast.stmt.decl.ASTClass;
import vct.col.ast.stmt.decl.ASTClass.ClassKind;
import vct.col.ast.stmt.decl.ASTFlags;
import vct.col.ast.type.*;
import vct.col.ast.stmt.decl.ASTSpecial;
import vct.col.ast.stmt.decl.AxiomaticDataType;
import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.expr.Dereference;
import vct.col.ast.stmt.decl.Method.Kind;
import vct.col.ast.expr.StandardOperator;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.expr.MethodInvokation;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.ASTUtils;
import vct.col.ast.util.AbstractRewriter;

public class JavaEncoder extends AbstractRewriter {

  public static final String INTERNAL = "internal_";
  
  
  public JavaEncoder(ProgramUnit source) {
    super(source);
  }

  @Override
  public void visit(ASTSpecial s){
    switch(s.kind){
    case Open:{
      MethodInvokation m=(MethodInvokation)s.args[0];
      ASTNode object=rewrite(m.object());
      currentBlock.add(create.special(ASTSpecial.Kind.Assert,
        create.expression(StandardOperator.EQ,
          create.expression(StandardOperator.TypeOf, object),
          rewrite(m.dispatch())
      )));

      String method= get_initial_definition(m.getDefinition()).name();
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
      ASTNode object=rewrite(m.object());
      currentBlock.add(create.special(ASTSpecial.Kind.Assert,
        create.expression(StandardOperator.EQ,
          create.expression(StandardOperator.TypeOf, object),
          rewrite(m.dispatch())
      )));

      String method= get_initial_definition(m.getDefinition()).name();
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
      String field= decl.name();
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
    if(d.field().equals(Dereference.ArrayLength()) || d.field().equals("item")) {
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
    String field= d.field();
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
            if (!(m.getContract() == null || m.getContract().isEmpty())) {
              m.getOrigin().report("info", "This method cannot be automatically inherited by class %s", cl.name());
              cl.getOrigin().report("error", "Automatic inheritance of method with non-empty contract is not supported");
              Fail("Automatic inheritance of method with contract not supported");
            }
            Contract external_contract=rewrite(m.getContract());
            Contract internal_contract=rewrite(m.getContract());
            Type returns=rewrite(m.getReturnType());
            String external_name = m.name();
            String internal_name = INTERNAL + m.name();
            boolean varArgs=m.usesVarArgs();
            res.add(create.method_kind(m.kind, returns, external_contract, external_name, parameters, varArgs, null));
            // We leave body empty, as the method is guaranteed to have no contract
            // Therefore no extra proof steps (unfolding predicates, checking pre/post conditions for implications)
            // are not necessary and we can leave the method abstract. However, when the empty contract check is
            // removed, a call to super or something similar should be added, such that the static/dynamic contract
            // implication is checked.
            res.add(create.method_kind(m.kind, returns, internal_contract, internal_name, parameters, varArgs, null));
            break;
          }
          case Predicate:{
            String name = m.name();
            res.add(create.predicate(name, null, parameters));
            name = INTERNAL + m.name();
            ASTNode body=create.invokation(
                create.reserved_name(ASTReserved.Super),
                null,
                m.name(),
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
    ASTClass definingClass = (ASTClass)m.getParent();
    ASTClass cls;
    // anything starting in a class named Atomic.... is inlined by CSL encoding.
    // uncomment the following lines if there is a problem with that....
    // cls=(ASTClass)m.getParent();
    // if (cls.name.startsWith("Atomic")) return true;

    // PB: what is the point of searching for superclasses to ascertain the method is final in a superclass? Surely
    // that should be a type error. We should keep the check for final defining class though.
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
          definingClass = cls;
          continue;
        }
      }
      break;
    }
    if (m != orig) return false;
    return definingClass.getFlag(ASTFlags.FINAL) || m.getFlag(ASTFlags.FINAL);
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
    Contract internal_contract=rewrite(m.getContract());
    Contract external_contract=rewrite(m.getContract());
    String name= m.name();
    ArrayList<DeclarationStatement> args=gen_pars(m);
    boolean varArgs=m.usesVarArgs();
    Type[] signals = rewrite(m.signals);
    if (m.getParent() instanceof ASTClass){
      ASTClass cls=(ASTClass)m.getParent();
      boolean direct=is_direct_definition(m);
      String internal_name = INTERNAL + m.name();
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
          Method res=create.method_kind(kind, returns, signals, external_contract, name, args, varArgs, body);
          res.setFlag(ASTFlags.FINAL, true);
          res.copyMissingFlags(m);
          currentTargetClass.add(res);         
        } else {
          Method abstractMethod = create.method_kind(kind, returns, signals, initial_contract, name, args, varArgs, null);
          abstractMethod.setFlag(ASTFlags.FINAL, true);
          currentTargetClass.add(abstractMethod);

          args=copy_rw.rewrite(args);
          ASTNode body=rewrite(m.getBody());

          Method implementedMethod = create.method_kind(kind, returns, signals, internal_contract, internal_name, args, varArgs, body);
          implementedMethod.setFlag(ASTFlags.FINAL, true);
          currentTargetClass.add(implementedMethod);
        }
        break;
      case Predicate:
        if (signals.length != 0) {
          Abort("Predicate with throws types detected");
        }
        if (direct){
          ASTNode body=rewrite(m.getBody());
          Method res=create.method_kind(kind, returns, null, name, args, varArgs, body);
          res.setFlag(ASTFlags.FINAL, true);
          res.copyMissingFlags(m);
          currentTargetClass.add(res);
        } else {
          Method abstractMethod = create.method_kind(kind, returns, null, name, args, varArgs, null);
          abstractMethod.setFlag(ASTFlags.FINAL, true);
          currentTargetClass.add(abstractMethod);

          args=copy_rw.rewrite(args);
          ASTNode body=rewrite(m.getBody());
          if (!isInitial){
            ASTNode args_names[]=new ASTNode[args.size()];
            for(int i=0;i<args_names.length;i++){
              args_names[i]=create.local_name(args.get(i).name());
            }
            ASTNode override=create.invokation(
                create.reserved_name(ASTReserved.Super), null, INTERNAL + m.name(), args_names);
            
            body=create.expression(StandardOperator.Star,override,body);
          }

          Method implementedMethod = create.method_kind(kind, returns, null, internal_name, args, varArgs, body);
          implementedMethod.setFlag(ASTFlags.FINAL, true);
          currentTargetClass.add(implementedMethod);
        }
        break;
      default:{
        ASTNode body=rewrite(m.getBody());
        result=create.method_kind(kind, returns, signals, external_contract, name, args, varArgs, body);
      }}
    } else {
      ASTNode body=rewrite(m.getBody());
      result=create.method_kind(kind, returns, signals, external_contract, name, args, varArgs, body);
    }
  }

  @Override
  public void visit(MethodInvokation s){
    Method m=s.getDefinition();
    if (m==null){
      super.visit(s);
      return;
    }
    ASTNode object;
    if (m.kind==Method.Kind.Constructor){
      object=rewrite(s.dispatch());
    } else {
      object=rewrite(s.object());
    }
    ClassType dispatch=rewrite(s.dispatch());
    String method;
    if (s.object()!=null && s.object().isReserved(ASTReserved.Super)
        && get_initial_definition(m)==get_initial_definition(current_method())){
      method = INTERNAL + m.name();
    } else if (dispatch!=null) {
      method = m.name();
    } else {
      // Dynamic dispatch or other call.
      if (m.getParent() instanceof ASTClass){
        m=get_initial_definition(m);
      }

      method= m.name();
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
