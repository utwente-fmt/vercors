package vct.col.util;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import scala.collection.JavaConverters;
import vct.col.ast.expr.NameExpression.Kind;
import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import vct.col.ast.util.RecursiveVisitor;
import vct.col.rewrite.MultiSubstitution;
import vct.col.rewrite.TypeVarSubstitution;
import vct.silver.SilverTypeMap;
import vct.util.Configuration;
import vct.col.util.SequenceUtils;

/**
 * This class implements type checking of simple object oriented programs.
 *
 * An object oriented programs is simple if it does not use overloading.
 *
 * @author Stefan Blom
 *
 */
@SuppressWarnings("incomplete-switch")
public class AbstractTypeCheck extends RecursiveVisitor<Type> {

  public void check(){
    for(ASTDeclaration entry:source().get()){
      entry.accept(this);
    }
  }

  @Override
  public void enter_after(ASTNode node){
    super.enter_after(node);
    if (node.isSpecial(ASTSpecial.Kind.Open)){
      variables.add("member",new VariableInfo(null,Kind.Label));
    }
  }

  public AbstractTypeCheck(ProgramUnit arg){
    super(arg,true);
  }

  public void visit(ConstantExpression e){
    Debug("constant %s",e);
    super.visit(e);
    if (e.getType()==null) Abort("untyped constant %s",e);
  }

  public void visit(PrimitiveType t){
    super.visit(t);

    int i = 0;
    for (ASTNode arg : t.argsJava()) {
      if (arg == null) Abort("argument %d not typed", i);
      i++;
    }

    t.setType(new PrimitiveType(PrimitiveSort.Class));
  }

  public void visit(ClassType t){
    super.visit(t);
    Debug("class type %s",t);
    String name[]=t.getNameFull();
    if (name.length==1){
      VariableInfo info=variables.lookup(name[0]);
      if (info!=null){
        Debug("kind is %s",info.kind);
        t.setType(t);
        return;
      } else {
        Debug("not a variable");
      }
    }
    ASTDeclaration decl=source().find_decl(t.getNameFull());
    if (decl==null){
      decl=source().find(t.getNameFull());
    }
    if (decl==null){
      Fail("type error: defined type "+t.getFullName()+" not found");
    }
    if (decl instanceof AxiomaticDataType){
      t.setType(t);
      t.setDefinition(decl);
      return;
    }

    ASTClass cl=source().find(t.getNameFull());
    if (cl==null) {
      Method m=null;
      // find external/dynamic dispatch predicates used for witnesses.
      if (name.length>1){
        m=source().find_predicate(t.getNameFull());
      }
      // find internal/static dispatch predicates used for witnesses.
      if (m==null){
        if (name.length >1){
          m=source().find_predicate(Arrays.copyOf(name, name.length-1));
        }
      }
      if (m==null &&
          !(name.length==3 && name[0].equals("java") && name[1].equals("lang") && name[2].equals("Object"))
          && !(name.length==1 && name[0].equals("Object"))){
        Fail("type error: class (or predicate) "+t.getFullName()+" not found");
      }
    }
    t.setType(t);
  }

  public Method find_method(MethodInvokation e){
    Method m=source().find_adt(e.method);
    if (m!=null){
      return m;
    }
    m=source().find_procedure(e.method);
    if (m!=null){
      return m;
    }
    if (e.object==null && e.dispatch!=null){
      // This is a constructor invokation.
      ClassType t=e.dispatch;
      ASTClass cl=source().find(t.getNameFull());
      if (cl==null){
        Fail("class %s not found",t);
      }
      ASTNode args[]=e.getArgs();
      Type c_args[]=new Type[args.length];
      for(int i=0;i<args.length;i++){
        c_args[i]=args[i].getType();
        if(c_args[i]==null){
          Fail("argument %d is not typed",i);
        }
      }
      m=cl.get_constructor(source(),c_args);
      if(m==null){
        Fail("Could not find constructor");
      } else {
        return m;
      }
    }
    if (e.object!=null){
      ClassType object_type=(ClassType)e.object.getType();
      int N=e.getArity();
      for(int i=0;i<N;i++){
        if (e.getArg(i).labels()>0) {
          for(int j=i+1;j<N;j++){
            if (e.getArg(j).labels()==0) Fail("positional argument following named argument");
          }
          N=i;
          break;
        }
      }
      Type type[]=new Type[N];
      for(int i=0;i<N;i++){
        type[i]=e.getArg(i).getType();
        if (type[i]==null) Abort("argument %d has no type.",i);
      }
      ASTClass cl=source().find(object_type.getNameFull());
      if (cl==null) Fail("could not find class %s used in %s",object_type.getFullName(),e);
      m=cl.find(e.method,object_type,type);
      while(m==null && cl.super_classes.length>0){
        cl=source().find(cl.super_classes[0].getNameFull());
        m=cl.find(e.method,object_type,type);
      }
      if (m==null) {
        /*
        String parts[]=e.method.split("_");
        if (parts.length==3 && parts[1].equals("get")){
          // TODO: check if parts[0] is a predicate.
          DeclarationStatement field=cl.find_field(parts[2]);
          if (field!=null) {
            Warning("assuming %s is implicit getter function",e.method);
            e.setType(field.getType());
          }
          return;
        }
        */
        String tmp="";
        if (N>0){
          tmp=type[0].toString();
          for(int i=1;i<N;i++){
            tmp=tmp+","+type[i].toString();
          }
        }
        Fail("could not find method %s(%s) in class %s at %s%n%s",e.method,tmp,object_type.getFullName(),e.getOrigin(),e);
      }
      return m;
    }
    Fail("Could not find method used in %s",e);
    throw null;
  }

  public void visit(MethodInvokation e){
    super.visit(e);
    ClassType object_type=null;
    if (e.object!=null){
      if(e.object.getType()==null){
        Fail("object has no type at %s",e.object.getOrigin());
      }
      if (!(e.object.getType() instanceof ClassType)){
        Fail("invokation on non-class");
      }
      object_type=(ClassType)e.object.getType();
    }
    int N=e.getArity();
    for(int i=0;i<N;i++){
      if (e.getArg(i).labels()>0) {
        for(int j=i+1;j<N;j++){
          if (e.getArg(j).labels()==0) Fail("positional argument following named argument");
        }
        N=i;
        break;
      }
    }
    Type type[]=new Type[N];
    for(int i=0;i<N;i++){
      type[i]=e.getArg(i).getType();
      if (type[i]==null) Abort("argument %d has no type.",i);
    }

    Method m=find_method(e);
    e.setDefinition(m);

    if (m.getParent() instanceof AxiomaticDataType){
      Type t=m.getReturnType();
      Map<String,Type> map=new HashMap<String, Type>();
      TypeVarSubstitution sigma=new TypeVarSubstitution(source(),map);
      if (!(e.object instanceof ClassType)){
        Fail("%s is not an ADT in %s",e.object,e);
      }
      SilverTypeMap.get_adt_subst(sigma.copy_rw,map,(ClassType)e.object);
      e.setType(sigma.rewrite(t));
      return;
    }

    for(int i = 0; i < N; i++) {
      Type argType = m.getArgType(i);
      ASTNode arg = e.getArg(i);

      if(argType.isPrimitive(PrimitiveSort.Option)) {
        arg.setType(argType);
      }
    }

    /**
     * The viper back-ends require integers and fractions to be distinguished.
     * Hence, we recursively force fractional arguments to be of
     * type fraction....
     */
    for(int i=0;i<N;i++){
      Type ti=m.getArgType(i);
      ASTNode arg=e.getArg(i);
      if (!ti.supertypeof(source(), arg.getType())){
        if (ti.isPrimitive(PrimitiveSort.Location)
          && (arg instanceof Dereference)
          && ((Type)ti.firstarg()).supertypeof(source(), arg.getType())
        ){
          // OK
        } else {
          Fail("argument type %d incompatible %s/%s:%s",i,ti,arg,arg.getType());
        }
      }
      if (ti.isPrimitive(PrimitiveSort.Fraction)||
          ti.isPrimitive(PrimitiveSort.ZFraction)){
        force_frac(arg);
      }
    }

    /*
    //m=source().find_procedure(e.method);
    if (m!=null){
      e.setDefinition(m);
      Type t=m.getReturnType();
      e.setType(t);
      int N=m.getArity();
      if (e.getArity()!=N){
        Fail("different number of arguments for %s (%d instead of %d)",m.name,e.getArity(),N);
      }
      for(int i=0;i<N;i++){
        Type ti=m.getArgType(i);
        ASTNode arg=e.getArg(i);
        if (!ti.supertypeof(source(), arg.getType())){
          Fail("argument type %d incompatible",i);
        }
        if (ti.isPrimitive(PrimitiveType.Sort.Fraction)||
            ti.isPrimitive(PrimitiveType.Sort.ZFraction)){
          force_frac(arg);
        }
      }
      return;
    }

    if (e.object==null){
      if (e.dispatch!=null){
        // This is a constructor invokation.
        ClassType t=e.dispatch;
        ASTClass cl=source().find(t.getNameFull());
        if (cl==null){
          Fail("class %s not found",t);
        }
        ASTNode args[]=e.getArgs();
        Type c_args[]=new Type[args.length];
        for(int i=0;i<args.length;i++){
          c_args[i]=args[i].getType();
          if(c_args[i]==null){
            Fail("argument %d is not typed",i);
          }
        }
        m=cl.get_constructor(source(),c_args);
        if(m==null){
          Fail("Could not find constructor");
        }
        e.setType(t);
        e.setDefinition(m);
        if (e.get_before()!=null) {
          enter_before(e);
          e.get_before().accept(this);
          leave_before(e);
        }
        if (e.get_after()!=null) {
          enter_after(e);
          e.get_after().accept(this);
          leave_after(e);
        }
        return;
      }
      Abort("unresolved method invokation (%s) at "+e.getOrigin(),e.method);
    }
    if (e.object.getType()==null) Abort("object has no type at %s",e.object.getOrigin());
    if (!(e.object.getType() instanceof ClassType)) Abort("invokation on non-class");
    ClassType object_type=(ClassType)e.object.getType();
    int N=e.getArity();
    for(int i=0;i<N;i++){
      if (e.getArg(i).labels()>0) {
        for(int j=i+1;j<N;j++){
          if (e.getArg(j).labels()==0) Fail("positional argument following named argument");
        }
        N=i;
        break;
      }
    }
    Type type[]=new Type[N];
    for(int i=0;i<N;i++){
      type[i]=e.getArg(i).getType();
      if (type[i]==null) Abort("argument %d has no type.",i);
    }
    ASTClass cl=source().find(object_type.getNameFull());
    if (cl==null) Fail("could not find class %s",object_type.getFullName());
    m=cl.find(e.method,object_type,type);
    while(m==null && cl.super_classes.length>0){
      cl=source().find(cl.super_classes[0].getNameFull());
      m=cl.find(e.method,object_type,type);
    }
    if (m==null){
      m=source().find_adt(e.method);
    }
    if (m==null) {
      String parts[]=e.method.split("_");
      if (parts.length==3 && parts[1].equals("get")){
        // TODO: check if parts[0] is a predicate.
        DeclarationStatement field=cl.find_field(parts[2]);
        if (field!=null) {
          Warning("assuming %s is implicit getter function",e.method);
          e.setType(field.getType());
        }
        return;
      }
      String tmp="";
      if (N>0){
        tmp=type[0].toString();
        for(int i=1;i<N;i++){
          tmp=tmp+","+type[i].toString();
        }
      }
      Fail("could not find method %s(%s) in class %s at %s",e.method,tmp,object_type.getFullName(),e.getOrigin());
    }
        */
    switch(m.kind){
    case Constructor:
      if (e.dispatch!=null){
        e.setType(e.dispatch);
      } else {
        e.setType((Type)e.object);
      }
      break;
    case Predicate:
      for(int i=0;i<N;i++){
        if (type[i].isPrimitive(PrimitiveSort.Fraction)){
          force_frac(e.getArg(i));
        }
      }
      e.setType(new PrimitiveType(PrimitiveSort.Resource));
      break;
    default:
      {
        MultiSubstitution sigma=m.getSubstitution(object_type);
        e.setType(sigma.rewrite(m.getReturnType()));
        break;
      }
    }
    if (e.get_before()!=null) {
      enter_before(e);
      e.get_before().accept(this);
      leave_before(e);
    }
    if (e.get_after()!=null) {
      enter_after(e);
      e.get_after().accept(this);
      leave_after(e);
    }
    auto_before_after=false;
  }

  public final void check_loc_val(Type loc_type,ASTNode val){
    check_loc_val(loc_type,val,"Types of location (%s) and value (%s) do not match.");
  }
  public final void check_loc_val(Type loc_type,ASTNode val,String fmt){
    if (loc_type==null) Abort("Location has no type.");
    Type val_type=val.getType();
    if (val_type==null) Abort("Value has no type has no type.");
    if (loc_type.toString().equals("<<label>>")) return;

    Debug("Comparing %s with %s as %s", loc_type, val, val_type);

    if(loc_type.isPrimitive(PrimitiveSort.Option)) {
      val.setType(loc_type);
    }

    if (!(loc_type.equals(val_type)
        ||loc_type.supertypeof(source(), val_type)
        ||loc_type.isNumeric()&&val_type.isNumeric()
    )){
      Fail(fmt,loc_type,val_type);
    }
    if (loc_type.isPrimitive(PrimitiveSort.Fraction)||loc_type.isPrimitive(PrimitiveSort.ZFraction)){
      force_frac(val);
    }
  }
  public void visit(AssignmentStatement s){
    ASTNode val=s.expression();
    val.accept(this);
    Type val_type=val.getType();
    if (val_type==null) Abort("Value %s has no type.",val);
    if (val_type.toString().equals("<<label>>")) {
      return;
    }
    s.location().accept(this);

    if(s.location().isa(StandardOperator.Subscript)) {
      // Need to check that the sequence is assignable
      OperatorExpression location = (OperatorExpression) s.location();
      SequenceUtils.SequenceInfo seqInfo = SequenceUtils.getTypeInfo(location.first().getType());
      if(!seqInfo.isAssignable()) {
        Fail("Elements of %s, which is of type %s, are immutable.", location.first(), location.first().getType());
      }
    }
    check_loc_val(s.location().getType(),s.expression());
  }

  public void visit(DeclarationStatement s){
    super.visit(s);
    Type t = s.getType();
    ASTNode e = s.initJava();
    if (e != null) {
      check_loc_val(t,e);
    }
  }

  public void visit(Method m){
    super.visit(m);
    String name=m.getName();
    ASTNode body=m.getBody();
    Contract contract=m.getContract();

    if (contract!=null){
      if (m.kind==Method.Kind.Predicate){
        ASTNode tt=new ConstantExpression(true);
        if (!contract.pre_condition.equals(tt)){
          Fail("predicates cannot have contracts (%s)",Configuration.getDiagSyntax().print(contract.pre_condition));
        }
        if (!contract.post_condition.equals(tt)){
          Fail("predicates cannot have contracts");
        }
      }
      Type pre_t=contract.pre_condition.getType();
      if (pre_t==null) Abort("untyped pre condition"); // TODO check boolean.
      if (!pre_t.isPrimitive(PrimitiveSort.Resource) && !pre_t.isPrimitive(PrimitiveSort.Boolean)){
        contract.pre_condition.getOrigin().report("error","pre condition is not a resource");
        Fail("type error");
      }
      Type post_t=contract.post_condition.getType();
      if (post_t==null) Abort("untyped post condition"); // TODO check boolean.
      if (!post_t.isPrimitive(PrimitiveSort.Boolean)){
        if (m.kind==Method.Kind.Pure){
          for(ASTNode clause:ASTUtils.conjuncts(contract.post_condition, StandardOperator.Star)){
            if (!clause.getType().isPrimitive(PrimitiveSort.Boolean)){
              clause.getOrigin().report("error","post condition of function "+m.name()+" is not a boolean");
              Fail("type error");
            }
          }
        } else if (!post_t.isPrimitive(PrimitiveSort.Resource)){
          contract.post_condition.getOrigin().report("error","post condition is not a resource");
          Fail("type error");
        }
      }
      if (contract.modifies!=null) for(ASTNode n:contract.modifies){
        n.accept(this);
      }
      if (contract.accesses!=null) for(ASTNode n:contract.accesses){
        n.accept(this);
      }
    }
    if (body!=null && (body instanceof BlockStatement)) {
      //TODO: determine type of block
      return;
    }
    if (body!=null) {
      Type bt=body.getType();
      if (bt==null) Abort("untyped body of %s has class %s",name,body.getClass());
      check_loc_val(m.getReturnType(),body,"return type (%s) does not match body (%s)");
    }
  }
  public void visit(NameExpression e){
    super.visit(e);
    Debug("%s name %s",e.getKind(),e.getName());
    Kind kind = e.getKind();
    String name=e.getName();
    switch(kind){
      case Unresolved:{
        VariableInfo info=variables.lookup(name);
        if (info!=null) {
          Debug("unresolved %s name %s found during type check",info.kind,name);
          e.setKind(info.kind);
          kind=info.kind;
        }
      }
    }
    switch(kind){
      case Argument:
      case Local:
      case Field:{
        VariableInfo info=variables.lookup(name);
        if (info==null) {
          for(String v:variables.keySet()){
            Debug("var %s : %s",v,variables.lookup(v).reference.getType());
          }
          e.getOrigin().report("undefined.name",String.format("%s name %s is undefined",kind,name));
          Fail("fatal error");
        }
        e.setSite(info.reference);
        if (info.kind!=kind){
          if ((kind==NameExpression.Kind.Local && info.kind==NameExpression.Kind.Argument)
            ||(kind==NameExpression.Kind.Argument && info.kind==NameExpression.Kind.Local)){
            Debug("mismatch of kinds %s/%s for name %s",kind,info.kind,name);
          } else {
            Abort("mismatch of kinds %s/%s for name %s",kind,info.kind,name);
          }
        }
        DeclarationStatement decl=(DeclarationStatement)info.reference;
        e.setType(decl.getType());
        break;
      }
      case Method:
        if (e.getType()!=null){
          Abort("type of member %s has been set illegally",name);
        }
        break;
      case Reserved:
        switch(e.reserved()){
        case EmptyProcess:{
          e.setType(new PrimitiveType(PrimitiveSort.Process));
          break;
        }
        case This:{
          ASTClass cl=current_class();
          if (cl==null){
            Fail("use of keyword this outside of class context");
          }
          e.setType(new ClassType(cl.getFullName()));
          break;
        }
        case Null:{
          e.setType(new ClassType("<<null>>"));
          break;
        }
        case FullPerm:{
          e.setType(new PrimitiveType(PrimitiveSort.Fraction));
          break;
        }
        case ReadPerm:{
          e.setType(new PrimitiveType(PrimitiveSort.Fraction));
          break;
        }
        case NoPerm:{
          e.setType(new PrimitiveType(PrimitiveSort.ZFraction));
          break;
        }
        case CurrentThread:{
          e.setType(new PrimitiveType(PrimitiveSort.Integer));
          break;
        }
        case OptionNone:{
          e.setType(new PrimitiveType(PrimitiveSort.Option,
              new ClassType("<<null>>")));
          break;
        }
      case Result:{
          Method m=current_method();
          if (m==null){
            Fail("Use of result keyword outside of a method context.");
          }
          e.setType(m.getReturnType());
          break;
        }
      case Super:{
          ASTClass cl=current_class();
          if (cl==null){
            Fail("use of keyword super outside of class context");
          }
          if (cl.super_classes.length==0) {
            Fail("class %s does not have a super type",cl.getName());
          }
          e.setType(cl.super_classes[0]);
          break;
        }
      case Any:{
          e.setType(new PrimitiveType(PrimitiveSort.Integer));
          break;
        }
        default:
            Abort("missing case for reserved name %s",name);
        }
        break;
      case Unresolved:{
        switch(name){
          case "tcount":
          case "gsize":
          case "tid":
          case "gid":
          case "lid":
            e.setType(new PrimitiveType(PrimitiveSort.Integer));
            break;
          default:
            for(String n:this.variables.keySet()){
              Debug("var %s: ...",n);
            }
            Abort("unresolved name %s found during type check at %s",name,e.getOrigin());
        }
        break;
      }
      case ADT:
        e.setType(new ClassType("<<adt>>"));
        break;
      case Label:
        e.setType(new ClassType("<<label>>"));
        break;
      case Output:
        VariableInfo info=variables.lookup(name);
        if (info==null) {
          Abort("%s name %s is undefined",kind,name);
        }
        e.setType(info.reference.getType());
        break;
      default:
        Abort("missing case for kind %s",kind);
        break;
    }
  }

  public void visit(OperatorExpression e) {
    Debug("operator %s", e.operator());
    StandardOperator op = e.operator();
    if (op == StandardOperator.PointsTo && e.arg(2).isa(StandardOperator.BindOutput)) {
      e.arg(0).accept(this);
      e.arg(1).accept(this);
      enter(e.arg(2));
      leave(e.arg(2));
      e.arg(2).setType(e.arg(1).getType());
      e.setType(new PrimitiveType(PrimitiveSort.Resource));
      return;
    }
    if (op == StandardOperator.AbstractState) {
      e.arg(0).accept(this);
      Type t = e.arg(0).getType();
      if (t == null) Fail("Data type unknown.");
      if (!(t instanceof ClassType)) {
        Fail("Data type must be a class type.");
      }
      ASTClass cl = source().find((ClassType) t);
      variables.enter();
      for (DeclarationStatement decl : cl.dynamicFields()) {
        variables.add(decl.name(), new VariableInfo(decl, Kind.Local));
      }
      e.arg(1).accept(this);
      t = e.arg(1).getType();
      if (t == null) Fail("Formula type unknown.");
      if (!t.isBoolean()) {
        Fail("expression type is %s rather than boolean", t);
      }
      variables.leave();
      e.setType(new PrimitiveType(PrimitiveSort.Resource));
      return;
    }
    super.visit(e);

    ASTNode[] operatorArgs = e.argsJava().toArray(new ASTNode[0]);

    if (e.operator().arity() != -1 && operatorArgs.length != e.operator().arity()) {
      throw Failure("Operator %s has incorrect number of argument (have %d, want %d)", e.operator(), operatorArgs.length, e.operator().arity());
    }

    Type[] tt = new Type[operatorArgs.length];

    for (int i = 0; i < operatorArgs.length; i++) {
      tt[i] = operatorArgs[i].getType();
      if (tt[i] == null) {
        throw Failure("Operator %s has argument %s, which has no type.", e.operator(), operatorArgs[i]);
      }
    }

    switch (op) {
      case VectorRepeat: {
        e.setType(new PrimitiveType(PrimitiveSort.Sequence, tt[0]));
        break;
      }
      case VectorCompare: {
        e.setType(new PrimitiveType(PrimitiveSort.Sequence, new PrimitiveType(PrimitiveSort.Integer)));
        break;
      }
      case MatrixRepeat: {
        e.setType(new PrimitiveType(PrimitiveSort.Sequence, new PrimitiveType(PrimitiveSort.Sequence, tt[0])));
        break;
      }
      case MatrixCompare: {
        e.setType(new PrimitiveType(PrimitiveSort.Sequence, new PrimitiveType(PrimitiveSort.Sequence, new PrimitiveType(PrimitiveSort.Integer))));
        break;
      }
      case MatrixSum: {
        Type t = (Type) ((PrimitiveType) tt[1]).firstarg();
        t = (Type) ((PrimitiveType) t).firstarg();
        e.setType(t);
        break;
      }
      case FoldPlus: {
        Type t = tt[0];
        if (t.isPrimitive(PrimitiveSort.Sequence)) {
          t = (Type) ((PrimitiveType) t).firstarg();
          if (!t.isPrimitive(PrimitiveSort.Integer)) {
            Fail("first argument of summation must be a sequence of integers");
          }
        } else {
          Fail("first argument of summation must be a sequence");
        }
        t = e.arg(1).getType();
        if (t.isPrimitive(PrimitiveSort.Sequence)) {
          t = (Type) ((PrimitiveType) t).firstarg();
        } else {
          Fail("argument of summation must be a sequence");
        }
        if (t.isPrimitive(PrimitiveSort.Boolean)) {
          e.setType(new PrimitiveType(PrimitiveSort.Integer));
        } else {
          e.setType(t);
        }
        break;
      }
      case IndependentOf: {
        e.setType(tt[0]);
        break;
      }
      case PVLidleToken:
      case PVLjoinToken: {
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      }
      case IterationOwner: {
        e.setType(new PrimitiveType(PrimitiveSort.Integer));
        break;
      }
      case TypeOf: {
        e.setType(new ClassType("<<null>>"));
        break;
      }
      case History: {
        String type = tt[0].toString();
        if (!type.endsWith("History")) {
          Fail("First argument of History must be a History class, not %s.", type);
        }
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      }
      case Future: {
        String type = tt[0].toString();
        if (!type.endsWith("Future")) {
          Fail("First argument of Future must be a Future class, not %s.", type);
        }
        force_frac(e.arg(1));
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      }
      case NewSilver: {
        // TODO: check arguments.
        e.setType(new ClassType("Ref"));
        break;
      }
      case RangeSeq: {
        if (!tt[0].isInteger()) Fail("type of left argument is %s rather than integer", tt[0]);
        if (!tt[1].isInteger()) Fail("type of right argument is %s rather than integer", tt[1]);
        e.setType(new PrimitiveType(PrimitiveSort.Sequence, tt[0]));
        break;
      }
      case Instance:
      case SubType:
      case SuperType: {
        e.setType(new PrimitiveType(PrimitiveSort.Boolean));
        break;
      }
      case Cast: {
        ASTNode t = e.arg(0);
        ASTNode exp = e.arg(1);
        if (t instanceof Type) {
          e.setType((Type) t);

          if (((Type) t).isPrimitive(PrimitiveSort.Option)) {
            exp.setType((Type) t);
          }
        } else {
          Fail("cannot cast to non-type %s", t.getClass());
        }
        break;
      }
      case Or: {
        if (tt[0].isPrimitive(PrimitiveSort.Process)) {
          if (!tt[1].isPrimitive(PrimitiveSort.Process)) {
            Fail("Cannot compose process with %s", tt[1]);
          }
          e.setType(tt[0]);
          break;
        }
        // fall through on purpose.
      }
      case And:
      case IFF: {
        if (!tt[0].isBoolean()) Fail("type of left argument is %s rather than boolean at %s", tt[0], e.getOrigin());
        if (!tt[1].isBoolean()) Fail("type of right argument is %s rather than boolean at %s", tt[1], e.getOrigin());
        e.setType(new PrimitiveType(PrimitiveSort.Boolean));
        break;
      }
      case Member: {
        if (tt[1].isPrimitive(PrimitiveSort.Sequence) || tt[1].isPrimitive(PrimitiveSort.Set) || tt[1].isPrimitive(PrimitiveSort.Bag)) {
          if (!tt[0].equals(tt[1].firstarg())) {
            Fail("%s cannot be a member of %s", tt[0], tt[1]);
          }
        } else {
          Fail("cannot determine members of %s", tt[1]);
        }
        if (tt[1].isPrimitive(PrimitiveSort.Bag)) {
          e.setType(new PrimitiveType(PrimitiveSort.Integer));
        } else {
          e.setType(new PrimitiveType(PrimitiveSort.Boolean));
        }
        break;
      }
      case NewArray: {
        tt[0] = (Type) e.arg(0);
        tt[1] = e.arg(1).getType();
        if (tt[1] == null) Fail("type of subscript unknown at %s", e.getOrigin());
        if (!tt[1].isInteger()) {
          Fail("subcript has type %s rather than integer", tt[1]);
        }

        e.setType(tt[0]);
        break;
      }
      case Implies: {
        if (!tt[0].isBoolean()) Fail("type of left argument is %s rather than boolean at %s", tt[0], e.getOrigin());
        if (!tt[1].isBoolean() && !tt[1].isPrimitive(PrimitiveSort.Resource)) {
          Fail("type of right argument is %s rather than boolean or resource at %s", tt[1], e.getOrigin());
        }
        e.setType(tt[1]);
        break;
      }
      case Star:
      case Wand: {
        if (!tt[0].isBoolean() && !tt[0].isPrimitive(PrimitiveSort.Resource))
          Fail("type of right argument is %s rather than boolean/resource at %s", tt[0], e.getOrigin());
        if (!tt[1].isBoolean() && !tt[1].isPrimitive(PrimitiveSort.Resource))
          Fail("type of right argument is %s rather than boolean/resource at %s", tt[1], e.getOrigin());
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      }
      case CurrentPerm: {
        check_location(e.arg(0), "argument of CurrentPerm");
        tt[0] = e.arg(0).getType();
        if (tt[0] == null) Fail("type of argument unknown at %s", e.getOrigin());
        e.setType(new PrimitiveType(PrimitiveSort.Fraction));
        break;
      }
      case Scale: {
        if (!tt[0].isNumeric()) Fail("scalar is %s rather than a numeric type at %s", tt[0], e.getOrigin());
        if (!tt[1].isResource()) Fail("Cannot scale type %s", tt[0]);
        force_frac(e.arg(0));
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      }
      case Unfolding: {
        if (!tt[0].isResource()) Fail("Cannot unfold type %s", tt[0]);
        e.setType(tt[1]);
        break;
      }
      case Held: {
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      }
      case HistoryPerm:
      case ActionPerm:
      case Perm: {
        check_location(e.arg(0), "first argument");
        if (!tt[1].isBoolean() && !tt[1].isNumeric())
          Fail("type of right argument is %s rather than a numeric type at %s", tt[1], e.getOrigin());
        force_frac(e.arg(1));
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      }
      case PointsTo: {
        check_location(e.arg(0), "first argument");
        tt[0] = e.arg(0).getType();
        if (tt[0] == null) Fail("type of left argument unknown at %s", e.getOrigin());
        tt[1] = e.arg(1).getType();
        if (tt[1] == null) Fail("type of middle argument unknown at %s", e.getOrigin());
        if (!tt[1].isBoolean() && !tt[1].isNumeric())
          Fail("type of middle argument is %s rather than a numeric type at %s", tt[1], e.getOrigin());
        force_frac(e.arg(1));
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        if (tt[2] == null) Fail("type of right argument unknown at %s", e.getOrigin());
        if (!tt[2].comparableWith(source(), tt[0])) {
          Fail("types of location and value (%s/%s) incomparable at %s",
                  tt[0], tt[2], e.getOrigin());
        }

        if (tt[0].isPrimitive(PrimitiveSort.Option)) {
          e.arg(2).setType(tt[0]);
        }
        break;
      }
      case Contribution: {
        tt[0] = e.arg(0).getType();
        if (tt[0] == null) Fail("type of left argument unknown at %s", e.getOrigin());
        check_loc_val(tt[0], e.arg(1), "Types of location (%s) and contribution (%s) do not match.");
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      }
      case Value:
        check_location(e.arg(0), "argument");
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      case AddsTo:
      case ReducibleSum:
      case ReducibleMin:
      case ReducibleMax:
      case ArrayPerm:
        // TODO: check arguments
        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      case Set:
      case Assign:
      case AddAssign:
      case SubAssign:
      case MulAssign:
      case DivAssign:
      case RemAssign:
      case AndAssign:
      case XorAssign:
      case OrAssign:
      case ShlAssign:
      case ShrAssign:
      case SShrAssign: {
        if (e.arg(0) instanceof NameExpression) {
          NameExpression name = (NameExpression) e.arg(0);
          if (name.getKind() == NameExpression.Kind.Label) break;
        }
        if (tt[0].getClass() != tt[1].getClass()) {
          Fail("Types of left and right-hand side arguments in assignment are incomparable at " + e.getOrigin());
        }
        e.setType(tt[0]);
        break;
      }
      case EQ:
      case NEQ: {
        if (!tt[0].comparableWith(source(), tt[1])) {
          Fail("Types of left and right-hand side argument are uncomparable: %s/%s", tt[0], tt[1]);
        }
        e.setType(new PrimitiveType(PrimitiveSort.Boolean));
        if (tt[0].isPrimitive(PrimitiveSort.ZFraction) || tt[0].isPrimitive(PrimitiveSort.Fraction)) {
          force_frac(e.arg(1));
        }
        if (tt[1].isPrimitive(PrimitiveSort.ZFraction) || tt[1].isPrimitive(PrimitiveSort.Fraction)) {
          force_frac(e.arg(0));
        }
        if (tt[0].isPrimitive(PrimitiveSort.Option)) {
          e.arg(1).setType(tt[0]);
        } else if (tt[1].isPrimitive(PrimitiveSort.Option)) {
          e.arg(0).setType(tt[1]);
        }
        break;
      }
      case ValidArray: {
        //TODO: check argument types.
        e.setType(new PrimitiveType(PrimitiveSort.Boolean));
        break;
      }
      case ValidMatrix: {
        //TODO: check argument types.
        e.setType(new PrimitiveType(PrimitiveSort.Boolean));
        break;
      }
      case ValidPointer:
        if (!tt[1].isIntegerType()) {
          Fail("The second argument to \\pointer should be an integer at %s", e.arg(1).getOrigin());
        }

        force_frac(e.arg(2));

        if (!tt[0].isPrimitive(PrimitiveSort.Pointer)) {
          SequenceUtils.expectArray(e.arg(0), "The first argument to \\pointer (%s) should be a pointer, but was of type %s");
        }

        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      case ValidPointerIndex:
        if (!tt[1].isIntegerType()) {
          Fail("The second argument to \\pointer_index should be an integer at %s", e.arg(1).getOrigin());
        }

        force_frac(e.arg(2));

        if (!tt[0].isPrimitive(PrimitiveSort.Pointer)) {
          SequenceUtils.expectArray(e.arg(0), "The first argument to \\pointer_index (%s) should be a pointer, but was of type %s");
        }

        e.setType(new PrimitiveType(PrimitiveSort.Resource));
        break;
      case Values: {
        Type t = e.arg(0).getType();
//      if (!t.isPrimitive(PrimitiveSort.Array)){
//
//      }
        if (t.isPrimitive(PrimitiveSort.Option)) {
          t = (Type) t.firstarg();
        }
        if (!t.isPrimitive(PrimitiveSort.Array)) {
          Abort("First argument to values must be array-like at " + e.getOrigin());
        } else {
          t = (Type) t.firstarg();
        }
        if (t.isPrimitive(PrimitiveSort.Cell)) {
          t = (Type) t.firstarg();
        }
        tt[0] = e.arg(1).getType();
        if (tt[0] == null) Fail("type of from argument unknown at " + e.getOrigin());
        if (!tt[0].isInteger()) Fail("type of from argument should be integer at " + e.getOrigin());
        tt[1] = e.arg(2).getType();
        if (tt[1] == null) Fail("type of upto argument unknown at " + e.getOrigin());
        if (!tt[1].isInteger()) Fail("type of upto argument should be integer at " + e.getOrigin());
        e.setType(new PrimitiveType(PrimitiveSort.Sequence, t));
        break;
      }
      case ITE: {
        Type t = e.arg(0).getType();
        if (!t.isBoolean()) {
          Abort("First argument of if-the-else must be boolean at " + e.getOrigin());
        }
        tt[0] = e.arg(1).getType();
        if (tt[0] == null) Fail("type of left argument unknown at " + e.getOrigin());
        tt[1] = e.arg(2).getType();
        if (tt[1] == null) Fail("type of right argument unknown at " + e.getOrigin());

        if (!tt[0].comparableWith(source(), tt[1])) {
          Fail("Types of left and right-hand side argument are uncomparable at " + e.getOrigin());
        }

        if (tt[1].supertypeof(source(), tt[0])) {
          //Warning("ITE type %s",tt[1]);
          e.setType(tt[1]);
        } else if (tt[0].supertypeof(source(), tt[1])) {
          //Warning("ITE type %s",tt[0]);
          e.setType(tt[0]);
        }
        if (tt[0].isPrimitive(PrimitiveSort.ZFraction) || tt[0].isPrimitive(PrimitiveSort.Fraction)) {
          force_frac(e.arg(2));
        }
        if (tt[1].isPrimitive(PrimitiveSort.ZFraction) || tt[1].isPrimitive(PrimitiveSort.Fraction)) {
          force_frac(e.arg(1));
        }
        break;
      }
      case Not: {
        Type t = e.arg(0).getType();
        if (!t.isBoolean()) {
          Abort("Argument of negation must be boolean at " + e.getOrigin());
        }
        e.setType(t);
        break;
      }
      case OptionSome: {
        Type t = e.arg(0).getType();
        e.setType(new PrimitiveType(PrimitiveSort.Option, t));
        break;
      }
      case OptionGet: {
        Type t = e.arg(0).getType();
        if (!t.isPrimitive(PrimitiveSort.Option)) {
          Fail("argument of option get is %s rather than option<?>", t);
        }
        e.setType((Type) ((PrimitiveType) t).firstarg());
        break;
      }
      case Identity: {
        Type t = e.arg(0).getType();
        e.setType(t);
        break;
      }
      case PreIncr:
      case PreDecr:
      case PostIncr:
      case PostDecr:
      case UMinus:
      case UPlus: {
        Type t = e.arg(0).getType();
        if (!t.isNumeric()) {
          Fail("Argument of %s must be a numeric type", op);
        }
        e.setType(t);
        break;
      }
      case Exp: {
        if (!tt[0].isNumeric()) {
          Fail("First argument of %s is %s rather than a numeric type", op, tt[0]);
        }
        if (!tt[1].isInteger()) {
          Fail("Second argument of %s is %s rather than integer", op, tt[1]);
        }
        e.setType(tt[0]);
        break;
      }
      case Plus: { // handle concatenation meaning of +
        if (tt[0].isPrimitive(PrimitiveSort.Sequence) || tt[0].isPrimitive(PrimitiveSort.Set) || tt[0].isPrimitive(PrimitiveSort.Bag)) {
          if (!tt[0].comparableWith(source(), tt[1])) {
            Fail("Types of left and right-hand side argument are uncomparable: %s/%s", tt[0], tt[1]);
          }
          e.setType(tt[0]);
          break;
        }
        if (tt[0].isPrimitive(PrimitiveSort.Process)) {
          if (!tt[1].isPrimitive(PrimitiveSort.Process)) {
            Fail("Cannot compose process with %s", tt[1]);
          }
          e.setType(tt[0]);
          break;
        }
        if (tt[0].isPrimitive(PrimitiveSort.Pointer) || SequenceUtils.getTypeInfo(tt[0]) != null) {
          if (!tt[1].isPrimitive(PrimitiveSort.Integer)) {
            Fail("Cannot add a value of type %s to a pointer", tt[1]);
          }
          e.setType(tt[0]);
          break;
        }
        checkMathOperator(e, op, tt[0], tt[1]);
        break;
      }
      case Mult: {
        // handle cartesian product meaning of *
        if (tt[0].isPrimitive(PrimitiveSort.Sequence) && tt[1].isPrimitive(PrimitiveSort.Sequence)) {
          tt[0] = (Type) ((PrimitiveType) tt[0]).firstarg();
          tt[1] = (Type) ((PrimitiveType) tt[1]).firstarg();
          e.setType(new PrimitiveType(PrimitiveSort.Sequence, new TupleType(new Type[]{tt[0], tt[1]})));
          break;
        }
        // handle intersection meaning of *
        if (tt[0].isPrimitive(PrimitiveSort.Set) || tt[0].isPrimitive(PrimitiveSort.Bag)) {
          if (!tt[0].comparableWith(source(), tt[1])) {
            Fail("Types of left and right-hand side argument are uncomparable: %s/%s", tt[0], tt[1]);
          }
          e.setType(tt[0]);
          break;
        }
        if (tt[0].isPrimitive(PrimitiveSort.Process)) {
          if (!tt[1].isPrimitive(PrimitiveSort.Process)) {
            Fail("Cannot compose process with %s", tt[1]);
          }
          e.setType(tt[0]);
          break;
        }
        checkMathOperator(e, op, tt[0], tt[1]);
        break;
      }
      case Minus: {
        // handle set minus meaning of -
        if (tt[0].isPrimitive(PrimitiveSort.Set) || tt[0].isPrimitive(PrimitiveSort.Bag)) {
          if (!tt[0].comparableWith(source(), tt[1])) {
            Fail("Types of left and right-hand side argument are uncomparable: %s/%s", tt[0], tt[1]);
          }
          e.setType(tt[0]);
          break;
        }
      }
      case Div:
      case FloorDiv:
      case Mod: {
        checkMathOperator(e, op, tt[0], tt[1]);
        break;
      }
      case BitAnd:
      case BitOr:
      case BitNot:
      case BitXor: {
        if (tt[0].equalSize(tt[1])) {
          e.setType(tt[0]);
        } else {
          Fail("Types of left and right-hand side argument are different (%s/%s).", tt[0], tt[1]);
        }
        break;
      }
      case RightShift:
      case LeftShift:
      case UnsignedRightShift: {
        if (!tt[0].isIntegerType()) {
          Fail("First argument of %s must be integer type, not %s", op, tt[0]);
        }
        if (!tt[1].isIntegerType()) {
          Fail("Second argument of %s must be integer type, not %s", op, tt[1]);
        }
        e.setType(tt[0]);
        break;
      }
      case GTE:
      case LTE:
      case LT:
      case GT: {
        if (!tt[0].isNumeric()) {
          Fail("First argument of %s is %s rather than a numeric type", op, tt[0]);
        }
        if (!tt[1].isNumeric()) {
          Fail("Second argument of %s is %s rather than a numeric type", op, tt[1]);
        }

        if(tt[0].isFraction()) force_frac(e.arg(1));
        else if(tt[1].isFraction()) force_frac(e.arg(0));

        e.setType(new PrimitiveType(PrimitiveSort.Boolean));
        break;
      }
      case Old: {
        Type t = e.arg(0).getType();
        if (t == null) Fail("type of argument is unknown at %s", e.getOrigin());
        e.setType(t);
        break;
      }
      case New: {
        ASTNode t = e.arg(0);
        if (!(t instanceof ClassType)) Fail("argument to new is not a class type");
        e.setType((Type) t);
        break;
      }
    case Drop:
    case Take:
    {
      SequenceUtils.SequenceInfo info = SequenceUtils.getTypeInfoOrFail(tt[0], "Expected this expression to be of a sequence type, but got %s.");

      if (info.getSequenceSort() != PrimitiveSort.Sequence && info.getSequenceSort() != PrimitiveSort.Array) {
        Fail("base must be of sequence type");
      }
      if (!tt[1].isInteger()) {
        Fail("count has type '%s' rather than integer", tt[1]);
      }
      e.setType(tt[0]);
      break;
    }
    case Slice:
    {
      if (!tt[0].isPrimitive(PrimitiveSort.Sequence)) {
        Fail("base must be of sequence type");
      }
      if (!tt[1].isInteger()) {
        Fail("left count has type '%s' rather than integer", tt[1]);
      }
      if (!tt[2].isInteger()) {
        Fail("right count has type '%s' rather than integer", tt[2]);
      }
      e.setType(tt[0]);
      break;
    }

    case SeqUpdate: {
      if (!tt[0].isPrimitive(PrimitiveSort.Sequence)) {
        Fail("base must be of sequence type");
      }

      // for example, if `tt[0]` is of type `seq<int>`, then `innerType` shall be `int`.
      Type innerType = (Type)tt[0].firstarg();

      if (!tt[1].isInteger()) {
        Fail("index has type '%s' rather than integer", tt[1]);
      }

      if (!tt[2].equals(innerType)) {
        Fail("the replacing element has type '%s' but should be '%s'", tt[2], innerType);
      }

      e.setType(tt[0]);
      break;
    }

    case Subscript:
    {
      if (!(tt[0] instanceof PrimitiveType)) Fail("base must be array or sequence type.");
      PrimitiveType t=(PrimitiveType)tt[0];
        if (t.isPrimitive(PrimitiveSort.Option)) {
          if (!(t.firstarg() instanceof PrimitiveType)) Fail("base must be array or sequence type.");
          t = (PrimitiveType) t.firstarg();
        }

        switch (t.sort) {
          case Pointer:
          case Sequence:
          case Array: {
            tt[0] = (Type) t.firstarg();
            break;
          }
          default:
            Fail("base must be array or sequence type.");
        }

        if (tt[0].isPrimitive(PrimitiveSort.Cell)) {
          tt[0] = (Type) tt[0].firstarg();
        }

        if (!tt[1].isInteger()) {
          Fail("subcript has type %s rather than integer", tt[1]);
        }
        e.setType(tt[0]);
        break;
      }
      case Head: {
        Type t = e.arg(0).getType();
        if (t == null) Fail("type of argument is unknown at %s", e.getOrigin());
        if (!t.isPrimitive(PrimitiveSort.Sequence)) Fail("argument of head is not a sequence");
        e.setType((Type) t.firstarg());
        break;
      }
      case Tail: {
        Type t = e.arg(0).getType();
        if (t == null) Fail("type of argument is unknown at %s", e.getOrigin());
        if (!t.isPrimitive(PrimitiveSort.Sequence)) Fail("argument of tail is not a sequence");
        e.setType(t);
        break;
      }
      case Size: {
        Type t = e.arg(0).getType();
        if (t == null) Fail("type of argument is unknown at %s", e.getOrigin());
        if (!(t.isPrimitive(PrimitiveSort.Sequence) || t.isPrimitive(PrimitiveSort.Bag) || t.isPrimitive(PrimitiveSort.Set))) {
          Fail("argument of size is not a set, sequence, or bag");
        }
        e.setType(new PrimitiveType(PrimitiveSort.Integer));
        break;
      }
      case Length: {
        Type t = e.arg(0).getType();
        if (t == null) Fail("type of argument is unknown at %s", e.getOrigin());

        SequenceUtils.expectArrayType(t, "Length expects an array as its argument, but got %s");
        e.setType(new PrimitiveType(PrimitiveSort.Integer));
        break;
      }
      case Append: {
        if (!tt[0].isPrimitive(PrimitiveSort.Sequence)) Fail("argument of size is not a sequence");
        if (!tt[1].isPrimitive(PrimitiveSort.Sequence)) Fail("argument of size is not a sequence");
        if (!tt[0].firstarg().equals(tt[1].firstarg())) {
          Fail("different sequence types in append");
        }
        e.setType(tt[0]);
        break;
      }
      case Wrap: {
        ASTNode args[] = e.argsJava().toArray(new ASTNode[0]);
        switch (args.length) {
          case 0:
            e.setType(new PrimitiveType(PrimitiveSort.Void));
            break;
          case 1:
            e.setType(args[0].getType());
            break;
          default:
            Type types[] = new Type[args.length];
            for (int i = 0; i < args.length; i++) {
              types[i] = args[i].getType();
            }
            e.setType(new TupleType(types));
            break;
        }
        break;
      }
      case Get: {
        if (tt[0] == null) Fail("type of argument is unknown at %s", e.getOrigin());
        e.setType(tt[0]);
        break;
      }
      case AddrOf:
        if (tt[0] == null) Fail("type of argument to AddrOf operator (&) is unknown at %s", e.getOrigin());
        // TODO: determine whether type checking is necessary here.
        e.setType(new PrimitiveType(PrimitiveSort.Pointer, e.arg(0).getType()));
        break;
      case Indirection:
        if (tt[0] == null) Fail("type of argument to Indirection operator (*) is unknown at %s", e.getOrigin());

        Type elementType;

        if (!tt[0].isPrimitive(PrimitiveSort.Pointer)) {
          SequenceUtils.SequenceInfo seqInfo = SequenceUtils.expectArray(e.arg(0), "The first argument to the indirection operator (*) should be a pointer, but was of type %s");
          elementType = seqInfo.getElementType();
        } else {
          elementType = (Type) tt[0].firstarg();
        }

        e.setType(elementType);
        break;
      default:
        Abort("missing case of operator %s", op);
        break;
    }
  }

  @Override
  public void visit(StructValue v){
    super.visit(v);
    // TODO: type check cannot derive a useful type from only the values
    v.setType(v.type());

    if(v.getType().isPrimitive(PrimitiveSort.Array)) {
      Type element = (Type) v.getType().firstarg();

      if(element.isPrimitive(PrimitiveSort.Cell)) {
        element = (Type) element.firstarg();
      }

      if(element.isPrimitive(PrimitiveSort.Option)) {
        for (ASTNode node : JavaConverters.asJavaIterable(v.values())) {
          node.setType(element);
        }
      }
    }
//    if (v.type()==null){
//      Abort("Build without type argument");
//    }
//    Type t=v.type();
//    v.setType(t);
//    if (t instanceof ClassType && !((ClassType) t).getFullName().equals("VCTArray")){
//      Abort("constructor encoded as struct value");
//    } else {
//      if (t.hasArguments()){
//        Fail("type without arguments: %s in %s",t,v);
//      }
//      t=(Type)t.firstarg();
//
//      if(t.isPrimitive(PrimitiveSort.Cell)) {
//        t = (Type) t.firstarg();
//      }
//
//      for (int i = 0; i < v.valuesLength(); i++) {
//        Type tt[1]=v.value(i).getType();
//        if (tt[1]==null){
//          Fail("untyped build argument %d",i);
//        }
//        if(t.equals(tt[1]) || t.supertypeof(source(), tt[1]) || (t instanceof ClassType && ((ClassType) t).getFullName().equals("Ref"))) {
//          if(t.isPrimitive(PrimitiveSort.Option)) {
//            v.value(i).setType(t);
//          }
//        } else {
//          Abort("cannot use %s to initialize %s", tt[1], t);
//        }
//      }
//    }
  }

  private void check_location(ASTNode arg,String what) {
    if (!(arg instanceof Dereference)
    && !(arg instanceof FieldAccess)
    && !arg.isa(StandardOperator.Subscript)
    && !((arg instanceof NameExpression) && (((NameExpression)arg).getKind()==Kind.Field))
    && !arg.getType().isPrimitive(PrimitiveSort.Location)
    ){
      Fail("%s is not a heap location",what);
    }
  }

  private void checkMathOperator(OperatorExpression e, StandardOperator op, Type t1, Type t2) {
    if (!t1.isNumeric()){
      Fail("First argument of %s is %s rather than a numeric type",op,t1);
    }
    if (!t2.isNumeric()){
      Fail("Second argument of %s is %s rather than a numeric type",op,t2);
    }

    if(op == StandardOperator.FloorDiv && (t1.isFraction() || t2.isFraction())) {
      Fail("Integer division may not involve fractions");
    }

    if (op==StandardOperator.Minus && t1.isPrimitive(PrimitiveSort.Fraction)){
      e.setType(new PrimitiveType(PrimitiveSort.ZFraction));
    } else if(op == StandardOperator.Div) {
      // If the numerator is a (z)frac, copy that type. Otherwise, try to infer the type from a constant integer expr
      // in the numerator.
      if(t1.isPrimitive(PrimitiveSort.Fraction)) {
        e.setType(new PrimitiveType(PrimitiveSort.Fraction));
      } else if(t1.isPrimitive(PrimitiveSort.ZFraction)) {
        e.setType(new PrimitiveType(PrimitiveSort.ZFraction));
      } else if(t1.isPrimitive(PrimitiveSort.Integer) && e.arg(0) instanceof ConstantExpression && !e.arg(0).isConstant(0)){
        e.setType(new PrimitiveType(PrimitiveSort.Fraction));
      } else {
        e.setType(new PrimitiveType(PrimitiveSort.ZFraction));
      }
    } else {
      e.setType(t1);
    }
  }


  private void force_frac(ASTNode arg) {
    if(arg instanceof OperatorExpression && ((OperatorExpression) arg).operator() == StandardOperator.FloorDiv) {
      Warning("Encountered an integer division '%s' where a fraction was expected, did you mean a fraction division here?", arg);
    }

    if(arg.getType().isPrimitive(PrimitiveSort.Integer)) {
      arg.setType(new PrimitiveType(PrimitiveSort.Fraction));
    }

    if(arg instanceof OperatorExpression) {
      OperatorExpression op = (OperatorExpression) arg;
      switch(op.operator()) {
        case ITE:
          force_frac(op.arg(1));
          force_frac(op.arg(2));
          break;
      }
    }

//    if (arg.getType().isPrimitive(PrimitiveSort.ZFraction)||
//        arg.getType().isPrimitive(PrimitiveSort.Fraction)) {
//      if (arg instanceof OperatorExpression){
//        OperatorExpression e=(OperatorExpression)arg;
//        switch(e.operator()){
//        case ITE:
//          force_frac(e.arg(1));
//          force_frac(e.arg(2));
//          break;
//        }
//      }
//      return;
//    }
//    arg.setType(new PrimitiveType(PrimitiveSort.Fraction));
//    if (arg instanceof OperatorExpression){
//      OperatorExpression e=(OperatorExpression)arg;
//      switch(e.operator()){
//      case Div:
//        //force_frac(e.getArg(0));
//        break;
//      default:
//        for(ASTNode n:e.argsJava()){
//          force_frac(n);
//        }
//        break;
//      }
//    }
  }

  public void visit(Dereference e){
    super.visit(e);

    if(e.obj().isa(StandardOperator.Subscript) && e.field().equals("item")) {
      // In the case that the underlying object is a subscript of a sequence, we need to restore the original cell type
      // when the dereference is to the item of the cell.
      ASTNode sequenceLike = ((OperatorExpression) e.obj()).first();
      SequenceUtils.SequenceInfo sequenceInfo = SequenceUtils.getInfoOrFail(sequenceLike, "Expected a sequence type at %s, but got %s");
      e.obj().setType(sequenceInfo.getSequenceTypeArgument());
    }

    Type object_type=e.obj().getType();

    if (object_type==null) Fail("type of object unknown at "+e.getOrigin());
    if (object_type.isPrimitive(PrimitiveSort.Location)){
      object_type=(Type)object_type.firstarg();
    }
    if (object_type instanceof PrimitiveType){
      if (e.field().equals("length")){
        e.setType(new PrimitiveType(PrimitiveSort.Integer));
        return;
      }
      if (e.field().equals("item")){
        e.setType((Type)object_type.firstarg());
        return;
      }
      Fail("%s is not a pseudo field of (%s).",e.field(),object_type);
    }
    if (!(object_type instanceof ClassType)) {
      Fail("cannot select member %s of non-object type %s",e.field(),object_type.getClass());
    }
    if (((ClassType)object_type).getFullName().equals("<<label>>")){
      //TODO: avoid this kludge to not typing labeled arguments
      e.setType(object_type);
    } else {
      Debug("resolving class "+((ClassType)object_type).getFullName()+" "+((ClassType)object_type).getNameFull().length);
      ASTClass cl=source().find(((ClassType)object_type).getNameFull());
      if (cl==null) Fail("could not find class %s",((ClassType)object_type).getFullName());
      Debug("looking in class "+cl.getName());
      DeclarationStatement decl=cl.find_field(e.field(),true);
      if (decl!=null){
        e.setType(decl.getType());
      } else {
        Method m=cl.find_predicate(e.field());
        if (m!= null && !m.isStatic()){
          Type [] args=m.getArgType();
          if (args.length==0){
            args=new Type[]{new PrimitiveType(PrimitiveSort.Void)};
          }
          e.setType(new FunctionType(args,m.getReturnType()));
        } else {
          Fail("Field nor predicate %s not found in class %s",e.field(),((ClassType)object_type).getFullName());
        }
      }
    }
  }

  public void visit(BlockStatement s){
    super.visit(s);
    // TODO: consider if type should be type of last statement.
  }
  public void visit(IfStatement s){
    super.visit(s);
    int N=s.getCount();
    for(int i=0;i<N;i++){
      Type t=s.getGuard(i).getType();
      if (t==null || !(t instanceof PrimitiveType) || (((PrimitiveType)t).sort!=PrimitiveSort.Boolean)){
        if (s.getGuard(i).isReserved(ASTReserved.Any)) continue;
        Fail("Guard %d of if statement is not a boolean at %s",i,s.getOrigin());
      }
    }
    // TODO: consider if this can be an if expression....
  }
  public void visit(ReturnStatement s){
    super.visit(s);
    // TODO: check expression against method type.
  }
  public void visit(ASTClass c){
    super.visit(c);
    // TODO: type checks on class.
  }

  @Override
  public void visit(LoopStatement s) {
    super.visit(s);
    for(ASTNode inv:s.getInvariants()){
      Type t=inv.getType();
      if (t==null || !(t.isBoolean() || t.isPrimitive(PrimitiveSort.Resource))){
        Abort("loop invariant is not a boolean or resource (%s)",t);
      }
    }
    ASTNode tmp;
    tmp=s.getEntryGuard();
    if (tmp!=null) {
      Type t=tmp.getType();
      if (t==null || !(t instanceof PrimitiveType) || (((PrimitiveType)t).sort!=PrimitiveSort.Boolean)){
        Abort("loop entry guard is not a boolean");
      }
    }
    tmp=s.getExitGuard();
    if (tmp!=null) {
      Type t=tmp.getType();
      if (t==null || !(t instanceof PrimitiveType) || (((PrimitiveType)t).sort!=PrimitiveSort.Boolean)){
        Abort("loop exit guard is not a boolean");
      }
    }
  }

  @Override
  public void visit(BindingExpression e){
    super.visit(e);
    //result=create.binder(e.binder, rewrite(e.getDeclarations()), rewrite(e.select), rewrite(e.main));
    Type t;
    if (e.select!=null){
      t=e.select.getType();
      if (t==null){
        Abort("Selection in binding expression without type.");
      }
      if (!t.isBoolean()){
        Fail("Selector in binding expression is %s instead of boolean.",t);
      }
    }
    t=e.main.getType();
    if (t==null){
      Abort("Binding expression without type.");
    }
    switch(e.binder){
    case Let:
      e.setType(t);
      break;
    case Star:{
      Type res=new PrimitiveType(PrimitiveSort.Resource);
      if (!res.supertypeof(source(), t)){
        Fail("main argument of %s quantifier must be resource",e.binder);
      }
      e.setType(res);
      break;
    }
    case Exists:
    case Forall:{
      Type res=new PrimitiveType(PrimitiveSort.Boolean);
      if (!res.supertypeof(source(), t)) {
        Fail("main argument of %s quantifier must be boolean",e.binder);
      }
      e.setType(res);
      break;
    }
    case Sum:
      e.setType(t);
      break;
    }
  }



  @Override
  public void visit(VectorBlock pb){
    if (!pb.iter().getType().isPrimitive(PrimitiveSort.Integer)){
      Fail("type of iteration variable must be integer");
    }
    ASTNode init=pb.iter().initJava();
    if (!init.isa(StandardOperator.RangeSeq)){
      Fail("value for iteration variable must be a range");
    }
    init.apply(this);
    pb.block().apply(this);
  }


  @Override
  public void visit(ParallelBlock pb){
    for (DeclarationStatement decl : pb.itersJava()) {
      if (!decl.getType().isPrimitive(PrimitiveSort.Integer)){
        Fail("type of iteration variable must be integer");
      }
      ASTNode init=decl.initJava();
      if (!init.isa(StandardOperator.RangeSeq)){
        Fail("value for iteration variable must be a range");
      }
      init.apply(this);
    }

    if (pb.contract() != null) {
      pb.contract().apply(this);
    }

    pb.block().apply(this);
  }

  @Override
  public void visit(ASTSpecial s){
    super.visit(s);
    Debug("special %s",s.kind);
    for(ASTNode n:s.args){
      Type t=n.getType();
      if (t==null){
        Abort("untyped argument to %s: %s",s.kind,Configuration.getDiagSyntax().print(n));
      }
    }
    Type t1;
    switch(s.kind){
    case Fresh:{
      // TODO: check arguments.
      break;
    }
    case Send:{
      t1=s.args[0].getType();
      if (t1==null) Fail("type of left argument unknown at "+s.getOrigin());
      if (!t1.isResource()) Fail("type of left argument is %s rather than resource at %s",t1,s.getOrigin());
      break;
    }
    case Recv:{
        t1=s.args[0].getType();
        if (t1==null) Fail("type of left argument unknown at "+s.getOrigin());
        if (!t1.isResource()) Fail("type of left argument is %s rather than resource at %s",t1,s.getOrigin());
        break;
      }
    case Fold:
    case Unfold:
    case Open:
    case Close:
    {
      ASTNode arg=s.args[0];
      if (!(arg instanceof MethodInvokation) && !(arg.isa(StandardOperator.Scale))){
        Fail("At %s: argument of [%s] must be a (scaled) predicate invokation",arg.getOrigin(),s.kind);
      }
      if (arg instanceof MethodInvokation){
        MethodInvokation prop=(MethodInvokation)arg;
        if (prop.getDefinition().kind != Method.Kind.Predicate){
          Fail("At %s: argument of [%s] must be predicate and not %s",arg.getOrigin(),s.kind,prop.getDefinition().kind);
        }
      }
      s.setType(new PrimitiveType(PrimitiveSort.Void));
      break;
    }
    case Use:
    case QED:
    case Apply:
    case Refute:
    case Assert:
    case HoarePredicate:
    case Assume:
    case Witness:
    {
      Type t=s.args[0].getType();
      if (t==null) Fail("type of argument is unknown at %s",s.getOrigin());
      if (!t.isBoolean()&&!t.isPrimitive(PrimitiveSort.Resource)){
        Fail("Argument of %s must be boolean or resource at %s",s.kind,s.getOrigin());
      }
      s.setType(new PrimitiveType(PrimitiveSort.Void));
      break;
    }
    }
    s.setType(new PrimitiveType(PrimitiveSort.Void));
  }

  @Override
  public void visit(FieldAccess a){
    super.visit(a);
    if (a.value() == null) {
      Dereference d = new Dereference(a.object(), a.name());
      visit(d);
      a.setType(d.getType());
    } else {
      a.setType(new PrimitiveType(PrimitiveSort.Void));
    }
  }

  @Override
  public void visit(Constraining c) {
	for (NameExpression var : c.varsJava()) {
      var.apply(this);
      Type t=var.getType();
      if (t==null){
        Fail("unresolved variable %s at %s",var,var.getOrigin());
      }
      if (!t.isPrimitive(PrimitiveSort.Fraction) && !t.isPrimitive(PrimitiveSort.ZFraction)){
        Fail("variable %s is %s rather than fraction at %s",var,t,var.getOrigin());
      }
    }
    c.block().apply(this);
  }
}
