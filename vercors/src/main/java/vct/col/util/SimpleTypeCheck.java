package vct.col.util;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import vct.col.ast.*;
import vct.col.ast.NameExpression.Kind;
import vct.col.ast.PrimitiveType.Sort;
import vct.col.rewrite.MultiSubstitution;
import vct.col.rewrite.TypeVarSubstitution;
import vct.silver.SilverTypeMap;
import vct.util.Configuration;

@SuppressWarnings("incomplete-switch")
public class SimpleTypeCheck extends RecursiveVisitor<Type> {
  
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
  
  public SimpleTypeCheck(ProgramUnit arg){
    super(arg,true);
  }

  public void visit(ConstantExpression e){
    Debug("constant %s",e);
    super.visit(e);
    if (e.getType()==null) Abort("untyped constant %s",e);
  }
  
  public void visit(PrimitiveType t){
    super.visit(t);
    int N=t.getArgCount();
    for(int i=0;i<N;i++){
      if (t.getArg(i)==null){
        Abort("argument %d not typed",i);
      }
    }
    t.setType(new PrimitiveType(PrimitiveType.Sort.Class));
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
 
  public void visit(MethodInvokation e){
    super.visit(e);
    Method m=source().find_adt(e.method);
    if (m!=null){
      //Warning("skipping ADT method");
      e.setDefinition(m);
      Type t=m.getReturnType();
      Map<String,Type> map=new HashMap<String, Type>();
      TypeVarSubstitution sigma=new TypeVarSubstitution(source(),map);
      if (!(e.object instanceof ClassType)){
        Fail("%s is not an ADT in %s",e.object,e);
      }
      SilverTypeMap.get_adt_subst(sigma.copy_rw,map,(ClassType)e.object);
      //System.err.printf("before %s %s %s%n",e.method,map,t);
      e.setType(sigma.rewrite(t));
      //System.err.printf("result %s after %s%n",e.method,e.getType());
      return;
    }
    m=source().find_procedure(e.method);
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
    switch(m.kind){
    case Constructor:
      e.setType((Type)e.object);
      break;
    case Predicate:
      for(int i=0;i<N;i++){
        if (type[i].isPrimitive(PrimitiveType.Sort.Fraction)){
          force_frac(e.getArg(i));
        }
      }
      e.setType(new PrimitiveType(PrimitiveType.Sort.Resource));
      break;
    default:
      {
        MultiSubstitution sigma=m.getSubstitution(object_type);
        e.setType(sigma.rewrite(m.getReturnType()));
        break;
      }
    }
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
  }
  
  public final void check_loc_val(Type loc_type,ASTNode val){
    check_loc_val(loc_type,val,"Types of location (%s) and value (%s) do not match.");
  }
  public final void check_loc_val(Type loc_type,ASTNode val,String fmt){
    if (loc_type==null) Abort("Location has no type.");
    Type val_type=val.getType();
    if (val_type==null) Abort("Value has no type has no type.");
    if (loc_type.toString().equals("<<label>>")) return;
    if (!(loc_type.equals(val_type)
        ||loc_type.supertypeof(source(), val_type)
        ||loc_type.isNumeric()&&val_type.isNumeric()
    )){
      Fail(fmt,loc_type,val_type);
    }
    if (loc_type.isPrimitive(Sort.Fraction)||loc_type.isPrimitive(Sort.ZFraction)){
      force_frac(val);
    }
    if (loc_type.isPrimitive(Sort.Option)){
      val.setType(loc_type);
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
    check_loc_val(s.location().getType(),s.expression());
  }
  
  public void visit(DeclarationStatement s){
    super.visit(s);
    Type t=s.getType();
    ASTNode e=s.getInit();
    if (e!=null) {
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
      if (!pre_t.isPrimitive(Sort.Resource) && !pre_t.isPrimitive(Sort.Boolean)){
        contract.pre_condition.getOrigin().report("error","pre condition is not a resource");
        Fail("type error");
      }
      Type post_t=contract.post_condition.getType();
      if (post_t==null) Abort("untyped post condition"); // TODO check boolean.
      if (!post_t.isPrimitive(Sort.Boolean)){
        if (m.kind==Method.Kind.Pure){
          for(ASTNode clause:ASTUtils.conjuncts(contract.post_condition, StandardOperator.Star)){
            if (!clause.getType().isPrimitive(Sort.Boolean)){
              clause.getOrigin().report("error","post condition of function "+m.name+" is not a boolean");
              Fail("type error");
            }
          }
        } else if (!post_t.isPrimitive(Sort.Resource)){
          contract.post_condition.getOrigin().report("error","post condition is not a resource");
          Fail("type error");
        }
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
          e.setType(new PrimitiveType(PrimitiveType.Sort.Process));
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
          e.setType(new PrimitiveType(PrimitiveType.Sort.Fraction));
          break;
        }
        case ReadPerm:{
          e.setType(new PrimitiveType(PrimitiveType.Sort.Fraction));
          break;
        }
        case NoPerm:{
          e.setType(new PrimitiveType(PrimitiveType.Sort.ZFraction));
          break;
        }
        case CurrentThread:{
          e.setType(new PrimitiveType(PrimitiveType.Sort.Integer));
          break;
        }
        case OptionNone:{
          e.setType(new PrimitiveType(PrimitiveType.Sort.Option,
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
          e.setType(new PrimitiveType(Sort.Integer));
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
            e.setType(new PrimitiveType(Sort.Integer));
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
  public void visit(OperatorExpression e){
    Debug("operator %s", e.operator());
    StandardOperator op = e.operator();
    if (op==StandardOperator.PointsTo && e.arg(2).isa(StandardOperator.BindOutput)){
      e.arg(0).accept(this);
      e.arg(1).accept(this);
      enter(e.arg(2));
      leave(e.arg(2));
      e.arg(2).setType(e.arg(1).getType());
      e.setType(new PrimitiveType(Sort.Resource));
      return;
    }
    if (op==StandardOperator.AbstractState){
      e.arg(0).accept(this);
      Type t=e.arg(0).getType();
      if (t==null) Fail("Data type unknown.");
      if (!(t instanceof ClassType)){
        Fail("Data type must be a class type.");
      }
      ASTClass cl=source().find((ClassType)t);
      variables.enter();
      for(DeclarationStatement decl:cl.dynamicFields()){
        variables.add(decl.getName(),new VariableInfo(decl,Kind.Local));
      }
      e.arg(1).accept(this);
      t=e.arg(1).getType();
      if (t==null) Fail("Formula type unknown.");
      if(!t.isBoolean()){
        Fail("expression type is %s rather than boolean",t);
      }
      variables.leave();
      e.setType(new PrimitiveType(Sort.Resource));
      return;
    }
    super.visit(e);
    ASTNode argss[]=e.args();
    Type tt[]=new Type[argss.length];
    for(int i=0;i<argss.length;i++){
      if (argss[i] instanceof Type) continue;
      tt[i]=argss[i].getType();
      if (tt[i]==null){
        Fail("type of argument %d is unknown at %s in expression %s",i+1,e.getOrigin(),Configuration.getDiagSyntax().print(e));
      }
    }
    Type t1=null,t2=null;
    if (op.arity()==2){
      t1=e.arg(0).getType();
      if (t1==null) Fail("type of left argument unknown");
      t2=e.arg(1).getType();
      if (t2==null) Fail("type of right argument unknown");
    }
    
    
    switch(op){
    case VectorRepeat:
    {
      Type t=e.arg(0).getType();
      e.setType(new PrimitiveType(Sort.Sequence,t));
      break;
    }
    case VectorCompare:
    {
      e.setType(new PrimitiveType(Sort.Sequence,new PrimitiveType(Sort.Integer)));
      break;
    }
    case MatrixRepeat:
    {
      Type t=e.arg(0).getType();
      e.setType(new PrimitiveType(Sort.Sequence,new PrimitiveType(Sort.Sequence,t)));
      break;
    }
    case MatrixCompare:
    {
      e.setType(new PrimitiveType(Sort.Sequence,new PrimitiveType(Sort.Sequence,new PrimitiveType(Sort.Integer))));
      break;
    }
    case MatrixSum:{
      Type t=e.arg(1).getType();
      t = (Type)((PrimitiveType) t).getArg(0);
      t = (Type)((PrimitiveType) t).getArg(0);
      e.setType(t);
      break;
    }
    case FoldPlus:
    {
      Type t=e.arg(0).getType();
      if (t.isPrimitive(Sort.Sequence)){
        t = (Type)((PrimitiveType) t).getArg(0);
        if (!t.isPrimitive(Sort.Integer)){
          Fail("first argument of summation must be a sequence of integers");
        }
      } else {
        Fail("first argument of summation must be a sequence");
      }      
      t=e.arg(1).getType();
      if (t.isPrimitive(Sort.Sequence)){
        t = (Type)((PrimitiveType) t).getArg(0);
      } else {
        Fail("argument of summation must be a sequence");
      }
      if(t.isPrimitive(Sort.Boolean)){
        e.setType(new PrimitiveType(Sort.Integer));
      } else {
        e.setType(t);
      }
      break;
    }
    case IndependentOf:
    {
      e.setType(t1);
      break;
    }
    case PVLidleToken:
    case PVLjoinToken:
    {
      e.setType(new PrimitiveType(Sort.Resource));
      break;
    }
    case IterationOwner:{
      e.setType(new PrimitiveType(Sort.Integer));
      break;
    }
    case TypeOf:{
      e.setType(new ClassType("<<null>>"));
      break;
    }
    case History:{
      String type=tt[0].toString();
      if(!type.endsWith("History")){
        Fail("First argument of History must be a History class, not %s.",type);
      }
      e.setType(new PrimitiveType(Sort.Resource));
      break;
    }
    case Future:{
      String type=tt[0].toString();
      if(!type.endsWith("Future")){
        Fail("First argument of Future must be a Future class, not %s.",type);
      }
      e.setType(new PrimitiveType(Sort.Resource));
      break;
    }
    case NewSilver:{
      // TODO: check arguments.
      e.setType(new ClassType("Ref"));
      break;
    }
    case RangeSeq:{
      if (!t1.isInteger()) Fail("type of left argument is %s rather than integer",t1);
      if (!t2.isInteger()) Fail("type of right argument is %s rather than integer",t2);
      e.setType(new PrimitiveType(Sort.Sequence,t1));
      break;
    }
    case Instance:
    case SubType:
    case SuperType:
    {
      e.setType(new PrimitiveType(Sort.Boolean));
      break;      
    }
    case Cast:
    {
      ASTNode t=e.arg(0);
      if (t instanceof Type) {
        e.setType((Type)t);
      } else {
        Fail("cannot cast to non-type %s",t.getClass());
      }
      break;
    }
    case Or:
    {
      if (t1.isPrimitive(Sort.Process)){
        if (!t2.isPrimitive(Sort.Process)){
          Fail("Cannot compose process with %s",t2);
        }
        e.setType(t1);
        break;
      }
      // fall through on purpose.
    }
    case And:
    case IFF:
    {
      if (!t1.isBoolean()) Fail("type of left argument is %s rather than boolean at %s",t1,e.getOrigin());
      if (!t2.isBoolean()) Fail("type of right argument is %s rather than boolean at %s",t2,e.getOrigin());
      e.setType(new PrimitiveType(Sort.Boolean));
      break;
    }
    case Member:
    {
      if (t2.isPrimitive(Sort.Sequence)||t2.isPrimitive(Sort.Set)||t2.isPrimitive(Sort.Bag)){
        if (!t1.equals(t2.getArg(0))){
          Fail("%s cannot be a member of %s",t1,t2);
        }
      } else {
        Fail("cannot determine members of %s",t2);
      }
      if (t2.isPrimitive(Sort.Bag)){
        e.setType(new PrimitiveType(Sort.Integer));
      } else {
        e.setType(new PrimitiveType(Sort.Boolean));
      }
      break;
    }
    case NewArray:
    {
      t1=(Type)e.arg(0);
      t2=e.arg(1).getType();
      if (t2==null) Fail("type of subscript unknown at %s",e.getOrigin());
      if (!t2.isInteger()) {
        Fail("subcript has type %s rather than integer",t2);
      }
      e.setType(new PrimitiveType(Sort.Array,t1));
      break;
    }
    case Implies:
    {
      if (!t1.isBoolean()) Fail("type of left argument is %s rather than boolean at %s",t1,e.getOrigin());
      if (!t2.isBoolean()&&!t2.isPrimitive(Sort.Resource)){
        Fail("type of right argument is %s rather than boolean or resource at %s",t2,e.getOrigin());
      }
      e.setType(t2);
      break;
    }
    case Star:
    case Wand:
    {
      if (!t1.isBoolean() && !t1.isPrimitive(Sort.Resource)) Fail("type of right argument is %s rather than boolean/resource at %s",t1,e.getOrigin());
      if (!t2.isBoolean() && !t2.isPrimitive(Sort.Resource)) Fail("type of right argument is %s rather than boolean/resource at %s",t2,e.getOrigin());
      e.setType(new PrimitiveType(Sort.Resource));
      break;
    }
    case CurrentPerm:{
      check_location(e.arg(0),"argument of CurrentPerm");
      t1=e.arg(0).getType();
      if (t1==null) Fail("type of argument unknown at %s",e.getOrigin());
      e.setType(new PrimitiveType(Sort.Fraction));
      break;
    }
    case Scale:
    {
      if (!t1.isNumeric()) Fail("scalar is %s rather than a numeric type at %s",t1,e.getOrigin());
      if (!t2.isResource()) Fail("Cannot scale type %s",t1);
      force_frac(e.arg(0));
      e.setType(new PrimitiveType(Sort.Resource));
      break;      
    }
    case Unfolding:{
      if (!t1.isResource()) Fail("Cannot unfold type %s",t1);
      e.setType(t2);
      break;  
    }
    case Held:
    {
      e.setType(new PrimitiveType(Sort.Resource));
      break;      
    }
    case HistoryPerm:
    case ActionPerm:
    case Perm:
    {
      check_location(e.arg(0),"first argument");
      if (!t2.isBoolean() && !t2.isNumeric()) Fail("type of right argument is %s rather than a numeric type at %s",t2,e.getOrigin());
      force_frac(e.arg(1));
      e.setType(new PrimitiveType(Sort.Resource));
      break;
    }
    case PointsTo:
    {
      check_location(e.arg(0),"first argument");
      t1=e.arg(0).getType();
      if (t1==null) Fail("type of left argument unknown at %s",e.getOrigin());
      t2=e.arg(1).getType();
      if (t2==null) Fail("type of middle argument unknown at %s",e.getOrigin());
      if (!t2.isBoolean() && !t2.isNumeric()) Fail("type of middle argument is %s rather than a numeric type at %s",t2,e.getOrigin());
      force_frac(e.arg(1));
      e.setType(new PrimitiveType(Sort.Resource));
      Type t3=e.arg(2).getType();
      if (t3==null) Fail("type of right argument unknown at %s",e.getOrigin());
      if (!t3.comparableWith(source(), t1)){
        Fail("types of location and value (%s/%s) incomparable at %s",
            t1,t3,e.getOrigin());
      }
      break;
    }
    case Contribution:
    {
      t1=e.arg(0).getType();
      if (t1==null) Fail("type of left argument unknown at %s",e.getOrigin());
      check_loc_val(t1,e.arg(1),"Types of location (%s) and contribution (%s) do not match.");
      e.setType(new PrimitiveType(Sort.Resource));
      break;
    } 
    case Value:
      check_location(e.arg(0),"argument");
      e.setType(new PrimitiveType(Sort.Resource));
      break;
    case AddsTo:
    case ReducibleSum:
    case ReducibleMin:
    case ReducibleMax:
    case ArrayPerm:
      // TODO: check arguments
      e.setType(new PrimitiveType(Sort.Resource));
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
    case SShrAssign:
    {
      if (e.arg(0) instanceof NameExpression){
        NameExpression name=(NameExpression)e.arg(0);
        if (name.getKind()==NameExpression.Kind.Label) break;
      }
      if (t1.getClass()!=t2.getClass()) {
        Fail("Types of left and right-hand side arguments in assignment are incomparable at "+e.getOrigin());
      }
      e.setType(t1);
      break;
    }    
    case EQ:
    case NEQ:
    {
      if (!t1.comparableWith(source(),t2)) {
        //vct.util.Configuration.getDiagSyntax().print(System.out,e.getArg(0));
        //System.out.print("/");
        //vct.util.Configuration.getDiagSyntax().print(System.out,e.getArg(1));
        //System.out.println();
        Fail("Types of left and right-hand side argument are uncomparable: %s/%s",t1,t2);
      }
      e.setType(new PrimitiveType(Sort.Boolean));
      if (t1.isPrimitive(Sort.ZFraction) || t1.isPrimitive(Sort.Fraction)){
        force_frac(e.arg(1));
      }
      if (t2.isPrimitive(Sort.ZFraction) || t2.isPrimitive(Sort.Fraction)){
        force_frac(e.arg(0));
      }
      if (t1.isPrimitive(Sort.Option)) {
        // TODO: use type inference instead of this quick fix
        Type tt1=(Type)t1.getArg(0);
        Type tt2=(Type)t2.getArg(0);
        if (tt1.toString().equals("<<null>>")) {
          e.arg(0).setType(t2);
        }
        if (tt2.toString().equals("<<null>>")){
          e.arg(1).setType(t1);
        }
      }
      break;
    }
    case ValidArray:{
      //TODO: check argument types.
      e.setType(new PrimitiveType(Sort.Boolean));
      break;
    }
    case ValidMatrix:{
      //TODO: check argument types.
      e.setType(new PrimitiveType(Sort.Boolean));
      break;
    }
    case Values:{
      Type t=e.arg(0).getType();
      if (!t.isPrimitive(Sort.Array)){
        Abort("First argument values must be array at "+e.getOrigin());
      }
      t1=e.arg(1).getType();
      if (t1==null) Fail("type of from argument unknown at "+e.getOrigin());
      if (!t1.isInteger()) Fail("type of from argument should be integer at "+e.getOrigin());
      t2=e.arg(2).getType();
      if (t2==null) Fail("type of upto argument unknown at "+e.getOrigin());
      if (!t2.isInteger()) Fail("type of upto argument should be integer at "+e.getOrigin());
      e.setType(new PrimitiveType(Sort.Sequence,((PrimitiveType)t).getArgs()));
      break;
    }
    case ITE:
    {
      Type t=e.arg(0).getType();
      if (!t.isBoolean()){
        Abort("First argument of if-the-else must be boolean at "+e.getOrigin());
      }
      t1=e.arg(1).getType();
      if (t1==null) Fail("type of left argument unknown at "+e.getOrigin());
      t2=e.arg(2).getType();
      if (t2==null) Fail("type of right argument unknown at "+e.getOrigin());
      if (t1.getClass()!=t2.getClass()) {
        Fail("Types of left and right-hand side argument are uncomparable at "+e.getOrigin());
      }
      if (t2.supertypeof(source(), t1)) {
        //Warning("ITE type %s",t2);
        e.setType(t2);        
      } else {
        //Warning("ITE type %s",t1);
        e.setType(t1);
      }
      if (t1.isPrimitive(Sort.ZFraction) || t1.isPrimitive(Sort.Fraction)){
        force_frac(e.arg(2));
      }
      if (t2.isPrimitive(Sort.ZFraction) || t2.isPrimitive(Sort.Fraction)){
        force_frac(e.arg(1));
      }
      break;
    }
    case Not:
    {
      Type t=e.arg(0).getType();
      if (!t.isBoolean()){
        Abort("Argument of negation must be boolean at "+e.getOrigin());
      }
      e.setType(t);
      break;
    }
    case OptionSome:{
      Type t=e.arg(0).getType();
      e.setType(new PrimitiveType(Sort.Option,t));
      break;
    }
    case OptionGet:{
      Type t=e.arg(0).getType();
      if (!t.isPrimitive(Sort.Option)){
        Fail("argument of option get is %s rather than option<?>",t);
      }
      e.setType((Type)((PrimitiveType)t).getArg(0));
      break;
    }
    case Identity:
    {
      Type t=e.arg(0).getType();
      e.setType(t);
      break;
    }
    case PreIncr:
    case PreDecr:
    case PostIncr:
    case PostDecr:
    case UMinus:
    case UPlus:
    {
      Type t=e.arg(0).getType();
      if (!t.isNumeric()){
        Fail("Argument of %s must be a numeric type",op);
      }
      e.setType(t);
      break;
    }
    case Exp:{
      if (!t1.isNumeric()){
        Fail("First argument of %s is %s rather than a numeric type",op,t1);
      }
      if (!t2.isInteger()){
        Fail("Second argument of %s is %s rather than integer",op,t2);
      }
      e.setType(t1);
      break;
    }
    case Plus:
    { // handle concatenation meaning of +
      if (t1.isPrimitive(Sort.Sequence)||t1.isPrimitive(Sort.Set)||t1.isPrimitive(Sort.Bag)){
        if (!t1.comparableWith(source(),t2)) {
          Fail("Types of left and right-hand side argument are uncomparable: %s/%s",t1,t2);
        }
        e.setType(t1);
        break;
      }
      if (t1.isPrimitive(Sort.Process)){
        if (!t2.isPrimitive(Sort.Process)){
          Fail("Cannot compose process with %s",t2);
        }
        e.setType(t1);
        break;
      }
      checkMathOperator(e, op, t1, t2);
      break;
    }
    case Mult:
    {
      // handle cartesian product meaning of *
      if (t1.isPrimitive(Sort.Sequence) && t2.isPrimitive(Sort.Sequence)){
        t1=(Type)((PrimitiveType)t1).getArg(0);
        t2=(Type)((PrimitiveType)t2).getArg(0);
        e.setType(new PrimitiveType(Sort.Sequence, new TupleType(new Type[] { t1, t2 })));
        break;
      }
      // handle intersection meaning of *
      if (t1.isPrimitive(Sort.Set)||t1.isPrimitive(Sort.Bag)){
        if (!t1.comparableWith(source(),t2)) {
          Fail("Types of left and right-hand side argument are uncomparable: %s/%s",t1,t2);
        }
        e.setType(t1);
        break;
      }
      if (t1.isPrimitive(Sort.Process)){
        if (!t2.isPrimitive(Sort.Process)){
          Fail("Cannot compose process with %s",t2);
        }
        e.setType(t1);
        break;
      }
      checkMathOperator(e, op, t1, t2);
      break;
    }
    case Minus:
    {
      // handle set minus meaning of -
      if (t1.isPrimitive(Sort.Set)||t1.isPrimitive(Sort.Bag)){
        if (!t1.comparableWith(source(),t2)) {
          Fail("Types of left and right-hand side argument are uncomparable: %s/%s",t1,t2);
        }
        e.setType(t1);
        break;
      }
    }
    case Div:
    case Mod:
    {
      checkMathOperator(e, op, t1, t2);
      break;
    }
    case BitAnd:
    case BitOr:
    case BitNot:
    case BitXor:
    {
      if (t1.equalSize(t2)){
        e.setType(t1);
      } else {
        Fail("Types of left and right-hand side argument are different (%s/%s).",t1,t2);
      }
      break;
    }
    case RightShift:
    case LeftShift:
    case UnsignedRightShift:
    {
      if (!t1.isIntegerType()){
        Fail("First argument of %s must be integer type, not %s",op,t1);
      }
      if (!t2.isIntegerType()){
        Fail("Second argument of %s must be integer type, not %s",op,t2);
      }
      e.setType(t1);
      break;
    }
    case GTE:
    case LTE:
    case LT:
    case GT:
    {
      if (!t1.isNumeric()){
        Fail("First argument of %s is %s rather than a numeric type",op,t1);
      }
      if (!t2.isNumeric()){
        Fail("Second argument of %s is %s rather than a numeric type",op,t2);
      }
      e.setType(new PrimitiveType(Sort.Boolean));      
      break;
    }
    case Old:
    {
      Type t=e.arg(0).getType();
      if (t==null) Fail("type of argument is unknown at %s",e.getOrigin());
      e.setType(t);      
      break;
    }
    case New:
    {
      ASTNode t=e.arg(0);
      if (!(t instanceof ClassType)) Fail("argument to new is not a class type");
      e.setType((Type)t);
      break;
    }
    case Drop:
    case Take:
    {
      if (!t1.isPrimitive(PrimitiveType.Sort.Sequence)) Fail("base must be sequence type.");
      if (!t2.isInteger()) {
        Fail("count has type %s rather than integer",t2);
      }
      e.setType(t1);
      break;
    }      
    case Subscript:
    {
      if (!(t1 instanceof PrimitiveType)) Fail("base must be array or sequence type.");
      PrimitiveType t=(PrimitiveType)t1;
      switch(t.sort){
        case Pointer:
        case Sequence:
        case Array:
        {
          t1=(Type)t.getArg(0);
          break;
        }
        default: Fail("base must be array or sequence type.");
      }
      if (!t2.isInteger()) {
        Fail("subcript has type %s rather than integer",t2);
      }
      e.setType(t1);
      break;
    }
    case Head:{
      Type t=e.arg(0).getType();
      if (t==null) Fail("type of argument is unknown at %s",e.getOrigin());
      if (!t.isPrimitive(Sort.Sequence)) Fail("argument of head is not a sequence");
      e.setType((Type)t.getArg(0));      
      break;      
    }
    case Tail:{
      Type t=e.arg(0).getType();
      if (t==null) Fail("type of argument is unknown at %s",e.getOrigin());
      if (!t.isPrimitive(Sort.Sequence)) Fail("argument of tail is not a sequence");
      e.setType(t);      
      break;      
    }
    case Size:
    {
      Type t=e.arg(0).getType();
      if (t==null) Fail("type of argument is unknown at %s",e.getOrigin());
      if (!(t.isPrimitive(Sort.Sequence)||t.isPrimitive(Sort.Bag)||t.isPrimitive(Sort.Set))) {
        Fail("argument of size is not a set, sequence, or bag");
      }
      e.setType(new PrimitiveType(Sort.Integer));      
      break;
    }
    case Length:
    {
      Type t=e.arg(0).getType();
      if (t==null) Fail("type of argument is unknown at %s",e.getOrigin());
      if (!t.isPrimitive(Sort.Array)) Fail("argument of length is not an array");
      e.setType(new PrimitiveType(Sort.Integer));      
      break;
    }
    case Append:
    {
      if (!t1.isPrimitive(Sort.Sequence)) Fail("argument of size is not a sequence");
      if (!t2.isPrimitive(Sort.Sequence)) Fail("argument of size is not a sequence");
      if (!t1.getArg(0).equals(t2.getArg(0))){
        Fail("different sequence types in append"); 
      }
      e.setType(t1);
      break;
    }
    case Wrap:{
      ASTNode args[]=e.args();
      switch(args.length){
      case 0:
        e.setType(new PrimitiveType(Sort.Void));      
        break;
      case 1:
        e.setType(args[0].getType());
        break;
      default:
        Type types[]=new Type[args.length];
        for(int i=0;i<args.length;i++){
          types[i]=args[i].getType();
        }
        e.setType(new TupleType(types));
        break;
      }
      break;
    }
    case Get:{
      Type t=e.arg(0).getType();
      if (t==null) Fail("type of argument is unknown at %s",e.getOrigin());
      e.setType(t);
      break;
    }
    default:
      Abort("missing case of operator %s",op);
      break;
    }
  }

  @Override
  public void visit(StructValue v){
    super.visit(v);
    if (v.type() == null) {
      Abort("Build without type argument");
    }
    Type t = v.type();
    v.setType(t);
    if (t instanceof ClassType){
      Abort("constructor encoded as struct value");
    } else {
      if (t.getArgCount()==0){
        Fail("type without arguments: %s in %s",t,v);
      }
      t=(Type)t.getArg(0);
      for (int i=0;i<v.values().length;i++) {
        Type t2=v.value(i).getType();
        if (t2 == null) {
          Fail("untyped build argument %d",i);
        }
        if (t.equals(t2)) continue;
        if (t.supertypeof(source(), t2)) continue;
        Abort("cannot use %s to initialize %s",t2,t);
      }
    }
  }
  
  private void check_location(ASTNode arg,String what) {
    if (!(arg instanceof Dereference)
    && !(arg instanceof FieldAccess)
    && !arg.isa(StandardOperator.Subscript)
    && !((arg instanceof NameExpression) && (((NameExpression)arg).getKind()==Kind.Field))
    && !arg.getType().isPrimitive(Sort.Location)
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
    if (op==StandardOperator.Minus && t1.isPrimitive(Sort.Fraction)){
      e.setType(new PrimitiveType(Sort.ZFraction));
    } else {
      e.setType(t1);
    }
  }
  
  
  private void force_frac(ASTNode arg) {
    if (arg.getType().isPrimitive(Sort.ZFraction)||
        arg.getType().isPrimitive(Sort.Fraction)) {
      if (arg instanceof OperatorExpression){
        OperatorExpression e=(OperatorExpression)arg;
        switch(e.operator()){
        case ITE:
          force_frac(e.arg(1));
          force_frac(e.arg(2));
          break;
        }
      }
      return;
    }
    arg.setType(new PrimitiveType(Sort.Fraction));
    if (arg instanceof OperatorExpression){
      OperatorExpression e=(OperatorExpression)arg;
      switch(e.operator()){
      case Div:
        //force_frac(e.getArg(0));
        break;
      default:
        for(ASTNode n:e.args()){
          force_frac(n);
        }
        break;
      }
    }
  }

  public void visit(Dereference e){
    super.visit(e);
    Type object_type=e.object().getType();
    if (object_type==null) Fail("type of object unknown at "+e.getOrigin());
    if (object_type.isPrimitive(Sort.Location)){
      object_type=(Type)object_type.getArg(0);
    }
    if (object_type instanceof PrimitiveType){
      if (e.field().equals("length")){
        e.setType(new PrimitiveType(Sort.Integer));
        return;
      }
      if (e.field().equals("item")){
        e.setType((Type)object_type.getArg(0));
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
            args=new Type[]{new PrimitiveType(Sort.Void)};
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
      if (t==null || !(t instanceof PrimitiveType) || (((PrimitiveType)t).sort!=Sort.Boolean)){
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
      if (t==null || !(t.isBoolean() || t.isPrimitive(Sort.Resource))){
        Abort("loop invariant is not a boolean or resource (%s)",t);
      }
    }
    ASTNode tmp;
    tmp=s.getEntryGuard();
    if (tmp!=null) {
      Type t=tmp.getType();
      if (t==null || !(t instanceof PrimitiveType) || (((PrimitiveType)t).sort!=Sort.Boolean)){
        Abort("loop entry guard is not a boolean");
      }
    }
    tmp=s.getExitGuard();
    if (tmp!=null) {
      Type t=tmp.getType();
      if (t==null || !(t instanceof PrimitiveType) || (((PrimitiveType)t).sort!=Sort.Boolean)){
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
    case LET:
      e.setType(t);
      break;
    case STAR:{
      Type res=new PrimitiveType(Sort.Resource);
      if (!res.supertypeof(source(), t)){
        Fail("main argument of %s quantifier must be resource",e.binder);
      }
      e.setType(res);
      break;
    }
    case EXISTS:
    case FORALL:{
      Type res=new PrimitiveType(Sort.Boolean);
      if (!res.supertypeof(source(), t)) {
        Fail("main argument of %s quantifier must be boolean",e.binder);
      }
      e.setType(res);
      break;
    }
    case SUM:
      e.setType(t);
      break;
    }
  }
  
  

  @Override
  public void visit(VectorBlock pb){
    if (!pb.iter().getType().isPrimitive(Sort.Integer)) {
      Fail("type of iteration variable must be integer");
    }
    ASTNode init = pb.iter().getInit();
    if (!init.isa(StandardOperator.RangeSeq)){
      Fail("value for iteration variable must be a range");
    }
    init.apply(this);
    pb.block().apply(this);
  }
  

  @Override
  public void visit(ParallelBlock pb){
    for(DeclarationStatement decl:pb.iters){
      if (!decl.getType().isPrimitive(Sort.Integer)){
        Fail("type of iteration variable must be integer");
      }
      ASTNode init=decl.getInit();
      if (!init.isa(StandardOperator.RangeSeq)){
        Fail("value for iteration variable must be a range");
      }
      init.apply(this);
    }
    if (pb.contract!=null) pb.contract.apply(this);
    pb.block.apply(this);
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
      s.setType(new PrimitiveType(Sort.Void));      
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
      if (!t.isBoolean()&&!t.isPrimitive(Sort.Resource)){
        Fail("Argument of %s must be boolean or resource at %s",s.kind,s.getOrigin());
      }
      s.setType(new PrimitiveType(Sort.Void));      
      break;
    }
    }
    s.setType(new PrimitiveType(Sort.Void));
  }

  @Override
  public void visit(FieldAccess a){
    super.visit(a);
    if (a.value() == null) {
      Dereference d = new Dereference(a.object(), a.name());
      visit(d);
      a.setType(d.getType());
    } else {
      a.setType(new PrimitiveType(Sort.Void));
    }
  }
  
  @Override
  public void visit(Constraining c){
    for (NameExpression var : c.vars()){
      var.apply(this);
      Type t=var.getType();
      if (t==null){
        Fail("unresolved variable %s at %s",var,var.getOrigin());
      }
      if (!t.isPrimitive(Sort.Fraction) && !t.isPrimitive(Sort.ZFraction)){
        Fail("variable %s is %s rather than fraction at %s",var,t,var.getOrigin());
      }
    }
    c.block().apply(this);
  }
}
