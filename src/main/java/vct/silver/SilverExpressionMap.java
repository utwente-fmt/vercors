package vct.silver;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import hre.ast.Origin;
import hre.lang.HREError;

import static hre.lang.System.Abort;

import vct.col.ast.expr.*;
import vct.col.ast.expr.constant.BooleanValue;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.constant.IntegerValue;
import vct.col.ast.expr.constant.StructValue;
import vct.col.util.ASTMapping;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.composite.*;
import vct.col.ast.stmt.decl.*;
import vct.col.ast.stmt.terminal.AssignmentStatement;
import vct.col.ast.stmt.terminal.ReturnStatement;
import vct.col.ast.type.*;
import viper.api.*;

public class SilverExpressionMap<T,E> implements ASTMapping<E> {

  public boolean failure=false;
  
  private ExpressionFactory<Origin,T,E> create;
  private TypeFactory<T> tf;
  private SilverTypeMap<T> type;

  public SilverExpressionMap(ViperAPI<Origin,?,T,E,?,?,?,?> backend,SilverTypeMap<T> type){
    this.create=backend.expr;
    tf=backend._type;
    this.type=type;
  }

  @Override
  public void pre_map(ASTNode n) {
  }

  @Override
  public E post_map(ASTNode n, E res) {
    if (res==null){
      Origin o=n.getOrigin();
      throw new HREError("cannot map %s to expression (%s)",n.getClass(),o!=null?o:"without origin");
    }
    return res;
  }

  @Override
  public E map(StandardProcedure p) {
    return null;
  }

  @Override
  public E map(ConstantExpression e) {
    if (e.value() instanceof IntegerValue){
      int v = ((IntegerValue)e.value()).value();
      if (e.getType().isPrimitive(PrimitiveSort.Rational)){
        switch(v){
          case 0 : return create.no_perm(e.getOrigin());
          case 1 : return create.write_perm(e.getOrigin());
          default: return create.frac(e.getOrigin(), create.Constant(e.getOrigin(), v), create.Constant(e.getOrigin(), 1));
        }
      } else {
        return create.Constant(e.getOrigin(),v);
      }
    } else if (e.value() instanceof BooleanValue) {
      return create.Constant(e.getOrigin(),((BooleanValue)e.value()).value());
    } else {
      throw new HREError("cannot map constant value %s",e.value().getClass());
    }
  }

  @Override
  public E map(OperatorExpression e) {
    Origin o = e.getOrigin();
    E e1=null;
    E e2=null;
    E e3=null;
    switch (e.operator().arity()) {
    case 3:
      e3=e.arg(2).apply(this);
    case 2:
      e2=e.arg(1).apply(this);
    case 1:
      e1=e.arg(0).apply(this);
    }
    switch(e.operator()){
    case PointsTo:{
      return create.and(o,create.field_access(o,e1,e2),create.eq(o, e1, e3));
    }
    case CurrentPerm: return create.current_perm(o,e1);
    case ITE: return create.cond(o,e1,e2,e3);
    case Perm: return create.field_access(o,e1,e2);
    case Value: return create.field_access(o,e1,create.read_perm(o));
    case Star: return create.and(o,e1,e2);
    case And: return create.and(o,e1,e2);
    case Or: return create.or(o,e1,e2);
    case Implies: return create.implies(o,e1,e2);
    case Not: return create.not(o,e1);
    case Unfolding: return create.unfolding_in(o,e1,e2);
    case Old: return create.old(o,e1);
    
    case Size: return create.size(o,e1);
    case Head: return create.index(o, e1, create.Constant(o,0));
    case Tail: return create.drop(o, e1, create.Constant(o,1));
    case Drop: return create.drop(o, e1, e2);
    case Take: return create.take(o, e1, e2);
    case Slice: return create.slice(o, e1, e2, e3);
    case SeqUpdate: return create.seq_update(o, e1, e2, e3);
    case Member: {
      if (e.arg(1).getType().isPrimitive(PrimitiveSort.Sequence)){
        return create.seq_contains(o,e1,e2);
      } else {
        return create.any_set_contains(o,e1,e2);
      }
    }
    case RangeSeq: return create.range(o,e1,e2);
      
    case Subscript: return create.index(o,e1,e2);
    case SubSet: create.and(o, create.any_set_subset(o, e1, e2), create.lt(o, create.size(o, e1), create.size(o, e2)));
    case SubSetEq: return create.any_set_subset(o, e1, e2);
    case GT: return create.gt(o,e1,e2);
    case LT: return create.lt(o,e1,e2);
    case GTE: return create.gte(o,e1,e2);
    case LTE: return create.lte(o,e1,e2);
    case EQ: return create.eq(o,e1,e2);
    case NEQ: return create.neq(o,e1,e2);
    case Mult:{
      if (e.getType().isPrimitive(PrimitiveSort.Set) || e.getType().isPrimitive(PrimitiveSort.Bag)){
        return create.any_set_intersection(o,e1,e2);
      } else {
        return create.mult(o,e1,e2);
      }
    }
    case FloorDiv:
      return create.floor_div(o, e1, e2);
    case Div:
      return create.frac(o, e1, e2);
    case Mod: return create.mod(o,e1,e2);
    case Plus:{
      if (e.getType().isPrimitive(PrimitiveSort.Sequence)){
        return create.append(o,e1,e2);
      } else if (e.getType().isPrimitive(PrimitiveSort.Set) || e.getType().isPrimitive(PrimitiveSort.Bag)){
        return create.union(o,e1,e2);
      } else if(e.getType().isPrimitive(PrimitiveSort.Rational)) {
        return create.perm_add(o, e1, e2);
      } else {
        return create.add(o,e1,e2);
      }
    }
    case Minus: {
      if (e.getType().isPrimitive(PrimitiveSort.Set) || e.getType().isPrimitive(PrimitiveSort.Bag)){
        return create.any_set_minus(o,e1,e2);
      } else {
        return create.sub(o,e1,e2);
      }
    }
    case UMinus: return create.neg(o,e1);
    case Scale:{
      return create.scale_access(o,e2, e1);
    }
    case Append:
      return create.append(o, e1,e2);
    default:
        throw new HREError("cannot map operator %s", e.operator());
    }
  }

  @Override
  public E map(NameExpression e) {
    Origin o = e.getOrigin();
    switch(e.getKind()){
    case Label:
      hre.lang.System.Warning("assuming label %s means local Ref at %s",e.getName(),e.getOrigin());
      return create.local_name(o,e.getName(),tf.Ref());
    case Local:
    case Argument:
      return create.local_name(o,e.getName(),e.getType().apply(type));
    case Reserved:
      switch(e.reserved()){
      case Null:
        return create.null_(o);
      case FullPerm: return create.write_perm(o);
      case ReadPerm: return create.read_perm(o);
      case NoPerm: return create.no_perm(o);
      case Result: return create.result(o,e.getType().apply(type));
      default:
      }
      // fall through on purpose
      default:
        throw new HREError("cannot map %s name %s",e.getKind(),e.getName());
    }
  }

  @Override
  public E map(ClassType t) {
    return null;
  }

  @Override
  public E map(FunctionType t) {
    return null;
  }

  @Override
  public E map(PrimitiveType t) {
    return null;
  }

  @Override
  public E map(RecordType t) {
    return null;
  }

  @Override
  public E map(MethodInvokation e) {
    Method m=e.getDefinition();
    Origin o=e.getOrigin();
    String name=e.method;
    ArrayList<E> args=new ArrayList<E>();
    for(ASTNode n:e.getArgs()){
      args.add(n.apply(this));
    }
    switch(m.kind){
    case Pure:{
      T rt=e.getType().apply(type);
      if(m.getParent() instanceof AxiomaticDataType){
        ArrayList<Triple<Origin,String,T>> pars=new ArrayList<Triple<Origin,String,T>> ();
        for(DeclarationStatement decl:m.getArgs()){
          Type t=e.getArg(pars.size()).getType();
          T t2;
          if (t.isNull()){
            t2=decl.getType().apply(type);
          } else {
            t2=t.apply(type);
          }
          pars.add(new Triple<Origin, String, T>(decl.getOrigin(),decl.name(),t2));
        }
        AxiomaticDataType adt=(AxiomaticDataType)m.getParent();
        HashMap<String, T> dpars=new HashMap<String, T>();
        type.domain_type(dpars,(ClassType)e.object);
        return create.domain_call(o, name, args, dpars, rt, adt.name());
      } else {
        
        ArrayList<Triple<Origin,String,T>> pars=new ArrayList<Triple<Origin,String,T>>();
        for(DeclarationStatement decl:m.getArgs()){
          pars.add(new Triple<Origin,String,T>(decl.getOrigin(),decl.name(),decl.getType().apply(type)));
        }
        return create.function_call(o, name, args, rt);
      }
    }
    case Predicate:{
      return create.predicate_call(o, name, args);
    }
    default:
      throw new HREError("calling a %d method is not a Silver expression");
    }
  }

  @Override
  public E map(BlockStatement s) {
    return null;
  }

  @Override
  public E map(IfStatement s) {
    return null;
  }

  @Override
  public E map(ReturnStatement s) {
    return null;
  }

  @Override
  public E map(AssignmentStatement s) {
    return null;
  }

  @Override
  public E map(DeclarationStatement s) {
    return null;
  }

  @Override
  public E map(LoopStatement s) {
    return null;
  }

  @Override
  public E map(Method m) {
    return null;
  }

  @Override
  public E map(ASTClass c) {
    return null;
  }

  @Override
  public E map(BindingExpression e) {
    Origin o = e.getOrigin();
    switch (e.binder) {
    case Star:
      if ((e.main instanceof BindingExpression)||e.getDeclarations().length>1){
        hre.lang.System.Warning("Simplification failure: %s",e);
        failure=true;
      } else {
        boolean good=false;
        if (e.main.getType().isBoolean()){
          good=true;
        } else if (e.main.isa(StandardOperator.Perm)||e.main.isa(StandardOperator.Value)){
          ASTNode loc=((OperatorExpression)e.main).arg(0);
          while (loc instanceof Dereference){
            loc=((Dereference)loc).obj();
          }
          if (loc instanceof MethodInvokation
                  && ((MethodInvokation) loc).object instanceof ClassType
                  && ((ClassType) ((MethodInvokation) loc).object).getName().equals("VCTArray")
                  && ((MethodInvokation) loc).method.startsWith("loc")) {
            loc = ((MethodInvokation) loc).getArg(0);
          }
          if(loc instanceof MethodInvokation && ((MethodInvokation) loc).method.startsWith("getVCTOption")) {
            loc = ((MethodInvokation) loc).getArg(0);
          }
          good=loc instanceof NameExpression;
        }
        if(!good){
          hre.lang.System.Warning("Possible simplification failure: %s",e);
        }
      }
    case Forall:
      E expr;
      if (e.select.isConstant(true)){
        expr=e.main.apply(this);
      } else {
        expr=create.implies(o, e.select.apply(this), e.main.apply(this));
      }
      List<List<E>> triggers=new ArrayList<List<E>>();
      if (e.triggers!=null){
        for (ASTNode trigger[]:e.triggers){
          List<E> tmp=new ArrayList<E>();
          for (ASTNode node:trigger){
            tmp.add(node.apply(this));
          }
          triggers.add(tmp);
        }
      }
      return create.forall(o, convert(e.getDeclarations()),triggers ,expr);
    case Exists:
      return create.exists(o, convert(e.getDeclarations()),create.and(o, e.select.apply(this), e.main.apply(this)));
    case Let:{
      DeclarationStatement decls[]=e.getDeclarations();
      E res=e.main.apply(this);
      for(int i=decls.length-1;i>=0;i--){
        res=create.let(
            o,
            decls[i].name(),
            decls[i].getType().apply(type),
            decls[i].initJava().apply(this),
            res
        );
      }
      return res;
    }
    default:
      Abort("binder %s not supported",e.binder);
    }
    return null;
  }

  private List<Triple<Origin,String,T>> convert(DeclarationStatement[] declarations) {
    ArrayList<Triple<Origin,String,T>> res=new ArrayList<Triple<Origin,String,T>>();
    for(DeclarationStatement d:declarations){
      res.add(new Triple<Origin,String,T>(d.getOrigin(),d.name(),d.getType().apply(type)));
    }
    return res;
  }

  @Override
  public E map(Dereference e) {
    return create.FieldAccess(e.getOrigin(), e.obj().apply(this), e.field(), e.getType().apply(type));
  }

  @Override
  public E map(Lemma lemma) {
    return null;
  }

  @Override
  public E map(ParallelBarrier parallelBarrier) {
    return null;
  }

  @Override
  public E map(ParallelBlock parallelBlock) {
    return null;
  }

  @Override
  public E map(Contract contract) {
    return null;
  }

  @Override
  public E map(ASTSpecial special) {
    return null;
  }

  @Override
  public E map(VariableDeclaration variableDeclaration) {
    return null;
  }

  @Override
  public E map(TupleType tupleType) {
    return null;
  }
  
  @Override
  public E map(TypeExpression t) {
    return null;
  }

  @Override
  public E map(AxiomaticDataType adt) {
    return null;
  }

  @Override
  public E map(Axiom axiom) {
    return null;
  }

  @Override
  public E map(Hole hole) {
    return null;
  }

  @Override
  public E map(ActionBlock actionBlock) {
    return null;
  }

  @Override
  public E map(ForEachLoop s) {
    return null;
  }

  @Override
  public E map(ParallelAtomic parallelAtomic) {
    return null;
  }

  @Override
  public E map(ParallelInvariant inv) {
    return null;
  }

  @Override
  public E map(NameSpace nameSpace) {
    return null;
  }

  @Override
  public E map(TryCatchBlock tcb) {
    return null;
  }

  @Override
  public E map(FieldAccess a) {
    return null;
  }

  @Override
  public E map(ParallelRegion region) {
    return null;
  }

  @Override
  public E map(TypeVariable v) {
    return null;
  }

  @Override
  public E map(StructValue v) {
    Origin o = v.getOrigin();
    ArrayList<E> elems = new ArrayList<E>();
    for (int i = 0; i < v.valuesLength(); i++) {
      elems.add(v.value(i).apply(this));
    }
    T t=((Type)((Type)v.type()).firstarg()).apply(type);
    switch(((PrimitiveType)v.type()).sort){
    case Sequence:
      return create.explicit_seq(o, t, elems);
    case Bag:
      return create.explicit_bag(o, t, elems);
    case Set:
      return create.explicit_set(o, t, elems);
    default:
      return null;
    }
  }

  @Override
  public E map(VectorBlock vb) {
    return null;
  }

  @Override
  public E map(Constraining c) {
    return null;
  }

  @Override
  public E map(Switch s) {
    return null;
  }
  
}
