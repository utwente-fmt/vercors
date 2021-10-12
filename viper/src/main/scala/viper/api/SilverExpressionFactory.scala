package viper.api

import viper.silver.ast._

import scala.jdk.CollectionConverters._
import java.util.List

import scala.math.BigInt.int2bigInt
import viper.silver.ast.SeqAppend

import hre.util
import viper.silver.ast.utility.{BVFactory, FloatFactory, RoundingMode}


class SilverExpressionFactory[O] extends ExpressionFactory[O,Type,Exp] with FactoryUtils[O] {
   
  override def Constant(o:O, i:Int): Exp = IntLit(i)(NoPosition,new OriginInfo(o))
  override def Constant(o:O, f:Float): Exp = {
    val rawIntBits = java.lang.Float.floatToRawIntBits(f)
    val bv32 = applyBackenFunc(o, int_to_bv32_func, Seq(IntLit(rawIntBits)(NoPosition,new OriginInfo(o))))
    applyBackenFunc(o, bv32_to_float_func, Seq(bv32))
  }
  override def Constant(o:O, d:Double): Exp = {
    val rawIntBits = java.lang.Double.doubleToRawLongBits(d)
    val bv64 = applyBackenFunc(o, int_to_bv64_func, Seq(IntLit(rawIntBits)(NoPosition,new OriginInfo(o))))
    applyBackenFunc(o, bv64_to_double_func, Seq(bv64))
  }
  override def Constant(o:O, b:Boolean): Exp =
    if(b) TrueLit()(NoPosition,new OriginInfo(o)) else FalseLit()(NoPosition,new OriginInfo(o))
 
  override def write_perm(o:O):Exp = add(FullPerm()_,o)
  override def read_perm(o:O):Exp = add(WildcardPerm()_,o)
  override def no_perm(o:O):Exp = add(NoPerm()_,o)

  override def field_access(o:O,e1:Exp,e2:Exp):Exp=FieldAccessPredicate(e1.asInstanceOf[FieldAccess],e2)(NoPosition,new OriginInfo(o))
  override def scale_access(o:O,e1:Exp,e2:Exp):Exp=PredicateAccessPredicate(e1.asInstanceOf[PredicateAccess],e2)(NoPosition,new OriginInfo(o))
  
  override def unfolding_in(o:O,e1:Exp,e2:Exp):Exp=Unfolding(e1.asInstanceOf[PredicateAccessPredicate],e2)(NoPosition,new OriginInfo(o))

  override def explicit_set(o:O,t:Type,elems:List[Exp]): Exp =
    if (elems.size() == 0) EmptySet(t)(NoPosition,new OriginInfo(o))
    else ExplicitSet(elems.asScala.toSeq)(NoPosition,new OriginInfo(o))
  override def explicit_bag(o:O,t:Type,elems:List[Exp]): Exp =
    if (elems.size() == 0) EmptyMultiset(t)(NoPosition,new OriginInfo(o))
    else ExplicitMultiset(elems.asScala.toSeq)(NoPosition,new OriginInfo(o))
  override def explicit_seq(o:O,t:Type,elems:List[Exp]): Exp =
    if (elems.size() == 0) EmptySeq(t)(NoPosition,new OriginInfo(o))
    else ExplicitSeq(elems.asScala.toSeq)(NoPosition,new OriginInfo(o))
   
  override def range(o:O, e1:Exp, e2:Exp): Exp = RangeSeq(e1, e2)(NoPosition,new OriginInfo(o))
  override def index(o:O, e1:Exp, e2:Exp): Exp = SeqIndex(e1, e2)(NoPosition,new OriginInfo(o))
  override def take(o:O, e1:Exp, e2:Exp): Exp = SeqTake(e1, e2)(NoPosition,new OriginInfo(o))
  override def drop(o:O, e1:Exp, e2:Exp): Exp = SeqDrop(e1, e2)(NoPosition,new OriginInfo(o))

  // WO: couldn't find a matching AST node in Silver, so for now I'm translating it like this
  override def slice(o: O, e1: Exp, e2: Exp, e3: Exp): Exp = {
    val sub = Sub(e3, e2)(NoPosition, new OriginInfo(o))
    val drop = SeqDrop(e1, e2)(NoPosition, new OriginInfo(o))
    SeqTake(drop, sub)(NoPosition, new OriginInfo(o))
  }

  override def seq_update(o: O, e1: Exp, e2: Exp, e3: Exp): Exp = {
    SeqUpdate(e1, e2, e3)(NoPosition, new OriginInfo(o))
  }

  override def size(o:O,e1:Exp) :Exp = {
      e1.typ match {
        case SeqType(_) => SeqLength(e1)(NoPosition,new OriginInfo(o))
        case MultisetType(_) => AnySetCardinality(e1)(NoPosition,new OriginInfo(o))
        case SetType(_) => AnySetCardinality(e1)(NoPosition,new OriginInfo(o))
        case _ => throw new Error("cannot convert size for type "+e1.typ);
      }
    }
  override def append(o:O,e1:Exp,e2:Exp) :Exp = SeqAppend(e1,e2)(NoPosition,new OriginInfo(o))
  override def union(o:O,e1:Exp,e2:Exp) :Exp = AnySetUnion(e1,e2)(NoPosition,new OriginInfo(o))

  override def seq_contains(o:O,e1:Exp,e2:Exp):Exp = SeqContains(e1,e2)(NoPosition,new OriginInfo(o))
  override def any_set_contains(o:O,e1:Exp,e2:Exp):Exp = AnySetContains(e1,e2)(NoPosition,new OriginInfo(o))
  override def any_set_minus(o:O,e1:Exp,e2:Exp):Exp = add(AnySetMinus(e1,e2)_,o)
  override def any_set_union(o:O,e1:Exp,e2:Exp):Exp = add(AnySetUnion(e1,e2)_,o)
  override def any_set_intersection(o:O,e1:Exp,e2:Exp):Exp = add(AnySetIntersection(e1,e2)_,o)
  override def any_set_subset(o:O,e1:Exp,e2:Exp):Exp = add(AnySetSubset(e1,e2)_,o)

  override def domain_call(o: O,name:String,args:List[Exp], dpars: java.util.Map[String,Type],
      rt:Type,domain:String) : Exp = {
      val tm : Map[viper.silver.ast.TypeVar,viper.silver.ast.Type] = dpars.entrySet().asScala.map {
        case e:java.util.Map.Entry[String,Type] => (TypeVar(e.getKey()),e.getValue())
      }.toMap
      val sargs = args.asScala.toSeq
      DomainFuncApp(name,sargs.toSeq,tm)(NoPosition,new OriginInfo(o),rt,domain, NoTrafos)
  }

  override def let(o:O,n:String,t:Type,e1:Exp,e2:Exp):Exp =
    add(Let(LocalVarDecl(n,t)(NoPosition,new OriginInfo(o)),e1,e2)_,o)
  override def function_call(o: O,name:String,args:List[Exp],rt:Type) : Exp = {
    FuncApp(name,args.asScala.toSeq)(NoPosition,new OriginInfo(o),rt,NoTrafos)
  }
  override def result(o: O,t:Type) : Exp = Result(t)(NoPosition, new OriginInfo(o), NoTrafos)
  
  override def predicate_call(o: O,name:String,args:List[Exp]) : Exp = {
    val e1=PredicateAccess(args.asScala.toSeq,name)(NoPosition,new OriginInfo(o))
    //val e2=FullPerm()(NoPosition,new OriginInfo(o))
    //PredicateAccessPredicate(e1,e2)(NoPosition,new OriginInfo(o))
    e1
  }
  
  override def FieldAccess(o:O, obj:Exp, field:String, t:Type):Exp = {
    val info = new OriginInfo(o)
    val f = Field(field, t)(NoPosition, info, NoTrafos)
    viper.silver.ast.FieldAccess(obj, f)(NoPosition, info, NoTrafos)
  }

  override def neq(o:O,e1:Exp,e2:Exp) :Exp = NeCmp(e1,e2)(NoPosition,new OriginInfo(o))
  override def eq(o:O,e1:Exp,e2:Exp) :Exp = EqCmp(e1,e2)(NoPosition,new OriginInfo(o))

  override def gt(o:O,e1:Exp,e2:Exp) :Exp = {
    e1.typ match {
      case Perm => PermGtCmp(e1, e2)(NoPosition, new OriginInfo(o))
      case _ => GtCmp(e1, e2)(NoPosition, new OriginInfo(o))
    }
  }

  override def lt(o: O, e1: Exp, e2: Exp): Exp = {
    e1.typ match {
      case Perm => PermLtCmp(e1, e2)(NoPosition, new OriginInfo(o))
      case _ => LtCmp(e1, e2)(NoPosition, new OriginInfo(o))
    }
  }

  override def gte(o:O,e1:Exp,e2:Exp) :Exp = {
    e1.typ match {
      case Perm => PermGeCmp(e1, e2)(NoPosition, new OriginInfo(o))
      case _ => GeCmp(e1, e2)(NoPosition, new OriginInfo(o))
    }
  }

  override def lte(o: O, e1: Exp, e2: Exp): Exp = {
    e1.typ match {
      case Perm => PermLeCmp(e1, e2)(NoPosition, new OriginInfo(o))
      case _ => LeCmp(e1, e2)(NoPosition, new OriginInfo(o))
    }
  }
    
  override def and(o:O,e1:Exp,e2:Exp) :Exp = And(e1,e2)(NoPosition,new OriginInfo(o))
  override def or(o:O,e1:Exp,e2:Exp) :Exp = Or(e1,e2)(NoPosition,new OriginInfo(o))
  override def implies(o:O,e1:Exp,e2:Exp) :Exp = Implies(e1,e2)(NoPosition,new OriginInfo(o))
  override def not(o:O,e1:Exp):Exp = Not(e1)(NoPosition,new OriginInfo(o))
  
  override def cond(o:O,e1:Exp,e2:Exp,e3:Exp) :Exp = CondExp(e1,e2,e3)(NoPosition,new OriginInfo(o))

  def perm_exp(e:Exp):Boolean = {
    e match {
      case LocalVar(_, typ) => typ==Perm
      case _:PermExp => true
      case CondExp(_,x,y) => perm_exp(x) || perm_exp(y)
      case _  => false
    }
  }
  override def mult(o:O,e1:Exp,e2:Exp) :Exp = {
    if (perm_exp(e1)){
      PermMul(e1,e2)(NoPosition,new OriginInfo(o))
    } else {
      if (perm_exp(e2)){
        IntPermMul(e1,e2)(NoPosition,new OriginInfo(o))
      } else {
        Mul(e1,e2)(NoPosition,new OriginInfo(o))
      }
    }
  }
  //def div(o:O,e1:Exp,e2:Exp) :Exp = Div(e1,e2)(NoPosition,new OriginInfo(o))
  override def floor_div(o:O, e1:Exp, e2:Exp) :Exp = {
    Div(e1, e2)(NoPosition, new OriginInfo(o))
  }

  override def div(o:O, e1:Exp, e2:Exp): Exp = {
    PermDiv(e1, e2)(NoPosition, new OriginInfo(o))
  }

  override def frac(o: O, e1: Exp, e2: Exp): Exp = {
    e1.typ match {
      case Int => FractionalPerm(e1, e2)(NoPosition, new OriginInfo(o))
      case _ => PermDiv(e1, e2)(NoPosition, new OriginInfo(o))
    }
  }
  override def mod(o:O,e1:Exp,e2:Exp) :Exp = Mod(e1,e2)(NoPosition,new OriginInfo(o))
  override def add(o:O,e1:Exp,e2:Exp) :Exp = {
    e1 match {
      case LocalVar(_, typ) => if (typ==Perm)
          PermAdd(e1,e2)(NoPosition,new OriginInfo(o))
        else
          Add(e1,e2)(NoPosition,new OriginInfo(o))
      case _:PermExp => PermAdd(e1,e2)(NoPosition,new OriginInfo(o))
      case _  => Add(e1,e2)(NoPosition,new OriginInfo(o))
    }
  }
  override def perm_add(o: O, e1: Exp, e2: Exp): Exp = {
    PermAdd(e1, e2)(NoPosition, new OriginInfo(o))
  }
  override def sub(o:O,e1:Exp,e2:Exp) :Exp = {
    e1 match {
      case LocalVar(_, typ) => if (typ==Perm)
          PermSub(e1,e2)(NoPosition,new OriginInfo(o))
        else
          Sub(e1,e2)(NoPosition,new OriginInfo(o))
      case _:PermExp => PermSub(e1,e2)(NoPosition,new OriginInfo(o))
      case _  => Sub(e1,e2)(NoPosition,new OriginInfo(o))
    }
  }
  override def neg(o:O,e1:Exp):Exp = Minus(e1)(NoPosition,new OriginInfo(o))

  override def fp_neg(o:O,e1:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).neg("fp_neg"), Seq(e1))
  override def fp_add(o:O,e1:Exp,e2:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).add("fp_add"), Seq(e1, e2))
  override def fp_sub(o:O,e1:Exp,e2:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).sub("fp_sub"), Seq(e1, e2))
  override def fp_mult(o:O, e1:Exp, e2:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).mul("fp_mult"), Seq(e1, e2))
  override def fp_div(o:O,e1:Exp,e2:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).div("fp_div"), Seq(e1, e2))
  override def fp_eq(o:O,e1:Exp,e2:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).eq("fp_eq"), Seq(e1, e2))
  override def fp_neq(o:O,e1:Exp,e2:Exp):Exp = not(o, applyBackenFunc(o, getFloatFactory(e1).eq("fp_eq"), Seq(e1, e2)))
  override def fp_lte(o:O, e1:Exp, e2:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).leq("fp_lte"), Seq(e1, e2))
  override def fp_gte(o:O, e1:Exp, e2:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).geq("fp_gte"), Seq(e1, e2))
  override def fp_lt(o:O,e1:Exp,e2:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).lt("fp_lt"), Seq(e1, e2))
  override def fp_gt(o:O,e1:Exp,e2:Exp):Exp = applyBackenFunc(o, getFloatFactory(e1).gt("fp_gt"), Seq(e1, e2))

  private def getFloatFactory(e:Exp):FloatFactory = if (e.typ == floatFactory.typ) floatFactory else doubleFactory
  private final val floatFactory = FloatFactory(24, 8, RoundingMode.RNE)
  private final val doubleFactory = FloatFactory(53, 11, RoundingMode.RNE)
  private final val bv32Factory = BVFactory(32)
  private final val bv64Factory = BVFactory(64)
  private final val int_to_bv32_func = bv32Factory.from_int("intToBv32")
  private final val int_to_bv64_func = bv64Factory.from_int("intToBv64")
  private final val bv32_to_float_func = floatFactory.from_bv("bv32ToFp32")
  private final val bv64_to_double_func = doubleFactory.from_bv("bv64ToFp64")
  private final val int_to_float_func = BackendFunc("intToFp32", s"(_ to_fp 8 24) RNE", floatFactory.typ, Seq(LocalVarDecl("x", Int)()))
  private final val int_to_double_func = BackendFunc("intToFp64", s"(_ to_fp 11 53) RNE", doubleFactory.typ, Seq(LocalVarDecl("x", Int)()))
  private final val double_to_float_func = BackendFunc("fp64ToFp32", s"(_ to_fp 8 24) RNE", floatFactory.typ, Seq(LocalVarDecl("x", doubleFactory.typ)()))
  private final val float_to_double_func = BackendFunc("fp32ToFp64", s"(_ to_fp 11 53) RNE", doubleFactory.typ, Seq(LocalVarDecl("x", floatFactory.typ)()))

  private def applyBackenFunc(o: O, func: BackendFunc, args: Seq[Exp]): BackendFuncApp =
    BackendFuncApp(func, args)(NoPosition, new OriginInfo(o))

  override def int_to_float(o: O, e1: Exp): Exp = applyBackenFunc(o, int_to_float_func, Seq(e1))

  override def int_to_double(o: O, e1: Exp): Exp = applyBackenFunc(o, int_to_double_func, Seq(e1))

  override def float_to_double(o: O, e1: Exp): Exp = applyBackenFunc(o, float_to_double_func, Seq(e1))

  override def double_to_float(o: O, e1: Exp): Exp = applyBackenFunc(o, double_to_float_func, Seq(e1))

  override def float_in_bounds(o: O, e1: Exp): Exp = not(o, applyBackenFunc(o, getFloatFactory(e1).isInfinite("isInfinite"), Seq(e1)))

  override def float_not_nan(o: O, e1: Exp): Exp = not(o, applyBackenFunc(o, getFloatFactory(e1).isNaN("isNaN"), Seq(e1)))

  override def local_name(o:O,name:String,t:Type):Exp = LocalVar(name, t)(NoPosition, new OriginInfo(o), NoTrafos)

  override def null_(o:O):Exp = NullLit()(NoPosition,new OriginInfo(o))

  override def forall(o:O, vars:List[util.Triple[O,String,Type]], triggers:List[List[Exp]], e:Exp):Exp = {
    val tmp=triggers.asScala map {
      l => Trigger(l.asScala.toSeq)(NoPosition,new OriginInfo(o))
    }
    Forall(to_decls(o,vars),tmp.toSeq,e)(NoPosition,new OriginInfo(o))
  }
  
  override def exists(o:O, vars:List[util.Triple[O,String,Type]], e:Exp):Exp = {
    Exists(to_decls(o,vars),Seq(),e)(NoPosition,new OriginInfo(o))
  }
  override def old(o:O,e:Exp):Exp = Old(e)(NoPosition,new OriginInfo(o))
 
  override def current_perm(o:O,e:Exp):Exp=CurrentPerm(e.asInstanceOf[LocationAccess])(NoPosition,new OriginInfo(o))

}
