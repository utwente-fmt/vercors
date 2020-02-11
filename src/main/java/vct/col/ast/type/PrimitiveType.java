package vct.col.ast.type;

import scala.collection.Iterable;
import scala.collection.JavaConverters;
import vct.col.ast.expr.constant.ConstantExpression;
import vct.col.ast.expr.NameExpression;
import vct.col.ast.expr.constant.StructValue;
import vct.col.util.ASTMapping;
import vct.col.util.ASTMapping1;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.util.ASTVisitor;
import vct.col.ast.util.TypeMapping;

import static hre.lang.System.Abort;
import static hre.lang.System.Debug;
import static hre.lang.System.Fail;

import java.util.Collections;
import java.util.List;

public final class PrimitiveType extends Type {

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map, A arg){
    return map.map(this,arg);
  }

  public final PrimitiveSort sort;
  public PrimitiveType(PrimitiveSort sort, ASTNode... args){
    super(args);
    int N=args.length;
    switch(sort){
      case Sequence:
      case Set:
      case Bag:
      case Cell:
      case Pointer:
      case Location:
      case Option:
        if (N!=1) Abort("illegal argument count");
        break;
      case Array:
        if (N<1 || N>2) Abort("illegal argument count");
        break;
      default:
        if (N!=0) Abort("illegal argument count");
    }
    this.sort=sort;
  }

  public PrimitiveType(PrimitiveSort sort, List<ASTNode> args){
	  this(sort, args.toArray(new ASTNode[0]));
  }

  public int hashCode(){
    return sort.hashCode();
  }

  public boolean equals(Object o){
    if (o instanceof PrimitiveType) {
      PrimitiveType t = (PrimitiveType)o;
      if (sort != t.sort) return false;
      return this.argsJava().equals(t.argsJava());
    } else if (o instanceof PrimitiveSort) {
      if (this.hasArguments()) return false;
      return o == sort;
    } else {
      return false;
    }
  }

  public boolean isBoolean() {
    return sort==PrimitiveSort.Boolean;
  }
  public boolean isResource() {
    return sort==PrimitiveSort.Resource || sort==PrimitiveSort.Boolean;
  }
  public boolean isDouble() {
    return sort==PrimitiveSort.Double;
  }
  public boolean isInteger() {
    return sort==PrimitiveSort.Integer;
  }
  public boolean isVoid() {
    return sort==PrimitiveSort.Void;
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

  @Override
  public <T> T accept_simple(TypeMapping<T> map){
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

  public String toString(){
    return sort.toString() + super.toString();
  }

  @SuppressWarnings("incomplete-switch")
  public boolean supertypeof(ProgramUnit context, Type t) {
    switch(this.sort) {
      case CVarArgs:
        return true;
      case Option:
        if (t.isNull()) {
        /* Java null is used for both objects (which should be COL null) and arrays (which should be COL None). Only
           after type checking we know which null's should be replaced with None, so we permit null as a value for
           options.
         */
          return true;
        }

        if (t.isPrimitive(PrimitiveSort.Option) && ((Type) t.firstarg()).isNull()) {
          // The type derived from the None value is a valid subclass
          return true;
        }

        return t.isPrimitive(PrimitiveSort.Option) && ((Type)firstarg()).supertypeof(context, ((Type)((PrimitiveType) t).firstarg()));
      case Cell:
        if(firstarg().equals(t)) {
          return true;
        }
        // fallthrough
      case Sequence:
        if (t.isNull()) {
          return true;
        }
        // fallthrough
      case Array:
        return t.isPrimitive(this.sort) && firstarg().equals(((PrimitiveType) t).firstarg());
      case Pointer:
        if(t.isNull()) {
          return true;
        }
        break;
      case ZFraction:
        if(t instanceof ClassType && ((ClassType) t).getName().equals("zfrac")) {
          return true;
        }
        // fallthrough
      case Fraction:
        if(t instanceof ClassType && ((ClassType) t).getName().equals("frac")) {
          return true;
        }
        break;
    }
    if (t instanceof PrimitiveType){
      PrimitiveType pt=(PrimitiveType)t;
      //Warning("testing (%s/%s)",this.sort,pt.sort);
      if (equals(t)) return true;
      switch(this.sort){
      case Void:
      case Set:
      case Bag:
      case Sequence:
      case Array:
        return false;
      case Rational:
        switch(pt.sort) {
          case ZFraction:
          case Fraction:
          case Integer:
          case Short:
          case Byte:
            return true;
        }
        break;
      case ZFraction:
        switch(pt.sort){
        case Fraction:
        case Integer:
        case Short:
        case Byte:
          return true;
        }
        break;
      case Fraction:
        switch(pt.sort){
        case Integer:
        case Short:
        case Byte:
          return true;
        }
        break;
      case Integer:
        switch(pt.sort){
        case Short:
        case Byte:
          return true;
        }
        break;
      case UInteger:
        switch(pt.sort){
        case ULong:
        case UShort:
        case Byte:
          return true;
        }
        break;
      case Long:
        switch(pt.sort){
        case Integer:
        case Short:
        case Byte:
          return true;
        }
        break;
      case ULong:
        switch(pt.sort){
        case Long:
        case Integer:
        case Short:
        case Byte:
          return true;
        }
        break;
      case Float:
        switch(pt.sort){
        case Integer:
        case Short:
        case Byte:
          return true;
        }
        break;
      case Boolean:
        break;
      case Resource:
        switch(pt.sort){
        case Boolean:
          return true;
        }
        break;
      case Char:
        break;
      case Double:
        break;
      case Pointer:
        if (t.isPrimitive(PrimitiveSort.String)){
          Type tt=((Type)firstarg());
          if (tt.isPrimitive(PrimitiveSort.Char)) return true;
          if (tt instanceof TypeExpression){
            TypeExpression te=(TypeExpression)tt;
            if (te.operator() == TypeOperator.Const && te.firstType().isPrimitive(PrimitiveSort.Char)) return true;
          }
          Fail("missing case in PrimitiveType.supertypeof (%s/%s)",this.sort,pt.sort);
        }
        break;
      default:
        Fail("missing case in PrimitiveType.supertypeof (%s/%s)",this.sort,pt.sort);
      }
    }
    return false;
  }

  @Override
  public boolean isPrimitive(PrimitiveSort sort) {
    if(sort==PrimitiveSort.String && this.sort==PrimitiveSort.Pointer && ((Type)firstarg()).isPrimitive(PrimitiveSort.Char)) return true;
    return this.sort==sort;
  }

  @Override
  public boolean isFraction() {
    return isPrimitive(PrimitiveSort.ZFraction) || isPrimitive(PrimitiveSort.Fraction) || isPrimitive(PrimitiveSort.Rational);
  }

  public ASTNode zero(){
    switch(sort){
    case Array:
      return new NameExpression(ASTReserved.Null);
    case Boolean:
    case Resource:
      return new ConstantExpression(false);
    case ZFraction:
      return new ConstantExpression(0);
    case Fraction:
      return new ConstantExpression(0);
    case Rational:
      return new NameExpression(ASTReserved.NoPerm);
    case Integer:
      return new ConstantExpression(0);
    case Long:
      return new ConstantExpression((long)0);
    case Double:
      return new ConstantExpression((double)0);
    case Char:
      return new ConstantExpression((char) 0);
    case Sequence:
    case Set:
    case Bag:
      return new StructValue(this);
    case Option:
      return new NameExpression(ASTReserved.OptionNone);
    case Pointer:
      return new NameExpression(ASTReserved.Null);
    default:
      return super.zero();
    }
  }
  public boolean equalSize(Type t2) {
    int tmp=size();
    if (tmp>0 && t2 instanceof PrimitiveType){
      return tmp==((PrimitiveType)t2).size();
    }
    return false;
  }
  private int size() {
    switch(sort){
      case Short:
      case UShort:
        return 16;
      case UInteger:
      case Integer:
        return 32;
      case Long:
      case ULong:
        return 64;
      default: return -1;
    }

  }

  public boolean isIntegerType() {
    return size()>0;
  }

  public boolean isNumeric() {
    return isIntegerType() || isFloatType() || sort==PrimitiveSort.Fraction || sort==PrimitiveSort.ZFraction || sort == PrimitiveSort.Rational;
  }
  private boolean isFloatType() {
    switch(sort){
      case Float:
      case Double:
        return true;
      default:
        return false;
    }
  }

  @Override
  public Iterable<String> debugTreeChildrenFields() {
    return JavaConverters.iterableAsScalaIterable(Collections.singletonList("args"));
  }

  @Override
  public Iterable<String> debugTreePropertyFields() {
    return JavaConverters.iterableAsScalaIterable(Collections.singletonList("sort"));
  }
}
