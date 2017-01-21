package vct.col.ast;

import static hre.lang.System.Abort;
import static hre.lang.System.Fail;

public final class PrimitiveType extends Type {

  @Override
  public <R,A> R accept_simple(ASTMapping1<R,A> map,A arg){
    return map.map(this,arg);
  }
  public static enum Sort {
    Boolean,
    Byte,
    Short,
    Integer,
    Long,
    UByte,
    UShort,
    UInteger,
    ULong,
    Float,
    Double,
    Char,
    /** non-zero fraction */
    Fraction,
    /** possibly zero fraction */
    ZFraction,
    Void,
    String,
    Class,
    Resource,
    Cell,
    Sequence,
    Set,
    Bag,
    Array,
    Location,
    Process,
    Pointer,
    CVarArgs,
    Option};

  public final Sort sort;
  public PrimitiveType(Sort sort,ASTNode ... args){
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
  public int hashCode(){
    return sort.hashCode();
  }
  public boolean equals(Object o){
    if (o instanceof PrimitiveType) {
      PrimitiveType t=(PrimitiveType)o;
      if (sort!=t.sort) return false;
      if (args.length != t.args.length) return false;
      for(int i=0;i<args.length;i++){
        if (!args[i].equals(t.args[i])) return false;
      }
      return true;
    } else if (o instanceof Sort) {
      if (args.length>0) return false;
      return o==sort;
    } else {
      return false;
    }
  }
  
  public boolean isBoolean() {
    return sort==Sort.Boolean;
  }
  public boolean isResource() {
    return sort==Sort.Resource || sort==Sort.Boolean;
  }
  public boolean isDouble() {
    return sort==Sort.Double;
  }
  public boolean isInteger() {
    return sort==Sort.Integer;
  }
  public boolean isVoid() {
    return sort==Sort.Void;
  }

  
  @Override
  public <T> void accept_simple(ASTVisitor<T> visitor){
    try {
      visitor.visit(this);
    } catch (Throwable t){
      if (thrown.get()!=t){
        System.err.printf("Triggered by %s:%n",getOrigin());
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
        System.err.printf("Triggered by %s:%n",getOrigin());
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
        System.err.printf("Triggered by %s:%n",getOrigin());
        thrown.set(t);
     }
      throw t;
    }
  }

  public String toString(){
    String res=sort.toString();
    if (args.length>0){
      res+="<";
      res+=args[0];
      for(int i=1;i<args.length;i++){
        res+=","+args[i];
      }
      res+=">";
    }
    return res;
  }
  
  @SuppressWarnings("incomplete-switch")
  public boolean supertypeof(ProgramUnit context, Type t){
    
    switch(this.sort){
    case Option:
    case CVarArgs:
      return true;
    case Array:
      if (t.isPrimitive(this.sort)){
        return args[0].equals(((PrimitiveType)t).args[0]);
      }
    case Sequence:
    case Cell:
      if (t instanceof ClassType) {
        ClassType ct=(ClassType)t;
        String name[]=ct.getNameFull();
        if (name.length==1 && name[0].equals("<<null>>")) return true;
      }
    }
    if (t instanceof PrimitiveType){
      PrimitiveType pt=(PrimitiveType)t;
      //Warning("testing (%s/%s)",this.sort,pt.sort);
      if (equals(t)) return true;
      switch(this.sort){
      case Void:
      case Sequence:
      case Array:
        return false;
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
        if (t.isPrimitive(Sort.String)){
          Type tt=((Type)args[0]);
          if (tt.isPrimitive(Sort.Char)) return true;
          if (tt instanceof TypeExpression){
            TypeExpression te=(TypeExpression)tt;
            if (te.operator() == TypeOperator.Const && te.firstType().isPrimitive(Sort.Char)) return true;
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
  public boolean isPrimitive(Sort sort) {
    if(sort==Sort.String && this.sort==Sort.Pointer && ((Type)args[0]).isPrimitive(Sort.Char)) return true;
    return this.sort==sort;
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
    case Integer:
      return new ConstantExpression(0);
    case Long:
      return new ConstantExpression((long)0);
    case Double:
      return new ConstantExpression((double)0);
    case Sequence:
    case Set:
    case Bag:
      return new StructValue(this,null);
    case Option:
      return new NameExpression(ASTReserved.OptionNone);
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
    return isIntegerType() || isFloatType() || sort==Sort.Fraction || sort==Sort.ZFraction ;
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


}
