package viper.api;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;

import hre.ast.MessageOrigin;
import hre.ast.Origin;
import hre.lang.HREError;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.langspecific.c.CFunctionType;
import vct.col.ast.type.*;
import vct.col.ast.util.TypeMapping;
import vct.col.ast.util.AbstractRewriter;

public class SilverTypeMap<T> implements TypeMapping<T> {

  private TypeFactory<T> create;
  
  public SilverTypeMap(ViperAPI<Origin, T,?,?,?,?,?> backend){
    this.create=backend._type;
  }
  
  @Override
  public void pre_map(Type t) {
  }

  @Override
  public T post_map(Type t, T res) {
    return res;
  }

  public static void get_adt_subst(
      AbstractRewriter copy,
      Map<String,Type> map,
      ClassType t
  ){
    for (ASTNode a : t.argsJava()) {
      String s=a.getLabel(0).toString();
      if( s.equals(a.toString())){
        // TODO: properly detect type vars!
        ClassType res=new ClassType(s);
        res.setOrigin(new MessageOrigin("ADT Type Variable"));
        map.put(s,res);
      } else {
        ASTNode n=copy.rewrite(a);
        n.clearLabels();
        map.put(s,(Type)n);
      }
    }
  }

  
  public T domain_type(HashMap<String,T> map,ClassType t){
    for (ASTNode a : t.argsJava()) {
      String s=a.getLabel(0).toString();
      if( s.toString().equals(a.toString())){
        // TODO: properly detect type vars!
        map.put(s,create.type_var(s));
      } else {
        map.put(s,((Type)a).apply(this));
      }
    }
    return create.domain_type(t.getName(),map);
  }
  
  @Override
  public T map(ClassType t) {
    if (t.getName().equals("Ref")){
      return create.Ref();
    } else {
      LinkedHashMap<String,T> map=new LinkedHashMap<String, T>();
      return domain_type(map,t);
    }
  }

  @Override
  public T map(FunctionType t) {
    throw new HREError("function types are not supported"); 
  }

  private T map(ASTNode n){
    if (n instanceof Type){
      return ((Type)n).apply(this);
    } else {
      throw new HREError("cannot type map %s",n.getClass());
    }
  }
  @Override
  public T map(PrimitiveType t) {
    switch(t.sort){
      /* Hacky types to make Java standard library support work: */
      case Long:
      case Byte:
      case Char:
      case Short:
      /* End of hacky types. */
      case Integer:
        return create.Int();
      case Float:
        return create.Float();
      case Double:
        return create.Double();
      case Boolean:
        return create.Bool();
      case ZFraction:
        return create.domain_type("zfrac", new HashMap<>());
      case Fraction:
        return create.domain_type("frac", new HashMap<>());
      case Rational:
        return create.Perm();
      case Sequence:
        return create.List(map(t.firstarg()));
      case Set:
        return create.Set(map(t.firstarg()));
      case Bag:
        return create.Bag(map(t.firstarg()));
      case String:
        return create.List(create.Int());
      default:
        throw new HREError("primitive type %s unsupported",t.sort);
    }
  }

  @Override
  public T map(RecordType t) {
    throw new HREError("record types are not supported");
  }

  @Override
  public T map(TupleType t) {
    throw new HREError("tuple types are not supported");
  }

  @Override
  public T map(TypeExpression t) {
    throw new HREError("type expressions are not supported");
  }

  @Override
  public T map(TypeVariable v) {
    return create.type_var(v.name());
  }

  @Override
  public T map(CFunctionType t) {
    throw new HREError("c function types are not supported");
  }
}
