package viper.api;

import hre.ast.MessageOrigin;
import vct.col.ast.type.ClassType;
import vct.col.ast.type.PrimitiveSort;
import vct.col.ast.type.Type;
import vct.col.ast.type.TypeVariable;
import vct.col.ast.util.ASTFactory;

public class VerCorsTypeFactory implements
    TypeFactory<Type> {

  public VerCorsTypeFactory(ASTFactory<?> create){
    this.create=create;
  }
  
  private ASTFactory<?> create;

  @Override
  public Type Bag(Type t) {
    return create.primitive_type(PrimitiveSort.Bag,t);
  }

  @Override
  public Type Bool() {
    return create.primitive_type(PrimitiveSort.Boolean);
  }
  
  @Override
  public Type domain_type(String name,java.util.Map<String,Type> args) {
    create.enter();
    create.setOrigin(new MessageOrigin("Silver Domain Type"));
    ClassType res=create.class_type(name,args);
    create.leave();
    return res;
  }

  @Override
  public Type Int() {
    return create.primitive_type(PrimitiveSort.Integer);
  }

  @Override
  public Type Float() {
    return create.primitive_type(PrimitiveSort.Float);
  }

  @Override
  public Type Double() {
    return create.primitive_type(PrimitiveSort.Double);
  }

  @Override
  public Type List(Type t) {
    return create.primitive_type(PrimitiveSort.Sequence,t);
  }

  @Override
  public Type Perm() {
    return create.primitive_type(PrimitiveSort.Rational);
  }
  
  @Override
  public Type Ref() {
    return create.class_type("Ref");
  }

  @Override
  public Type Set(Type t) {
    return create.primitive_type(PrimitiveSort.Set,t);
  }

  @Override
  public Type type_var(String name) {
    TypeVariable res=new TypeVariable(name);
    res.setOrigin(new MessageOrigin("Silver Type Variable"));
    return res;
  }

}
