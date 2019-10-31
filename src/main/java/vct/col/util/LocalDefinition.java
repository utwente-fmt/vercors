package vct.col.util;

import vct.col.ast.expr.NameExpression.Kind;
import vct.col.ast.type.Type;

public class LocalDefinition extends AnyDefinition{

  private Type type;
  private Kind kind;
  
  public LocalDefinition(String name,Kind kind,Type type){
    super(name);
    this.type=type;
    this.kind=kind;
  }

  public Type getType() {
    return type;
  }

  public Kind getKind() {
    return kind;
  }
  
}
