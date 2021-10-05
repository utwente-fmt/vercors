package vct.col.ast.util;

import vct.col.ast.langspecific.c.CFunctionType;
import vct.col.ast.type.*;

public interface TypeMapping<R> {
  
  void pre_map(Type t);
  
  R post_map(Type t,R res);

  R map(ClassType t);
  
  R map(FunctionType t);
  
  R map(PrimitiveType t);
  
  R map(RecordType t);
  
  R map(TupleType t);

  R map(TypeExpression t);

  R map(TypeVariable v);

  R map(CFunctionType t);
}
