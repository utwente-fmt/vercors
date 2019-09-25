package vct.col.rewrite

import vct.col.ast.`type`.{PrimitiveSort, Type}
import vct.col.ast.expr.StandardOperator
import vct.col.ast.expr.constant.StructValue
import vct.col.ast.stmt.decl.ProgramUnit

class InferADTTypes(source: ProgramUnit) extends AbstractRewriter(source, true) {

  override def visit(v: StructValue): Unit = {
    if(v.getType.isPrimitive(PrimitiveSort.Sequence)) { //&&
      result = create.struct_value(create.primitive_type(PrimitiveSort.Sequence, v.getType.firstarg), null, v.values: _*)
//      return create.struct_value(create.primitive_type(PrimitiveSort.Sequence, create.primitive_type(PrimitiveSort.Void)), null, args)

      //      v.getType.firstarg.asInstanceOf[Type].isVoid) {
//
//      val valueTypes = v.values.toStream.map(_.getType).filter(_ != null).toSeq
//
//      if (valueTypes.toSet.nonEmpty)
//        result = create.struct_value(
//          create.primitive_type(PrimitiveSort.Sequence, create.primitive_type(PrimitiveSort.Void)),
//        null,
//          v.values : _*
//      )
//
//
//
//
//      result = null
    } else {
      super.visit(v)
    }
  }
}
