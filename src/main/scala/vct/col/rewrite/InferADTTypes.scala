package vct.col.rewrite

import vct.col.ast.`type`.{PrimitiveSort, PrimitiveType, TypeVariable}
import vct.col.ast.expr.constant.StructValue
import vct.col.ast.stmt.decl.ProgramUnit

object InferADTTypes {
  val typeVariableName = "INFER_ADT_TYPE"
}

/**
 *
 */
class InferADTTypes(source: ProgramUnit) extends AbstractRewriter(source, true) {


  override def visit(v: StructValue): Unit = {
    val collections = List(PrimitiveSort.Sequence, PrimitiveSort.Set, PrimitiveSort.Bag)

    if((v.`type`.isPrimitive(PrimitiveSort.Sequence) || v.`type`.isPrimitive(PrimitiveSort.Set) || v.`type`.isPrimitive(PrimitiveSort.Bag)) &&
      v.`type`.args.nonEmpty &&
      v.`type`.firstarg.isInstanceOf[TypeVariable] &&
      v.`type`.firstarg.asInstanceOf[TypeVariable].name == InferADTTypes.typeVariableName
    ) {
      // If the inference succeeded in the type checker, then the type should be v.getType
      result = create.struct_value(create.primitive_type(v.`type`.asInstanceOf[PrimitiveType].sort, v.getType.firstarg), null, v.values: _*)
    } else {
      super.visit(v)
    }
  }
}
