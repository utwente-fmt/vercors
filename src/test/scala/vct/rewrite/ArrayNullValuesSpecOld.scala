package vct.rewrite

import vct.col.ast._
import vct.col.ast.`type`.{ASTReserved, PrimitiveSort}
import vct.col.ast.expr.StandardOperator
import vct.col.ast.generic.ASTNode
import vct.col.ast.stmt.decl.ProgramUnit
import vct.col.rewrite.ArrayNullValues
import vct.col.util.AbstractTypeCheck

class ArrayNullValuesSpecOld extends RewriteSpecOld(new ArrayNullValues(null), before=new AbstractTypeCheck(null, new ProgramUnit())) {
  def INT_TYPE = create primitive_type PrimitiveSort.Integer
  def SEQ_TYPE = create class_type "seq"
  def BOOL_TYPE = create primitive_type PrimitiveSort.Boolean

  def OPT_INT_ARRAY_TYPE = create.primitive_type(PrimitiveSort.Option,
    create.primitive_type(PrimitiveSort.Array,
      create.primitive_type(PrimitiveSort.Cell, INT_TYPE)))

  def SEQ_ARRAY_TYPE = create.primitive_type(PrimitiveSort.Array,
    create.primitive_type(PrimitiveSort.Cell, SEQ_TYPE))

  def OPT_SEQ_ARRAY_TYPE = create.primitive_type(PrimitiveSort.Option, SEQ_ARRAY_TYPE)

  def SEQ_ARRAY_ARRAY_TYPE = create.primitive_type(PrimitiveSort.Array,
      create.primitive_type(PrimitiveSort.Cell, OPT_SEQ_ARRAY_TYPE))


  def NULL = create reserved_name ASTReserved.Null
  def NONE = create reserved_name ASTReserved.OptionNone
  def CASTED_NONE_INT_ARRAY = create expression(StandardOperator.Cast, OPT_INT_ARRAY_TYPE, NONE)
  def CASTED_NONE_SEQ_ARRAY = create expression(StandardOperator.Cast, OPT_SEQ_ARRAY_TYPE, NONE)

  "The ArrayNullValues pass" should "not rewrite arbitrary null values" in {
    // The desired behaviour is that the type check pass strips the garbage assigned type, so the pass does not rewrite
    // the value, even though it is initially marked as an array.
    NULL asType OPT_INT_ARRAY_TYPE should rewriteTo(NULL)
  }

  it should "rewrite array comparison with null to none" in {
    var comparison = (right: ASTNode) => create expression(StandardOperator.EQ,
      create expression(StandardOperator.NewArray, OPT_INT_ARRAY_TYPE, create constant 5),
      right
    )

    comparison(NULL) should rewriteTo(comparison(CASTED_NONE_INT_ARRAY))
  }

  it should "propagate typing of if-then-else expressions" in {
    var comparison = (right: ASTNode) => create.expression(StandardOperator.EQ,
      create.expression(StandardOperator.NewArray, OPT_INT_ARRAY_TYPE, create constant 5),
      create.expression(StandardOperator.ITE,
        create constant true,
        right,
        right
      )
    )

    comparison(NULL) should rewriteTo(comparison(CASTED_NONE_INT_ARRAY))
  }

  def wrappingOperator(operator: StandardOperator): Unit = {
    var comparison = (right: ASTNode) => create.expression(StandardOperator.EQ,
      create.expression(StandardOperator.NewArray, OPT_INT_ARRAY_TYPE, create constant 5),
      create.expression(operator, right)
    )

    comparison(NULL) should rewriteTo(comparison(CASTED_NONE_INT_ARRAY))
  }

  it should "propagate typing in \\old expressions" in {
    wrappingOperator(StandardOperator.Old)
  }

  it should "propagate typing in Wrap expressions" in {
    wrappingOperator(StandardOperator.Wrap)
  }

  it should "rewrite array values within StructValues" in {
    var structValue = (innerValue: ASTNode) => create struct_value(SEQ_ARRAY_ARRAY_TYPE, null,
      innerValue
    )

    structValue(NULL) should rewriteTo(structValue(CASTED_NONE_SEQ_ARRAY))
  }

  it should "not rewrite element null values within StructValues" in {
    var structValue = (innerValue: ASTNode) => create struct_value(SEQ_ARRAY_ARRAY_TYPE, null,
      create expression(StandardOperator.OptionSome, create struct_value(SEQ_ARRAY_TYPE, null,
        innerValue
      ))
    )

    structValue(NULL) should rewriteTo(structValue(NULL))
  }
}
