package vct.col.ast.family.location

import vct.col.ast.{
  AmbiguousLocation,
  ArrayLocation,
  FieldLocation,
  HeapVariableLocation,
  InLinePatternLocation,
  Location,
  ModelLocation,
  PointerLocation,
  PredicateLocation,
  SilverFieldLocation,
  Type,
}
import vct.col.ast.node.NodeFamilyImpl
import vct.col.ast.ops.LocationFamilyOps
import vct.col.check.{CheckContext, CheckError}

trait LocationImpl[G] extends NodeFamilyImpl[G] with LocationFamilyOps[G] {
  this: Location[G] =>
  override def check(context: CheckContext[G]): Seq[CheckError] =
    super.check(context)

  def t: Type[G] = {
    this match {
      case FieldLocation(obj, field) => field.decl.t
      case ModelLocation(obj, field) => field.decl.t
      case SilverFieldLocation(obj, field) => field.decl.t
      case ArrayLocation(array, subscript) => array.t.asArray.get.element
      case PointerLocation(pointer) => pointer.t.asPointer.get.element
      case PredicateLocation(inv) => ???
      case AmbiguousLocation(expr) => expr.t
      case InLinePatternLocation(loc, _) => loc.t
      case HeapVariableLocation(ref) => ref.decl.t
    }
  }
}
