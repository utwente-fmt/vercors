package vct.col.ast.family.location

import vct.col.ast.{AmbiguousLocation, ArrayLocation, FieldLocation, InstancePredicateLocation, Location, ModelLocation, PointerLocation, PredicateLocation, SilverFieldLocation, Type}

trait LocationImpl[G] { this: Location[G] =>
  def t: Type[G] = {
    this match {
      case FieldLocation(obj, field) => field.decl.t
      case ModelLocation(obj, field) => field.decl.t
      case SilverFieldLocation(obj, field) => field.decl.t
      case ArrayLocation(array, subscript, _) => array.t.asArray.get.element
      case PointerLocation(pointer, _) => pointer.t.asPointer.get.element
      case PredicateLocation(predicate, args) => ???
      case InstancePredicateLocation(predicate, obj, args) => ???
      case AmbiguousLocation(expr, _) => expr.t
    } }
}
