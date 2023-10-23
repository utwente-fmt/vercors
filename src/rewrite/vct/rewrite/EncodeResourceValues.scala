package vct.rewrite

import hre.util.ScopedStack
import vct.col.ast
import vct.col.ast._
import vct.col.origin.Origin
import vct.col.rewrite.ResolveScale.WrongScale
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}
import vct.col.util.DeclarationBox
import vct.result.VerificationError.{SystemError, UserError}
import vct.rewrite.EncodeResourceValues.{UnknownResourceValue, UnsupportedResourceValue}
import vct.col.util.AstBuildHelpers.forall

import scala.collection.mutable

case object EncodeResourceValues extends RewriterBuilder {
  override def key: String = "resourceValues2"
  override def desc: String = "Encode resource value conversions."

  case class UnsupportedResourceValue(node: Node[_], kind: String) extends UserError {
    override def code: String = "wrongResourceValue"
    override def text: String =
      node.o.messageInContext(s"$kind cannot yet be stored in a resource value")
  }

  case class UnknownResourceValue(node: Expr[_]) extends SystemError {
    override def text: String = node.o.messageInContext("Unknown resource kind")
  }
}

case class EncodeResourceValues[Pre <: Generation]() extends Rewriter[Pre] {
  sealed trait ResourcePattern

  object ResourcePattern {
    import vct.col.{ast => col}
    object Bool extends ResourcePattern
    case class Perm(loc: ResourcePatternLoc) extends ResourcePattern
    case class Value(loc: ResourcePatternLoc) extends ResourcePattern
    case class Predicate(p: col.Predicate[Pre]) extends ResourcePattern
    case class InstancePredicate(p: col.InstancePredicate[Pre]) extends ResourcePattern
    case class Star(left: ResourcePattern, right: ResourcePattern) extends ResourcePattern
    case class Implies(res: ResourcePattern) extends ResourcePattern
    case class Select(whenTrue: ResourcePattern, whenFalse: ResourcePattern) extends ResourcePattern
    // Let, Starall: probably need to support lambda's first?
    // Scale: I don't know about wf concerns, maybe just encode it away first?

    sealed trait ResourcePatternLoc
    final case class HeapVariableLocation(ref: col.HeapVariable[Pre]) extends ResourcePatternLoc
    final case class FieldLocation(ref: col.InstanceField[Pre]) extends ResourcePatternLoc
    final case class ModelLocation(ref: col.ModelField[Pre]) extends ResourcePatternLoc
    final case class SilverFieldLocation(ref: col.SilverField[Pre]) extends ResourcePatternLoc
    final case class ArrayLocation(t: Type[Pre]) extends ResourcePatternLoc
    final case class PointerLocation(t: Type[Pre]) extends ResourcePatternLoc
    final case class PredicateLocation(ref: col.Predicate[Pre]) extends ResourcePatternLoc
    final case class InstancePredicateLocation(ref: col.InstancePredicate[Pre]) extends ResourcePatternLoc

    def scan(loc: col.Location[Pre]): ResourcePatternLoc = loc match {
      case col.HeapVariableLocation(ref) => HeapVariableLocation(ref.decl)
      case col.FieldLocation(_, field) => FieldLocation(field.decl)
      case col.ModelLocation(_, field) => ModelLocation(field.decl)
      case col.SilverFieldLocation(_, field) => SilverFieldLocation(field.decl)
      case col.ArrayLocation(arr, _) => ArrayLocation(arr.t.asArray.get.element)
      case col.PointerLocation(ptr) => PointerLocation(ptr.t.asPointer.get.element)
      case col.PredicateLocation(predicate, _) => PredicateLocation(predicate.decl)
      case col.InstancePredicateLocation(predicate, _, _) => InstancePredicateLocation(predicate.decl)
      case AmbiguousLocation(expr) => throw UnknownResourceValue(expr)
    }

    def scan(e: Expr[Pre]): ResourcePattern = e match {
      case s: Scale[Pre] => throw UnsupportedResourceValue(s, "Scaled resources")
      case s: Starall[Pre] => throw UnsupportedResourceValue(s, "Quantified resources")
      case l: Let[Pre] => throw UnsupportedResourceValue(l, "Let expressions")

      case e if TBool().superTypeOf(e.t) => Bool
      case col.Perm(loc, _) => Perm(scan(loc))
      case col.Value(loc) => Value(scan(loc))
      case apply: PredicateApply[Pre] => Predicate(apply.ref.decl)
      case apply: InstancePredicateApply[Pre] => InstancePredicate(apply.ref.decl)

      case col.Star(left, right) => Star(scan(left), scan(right))
      case col.Implies(_, res) => Implies(scan(res))
      case col.Select(_, whenTrue, whenFalse) => Select(scan(whenTrue), scan(whenFalse))

      case other => throw UnknownResourceValue(other)
    }
  }

  case class PatternBuilder(index: Int, toValue: Expr[Pre] => Expr[Post], fromValue: Expr[Pre] => Expr[Post])

  val patternBuilders: ScopedStack[Map[ResourcePattern, PatternBuilder]] = ScopedStack()

  override def dispatch(program: Program[Pre]): Program[Post] = {
    implicit val o: Origin = program.o

    val patterns = program.collect {
      case ResourceValue(res) => ResourcePattern.scan(res)
    }

    if (patterns.isEmpty) {
      return patternBuilders.having(Map.empty) {
        rewriteDefault(program)
      }
    }

    val fieldOwner = program.flatCollect {
      case cls: Class[Pre] => cls.collect {
        case field: Field[Pre] => field -> cls
      }
    }.toMap

    val modelFieldOwner = program.flatCollect {
      case model: Model[Pre] => model.collect {
        case field: ModelField[Pre] => field -> model
      }
    }.toMap

    val predicateOwner = program.flatCollect {
      case cls: Class[Pre] => cls.collect {
        case pred: InstancePredicate[Pre] => pred -> cls
      }
    }.toMap

    val m = mutable.Map[ResourcePattern, PatternBuilder]()

    val adt = DeclarationBox[Post, AxiomaticDataType[Post]]()
    val valType = TAxiomatic(adt.ref, Nil)
    val kind = new ADTFunction[Post](Seq(new Variable(valType)), TInt())

    val decls = patterns.zipWithIndex.flatMap { case (pattern, index) =>
      def freeTypesLoc(location: ResourcePattern.ResourcePatternLoc): Seq[Type[Post]] = location match {
        case ResourcePattern.HeapVariableLocation(_) => Nil
        case ResourcePattern.FieldLocation(f) => Seq(TClass(succ(fieldOwner(f))))
        case ResourcePattern.ModelLocation(f) => Seq(TModel(succ(modelFieldOwner(f))))
        case ResourcePattern.SilverFieldLocation(_) => Seq(TRef())
        case ResourcePattern.ArrayLocation(t) => Seq(TArray(dispatch(t)), TInt())
        case ResourcePattern.PointerLocation(t) => Seq(TPointer(dispatch(t)))
        case ResourcePattern.PredicateLocation(ref) => ref.args.map(_.t).map(dispatch)
        case ResourcePattern.InstancePredicateLocation(ref) => TClass[Post](succ(predicateOwner(ref))) +: ref.args.map(_.t).map(dispatch)
      }

      def freeTypes(pattern: ResourcePattern): Seq[Type[Post]] = pattern match {
        case ResourcePattern.Bool => Nil
        case ResourcePattern.Perm(loc) => freeTypesLoc(loc) :+ TRational()
        case ResourcePattern.Value(loc) => freeTypesLoc(loc)
        case ResourcePattern.Predicate(p) => p.args.map(_.t).map(dispatch)
        case ResourcePattern.InstancePredicate(p) => TClass[Post](succ(predicateOwner(p))) +: p.args.map(_.t).map(dispatch)
        case ResourcePattern.Star(left, right) => freeTypes(left) ++ freeTypes(right)
        case ResourcePattern.Implies(res) => freeTypes(res)
        case ResourcePattern.Select(whenTrue, whenFalse) => freeTypes(whenTrue) ++ freeTypes(whenFalse)
      }

      val ts = freeTypes(pattern)

      Nil
    }

    patternBuilders.having(m.toMap) {
      rewriteDefault(program)
    }
  }
}
