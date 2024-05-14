package vct.rewrite

import com.typesafe.scalalogging.LazyLogging
import hre.util.ScopedStack
import vct.col.ast
import vct.col.ast.RewriteHelpers.RewriteProgram
import vct.col.ast.{Forall, _}
import vct.col.origin.{Origin, PanicBlame}
import vct.col.rewrite.ResolveScale.WrongScale
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder, Rewritten}
import vct.col.util.DeclarationBox
import vct.result.VerificationError.{SystemError, UserError}
import vct.rewrite.EncodeResourceValues.{
  GenericsNotSupported,
  UnknownResourceValue,
  UnsupportedResourceValue,
  WrongResourcePattern,
}
import vct.col.util.AstBuildHelpers.{ExprBuildHelpers, const, forall}

import scala.collection.mutable

case object EncodeResourceValues extends RewriterBuilder {
  override def key: String = "resourceValues2"
  override def desc: String = "Encode resource value conversions."

  case class UnsupportedResourceValue(node: Node[_], kind: String)
      extends UserError {
    override def code: String = "wrongResourceValue"
    override def text: String =
      node.o.messageInContext(s"$kind cannot yet be stored in a resource value")
  }

  case class GenericsNotSupported(node: Node[_]) extends UserError {
    override def code: String = "genericsNotSupported"
    override def text: String =
      node.o.messageInContext("Generics not supported")
  }

  case class UnknownResourceValue(node: Expr[_]) extends SystemError {
    override def text: String = node.o.messageInContext("Unknown resource kind")
  }

  case class WrongResourcePattern(node: Node[_]) extends SystemError {
    override def text: String =
      node.o.messageInContext("Wrong resource pattern encoding")
  }
}

case class EncodeResourceValues[Pre <: Generation]()
    extends Rewriter[Pre] with LazyLogging {
  sealed trait ResourcePattern

  object ResourcePattern {
    import vct.col.{ast => col}
    object Bool extends ResourcePattern
    case class Perm(loc: ResourcePatternLoc) extends ResourcePattern
    case class Value(loc: ResourcePatternLoc) extends ResourcePattern
    case class Predicate(p: col.Predicate[Pre]) extends ResourcePattern
    case class InstancePredicate(p: col.InstancePredicate[Pre])
        extends ResourcePattern
    case class Star(left: ResourcePattern, right: ResourcePattern)
        extends ResourcePattern
    case class Implies(res: ResourcePattern) extends ResourcePattern
    case class Select(whenTrue: ResourcePattern, whenFalse: ResourcePattern)
        extends ResourcePattern
    // Let, Starall: probably need to support lambda's first?
    // Scale: I don't know about wf concerns, maybe just encode it away first?

    sealed trait ResourcePatternLoc
    final case class HeapVariableLocation(ref: col.HeapVariable[Pre])
        extends ResourcePatternLoc
    final case class FieldLocation(ref: col.InstanceField[Pre])
        extends ResourcePatternLoc
    final case class ModelLocation(ref: col.ModelField[Pre])
        extends ResourcePatternLoc
    final case class SilverFieldLocation(ref: col.SilverField[Pre])
        extends ResourcePatternLoc
    final case class ArrayLocation(t: Type[Pre]) extends ResourcePatternLoc
    final case class PointerLocation(t: Type[Pre]) extends ResourcePatternLoc
    final case class PredicateLocation(ref: col.Predicate[Pre])
        extends ResourcePatternLoc
    final case class InstancePredicateLocation(ref: col.InstancePredicate[Pre])
        extends ResourcePatternLoc

    def scan(loc: col.Location[Pre]): ResourcePatternLoc =
      loc match {
        case col.HeapVariableLocation(ref) => HeapVariableLocation(ref.decl)
        case col.FieldLocation(_, field) => FieldLocation(field.decl)
        case col.ModelLocation(_, field) => ModelLocation(field.decl)
        case col.SilverFieldLocation(_, field) =>
          SilverFieldLocation(field.decl)
        case col.ArrayLocation(arr, _) =>
          ArrayLocation(arr.t.asArray.get.element)
        case col.PointerLocation(ptr) =>
          PointerLocation(ptr.t.asPointer.get.element)
        case col.PredicateLocation(predicate, _) =>
          PredicateLocation(predicate.decl)
        case col.InstancePredicateLocation(predicate, _, _) =>
          InstancePredicateLocation(predicate.decl)
        case col.InLinePatternLocation(loc, _) => scan(loc)
        case AmbiguousLocation(expr) => throw UnknownResourceValue(expr)
      }

    def scan(e: Expr[Pre]): ResourcePattern =
      e match {
        case s: Scale[Pre] =>
          throw UnsupportedResourceValue(s, "Scaled resources")
        case s: Starall[Pre] =>
          throw UnsupportedResourceValue(s, "Quantified resources")
        case l: Let[Pre] => throw UnsupportedResourceValue(l, "Let expressions")

        case e if TBool().superTypeOf(e.t) => Bool
        case col.Perm(loc, _) => Perm(scan(loc))
        case col.Value(loc) => Value(scan(loc))
        case apply: PredicateApply[Pre] => Predicate(apply.ref.decl)
        case apply: InstancePredicateApply[Pre] =>
          InstancePredicate(apply.ref.decl)

        case col.Star(left, right) => Star(scan(left), scan(right))
        case col.Implies(_, res) => Implies(scan(res))
        case col.Select(_, whenTrue, whenFalse) =>
          Select(scan(whenTrue), scan(whenFalse))

        case other => throw UnknownResourceValue(other)
      }
  }

  case class PatternBuilder(
      index: Int,
      toValue: Expr[Pre] => Expr[Post],
      fromValue: Expr[Post] => Expr[Post],
  )

  val patternBuilders: ScopedStack[Map[ResourcePattern, PatternBuilder]] =
    ScopedStack()
  val valAdt: ScopedStack[AxiomaticDataType[Post]] = ScopedStack()
  val kindFunc: ScopedStack[ADTFunction[Post]] = ScopedStack()
  val arbitraryResourceValue: ScopedStack[Predicate[Post]] = ScopedStack()

  def isGeneric(cls: Class[Pre]): Boolean = cls.typeArgs.isEmpty
  def nonGeneric(cls: Class[Pre]): Unit =
    if (isGeneric(cls))
      throw GenericsNotSupported(cls)

  override def dispatch(program: Program[Pre]): Program[Post] = {
    implicit val o: Origin = program.o

    val patterns =
      program.collect { case ResourceValue(res) => ResourcePattern.scan(res) }
        .toIndexedSeq

    if (patterns.isEmpty) {
      return patternBuilders.having(Map.empty) { rewriteDefault(program) }
    }

    val fieldOwner =
      program.flatCollect { case cls: Class[Pre] =>
        cls.collect { case field: Field[Pre] => field -> cls }
      }.toMap

    val modelFieldOwner =
      program.flatCollect { case model: Model[Pre] =>
        model.collect { case field: ModelField[Pre] => field -> model }
      }.toMap

    val predicateOwner =
      program.flatCollect { case cls: Class[Pre] =>
        cls.collect { case pred: InstancePredicate[Pre] => pred -> cls }
      }.toMap

    program.rewrite(globalDeclarations.collect {
      val adt = DeclarationBox[Post, AxiomaticDataType[Post]]()
      val valType = TAxiomatic(adt.ref, Nil)
      val kind =
        new ADTFunction[Post](
          Seq(new Variable(valType)(o.where(name = "val"))),
          TInt(),
        )(o.where(name = "kind"))

      val m = mutable.Map[ResourcePattern, PatternBuilder]()

      val decls = patterns.zipWithIndex.flatMap { case (pattern, index) =>
        def freeTypesLoc(
            location: ResourcePattern.ResourcePatternLoc
        ): Seq[Type[Post]] =
          location match {
            case ResourcePattern.HeapVariableLocation(_) => Nil
            case ResourcePattern.FieldLocation(f) =>
              nonGeneric(fieldOwner(f))
              Seq(fieldOwner(f) match {
                case cls: ByReferenceClass[Pre] =>
                  TByReferenceClass(succ(cls), Seq())
                case cls: ByValueClass[Pre] => TByValueClass(succ(cls), Seq())
              })
            case ResourcePattern.ModelLocation(f) =>
              Seq(TModel(succ(modelFieldOwner(f))))
            case ResourcePattern.SilverFieldLocation(_) => Seq(TRef())
            case ResourcePattern.ArrayLocation(t) =>
              Seq(TArray(dispatch(t)), TInt())
            case ResourcePattern.PointerLocation(t) =>
              Seq(TPointer(dispatch(t)))
            case ResourcePattern.PredicateLocation(ref) =>
              ref.args.map(_.t).map(dispatch)
            case ResourcePattern.InstancePredicateLocation(ref) =>
              nonGeneric(predicateOwner(ref))
              (predicateOwner(ref) match {
                case cls: ByReferenceClass[Pre] =>
                  TByReferenceClass(succ[Class[Post]](cls), Seq())
                case cls: ByValueClass[Pre] =>
                  TByValueClass(succ[Class[Post]](cls), Seq())
              }) +: ref.args.map(_.t).map(dispatch)
          }

        def freeTypes(pattern: ResourcePattern): Seq[Type[Post]] =
          pattern match {
            case ResourcePattern.Bool => Seq(TBool())
            case ResourcePattern.Perm(loc) => freeTypesLoc(loc) :+ TRational()
            case ResourcePattern.Value(loc) => freeTypesLoc(loc)
            case ResourcePattern.Predicate(p) => p.args.map(_.t).map(dispatch)
            case ResourcePattern.InstancePredicate(p) =>
              nonGeneric(predicateOwner(p))
              (predicateOwner(p) match {
                case cls: ByReferenceClass[Pre] =>
                  TByReferenceClass(succ[Class[Post]](cls), Seq())
                case cls: ByValueClass[Pre] =>
                  TByValueClass(succ[Class[Post]](cls), Seq())
              }) +: p.args.map(_.t).map(dispatch)
            case ResourcePattern.Star(left, right) =>
              freeTypes(left) ++ freeTypes(right)
            case ResourcePattern.Implies(res) => freeTypes(res)
            case ResourcePattern.Select(whenTrue, whenFalse) =>
              freeTypes(whenTrue) ++ freeTypes(whenFalse)
          }

        val ts = freeTypes(pattern)

        val buildFunc =
          new ADTFunction(
            ts.map(new Variable[Post](_)(o.where(name = "x"))),
            valType,
          )(o.where(name = s"ResVal$index"))

        val kindAxiom = {
          val vars = ts.map(new Variable[Post](_)(o.where(name = "x")))
          new ADTAxiom(Forall(
            vars,
            Nil,
            ADTFunctionInvocation[Post](
              Some(adt.ref -> Nil),
              kind.ref,
              Seq(ADTFunctionInvocation[Post](
                Some(adt.ref -> Nil),
                buildFunc.ref,
                vars.map(v => Local[Post](v.ref)),
              )),
            ) === const(index),
          ))
        }

        val getters = ts.zipWithIndex.map { case (t, typeIndex) =>
          new ADTFunction[Post](
            Seq(new Variable(valType)(o.where(name = "val"))),
            t,
          )(o.where(name = s"ResVal${index}_get$typeIndex"))
        }

        val getterAxioms = ts.zipWithIndex.map { case (t, index) =>
          val vars = ts.map(new Variable[Post](_)(o.where(name = "x")))
          new ADTAxiom(Forall(
            vars,
            Nil,
            ADTFunctionInvocation[Post](
              Some(adt.ref -> Nil),
              getters(index).ref,
              Seq(ADTFunctionInvocation[Post](
                Some(adt.ref -> Nil),
                buildFunc.ref,
                vars.map(v => Local[Post](v.ref)),
              )),
            ) === Local(vars(index).ref),
          ))
        }

        def makeLoc(
            loc: Location[Pre],
            pat: ResourcePattern.ResourcePatternLoc,
        ): Seq[Expr[Post]] =
          (pat, loc) match {
            case ResourcePattern.HeapVariableLocation(_) ->
                HeapVariableLocation(_) =>
              Nil
            case ResourcePattern.FieldLocation(_) -> FieldLocation(obj, _) =>
              Seq(dispatch(obj))
            case ResourcePattern.ModelLocation(_) -> ModelLocation(model, _) =>
              Seq(dispatch(model))
            case ResourcePattern.SilverFieldLocation(_) ->
                SilverFieldLocation(ref, _) =>
              Seq(dispatch(ref))
            case ResourcePattern.ArrayLocation(_) -> ArrayLocation(arr, idx) =>
              Seq(dispatch(arr), dispatch(idx))
            case ResourcePattern.PointerLocation(_) -> PointerLocation(ptr) =>
              Seq(dispatch(ptr))
            case ResourcePattern.PredicateLocation(_) ->
                PredicateLocation(_, args) =>
              args.map(dispatch)
            case ResourcePattern.InstancePredicateLocation(_) ->
                InstancePredicateLocation(_, obj, args) =>
              dispatch(obj) +: args.map(dispatch)
            case _ -> _ => ???
          }

        def make(e: Expr[Pre], pat: ResourcePattern): Seq[Expr[Post]] =
          (pat, e) match {
            case ResourcePattern.Bool -> e => Seq(dispatch(e))
            case ResourcePattern.Perm(locPat) -> Perm(loc, perm) =>
              makeLoc(loc, locPat) :+ dispatch(perm)
            case ResourcePattern.Value(locPat) -> Value(loc) =>
              makeLoc(loc, locPat)
            case ResourcePattern.Predicate(_) ->
                PredicateApply(_, args, perm) =>
              args.map(dispatch) :+ dispatch(perm)
            case ResourcePattern.InstancePredicate(p) ->
                InstancePredicateApply(obj, _, args, perm) =>
              Seq(dispatch(obj)) ++ args.map(dispatch) ++ Seq(dispatch(perm))
            case ResourcePattern.Star(leftPat, rightPat) -> Star(left, right) =>
              make(left, leftPat) ++ make(right, rightPat)
            case ResourcePattern.Implies(pat) -> Implies(cond, res) =>
              dispatch(cond) +: make(res, pat)
            case ResourcePattern.Select(whenTruePat, whenFalsePat) ->
                Select(cond, whenTrue, whenFalse) =>
              Seq(dispatch(cond)) ++ make(whenTrue, whenTruePat) ++
                make(whenFalse, whenFalsePat)
            case pat -> e => throw WrongResourcePattern(e)
          }

        def toResourceLoc(
            getters: Seq[Expr[Post]],
            pat: ResourcePattern.ResourcePatternLoc,
        )(implicit o: Origin): Location[Post] =
          pat match {
            case ResourcePattern.HeapVariableLocation(ref) =>
              HeapVariableLocation(succ(ref))
            case ResourcePattern.FieldLocation(ref) =>
              FieldLocation(getters.head, succ(ref))
            case ResourcePattern.ModelLocation(ref) =>
              ModelLocation(getters.head, succ(ref))
            case ResourcePattern.SilverFieldLocation(ref) =>
              SilverFieldLocation(getters.head, succ(ref))
            case ResourcePattern.ArrayLocation(t) =>
              ArrayLocation(getters(0), getters(1))(PanicBlame(
                "Design flaw: the structure should include wf somehow"
              ))
            case ResourcePattern.PointerLocation(t) =>
              PointerLocation(getters.head)(PanicBlame(
                "Design flaw: the structure should include wf somehow"
              ))
            case ResourcePattern.PredicateLocation(ref) =>
              PredicateLocation(succ(ref), getters)
            case ResourcePattern.InstancePredicateLocation(ref) =>
              InstancePredicateLocation(succ(ref), getters.head, getters.tail)
          }

        def toResource(getters: Seq[Expr[Post]], pat: ResourcePattern)(
            implicit o: Origin
        ): Expr[Post] =
          pat match {
            case ResourcePattern.Bool => getters.head
            case ResourcePattern.Perm(loc) =>
              Perm(toResourceLoc(getters.init, loc), getters.last)
            case ResourcePattern.Value(loc) =>
              Value(toResourceLoc(getters, loc))
            case ResourcePattern.Predicate(p) =>
              PredicateApply(succ(p), getters.init, getters.last)
            case ResourcePattern.InstancePredicate(p) =>
              InstancePredicateApply(
                getters.head,
                succ(p),
                getters.tail.init,
                getters.last,
              )
            case ResourcePattern.Star(left, right) =>
              val split =
                freeTypes(left).size // indicative of a Really Good Abstraction
              Star(
                toResource(getters.take(split), left),
                toResource(getters.drop(split), right),
              )
            case ResourcePattern.Implies(res) =>
              Implies(getters.head, toResource(getters.tail, res))
            case ResourcePattern.Select(whenTrue, whenFalse) =>
              val whenTrueCount = freeTypes(whenTrue).size
              Select(
                getters.head,
                toResource(getters.tail.take(whenTrueCount), whenTrue),
                toResource(getters.tail.drop(whenTrueCount), whenFalse),
              )
          }

        m(pattern) = PatternBuilder(
          index = index,
          toValue =
            e =>
              ADTFunctionInvocation[Post](
                Some(adt.ref -> Nil),
                buildFunc.ref,
                make(e, pattern),
              ),
          fromValue =
            e =>
              toResource(
                getters.map(getter =>
                  ADTFunctionInvocation(
                    Some(adt.ref -> Nil),
                    getter.ref,
                    Seq(e),
                  )
                ),
                pattern,
              ),
        )

        buildFunc +: kindAxiom +: (getters ++ getterAxioms)
      }

      adt.fill(globalDeclarations.declare(
        new AxiomaticDataType[Post](kind +: decls, Nil)(
          o.where(name = "ResourceVal")
        )
      ))

      val arbitraryValue =
        new Predicate(Seq(new Variable(valType)(o.where(name = "val"))), None)(
          o.where(name = "arbitraryResourceValue")
        )
      globalDeclarations.declare(arbitraryValue)

      patternBuilders.having(m.toMap) {
        valAdt.having(adt.get) {
          arbitraryResourceValue.having(arbitraryValue) {
            kindFunc.having(kind) { program.declarations.foreach(dispatch) }
          }
        }
      }
    }._1)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] =
    e match {
      case ResourceValue(res) =>
        patternBuilders.top(ResourcePattern.scan(res)).toValue(res)

      case ResourceOfResourceValue(resourceValue) =>
        implicit val o: Origin = e.o
        val binding =
          new Variable[Post](TAxiomatic(valAdt.top.ref, Nil))(
            e.o.where(name = "v")
          )
        val v = Local[Post](binding.ref)

        val alts: Seq[(Expr[Post], Expr[Post])] =
          patternBuilders.top.values.map { builder =>
            val cond =
              ADTFunctionInvocation[Post](
                Some(valAdt.top.ref -> Nil),
                kindFunc.top.ref,
                Seq(v),
              ) === const(builder.index)
            val res = builder.fromValue(v)
            cond -> res
          }.toSeq

        val otherwise = PredicateApply[Post](
          arbitraryResourceValue.top.ref,
          Seq(v),
          WritePerm(),
        )

        val select =
          alts.foldRight[Expr[Post]](otherwise) {
            case (cond -> res, otherwise) => Select(cond, res, otherwise)
          }

        Let(binding, dispatch(resourceValue), select)

      case other => rewriteDefault(other)
    }

  override def dispatch(t: Type[Pre]): Type[Post] =
    t match {
      case TResourceVal() => TAxiomatic(valAdt.top.ref, Nil)
      case other => rewriteDefault(other)
    }
}
