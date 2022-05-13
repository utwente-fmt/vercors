package vct.col.newrewrite

import hre.lang.System.Warning
import hre.util.ScopedStack
import vct.col.ast.{Star, _}
import vct.col.origin._
import vct.col.ast.RewriteHelpers._
import vct.col.ref.Ref
import vct.col.rewrite.{Generation, Rewriter, RewriterBuilder}

import scala.collection.mutable
import vct.col.util.AstBuildHelpers._
import vct.col.util.{AstBuildHelpers, SuccessionMap}

case object CheckProcessAlgebra extends RewriterBuilder {
  override def key: String = "processAlgebra"
  override def desc: String = "TODO description"
}

case class CheckProcessAlgebra[Pre <: Generation]() extends Rewriter[Pre] {
  case class ModelPostconditionFailed(process: ModelProcess[_]) extends Blame[CallableFailure] {
    override def blame(error: CallableFailure): Unit = error match {
      case post: PostconditionFailed => process.blame.blame(post)
      case ctx: ContextEverywhereFailedInPost =>
        PanicBlame("Generated methods for models do not have context_everywhere clauses.").blame(ctx)
      case _: SignalsFailed | _: ExceptionNotInSignals =>
        PanicBlame("Generated methods for models do not throw exceptions.").blame(error)
    }
  }

  case class InsufficientPermissionForModelField(modelDeref: ModelDeref[_]) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit = modelDeref.blame.blame(ModelInsufficientPermission(modelDeref))
  }

  val modelFieldSuccessors: SuccessionMap[ModelField[Pre], InstanceField[Post]] = SuccessionMap()
  val processSuccessors: SuccessionMap[ModelProcess[Pre], InstanceMethod[Post]] = SuccessionMap()
  val modelSuccessors: SuccessionMap[Model[Pre], Class[Post]] = SuccessionMap()
  val currentModel: ScopedStack[Model[Pre]] = ScopedStack()

  override def dispatch(model: Declaration[Pre]): Unit = model match {
    //      val x = Function().declareDefault()
    //      model.succeedDefault(this, x)
    //      model.rewrite().declareDefault()
    case model: Model[Pre] =>
      // We put all permutations of every top-level parallel process
      // in a map to detect overlapping ones.
      // I think I'd prefer this to be done on the fly, instead of generating _all_ permutations.
      // Putting all processes in a set and comparing two sets is better
      val compositeMap: mutable.Map[Set[Expr[Pre]], ModelProcess[Pre]] = mutable.Map()

      // TODO: Refactor this to separate method. Or, maybe in typechecker/frontend? As it could be part of a well-formedness requirement
      model.declarations.foreach {
        case process: ModelProcess[Pre] =>
          process.impl match {
            case processPar: ProcessPar[Pre] =>
              val parallelCompositionElems = processPar.unfoldProcessPar.toSet
              if (parallelCompositionElems.forall(_.isInstanceOf[ProcessApply[Pre]])) {
                if (compositeMap.contains(parallelCompositionElems)) {
                  Warning(
                    "Collision detected: %s vs. %s have same set of process elements composed in parallel",
                    process.o.preferredName,
                    compositeMap(parallelCompositionElems).o.preferredName
                  )
                } else {
                  compositeMap.put(parallelCompositionElems, process)
                }
              } else {
                // TODO: Should this be done by typechecking?
                Warning("Process detected that composes non-process elements in parallel")
              }
            case _ =>
          }
        case _ =>
      }

      val newClass = currentModel.having(model) {
        new Class(
          collectInScope(classScopes) {
            model.declarations.foreach(dispatch(_))
          }, Nil, tt,
        )(model.o)
      }
      newClass.declareDefault(this)
      modelSuccessors(model) = newClass

    case process: ModelProcess[Pre] =>
      implicit val o = process.o

      val currentThis = ThisObject[Post](modelSuccessors.ref(currentModel.top))

      def fieldRefToPerm(p: Expr[Post], f: Ref[Pre, ModelField[Pre]]) =
        fieldPerm[Post](currentThis, modelFieldSuccessors.ref(f.decl), p)

      val fieldPerms = AstBuildHelpers.foldStar(
        process.modifies.map(f => fieldRefToPerm(WritePerm(), f)) ++
          process.accessible.map(f => fieldRefToPerm(ReadPerm(), f)))

      val args = collectInScope(variableScopes)(process.args.foreach(dispatch(_)))

      new InstanceMethod[Post](
        TVoid(),
        args,
        Nil, Nil,
        None, // TODO: Body
        ApplicableContract(
          // TODO: Is reusing fieldPerms allowed?
          UnitAccountedPredicate(Star(fieldPerms, rewriteDefault(process.requires))),
          UnitAccountedPredicate(Star(fieldPerms, rewriteDefault(process.ensures))),
          tt,
          Seq(),
          Seq(),
          Seq()
        )(???),
        false,
        false
      )(ModelPostconditionFailed(process)).declareDefault(this)

    case modelField: ModelField[Pre] =>
      val instanceField = new InstanceField[Post](dispatch(modelField.t), Set())(modelField.o)
      instanceField.declareDefault(this)
      modelFieldSuccessors(modelField) = instanceField

    case other => rewriteDefault(other)
  }

  override def dispatch(expr: Expr[Pre]): Expr[Post] = expr match {
    case p @ ProcessApply(process, args) => MethodInvocation[Post](
        AmbiguousThis()(p.o),
        processSuccessors.ref(process.decl),
        args.map(dispatch(_)), Nil, Nil, Nil, Nil,
      )(null)(p.o)

    case modelDeref: ModelDeref[Pre] =>
      implicit val o = modelDeref.o
      val blame = InsufficientPermissionForModelField(modelDeref)
      Deref[Post](dispatch(modelDeref.obj), modelFieldSuccessors.ref(modelDeref.ref.decl))(blame)

    case x => rewriteDefault(x)
  }

  def inline(a: ModelProcess[Pre], b: Seq[Expr[_]]): Nothing = ???

  // TODO: How to determine at what point to rewrite EmptyProcess/ActionApply? When encountered in expandUnguarded?

  // PB: added dispatch arbitrarily where demanded
  def expandUnguarded(p: Expr[Pre]) : Expr[Post] = p match {
    case p: EmptyProcess[Pre] => p.rewrite()
    case p: ActionApply[Pre] => p.rewrite()
    case ProcessApply(process, args) => expandUnguarded(inline(process.decl, args))
    case ProcessSeq(q, r) => ProcessSeq(expandUnguarded(q), dispatch(r))(p.o)
    case ProcessChoice(q, r) => ProcessChoice(expandUnguarded(q), expandUnguarded(r))(p.o)
    case ProcessPar(q, r) => ProcessChoice[Post](leftMerge(expandUnguarded(q), dispatch(r)), leftMerge(expandUnguarded(r), dispatch(q)))(p.o)
    case ProcessSelect(cond, q, r) =>
      ProcessSelect(dispatch(cond), expandUnguarded(q), expandUnguarded(r))(p.o)
    case _ => ???
  }



  def leftMerge[G](p: Expr[G], q: Expr[G]): Expr[G] = p match {
    case EmptyProcess() => q
    case p: ActionApply[G] => ProcessSeq(p, q)(DiagnosticOrigin)
    case ProcessChoice(pLeft, pRight) => ProcessChoice(leftMerge(pLeft, q), leftMerge(pRight, q))(DiagnosticOrigin)
    case ProcessSeq(pLeft, pRight) => // TODO
      ???
    case ProcessSelect(cond, pLeft, pRight) => ProcessSelect(cond, leftMerge(pLeft, q), leftMerge(pRight, q))(DiagnosticOrigin)
    case ProcessPar(pLeft, pRight) => ??? // Not allowed
    case _ => ???
  }
}
