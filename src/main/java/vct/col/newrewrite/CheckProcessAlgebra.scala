package vct.col.newrewrite

import hre.lang.System.Warning
import hre.util.ScopedStack
import vct.col.ast.{Star, _}
import vct.col.origin._
import vct.col.ast.RewriteHelpers._
import vct.col.ref.Ref
import vct.col.rewrite.Rewriter

import scala.collection.mutable
import vct.col.util.AstBuildHelpers._
import vct.col.util.{AstBuildHelpers, SuccessionMap}

case class CheckProcessAlgebra() extends Rewriter {
  case class ModelPostconditionFailed(process: ModelProcess) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit = process.blame.blame(error)
  }

  case class InsufficientPermissionForModelField(modelDeref: ModelDeref) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit = modelDeref.blame.blame(ModelInsufficientPermission(modelDeref))
  }

  val modelFieldSuccessors: SuccessionMap[ModelField, InstanceField] = SuccessionMap()
  val processSuccessors: SuccessionMap[ModelProcess, InstanceMethod] = SuccessionMap()
  val modelSuccessors: SuccessionMap[Model, Class] = SuccessionMap()
  val currentModel: ScopedStack[Model] = ScopedStack()

  override def dispatch(model: Declaration): Unit = model match {
    //      val x = Function().declareDefault()
    //      model.succeedDefault(this, x)
    //      model.rewrite().declareDefault()
    case model: Model =>
      // We put all permutations of every top-level parallel process
      // in a map to detect overlapping ones.
      // I think I'd prefer this to be done on the fly, instead of generating _all_ permutations.
      // Putting all processes in a set and comparing two sets is better
      val compositeMap: mutable.Map[Set[Expr], ModelProcess] = mutable.Map()

      // TODO: Refactor this to separate method. Or, maybe in typechecker/frontend? As it could be part of a well-formedness requirement
      model.declarations.foreach {
        case process: ModelProcess =>
          process.impl match {
            case processPar: ProcessPar =>
              val parallelCompositionElems = processPar.unfoldProcessPar.toSet
              if (parallelCompositionElems.forall(_.isInstanceOf[ProcessApply])) {
                if (compositeMap.contains(parallelCompositionElems)) {
                  Warning(
                    "Collision detected: %s vs. %s have same set of process elements composed in parallel",
                    process.o.preferredName,
                    compositeMap.get(parallelCompositionElems).get.o.preferredName
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

    case process: ModelProcess =>
      implicit val o = process.o

      val currentThis = ThisObject(modelSuccessors.ref(currentModel.top))

      def fieldRefToPerm(p: Expr, f: Ref[ModelField]) =
        fieldPerm(currentThis, modelFieldSuccessors.ref(f.decl), p)

      val fieldPerms = AstBuildHelpers.foldStar(
        process.modifies.map(f => fieldRefToPerm(WritePerm(), f)) ++
          process.accessible.map(f => fieldRefToPerm(ReadPerm(), f)))

      val args = collectInScope(variableScopes)(process.args.foreach(dispatch(_)))

      new InstanceMethod(
        TVoid(),
        args,
        Nil, Nil,
        None, // TODO: Body
        ApplicableContract(
          // TODO: Is reusing fieldPerms allowed?
          Star(fieldPerms, rewriteDefault(process.requires)),
          Star(fieldPerms, rewriteDefault(process.ensures)),
          tt,
          Seq(),
          Seq(),
          Seq()
        ),
        false,
        false
      )(ModelPostconditionFailed(process)).declareDefault(this)

    case modelField: ModelField =>
      val instanceField = new InstanceField(modelField.t, Set())(modelField.o)
      instanceField.declareDefault(this)
      modelFieldSuccessors(modelField) = instanceField

    case other => rewriteDefault(other)
  }

  override def dispatch(expr: Expr): Expr = expr match {
    case p @ ProcessApply(process, args) => MethodInvocation(
        AmbiguousThis()(p.o),
        processSuccessors.ref(process.decl),
        args.map(dispatch(_)), Nil, Nil,
      )(null)(p.o)

    case modelDeref: ModelDeref =>
      implicit val o = modelDeref.o
      val blame = InsufficientPermissionForModelField(modelDeref)
      Deref(dispatch(modelDeref.obj), modelFieldSuccessors.ref(modelDeref.ref.decl))(blame)

    case x => rewriteDefault(x)
  }

  def inline(a: ModelProcess, b: Seq[Expr]): Nothing = ???

  // TODO: How to determine at what point to rewrite EmptyProcess/ActionApply? When encountered in expandUnguarded?

  def expandUnguarded(p: Expr) : Expr = p match {
    case p: EmptyProcess => p.rewrite()
    case p: ActionApply => p.rewrite()
    case ProcessApply(process, args) => expandUnguarded(inline(process.decl, args))
    case ProcessSeq(q, r) => ProcessSeq(expandUnguarded(q), r)(p.o)
    case ProcessChoice(q, r) => ProcessChoice(expandUnguarded(q), expandUnguarded(r))(p.o)
    case ProcessPar(q, r) => ProcessChoice(leftMerge(expandUnguarded(q), r), leftMerge(expandUnguarded(r), q))(p.o)
    case ProcessSelect(cond, q, r) =>
      ProcessSelect(dispatch(cond), expandUnguarded(q), expandUnguarded(r))(p.o)
    case _ => ???
  }



  def leftMerge(p: Expr, q: Expr): Expr = p match {
    case EmptyProcess() => q
    case p: ActionApply => ProcessSeq(p, q)(DiagnosticOrigin)
    case ProcessChoice(pLeft, pRight) => ProcessChoice(leftMerge(pLeft, q), leftMerge(pRight, q))(DiagnosticOrigin)
    case ProcessSeq(pLeft, pRight) => // TODO
      ???
    case ProcessSelect(cond, pLeft, pRight) => ProcessSelect(cond, leftMerge(pLeft, q), leftMerge(pRight, q))(DiagnosticOrigin)
    case ProcessPar(pLeft, pRight) => ??? // Not allowed
    case _ => ???
  }
}
