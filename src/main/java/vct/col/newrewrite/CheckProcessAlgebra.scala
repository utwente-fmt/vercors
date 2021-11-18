package vct.col.newrewrite

import hre.util.ScopedStack
import vct.col.ast.{Star, _}
import vct.col.origin._
import vct.col.ast.RewriteHelpers._
import vct.col.rewrite.Rewriter
import vct.col.ast.Constant._

import scala.collection.mutable
import vct.col.newrewrite.util.Substitute
import vct.result.VerificationResult.{SystemError, Unreachable, UserError}
import vct.col.util.AstBuildHelpers._
import vct.col.util.SuccessionMap

case class CheckProcessAlgebra() extends Rewriter {
  case class ModelPostconditionFailed(process: ModelProcess) extends Blame[PostconditionFailed] {
    override def blame(error: PostconditionFailed): Unit = process.blame.blame(error)
  }

  case class InsufficientPermissionForModelField(modelDeref: ModelDeref) extends Blame[InsufficientPermission] {
    override def blame(error: InsufficientPermission): Unit = modelDeref.blame.blame(ModelInsufficientPermission(modelDeref))
  }

  val modelFieldSuccessors: SuccessionMap[ModelField, InstanceField] = SuccessionMap()
  val processSuccessors: SuccessionMap[ModelProcess, InstanceMethod] = SuccessionMap()
  val actionSuccessors: SuccessionMap[ModelAction, InstanceMethod] = SuccessionMap()
  val modelSuccessors: SuccessionMap[Model, Class] = SuccessionMap()
  val currentModel: ScopedStack[Model] = ScopedStack()

  var randomBool: Option[Procedure] = None

  def getRandomBoolProc: Procedure = randomBool match {
    case Some(proc) => proc
    case None =>
      implicit val o = DiagnosticOrigin
      // TODO: Add origin s.t. it gets a semi useful name in the backend
      val proc = procedure(
        ???, // Pretty sure this postcondition cannot fail, but should probably have some sane default value here
        TBool()
      )
      randomBool = Some(proc)
      proc
  }

  override def dispatch(program: Program): Unit = {
    val decls = collectInScope(globalScopes) { program.foreach(rewriter.dispatch) }
    val decls2 = randomBool match {
        case Some(value) => value +: decls
        case None => decls
      }
    program.rewrite(decls2)
  }

  override def dispatch(model: Declaration): Unit = model match {
    case model: Model =>
      val newClass = currentModel.having(model) {
        new Class(
          collectInScope(classScopes) {
            model.declarations.foreach(dispatch(_))
          },
          Seq()
        )(model.o)
      }
      newClass.declareDefault(this)
      modelSuccessors(model) = newClass

    case process: ModelProcess =>
      implicit val o = process.o

      def fieldRefToPerm(p: Expr, f: Ref[ModelField]) =
        fieldPerm(This(modelSuccessors(currentModel.top).ref), modelFieldSuccessors.ref(f.decl), p)

      val fieldPerms = Star.fold(
        process.modifies.map(f => fieldRefToPerm(WritePerm(), f)) ++
          process.accessible.map(f => fieldRefToPerm(ReadPerm(), f)))

      val args = collectInScope(variableScopes)(process.args.foreach(dispatch(_)))

      new InstanceMethod(
        TVoid(),
        args,
        Nil, Nil,
        Some(Block(createBody(expandUnguarded(process.impl)))),
        ApplicableContract(
          Star(fieldPerms, rewriteDefault(process.requires)),
          Star(fieldPerms, rewriteDefault(process.ensures)),
          Constant.BooleanValue(true),
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

    case declaration: ClassDeclaration => declaration match {
      case _: ExtraClassDeclaration => // Discard
      case _: InstanceMethod => // Discard
      case function: InstanceFunction => function.rewrite()
      case predicate: InstancePredicate => predicate.rewrite()
      case field: Field => field.rewrite()
    }

    case _ =>
  }

  override def dispatch(t: Type): Type = t match {
    case TModel(modelRef) => TClass(modelSuccessors(modelRef.decl).ref)
    case t => t.rewrite()
  }

  override def dispatch(expr: Expr): Expr = expr match {
    case modelDeref: ModelDeref =>
      implicit val o = modelDeref.o
      val blame = InsufficientPermissionForModelField(modelDeref)
      Deref(dispatch(modelDeref.obj), modelFieldSuccessors.ref(modelDeref.ref.decl))(blame)

    case mt @ ModelThis(modelRef) =>
      implicit val o = mt.o
      This(modelSuccessors(modelRef.decl).ref)

    case x => x.rewrite()
  }

  def inline(a: Apply): Option[Expr] = {
    a.ref.decl.body.get match {
      case e: Expr =>
        implicit val o = DiagnosticOrigin
        val subs = a.ref.decl.args.map(_.get.asInstanceOf[Expr]).zip(a.args).toMap
        Some(Substitute(subs).dispatch(e))
      case _ => None
    }
  }

  def expandUnguarded(p: Expr) : ProcessExpr = p match {
    case p: EmptyProcess => p
    case p: ActionApply => p
    case p: ProcessApply if p.process.decl.body.isEmpty => p
    case p: ProcessApply => expandUnguarded(inline(p).get)
    case ProcessSeq(q, r) => ProcessSeq(expandUnguarded(q), r)(p.o)
    case ProcessChoice(q, r) => ProcessChoice(expandUnguarded(q), expandUnguarded(r))(p.o)
    case ProcessPar(q, r) => ProcessChoice(leftMerge(expandUnguarded(q), r), leftMerge(expandUnguarded(r), q))(p.o)
    case ProcessSelect(cond, q, r) =>
      ProcessSelect(dispatch(cond), expandUnguarded(q), expandUnguarded(r))(p.o)
    case _ => ???
  }

  def leftMerge(p: Expr, q: Expr): Expr = (p, q) match {
    case (EmptyProcess(), _) => q
    case (p: ActionApply, _) => ProcessSeq(p, q)(DiagnosticOrigin)
    case (ProcessChoice(pLeft, pRight), _) => ProcessChoice(leftMerge(pLeft, q), leftMerge(pRight, q))(DiagnosticOrigin)
    case (ProcessSeq(pLeft: Apply, pRight: Apply), q: Apply) if pRight.args.isEmpty && q.args.isEmpty =>
      // This was implemented as a complicated routine in the old CheckProcessAlgebra.java
      // To summarize: replace Seq(pLeft(xs...), pRight(ys...)) with Q(xs..., ys...),for some process Q(zs1..., zs2...) = pLeft(zs1) || pRight(zs2);
      // But no examples triggered this behavior. The correctness of the behavior implemented in CheckProcessAlgebra.java
      // is also doubtful (concatenating some arguments is strange behavior). Therefore we have not implemented it here.
      // Instead we only support the special case without arguments.
      val requiredProcesses = Set(pRight.ref.decl, q.ref.decl);
      val representativeProcesses: Seq[ModelProcess] = currentModel.top.declarations.collect {
        case candidateProcess: ModelProcess =>
          candidateProcess.impl match {
            case ProcessPar(l: Apply, r: Apply) if requiredProcesses == Set(l.ref.decl, r.ref.decl) => candidateProcess
          }
      }
      representativeProcesses match {
        case Seq() => throw RepresentativeMissing(pRight.ref.decl, q.ref.decl)
        case xs if xs.size > 1 => throw AmbiguousRepresentative(pRight.ref.decl, q.ref.decl)
        case Seq(representativeProcess) =>
          val guess = ProcessApply(representativeProcess.ref, Seq())(DiagnosticOrigin)
          ProcessSeq(pLeft, guess)(DiagnosticOrigin)
      }
    case (ProcessSelect(cond, pLeft, pRight), _) => ProcessSelect(cond, leftMerge(pLeft, q), leftMerge(pRight, q))(DiagnosticOrigin)
    case (ProcessPar(_, _), _) =>
      throw Unreachable("Unexpected parallel composition inside left merge, should've been removed by expandUnguarded")
    case _ => ???
  }

  def createBody(p: ProcessExpr): Seq[Statement] = {
    implicit val o = DiagnosticOrigin
    p match {
      case EmptyProcess() => throw Unreachable("The empty process should be optimized away by leftMerge")
      case a @ ActionApply(action, args) =>
        // TODO: Accessible fields, precondition failed blame
        Eval(MethodInvocation(
          This(modelSuccessors(currentModel.top).ref),
          actionSuccessors(action.decl).ref,
          args.map(_.rewrite()),
          Seq(), Seq())(null))
      case ProcessApply(process, args) =>
        // TODO: Accessible fields, precondition failed blame
        Eval(MethodInvocation(
          This(modelSuccessors(currentModel.top).ref),
          processSuccessors(process.decl).ref,
          args.map(_.rewrite()),
            Seq(), Seq())(null)
        )
      case ProcessSeq(left: ProcessExpr, right: ProcessExpr) => createBody(left) ++ createBody(right)
      case ProcessChoice(left, right) => Seq(Branch(Seq(
        // TODO: Precondition of getRandomBool can't fail, so how to blame?
        (ProcedureInvocation(getRandomBoolProc.ref, Seq(), Seq(), Seq())(???), left.rewrite()),
        (tt, right.rewrite())
      )))
      case ProcessSelect(cond, whenTrue, whenFalse) => Seq(Branch(Seq(
        (cond.rewrite(), whenTrue.rewrite()),
        (tt, whenFalse.rewrite())
      )))
      case ProcessPar(left, right) => throw Unreachable("Parallel composition of processes should be removed before entering createBody")
    }
  }
}

case class RepresentativeMissing(left: Applicable, right: Applicable) extends UserError {
  override def code: String = "representativeMissing"

  override def text: String = s"No top-level processes possible for combination:\n- $left\n- $right"
}

case class AmbiguousRepresentative(left: Applicable, right: Applicable) extends UserError {
  override def code: String = "ambiguousRepresentative"

  override def text: String = s"Multiple top-level processes possible for combination:\n- $left\n- $right"
}