package vct.col.rewrite

import vct.col.ast._
import vct.col.origin._
import vct.col.ref.Ref
import vct.col.util.AstBuildHelpers._

case object EncodeProofHelpers extends RewriterBuilder {
  override def key: String = "proofHelpers"
  override def desc: String = "Encode statements framed with FramedProof, and indeterminate integers."

  case object Once extends Origin {
    override def preferredName: String = "once"
    override def context: String = "[At node generated to execute a while loop once]"
    override def inlineContext: String = "Node generated to execute a while loop once"
    override def shortPosition: String = "generated"
  }

  case object Indet extends Origin {
    override def preferredName: String = "indet"
    override def context: String = "[At node generated to contain an indeterminate integer]"
    override def inlineContext: String = "Node generated to contain an indeterminate integer"
    override def shortPosition: String = "generated"
  }

  case object Before extends Origin {
    override def preferredName: String = "beforeFrame"
    override def context: String = "[At node generated to indicate the point before a proof frame]"
    override def inlineContext: String = "Node generated to indicate the point before a proof frame"
    override def shortPosition: String = "generated"
  }

  case class BeforeVar(preferredName: String) extends Origin {
    override def context: String = "[At variable generated for forperm]"
    override def inlineContext: String = "Variable generated for forperm"
    override def shortPosition: String = "generated"
  }

  case class FramedProofLoopInvariantFailed(proof: FramedProof[_]) extends Blame[LoopInvariantFailure] {
    override def blame(error: LoopInvariantFailure): Unit = error match {
      case LoopInvariantNotEstablished(failure, _) =>
        proof.blame.blame(FramedProofPreFailed(failure, proof))
      case LoopInvariantNotMaintained(failure, _) =>
        proof.blame.blame(FramedProofPostFailed(failure, proof))
    }
  }

  sealed trait LocationKind[G]
  case class FieldKind[G](ref: Ref[G, InstanceField[G]]) extends LocationKind[G]
  case class ModelFieldKind[G](ref: Ref[G, ModelField[G]]) extends LocationKind[G]
  case class SilverFieldKind[G](ref: Ref[G, SilverField[G]]) extends LocationKind[G]
  case class ArrayKind[G](t: Type[G]) extends LocationKind[G]
  case class PointerKind[G](t: Type[G]) extends LocationKind[G]
}

case class EncodeProofHelpers[Pre <: Generation]() extends Rewriter[Pre] {
  import vct.col.rewrite.EncodeProofHelpers._

  override def dispatch(stat: Statement[Pre]): Statement[Post] = stat match {
    case proof @ FramedProof(pre, body, post) =>
      implicit val o: Origin = stat.o

      val beforeLabel = new LabelDecl[Post]()(Before)
      val locationKinds = pre.collect {
        case FieldLocation(_, field) => FieldKind(field)
        case ModelLocation(_, field) => ModelFieldKind(field)
        case SilverFieldLocation(obj, field) => SilverFieldKind(field)
        case ArrayLocation(array, _) => ArrayKind(array.t.asArray.get.element)
        case PointerLocation(pointer) => PointerKind(pointer.t.asPointer.get.element)
      }.distinct

      val locationsSame = {
        implicit val o: Origin = Before
        locationKinds.map {
          case FieldKind(ref) =>
            val obj = new Variable[Post](TAnyClass())(BeforeVar("obj"))
            val before = Old(Deref[Post](obj.get, succ(ref.decl))(FramedByForPerm), Some(beforeLabel.ref))(PanicBlame("loop reached after loop label"))
            val after = Deref[Post](obj.get, succ(ref.decl))(FramedByForPerm)
            ForPerm(Seq(obj), FieldLocation[Post](obj.get, succ(ref.decl)), before === after)
          case ModelFieldKind(ref) =>
            val obj = new Variable[Post](TAnyClass())(BeforeVar("obj"))
            val before = Old(ModelDeref[Post](obj.get, succ(ref.decl))(FramedByForPerm), Some(beforeLabel.ref))(PanicBlame("loop reached after loop label"))
            val after = ModelDeref[Post](obj.get, succ(ref.decl))(FramedByForPerm)
            ForPerm(Seq(obj), FieldLocation[Post](obj.get, succ(ref.decl)), before === after)
          case SilverFieldKind(ref) =>
            val obj = new Variable[Post](TRef())(BeforeVar("obj"))
            val before = Old(SilverDeref[Post](obj.get, succ(ref.decl))(FramedByForPerm), Some(beforeLabel.ref))(PanicBlame("loop reached after loop label"))
            val after = SilverDeref[Post](obj.get, succ(ref.decl))(FramedByForPerm)
            ForPerm(Seq(obj), FieldLocation[Post](obj.get, succ(ref.decl)), before === after)
          case ArrayKind(t) =>
            val arr = new Variable[Post](TArray(dispatch(t)))(BeforeVar("arr"))
            val i = new Variable[Post](TInt())(BeforeVar("i"))
            val inRange = const[Post](0) <= i.get && arr.get !== Null() && i.get < Length(arr.get)(PanicBlame("framed not null"))
            val before = Old(ArraySubscript[Post](arr.get, i.get)(FramedByForPerm), Some(beforeLabel.ref))(PanicBlame("loop reached after loop label"))
            val after = ArraySubscript[Post](arr.get, i.get)(FramedByForPerm)
            ForPerm(Seq(arr, i), ArrayLocation[Post](arr.get, i.get)(PanicBlame("schematic")), inRange ==> (before === after))
          case PointerKind(t) =>
            val ptr = new Variable[Post](TPointer(dispatch(t)))(BeforeVar("ptr"))
            val inRange = ptr.get !== Null()
            val before = Old(DerefPointer(ptr.get)(FramedByForPerm), Some(beforeLabel.ref))(PanicBlame("loop reached after loop label"))
            val after = DerefPointer(ptr.get)(FramedByForPerm)
            ForPerm(Seq(ptr), PointerLocation(ptr.get)(PanicBlame("schematic")), inRange ==> (before === after))
        }
      }

      // PB: assume all mentioned permission locations in pre have the same value as before the loop, since it is only
      // executed once.
      val allLocationsSame = foldAnd(locationsSame.map(PolarityDependent[Post](_, tt)))

      val once = new Variable[Post](TBool())(Once)
      val loop = Loop(
        init = assignLocal(once.get, tt),
        cond = once.get,
        update = assignLocal(once.get, ff),

        contract = LoopInvariant(
          (once.get ==> (dispatch(pre) &* allLocationsSame)) &*
            (!once.get ==> dispatch(post)),
          Some(DecreasesClauseNoRecursion[Post]()),
        )(FramedProofLoopInvariantFailed(proof)),
        body = dispatch(body),
      )

      Scope(Seq(once), Block(Seq(Label(beforeLabel, Block(Nil)), loop)))

    case IndetBranch(branches) =>
      // PB: note that if branches == Nil, this is the same as `inhale false`. This is intended.
      implicit val o: Origin = stat.o
      val v = new Variable[Post](TInt())(Indet)
      Scope(Seq(v), Block(Seq(
        Inhale(v.get >= const(0)),
        Inhale(v.get < const(branches.size)),
        branches match {
          case Nil => Block(Nil)
          case branches => Branch(branches.zipWithIndex.map {
            case (arm, no) => (v.get === const(no), dispatch(arm))
          })
        }
      )))

    case other => rewriteDefault(other)
  }

  override def dispatch(e: Expr[Pre]): Expr[Post] = e match {
    case IndeterminateInteger(min, max) =>
      // PB: note that if max <= min, this is the same as `inhale false`. This is intended.
      implicit val o: Origin = e.o
      val v = new Variable[Post](TInt())(Indet)
      ScopedExpr(Seq(v), With(Block(Seq(
        Inhale(v.get >= dispatch(min)),
        Inhale(v.get < dispatch(max)),
      )), v.get))
    case other => rewriteDefault(other)
  }
}
