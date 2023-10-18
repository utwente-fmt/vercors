package vct.col.ast.node

import vct.col.ast._
import vct.col.check._
import vct.col.origin.Origin.{BOLD_HR, HR}
import vct.col.origin._
import vct.col.print._
import vct.col.ref.Ref
import vct.result.VerificationError
import vct.col.util.CurrentCheckNodeContext

import scala.runtime.ScalaRunTime

/** [[Node]] is the root type for all nodes in the abstract syntax tree. The
  * family is sealed, meaning the compiler guarantees that all node definitions
  * occur in one file; Node.scala.
  *
  * Note that the structure of the Node file is restricted: helpers are
  * generated based on the definitions in the file, so you cannot write
  * arbitrary code in the file. Please refer to the developer documentation in
  * the <a href="https://github.com/utwente-fmt/vercors/wiki">VerCors wiki</a>
  * for details.
  *
  * [[Node]] is subdivided into categories. The root of each category extends
  * the marker trait [[NodeFamily]]; the code generation looks for this trait to
  * determine the node categories. Some of the bigger categories include:
  *
  * <ul> <li>[[Type]] contains all the types that can be declared and stored, as
  * well as some helper types.</li> <li>[[Statement]] contains nodes that are
  * executable, and may change the state.</li> <li>[[Expr]] contains nodes that
  * have a defined value and type, given a state.</li> </ul>
  *
  * The [[Declaration]] family has a special status, and is hence not marked as
  * a [[NodeFamily]]. All direct descendants of [[Declaration]] together
  * constitute the "declaration kinds".
  *
  * For each node `equals` must be implemented in such a way that equality means
  * they are semantically equivalent. For most nodes that means simple
  * structural equality, while for (references to) declarations this means
  * identity (reference) equality.
  *
  * @tparam G
  *   The generation marker: not used as a concrete type.
  */
trait NodeImpl[G] extends Show {
  this: Node[G] =>
  def check(context: CheckContext[G]): Seq[CheckError]
  def o: Origin

  def enterCheckContext(context: CheckContext[G]): CheckContext[G] = context

  /* Check children first, so that the check of nodes higher in the tree may depend on the type and correctness of
    subnodes */
  final def checkTrans(context: CheckContext[G]): Seq[CheckError] = {
    val childrenErrors =
      checkContextRecursor(context, (ctx, node) => node.checkTrans(ctx)).flatten

    if (childrenErrors.nonEmpty) { childrenErrors }
    else {
      VerificationError.context(CurrentCheckNodeContext(this)) {
        check(context)
      }
    }
  }

  /** Applies a function to all subnodes, passing the appropriate context per
    * subnode. By default, all subnodes get the same context, as defined by
    * [[enterCheckContext]]
    * @param context
    *   the initial context
    * @param f
    *   the function to apply to all subnodes
    * @tparam T
    *   the result type of the function
    * @return
    *   the result of applying `f` with the appropriate context per subnode to
    *   all subnodes
    */
  def checkContextRecursor[T](
      context: CheckContext[G],
      f: (CheckContext[G], Node[G]) => T,
  ): Seq[T] = subnodes.map(f(enterCheckContext(context), _))

  def subnodes: Seq[Node[G]] = Subnodes.subnodes(this)

  def transSubnodes: LazyList[Node[G]] =
    this #:: subnodes.to(LazyList).flatMap(_.transSubnodes)

  def map[T](f: PartialFunction[Node[G], T]): LazyList[T] =
    f.lift(this) match {
      case Some(value) => LazyList(value)
      case None => subnodes.to(LazyList).flatMap(_.map(f))
    }

  def flatMap[T](f: PartialFunction[Node[G], Seq[T]]): LazyList[T] =
    f.lift(this) match {
      case Some(value) => value.to(LazyList)
      case None => subnodes.to(LazyList).flatMap(_.flatMap(f))
    }

  def collect[T](f: PartialFunction[Node[G], T]): LazyList[T] =
    transSubnodes.collect(f)

  def flatCollect[T](
      f: PartialFunction[Node[G], IterableOnce[T]]
  ): LazyList[T] = transSubnodes.collect(f).flatten

  def collectFirst[T](f: PartialFunction[Node[G], T]): Option[T] =
    collect(f).headOption

  def exists[T](f: PartialFunction[Node[G], Boolean]): Boolean =
    collectFirst(f).getOrElse(false)

  def unsafeTransmuteGeneration[TNode[_] <: Node[_], G2](implicit
      witness: this.type <:< TNode[G]
  ): TNode[G2] = (this: TNode[G]).asInstanceOf[TNode[G2]]

  private def debugLayout(x: scala.Any)(implicit ctx: Ctx): Doc =
    x match {
      case n: Node[_] => n.show
      case r: Ref[_, _] => Text("Ref(") <> ctx.name(r) <> ")"
      case o: scala.Option[scala.Any] if o.isEmpty => Text("None")
      case o: scala.Option[scala.Any] =>
        Text("Some(") <> debugLayout(o.get) <> ")"
      case p: scala.Product =>
        Group(
          Text(p.getClass.getSimpleName) <> "(" <>
            Doc.args(p.productIterator.map(debugLayout).toSeq) <> ")"
        )
      case i: scala.Iterable[scala.Any] =>
        Group(
          Text(i.getClass.getSimpleName) <> "(" <>
            Doc.args(i.map(debugLayout).toSeq) <> ")"
        )
      case other => Text(other.toString)
    }

  final def show(implicit ctx: Ctx): Doc = NodeDoc(this, layout)
  protected[this] def layout(implicit ctx: Ctx): Doc =
    this match {
      case p: scala.Product =>
        Group(
          Text(s"??${this.getClass.getSimpleName}??(") <>
            Doc.args(p.productIterator.map(debugLayout).toSeq) <> ")"
        )
      case _ =>
        Group(
          Text(s"??${this.getClass.getSimpleName}??(") <> Doc.args(subnodes) <>
            ")"
        )
    }

  override def toString: String = toStringWithContext(Ctx().namesIn(this))

  def toInlineString: String = {
    implicit val ctx = Ctx().namesIn(this).copy(width = Int.MaxValue)
    Group(show).toStringWithContext
  }

  def messageInContext(node: Node[_], message: String): String = {
    implicit val ctx: Ctx = Ctx().namesIn(this)
    BOLD_HR + this.show.highlight(node).strip() + "\n" + HR + message + "\n" +
      BOLD_HR
  }
}
