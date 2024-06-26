package vct.col.ast.node

import vct.col.ast._
import vct.col.check._
import vct.col.compare.CompareResult
import vct.col.origin._
import vct.col.print._
import vct.col.ref.Ref
import vct.result.{HasContext, VerificationError}
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
trait NodeImpl[G]
    extends Show
    with NodeCheckOps[G]
    with NodeSubnodeOps[G]
    with NodeLayoutOps[G] {
  this: Node[G] =>
  def check(context: CheckContext[G]): Seq[CheckError]
  def o: Origin

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

  def compare[G1](other: Node[G1]): LazyList[CompareResult[G, G1]]
  def rewriteDefault[G1]()(implicit rw: AbstractRewriter[G, G1]): Node[G1]
  def serialize(decls: Map[Declaration[G], Long]): scalapb.GeneratedMessage
  def serializeFamily(
      decls: Map[Declaration[G], Long]
  ): scalapb.GeneratedMessage

  def subnodes: Seq[Node[G]]

  def unsafeTransmuteGeneration[TNode[_] <: Node[_], G2]: TNode[G2] =
    this.asInstanceOf[TNode[G2]]
}
