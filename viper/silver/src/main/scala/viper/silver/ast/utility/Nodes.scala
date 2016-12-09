/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package viper.silver.ast.utility

import scala.collection.mutable
import viper.silver.ast._

/** Utility methods for AST nodes. */
object Nodes {

  /** Returns a list of all direct sub-nodes of this node. The type of nodes is
    * included in this list only for declarations (but not for expressions, for
    * instance).
    *
    * Furthermore, pointers to declarations are not included either (e.g., the
    * `field` (which is of type `Node`) of a `FieldAccess`), and neither are
    * non-`Node` children (e.g., the `predicateName` (a `String`) of a
    * `PredicateAccess`).
    *
    * As a consequence, it is not sufficient to compare the subnodes of two
    * nodes for equality if one has to compare those two nodes for equality.
   */
  def subnodes(n: Node): Seq[Node] = {
    val subnodesWithType: Seq[Node] = n match {
      case Trigger(exps) => exps
      case Program(domains, fields, functions, predicates, methods) =>
        domains ++ fields ++ functions ++ predicates ++ methods
      case m: Member =>
        m match {
          case Field(name, typ) => Nil
          case Function(name, formalArgs, typ, pres, posts, body) =>
            formalArgs ++ pres ++ posts ++ body
          case Method(name, formalArgs, formalReturns, pres, posts, locals, body) =>
            formalArgs ++ formalReturns ++ pres ++ posts ++ locals ++ Seq(body)
          case Predicate(name, formalArg, body) => formalArg ++ body.toSeq
          case Domain(name, functions, axioms, typVars) =>
            functions ++ axioms ++ typVars
        }
      case dm: DomainMember =>
        dm match {
          case DomainAxiom(_, exp) => Seq(exp)
          case DomainFunc(_, formalArgs, _, _) => formalArgs
        }
      case s: Stmt =>
        s match {
          case NewStmt(target, fields) => Seq(target) ++ fields
          case LocalVarAssign(lhs, rhs) => Seq(lhs, rhs)
          case FieldAssign(lhs, rhs) => Seq(lhs, rhs)
          case Fold(e) => Seq(e)
          case Unfold(e) => Seq(e)
          case Package(e) => Seq(e)
          case Apply(e) => Seq(e)
          case Inhale(e) => Seq(e)
          case Exhale(e) => Seq(e)
          case Assert(e) => Seq(e)
          case MethodCall(mname, args, targets) => args ++ targets
          case Seqn(ss) => ss
          case While(cond, invs, locals, body) => Seq(cond) ++ invs ++ locals ++ Seq(body)
          case If(cond, thn, els) => Seq(cond, thn, els)
          case Label(name, invs) => invs
          case Goto(target) => Nil
          case Fresh(vars) => vars
          case Constraining(vars, body) => vars ++ Seq(body)
        }
      case vd: LocalVarDecl => Nil
      case e: Exp =>
        // Note: If you have to update this pattern match to make it exhaustive, it
        // might also be necessary to update the PrettyPrinter.toParenDoc method.
        e match {
          case _: Literal => Nil
          case _: AbstractLocalVar => Nil
          case FieldAccess(rcv, field) => Seq(rcv)
          case PredicateAccess(params, _) => params
          case Unfolding(acc, body) => Seq(acc, body)
          case UnfoldingGhostOp(acc, body) => Seq(acc, body)
          case FoldingGhostOp(acc, body) => Seq(acc, body)
          case ApplyingGhostOp(wand, in) => Seq(wand, in)
          case PackagingGhostOp(wand, in) => Seq(wand, in)
          case Old(exp) => Seq(exp)
          case CondExp(cond, thn, els) => Seq(cond, thn, els)
          case Let(v, exp, body) => Seq(v, exp, body)
          case Exists(v, exp) => v ++ Seq(exp)
          case Forall(v, triggers, exp) => v ++ triggers ++ Seq(exp)
          case ForPerm(v, fields, exp) => v +: fields :+ exp
          case InhaleExhaleExp(in, ex) => Seq(in, ex)
          case WildcardPerm() => Nil
          case FullPerm() => Nil
          case NoPerm() => Nil
          case EpsilonPerm() => Nil
          case CurrentPerm(loc) => Seq(loc)
          case FractionalPerm(left, right) => Seq(left, right)
          case AccessPredicate(loc, perm) => Seq(loc, perm)
          case BinExp(left, right) => Seq(left, right)
          case UnExp(exp) => Seq(exp)
          case FuncApp(_, args) => args
          case DomainFuncApp(_, args, m) =>
            args ++ m.keys ++ m.values

          case EmptySeq(elemTyp) => Seq(elemTyp)
          case ExplicitSeq(elems) => elems
          case RangeSeq(low, high) => Seq(low, high)
          case SeqAppend(left, right) => Seq(left, right)
          case SeqIndex(seq, idx) => Seq(seq, idx)
          case SeqTake(seq, nn) => Seq(seq, nn)
          case SeqDrop(seq, nn) => Seq(seq, nn)
          case SeqContains(elem, seq) => Seq(elem, seq)
          case SeqUpdate(seq, idx, elem) => Seq(seq, idx, elem)
          case SeqLength(seq) => Seq(seq)

          case EmptySet(elemTyp) => Seq(elemTyp)
          case ExplicitSet(elems) => elems
          case EmptyMultiset(elemTyp) => Seq(elemTyp)
          case ExplicitMultiset(elems) => elems
        }
      case t: Type => Nil
    }
    n match {
      case t: Typed => subnodesWithType ++ Seq(t.typ)
      case _ => subnodesWithType
    }
  }

  /** Returns a pair `(xs, ys)` of sequences, where `xs` are the subnodes
    * returned by [[Nodes.subnodes()]], and where `ys` are all other relevant
    * children of the given `node`.
    *
    * "Relevant" is best described by the following use-case for `children`:
    * Assume a verification in process, and two `FieldAccess` nodes, e.g.,
    * `FieldAccess(e1, f1)` and `FieldAccess(e2, f2)`, where `e1` and `e2` are
    * expressions denoting the receivers, and where `f1` and `f2` denote
    * fields.
    * The task is to compare both nodes for semantic equality, i.e.,
    * they are equal if (i) the values of `e1` and `e2` are semantically
    * equivalent (in some state, say, the current one), and if `f1` and `f2`
    * denote the same field, i.e., if they are equivalent in the sense of
    * object equality.
    * This type of equality can be implemented by using the verifier to decide
    * equality between the subnodes (`xs` from above), and object equality to
    * decide equality between the other relevant children (`ys` from above).
    *
    * In order to support this use-case, `ys` thus neither includes positional
    * information, nor comments or other meta-data.
    *
    * @param node
    * @return
    */
  def children(node: Node): (Seq[Node], Seq[Any]) = {
    /* This match is only exhaustive if all concrete instances of `Node` extend
     * `Product`, which, for example, is the case for case classes.
     *
     * Elements of the productIterator that are `Traversable`s but not `Node`s
     * themselves are effectively flattened (cannot call `t.flatten` directly
     * because of a missing implicit).
     */
    val relevantChildren = (node match {
      case p: Product => p.productIterator.flatMap {
        case t: Traversable[_] if !t.isInstanceOf[Node] => t
        case other => other :: Nil
      }
    }).toSeq

    relevantChildren.partition(_.isInstanceOf[Node])
                    .asInstanceOf[(Seq[Node], Seq[Any])]
  }

  /** Returns all subnodes of `root` that reference (other) Silver member.
    * The returned sequence may contain `root` itself, and it may contain
    * duplicates.
    */
  def memberReferencingNodes(root: Node): Seq[Node] = {
    val collected = mutable.ListBuffer[Node]()

    root visit {
      case n: MethodCall => collected += n
      case n: FuncApp => collected += n
      case n: Unfolding => collected += n
      case n: Unfold => collected += n
      case n: Fold => collected += n
    }

    collected.result()
  }

  /** Returns all members that are referenced from inside `root`.
    * The returned sequence may contain `root` itself, and it it duplicate-free.
    */
  def referencedMembers(root: Node, program: Program): Seq[Member] =
    memberReferencingNodes(root).map {
      case n: MethodCall => program.findMethod(n.methodName)
      case n: FuncApp => program.findFunction(n.funcname)
      case n: Unfolding => program.findPredicate(n.acc.loc.predicateName)
      case n: Unfold => program.findPredicate(n.acc.loc.predicateName)
      case n: Fold => program.findPredicate(n.acc.loc.predicateName)
      case other => sys.error(s"Unexpected node $other")
    }.distinct
}
