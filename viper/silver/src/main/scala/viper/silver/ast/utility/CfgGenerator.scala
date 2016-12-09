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

import collection.mutable.ListBuffer
import viper.silver.ast._

/**
 * An object that can take an AST and translate it into the corresponding CFG.
 *
 * The generation proceeds in four phases:
 * 1. From the AST, a list of 'extended nodes' is generated.  These extended nodes correspond to
 * statements (but no labels or gotos), and some additional instructions to model control flow (jumps
 * or conditional jumps).
 * 2. The list of extended nodes is turned into a temporary CFG (TmpBlock).  This representation has various
 * mutable fields to make generation easier.
 * 3. Some degenerated cases are cleaned up (e.g., empty basic blocks).
 * 4. The temporary CFG is converted to the final CFG.
 */
object CfgGenerator {

  /**
   * Returns a control flow graph that corresponds to a statement.
   */
  def toCFG(ast: Stmt): Block = {
    val p1 = new Phase1(ast)
    val p2 = new Phase2(p1)
    val p3 = new Phase3(p2)
    val p4 = new Phase4(p3)
    p4.result
  }

  // temporary CFG blocks (mutable to allow incremental creation)
  trait TmpBlock {
    var pred: ListBuffer[TmpBlock] = ListBuffer()

    def succs: Seq[TmpBlock]

    def +=(e: TmpEdge) {
      e.dest.pred += this
    }
  }
  class VarBlock extends TmpBlock {
    var stmts: ListBuffer[Stmt] = ListBuffer()
    var edges: ListBuffer[TmpEdge] = ListBuffer()
    override def +=(e: TmpEdge) {
      super.+=(e)
      edges += e
    }
    def stmt = if (stmts.size == 1) stmts.head else Seqn(stmts)(NoPosition)
    override def toString = s"VarBlock(${stmts.mkString(";\n  ")}, ${edges.mkString("[", ",", "]")})"
    def succs = (edges map (_.dest)).toSeq
  }
  class TmpLoopBlock(val loop: While, private var _body: TmpBlock) extends TmpBlock {
    body.pred += this
    def body = _body
    var edge: TmpEdge = null
    override def +=(e: TmpEdge) {
      super.+=(e)
      require(edge == null)
      edge = e
    }
    def body_=(b: TmpBlock) {
      _body = b
      b.pred += this
    }
    override def toString = s"TmpLoopBlock(${loop.cond}, $edge)"
    def succs = if (edge == null) Nil else List(edge.dest)
  }
  class TmpConstrainingBlock(val constraining: Constraining, private var _body: TmpBlock) extends TmpBlock {
    body.pred += this
    def body = _body
    var edge: TmpEdge = null
    override def +=(e: TmpEdge) {
      super.+=(e)
      require(edge == null)
      edge = e
    }
    def body_=(b: TmpBlock) {
      _body = b
      b.pred += this
    }
    override def toString = s"TmpConstrainingBlock(${constraining.vars}, $edge)"
    def succs = if (edge == null) Nil else List(edge.dest)
  }
  trait TmpEdge {
    def dest: TmpBlock
  }
  case class CondEdge(dest: TmpBlock, cond: Exp) extends TmpEdge
  case class UncondEdge(dest: TmpBlock) extends TmpEdge

  /**
   * Performs a breadth-first search over a temporary control flow graph.
   */
  def bfs(block: TmpBlock)(f: TmpBlock => Unit) {
    val worklist = collection.mutable.Queue[TmpBlock]()
    worklist.enqueue(block)
    val visited = collection.mutable.Set[TmpBlock]()

    while (worklist.nonEmpty) {
      val tb = worklist.dequeue()
      val succsAndBody = tb.succs ++ (tb match {
        case l: TmpLoopBlock => Seq(l.body)
        case tmpCB: TmpConstrainingBlock => Seq(tmpCB.body)
        case _ => Nil
      })
      worklist.enqueue(succsAndBody filterNot visited.contains : _*)
      visited ++= succsAndBody
      f(tb)
    }
  }

  /**
   * A label that is the target of a jump (the name identifies the target, and we
   * use a special format for labels with generated names).
   */
  case class Lbl(name: String)
  object Lbl {
    var id = 0

    def apply(name: String, generated: Boolean) = {
      id += 1
      new Lbl("$$" + name + "_" + id)
    }
  }

  /** Extended statements are used during the translation of an AST to a CFG. */
  sealed trait ExtendedStmt {
    var isLeader = false
  }
  case class RegularStmt(stmt: Stmt) extends ExtendedStmt
  case class Jump(lbl: Lbl) extends ExtendedStmt
  case class CondJump(thn: Lbl, els: Lbl, cond: Exp) extends ExtendedStmt
  case class Loop(after: Lbl, loop: While) extends ExtendedStmt
  case class ConstrainingES(after: Lbl, constraining: Constraining) extends ExtendedStmt
  case class EmptyStmt() extends ExtendedStmt

  /**
   * Convert the temporary CFG to the final one.
   */
  class Phase4(p3: Phase3) {
    val b2b: collection.mutable.HashMap[TmpBlock, Block] = collection.mutable.HashMap()

    def result = b2b(p3.start)

    // create a Block for every TmpBlock
    bfs(p3.start) {
      tb =>
        var b: Block = null
        tb match {
          case loop: TmpLoopBlock =>
            val lb = LoopBlock(null, loop.loop.cond, loop.loop.invs, loop.loop.locals, null)(loop.loop.pos, loop.loop.info)
            lb.srcAst = Some(loop.loop)
            b = lb
          case tmpCB: TmpConstrainingBlock =>
            b = ConstrainingBlock(tmpCB.constraining.vars, null, null)
          case vb: VarBlock if vb.edges.isEmpty => /* length 0 */
            b = TerminalBlock(vb.stmt)
          case vb: VarBlock if vb.edges.tail.isEmpty => /* length 1 */
            b = NormalBlock(vb.stmt, null)
          case vb: VarBlock if vb.edges.tail.tail.isEmpty => /* length 2 */
            b = ConditionalBlock(vb.stmt, vb.edges.head.asInstanceOf[CondEdge].cond, null, null)
          case _ =>
            sys.error("unexpected block")
        }
        b2b += tb -> b
    }

    // add all edges between blocks
    bfs(p3.start) {
      tb =>
        val b: Block = b2b(tb)
        tb match {
          case loop: TmpLoopBlock =>
            val lb = b.asInstanceOf[LoopBlock]
            lb.body = b2b(loop.body)
            lb.succ = b2b(loop.edge.dest)
          case tmpCB: TmpConstrainingBlock =>
            val cb = b.asInstanceOf[ConstrainingBlock]
            cb.body = b2b(tmpCB.body)
            cb.succ = b2b(tmpCB.edge.dest)
          case vb: VarBlock if vb.edges.isEmpty => /* length 0 - nothing to do, no successors */
          case vb: VarBlock if vb.edges.tail.isEmpty => /* length 1 */
            b.asInstanceOf[NormalBlock].succ = b2b(vb.edges.head.dest)
          case vb: VarBlock if vb.edges.tail.tail.isEmpty => /* length 2 */
            val cb = b.asInstanceOf[ConditionalBlock]
            cb.thn = b2b(vb.edges.head.dest)
            cb.els = b2b(vb.edges(1).dest)
          case _ =>
            sys.error("unexpected block")
        }
    }
  }

  /**
   * Clean up degenerated cases in the CFG:
   * - empty blocks
   */
  class Phase3(p2: Phase2) {
    var start = p2.start

    // find and remove empty blocks
    bfs(start) {
      case vb: VarBlock if vb.stmt.children.isEmpty =>
        vb.edges match {
          case ListBuffer(UncondEdge(succ)) =>
            // go through all predecessors and relink them to 'succ', and update the predecessors of 'succ'
            succ.pred -= vb
            for (pred <- vb.pred) {
              succ.pred += pred
              pred match {
                case loop: TmpLoopBlock =>
                  if (loop.edge.dest == vb) {
                    loop.edge = UncondEdge(succ)
                  } else {
                    loop.body = succ
                  }
                case tmpCB: TmpConstrainingBlock =>
                  if (tmpCB.edge.dest == vb) {
                    tmpCB.edge = UncondEdge(succ)
                  } else {
                    tmpCB.body = succ
                  }
                case predVb: VarBlock if predVb.edges.size == 1 =>
                  predVb.edges(0) = UncondEdge(succ)
                case predVb: VarBlock if predVb.edges.size == 2 =>
                  if (predVb.edges.head.dest == vb) {
                    predVb.edges(0) = CondEdge(succ, predVb.edges.head.asInstanceOf[CondEdge].cond)
                  } else {
                    predVb.edges(1) = CondEdge(succ, predVb.edges(1).asInstanceOf[CondEdge].cond)
                  }
                case _ =>
                  sys.error("unexpected block")
              }
            }
            if (vb.pred.isEmpty) {
              assert(vb == start)
              start == succ
            }
          case _ =>
        }
      case _ =>
    }
  }

  /**
   * Create a temporary CFG from the node list of phase 1.  The CFG might contain empty blocks.
   */
  class Phase2(p1: Phase1) {
    val start = new VarBlock()
    var cur = start
    var nodes = p1.nodes.toSeq
    val allNodes = nodes
    var offset = 0
    val nodeToBlock: java.util.IdentityHashMap[ExtendedStmt, TmpBlock] = new java.util.IdentityHashMap()

    // assumes that all labels are defined
    def resolveLbl(lbl: Lbl) = allNodes(p1.lblmap.get(lbl).get)
    def lblToIdx(lbl: Lbl) = p1.lblmap.get(lbl).get - offset

    // a list of missing edges: every entry (stmt, f) stands for a missing edge
    // to the block that `stmt` will be part of (stmt must be a leader). the
    // function f sets the edge on the source correctly (whether it is a conditional
    // or unconditional edge).
    val missingEdges: ListBuffer[(ExtendedStmt, (TmpBlock => Unit))] = ListBuffer()

    // run phase 2
    run()
    addMissingEdges()

    /**
     * Goes through the extended nodes in `nodes` and adds them to blocks
     * in the appropriate way.  Forward edges cannot be set yet (because the following
     * block has most likely not been constructed yet), and thus these edges are recorded
     * and added later.
     */
    private def run() {
      var i = 0
      while (i < nodes.size) {
        var b: TmpBlock = null
        val n = nodes(i)
        n match {
          case Jump(lbl) =>
            if (n.isLeader) {
              val newCur = new VarBlock()
              cur += UncondEdge(newCur)
              cur = newCur
              b = cur
            }
            // finish current block and add a missing edge
            val c = cur
            missingEdges += ((resolveLbl(lbl), (t: TmpBlock) => c += UncondEdge(t)))
            cur = new VarBlock()
          case CondJump(thn, els, cond) =>
            if (n.isLeader) {
              val newCur = new VarBlock()
              cur += UncondEdge(newCur)
              cur = newCur
              b = cur
            }
            // finish current block and add two missing edges
            val notCond = Not(cond)(NoPosition)
            val c = cur
            missingEdges += ((resolveLbl(thn), (t: TmpBlock) => c += CondEdge(t, cond)))
            missingEdges += ((resolveLbl(els), (t: TmpBlock) => c += CondEdge(t, notCond)))
            cur = new VarBlock()
          case Loop(after, l) =>
            // handle loop body: start with a new block, and a different
            // set of nodes (the ones in the loop body), but use the same
            // missingEdges
            val oldCur = cur
            val body = new VarBlock()
            cur = body
            val oldNodes = nodes
            val oldOffset = offset
            nodes = nodes.slice(i + 1, lblToIdx(after))
            offset = oldOffset + (i + 1)
            run()
            nodes = oldNodes
            offset = oldOffset
            i = lblToIdx(after) - 1

            // create the loop block
            val loop = new TmpLoopBlock(l, body)
            oldCur += UncondEdge(loop)
            cur = new VarBlock()
            loop += UncondEdge(cur)
            b = loop
          case ConstrainingES(after, constraining) =>
            val oldCur = cur
            val body = new VarBlock()
            cur = body
            val oldNodes = nodes
            val oldOffset = offset
            nodes = nodes.slice(i + 1, lblToIdx(after))
            offset = oldOffset + (i + 1)
            run()
            nodes = oldNodes
            offset = oldOffset
            i = lblToIdx(after) - 1

            // create the frp block
            val tmpCB = new TmpConstrainingBlock(constraining, body)
            oldCur += UncondEdge(tmpCB)
            cur = new VarBlock()
            tmpCB += UncondEdge(cur)
            b = tmpCB
          case EmptyStmt() =>
            // skip it, and only deal with leader stuff
            if (n.isLeader) {
              val newCur = new VarBlock()
              cur += UncondEdge(newCur)
              cur = newCur
              b = cur
            }
          case RegularStmt(s) =>
            if (n.isLeader) {
              val newCur = new VarBlock()
              cur += UncondEdge(newCur)
              cur = newCur
              b = cur
            }
            cur.stmts += s
        }
        i += 1
        if (n.isLeader) {
          assert(b != null)
          nodeToBlock.put(n, b)
        }
      }
    }

    /** Add all the missing edges. */
    private def addMissingEdges() {
      for ((n, f) <- missingEdges) {
        f(nodeToBlock.get(n))
      }
    }
  }

  class Phase1(ast: Stmt) {
    /** The extended statements used in phase one. */
    val nodes: ListBuffer[ExtendedStmt] = ListBuffer()

    /** A mapping of Labels to indices into `nodes`.
      * While `nodes` and `lblmap` are populated, the indices in `lblmap` can
      * point past the end of `nodes`, because they point to a block that
      * will be added to `nodes` by the computation still to come.
      */
    val lblmap: collection.mutable.Map[Lbl, Int] = collection.mutable.Map()

    /** The extended statements that are leaders (indices into `nodes`). */
    val leaders: ListBuffer[Int] = ListBuffer()

    run(ast)
    // in case anything points to the 'next' node.
    nodes += EmptyStmt()
    setLeader()

    /**
     * Translates a statement and adds the appropriate information to `nodes` and `lblmap`. Note that
     * every statement must extend `nodes` by at least one (so that it is OK to point to the next
     * free place in `nodes` before calling `run`).
     */
    private def run(s: Stmt) {
      s match {
        case If(cond, thn, els) =>
          val thnTarget = Lbl("then", generated = true)
          val elsTarget = Lbl("else", generated = true)
          val afterTarget = Lbl("afterIf", generated = true)
          nodes += CondJump(thnTarget, elsTarget, cond)
          lblmap += thnTarget -> nextNode
          run(thn)
          nodes += Jump(afterTarget)
          lblmap += elsTarget -> nextNode
          run(els)
          lblmap += afterTarget -> nextNode
        case w@While(cond, _, locals, body) =>
          val afterLoop = Lbl("afterLoop", generated = true)
          nodes += Loop(afterLoop, w)
          run(body)
          // Allow labels in the loop body to point to the end of the body,
          // for instance the 'afterTarget' label of an if statement.
          nodes += EmptyStmt()
          lblmap += afterLoop -> nextNode
        case constraining @ Constraining(vars, body) =>
          val afterConstraining = Lbl("afterConstraining", generated = true)
          nodes += ConstrainingES(afterConstraining, constraining)
          run(body)
          // Allow labels in the body to target the end of the body.
          nodes += EmptyStmt()
          lblmap += afterConstraining -> nextNode
        case Seqn(ss) =>
          nodes += EmptyStmt()
          for (s <- ss) run(s)
        case Goto(target) =>
          nodes += Jump(Lbl(target))
        case lbl @ Label(name, invs) =>
          lblmap += Lbl(name) -> nextNode
          /* Since labels are also used for labelled old-expressions, they (also) need to be
           * translated analogous to other regular statements.
           */
          nodes += RegularStmt(lbl)
        case _: LocalVarAssign | _: FieldAssign |
             _: Inhale | _: Exhale |
             _: Fold | _: Unfold |
             _: Package | _: Apply |
             _: MethodCall | _: Fresh |
             _: NewStmt | _: Assert =>
          // regular, non-control statements
          nodes += RegularStmt(s)
      }
    }

    /** The index of the next extended node. */
    private def nextNode = {
      leaders += nodes.size
      nodes.size
    }

    private def setLeader() {
      for ((n, i) <- nodes.zipWithIndex) {
        n.isLeader = leaders contains i
      }
    }
  }
}
