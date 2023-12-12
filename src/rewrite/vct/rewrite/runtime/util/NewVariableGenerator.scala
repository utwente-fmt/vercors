package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast.{Local, Variable}
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.origin.Origin





class NewVariableGenerator[Pre <: Generation](val rewriter: Rewriter[Pre] = new Rewriter[Pre]) {
  type Post = Rewritten[Pre]

  val newVariables: ScopedStack[SuccessionMap[Variable[Pre], Variable[Post]]] = new ScopedStack();

  def get(v: Variable[Pre]): Option[Variable[Post]] = {
    newVariables.top.get(v)
  }

  private def create(v: Variable[Pre]): Variable[Post] = {
    val newOrigin = Origin(Seq.empty).addPrefName(v.o.getPreferredNameOrElse())
    new Variable[Post](rewriter.dispatch(v.t))(newOrigin)
  }

  def getOrCreate(v: Variable[Pre]): Unit = {
    newVariables.top.getOrElseUpdate(v, create(v))
  }

  def createNew(v: Variable[Pre]): Variable[Post] = {
    val newVar = create(v)
    newVariables.top.update(v, newVar)
    newVar
  }

  def collect[R](f: => R): (R, SuccessionMap[Variable[Pre], Variable[Post]]) = {
    newVariables.having(new SuccessionMap()) {
      (f, newVariables.top)
    }
  }

  def getLocal(local: Local[Pre]): Local[Post] = {
    newVariables.top.get(local.ref.decl) match {
      case Some(v) => Local[Post](v.ref)(v.o)
      case None => {
        createNew(local.ref.decl)
        Local[Post](newVariables.top.ref(local.ref.decl))(local.o)
      }
    }
  }

  def getLocal(v: Variable[Pre]): Local[Post] = {
    Local[Post](newVariables.top.ref(v))(v.o)
  }

  def getLocal(v: Variable[Pre], o: Origin): Local[Post] = {
    Local[Post](newVariables.top.ref(v))(o)
  }

  def nonEmpty(): Boolean = {
    newVariables.nonEmpty
  }

  def prevOrEmpty(): SuccessionMap[Variable[Pre], Variable[Post]] = {
    try {
      val tmp = newVariables.pop()
      val prev = newVariables.top
      newVariables.push(tmp)
      prev
    } catch {
      case _: Throwable => new SuccessionMap()
    }
  }
}
