package vct.rewrite.runtime.util

import hre.util.ScopedStack
import vct.col.ast._
import vct.col.rewrite.{Generation, Rewriter, Rewritten}
import vct.col.util.SuccessionMap
import vct.col.origin.Origin

import scala.collection.mutable


object NewVariableGenerator{

  def newGenerator[G <: Generation] : NewVariableGenerator[G] = {
    new NewVariableGenerator[G]()
  }
}



class NewVariableGenerator[Pre <: Generation](val rewriter: Rewriter[Pre] = new Rewriter[Pre]) {
  type Post = Rewritten[Pre]

  val newVariables: ScopedStack[SuccessionMap[Declaration[_], Variable[Post]]] = new ScopedStack();
  val inputs: ScopedStack[mutable.ArrayBuffer[Declaration[_]]] = new ScopedStack()
  val outputs: ScopedStack[mutable.ArrayBuffer[Variable[Post]]] = new ScopedStack()

  def get(v: Variable[Pre]): Option[Variable[Post]] = {
    newVariables.top.get(v)
  }

  private def create(d: Declaration[_], t: Type[Post]) : Variable[Post] = {
    inputs.top.addOne(d)
    val newOrigin = d.o.replacePrefName(d.o.getPreferredNameOrElse())
    val res = new Variable[Post](t)(newOrigin)
    outputs.top.addOne(res)
    res
  }

  private def create(v: Variable[Pre]): Variable[Post] = {
    create(v, rewriter.dispatch(v.t))
  }

  def getOrCreate(v: Variable[Pre]): Variable[Post] = {
    newVariables.top.getOrElseUpdate(v, create(v))
  }

  def createNew(v: Variable[Pre]): Variable[Post] = {
    val newVar = create(v)
    newVariables.top.update(v, newVar)
    newVar
  }

  def createNewFromInstanceField(instanceField: InstanceField[Post]): Variable[Post] = {
    create(instanceField, instanceField.t)
  }

  def collect[R](f: => R): NewVariableResult[Pre, R] = {
    newVariables.having(new SuccessionMap()) {
      inputs.having(new mutable.ArrayBuffer()) {
        outputs.having(new mutable.ArrayBuffer()) {
          val res: R = f
          NewVariableResult(inputs.top.toSeq, newVariables.top, outputs.top.toSeq, res)
        }
      }
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

  def freezeOption() : Option[NewVariableResult[Pre, _]] = {
    Option.when(inputs.nonEmpty && outputs.nonEmpty && newVariables.nonEmpty){NewVariableResult(inputs.top.toSeq, newVariables.top, outputs.top.toSeq, null)}
  }

  def freeze() : NewVariableResult[Pre, _] = {
    freezeOption().getOrElse(NewVariableResult(Seq.empty, new SuccessionMap[Declaration[_], Variable[Post]](), Seq.empty, null))
  }

  def nonEmpty : Boolean = newVariables.nonEmpty

}


case class NewVariableResult[Pre <: Generation, R](inputs: Seq[Declaration[_]], mapping: SuccessionMap[Declaration[_], Variable[Rewritten[Pre]]], outputs: Seq[Variable[Rewritten[Pre]]], result: R)