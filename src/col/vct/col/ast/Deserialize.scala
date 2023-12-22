package vct.col.ast

import vct.col.ast.ops.deserialize.DeserializeProgram

import scala.collection.mutable

object Deserialize {
  def deserializeProgram[G](program: vct.col.ast.serialize.Program, y: scala.Any): Program[G] = {
    DeserializeProgram.deserialize(program, mutable.HashMap())
  }
}
