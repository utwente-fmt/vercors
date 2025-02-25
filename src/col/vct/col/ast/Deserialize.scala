package vct.col.ast

import vct.col.ast.ops.deserialize.DeserializeProgram
import vct.col.origin.{Blame, Origin, VerificationFailure}

import scala.collection.mutable

object Deserialize {
  def deserializeProgram[G](
      program: vct.col.ast.serialize.Program,
      blameProvider: Origin => Blame[VerificationFailure],
  ): Program[G] = {
    DeserializeProgram.deserialize(program, mutable.HashMap(), blameProvider)
  }
}
