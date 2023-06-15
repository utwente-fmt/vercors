package vct.col.failure

import vct.col.ast.{Blame1, BlameVerCors}
import vct.col.origin.Origin

object BlameVerCorsReasons {
  implicit object VerCorsBlameOrigin extends Origin {
    override def preferredName: String = "unknown"
    override def context: String = "At: [generated]"
    override def inlineContext: String = "[generated]"
    override def shortPosition: String = "generated"
  }

  private def blame[G](reason: String): Blame1[G] = Blame1(BlameVerCors(reason), Nil)

  object Structure {
    def AssignLocal[G]: Blame1[G] = blame("Assigning to a local is infallible.")
    def AssignField[G]: Blame1[G] = blame(
      "Dereferences that are the target of an assignment must not fail. " +
      "Instead, the assignment itself must fail."
    )
    def Trigger[G]: Blame1[G] = blame("Expressions in a trigger are not evaluated, and thus infallible.")
  }

  object Contract {
    def TrueIsSatisfiable[G]: Blame1[G] = blame("The constant `true` is always satisfiable.")
  }

  object Applicable {
    def PreIsTrue[G]: Blame1[G] = blame("The precondition `true` always holds.")
    def PostIsTrue[G]: Blame1[G] = blame("The postcondition `true` always holds.")
    def PostOfAbstract[G]: Blame1[G] = blame("The postcondition of an abstract applicable is not checked.")
  }

  object WellFormedness {
    def OptNotEmpty[G]: Blame1[G] = blame("The path condition ensures this option cannot be empty.")
    def EitherNotLeft[G]: Blame1[G] = blame("The path condition ensures this either cannot be left.")
    def EitherNotRight[G]: Blame1[G] = blame("The path condition ensures this either cannot be right.")
    def SeqBound[G]: Blame1[G] = blame("The path condition ensures this subscript cannot be out of bounds.")
    def MapContains[G]: Blame1[G] = blame("The path condition ensures this key is in the map.")
  }
}
