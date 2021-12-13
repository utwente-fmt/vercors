package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import vct.col.ast._
import vct.col.newrewrite.exc.ContinueToBreak
import vct.col.origin._
import vct.col.rewrite.InitialGeneration
import vct.helper.ColHelper

class ContinueToBreakSpec extends AnyFlatSpec with should.Matchers {
  type G = InitialGeneration
  implicit val o: Origin = DiagnosticOrigin

  it should "replace a labeled continue with the proper labeled break" in {
    val loopLabel = new LabelDecl[G]()
    val before = {
      Label(loopLabel,
        Loop(
          Block(Nil),
          BooleanValue(true),
          Block(Nil),
          LoopInvariant(BooleanValue(true)),
          Block(Seq(
            Continue[G](Some(loopLabel.ref))
          ))
        )
      )
    }

    val after = {
      val loopLabel = new LabelDecl[G]()
      val continueLoopLabel = new LabelDecl[G]()
      Label(loopLabel,
        Loop(
          Block(Nil),
          BooleanValue(true),
          Block(Nil),
          LoopInvariant(BooleanValue(true)),
          Label(continueLoopLabel,
            Block(Seq(
              Break[G](Some(continueLoopLabel.ref))
            ))
          )
        )
      )
    }

    ColHelper.assertEquals(ContinueToBreak().dispatch(before), after)
  }

  it should "only wrap the other loop when only continuing from the outer loop" in {
    val before = {
      val innerLoop = new LabelDecl[G]()
      val outerLoop = new LabelDecl[G]()

      Label(outerLoop,
        Loop(
          Block(Nil),
          BooleanValue(true),
          Block(Nil),
          LoopInvariant(BooleanValue(true)),
          Label(innerLoop,
            Loop(
              Block(Nil),
              BooleanValue(true),
              Block(Nil),
              LoopInvariant(BooleanValue(true)),
              Block(Seq(
                Continue(Some(outerLoop.ref))
              ))
            )
          )
        )
      )
    }

    val after = {
      val innerLoop = new LabelDecl[G]()
      val outerLoop = new LabelDecl[G]()
      val continueOuterLoop = new LabelDecl[G]()

      Label(outerLoop,
        Loop(
          Block(Nil),
          BooleanValue(true),
          Block(Nil),
          LoopInvariant(BooleanValue(true)),
          Label(continueOuterLoop,
            Label(innerLoop,
              Loop(
                Block(Nil),
                BooleanValue(true),
                Block(Nil),
                LoopInvariant(BooleanValue(true)),
                Block(Seq(
                  Break(Some(continueOuterLoop.ref))
                ))
              )
            )
          )
        )
      )
    }

    ColHelper.assertEquals(ContinueToBreak().dispatch(before), after)
  }
}
