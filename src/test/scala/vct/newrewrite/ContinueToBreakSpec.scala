package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import vct.col.ast._
import vct.col.newrewrite.exc.ContinueToBreak
import vct.col.origin._
import vct.helper.ColHelper

class ContinueToBreakSpec extends AnyFlatSpec with should.Matchers {
  implicit val o: Origin = DiagnosticOrigin

  it should "replace a labeled continue with the proper labeled break" in {
    val loopLabel = new LabelDecl()
    val before = {
      Label(loopLabel,
        Loop(
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Nil),
          LoopInvariant(Constant.BooleanValue(true)),
          Block(Seq(
            Continue(Some(loopLabel.ref))
          ))
        )
      )
    }

    val after = {
      val loopLabel = new LabelDecl()
      val continueLoopLabel = new LabelDecl()
      Label(loopLabel,
        Loop(
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Nil),
          LoopInvariant(Constant.BooleanValue(true)),
          Label(continueLoopLabel,
            Block(Seq(
              Break(Some(continueLoopLabel.ref))
            ))
          )
        )
      )
    }

    ColHelper.assertEquals(ContinueToBreak().dispatch(before), after)
  }

  it should "only wrap the other loop when only continuing from the outer loop" in {
    val before = {
      val innerLoop = new LabelDecl()
      val outerLoop = new LabelDecl

      Label(outerLoop,
        Loop(
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Nil),
          LoopInvariant(Constant.BooleanValue(true)),
          Label(innerLoop,
            Loop(
              Block(Nil),
              Constant.BooleanValue(true),
              Block(Nil),
              LoopInvariant(Constant.BooleanValue(true)),
              Block(Seq(
                Continue(Some(outerLoop.ref))
              ))
            )
          )
        )
      )
    }

    val after = {
      val innerLoop = new LabelDecl()
      val outerLoop = new LabelDecl
      val continueOuterLoop = new LabelDecl

      Label(outerLoop,
        Loop(
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Nil),
          LoopInvariant(Constant.BooleanValue(true)),
          Label(continueOuterLoop,
            Label(innerLoop,
              Loop(
                Block(Nil),
                Constant.BooleanValue(true),
                Block(Nil),
                LoopInvariant(Constant.BooleanValue(true)),
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
