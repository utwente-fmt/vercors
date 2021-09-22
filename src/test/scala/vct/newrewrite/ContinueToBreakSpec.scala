package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import vct.col.ast._
import vct.col.newrewrite.ContinueToBreak
import vct.helper.ColHelper

class ContinueToBreakSpec extends AnyFlatSpec with should.Matchers {
  implicit val o = DiagnosticOrigin

  it should "replace a labeled continue with the proper labeled break" in {
    val loopLabel = new LabelDecl()
    val before = {
      Block(Seq(
        Label(loopLabel),
        Loop(
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Seq(
            Continue(Some(loopLabel.ref))
          ))
        )
      ))
    }

    val after = {
      val loopLabel = new LabelDecl()
      val continueLoopLabel = new LabelDecl()
      Block(Seq(
        Label(loopLabel),
        Loop(
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Seq(
            Label(continueLoopLabel),
            Block(Seq(
              Break(Some(continueLoopLabel.ref))
            ))
          ))
        )
      ))
    }

    ColHelper.assertEquals(ContinueToBreak().dispatch(before), after)
  }

  it should "only wrap the other loop when only continuing from the outer loop" in {
    val before = {
      val innerLoop = new LabelDecl()
      val outerLoop = new LabelDecl

      Block(Seq(
        Label(outerLoop),
        Loop(
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Seq(
            Label(innerLoop),
            Loop(
              Block(Nil),
              Constant.BooleanValue(true),
              Block(Nil),
              Constant.BooleanValue(true),
              Block(Seq(
                Continue(Some(outerLoop.ref))
              ))
            )
          ))
        )
      ))
    }

    val after = {
      val innerLoop = new LabelDecl()
      val outerLoop = new LabelDecl
      val continueOuterLoop = new LabelDecl

      Block(Seq(
        Label(outerLoop),
        Loop(
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Nil),
          Constant.BooleanValue(true),
          Block(Seq(
            Label(continueOuterLoop),
            Block(Seq(
              Label(innerLoop),
              Loop(
                Block(Nil),
                Constant.BooleanValue(true),
                Block(Nil),
                Constant.BooleanValue(true),
                Block(Seq(
                  Break(Some(continueOuterLoop.ref))
                ))
              )
            ))
          ))
        )
      ))
    }

    ColHelper.assertEquals(ContinueToBreak().dispatch(before), after)
  }
}
