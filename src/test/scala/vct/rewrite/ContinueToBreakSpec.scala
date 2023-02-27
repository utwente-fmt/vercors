package vct.rewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import vct.col.ast._
import vct.col.rewrite.exc.ContinueToBreak
import vct.col.origin._
import vct.col.rewrite.InitialGeneration
import vct.helper.ColHelper

class ContinueToBreakSpec extends AnyFlatSpec with should.Matchers {
  type G = InitialGeneration
  implicit val o: Origin = DiagnosticOrigin
  private val blame = PanicBlame("empty loop invariant")

  it should "replace a labeled continue with the proper labeled break" in {
    val loopLabel = new LabelDecl[G]()
    val before = {
      Label(loopLabel,
        Loop(
          Block(Nil),
          BooleanValue(true),
          Block(Nil),
          LoopInvariant(BooleanValue(true), None)(blame),
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
          LoopInvariant(BooleanValue(true), None)(blame),
          Label(continueLoopLabel,
            Block(Seq(
              Break[G](Some(continueLoopLabel.ref))
            ))
          )
        )
      )
    }

    val rw = ContinueToBreak[G]()
    ColHelper.assertEquals(rw.labelDecls.scope { rw.dispatch(before) }, after)
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
          LoopInvariant(BooleanValue(true), None)(blame),
          Label(innerLoop,
            Loop(
              Block(Nil),
              BooleanValue(true),
              Block(Nil),
              LoopInvariant(BooleanValue(true), None)(blame),
              Block(Seq(
                Continue(Some(outerLoop.ref))
              ))
            )(o)
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
          LoopInvariant(BooleanValue(true), None)(blame),
          Label(continueOuterLoop,
            Label(innerLoop,
              Loop(
                Block(Nil),
                BooleanValue(true),
                Block(Nil),
                LoopInvariant(BooleanValue(true), None)(blame),
                Block(Seq(
                  Break(Some(continueOuterLoop.ref))
                ))
              )(o)
            )
          )
        )
      )
    }

    val rw = ContinueToBreak[G]()
    ColHelper.assertEquals(rw.labelDecls.scope { rw.dispatch(before) }, after)
  }
}
