package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import vct.col.newrewrite.exc.SpecifyImplicitLabels
import vct.col.origin._
import vct.helper.ColHelper

class SpecifyImplicitLabelsSpec extends AnyFlatSpec with Matchers {
  implicit val o: Origin = DiagnosticOrigin

  it should "add a label to a switch without a label" in {
    val before = {
      Switch(
        Constant.IntegerValue(5),
        Block(Seq(
          Case(Constant.IntegerValue(0)),
          Break(None),
          Case(Constant.IntegerValue(5)),
          Break(None),
        ))
      )
    }

    val after = {
      val switchLabel = new LabelDecl()
      Label(switchLabel,
        Switch(
          Constant.IntegerValue(5),
          Block(Seq(
            Case(Constant.IntegerValue(0)),
            Break(Some(switchLabel.ref)),
            Case(Constant.IntegerValue(5)),
            Break(Some(switchLabel.ref)),
          ))
        )
      )
    }

    val rewriter = SpecifyImplicitLabels()
    ColHelper.assertEquals(rewriter.dispatch(before), after)
    assert(rewriter.labelStack.isEmpty)
  }

  it should "reuse labels already present" in {
    val before = {
      val switchLabel = new LabelDecl()
      Label(switchLabel,
        Switch(
          Constant.IntegerValue(5),
          Block(Seq(
            Case(Constant.IntegerValue(0)),
            Break(None),
            Case(Constant.IntegerValue(5)),
            Break(None),
          ))
        )
      )
    }

    val after = {
      val switchLabel = new LabelDecl()
      Label(switchLabel,
        Switch(
          Constant.IntegerValue(5),
          Block(Seq(
            Case(Constant.IntegerValue(0)),
            Break(Some(switchLabel.ref)),
            Case(Constant.IntegerValue(5)),
            Break(Some(switchLabel.ref)),
          ))
        )
      )
    }

    val rewriter = SpecifyImplicitLabels()
    ColHelper.assertEquals(rewriter.dispatch(before), after)
    assert(rewriter.labelStack.isEmpty)
  }

  it should "not touch labeled break" in {
    val before = {
      val switchLabel = new LabelDecl()
      Label(switchLabel,
        Switch(
          Constant.IntegerValue(5),
          Block(Seq(
            Case(Constant.IntegerValue(0)),
            Break(Some(switchLabel.ref)),
          ))
        )
      )
    }

    val after = {
      val switchLabel = new LabelDecl()
      Label(switchLabel,
        Switch(
          Constant.IntegerValue(5),
          Block(Seq(
            Case(Constant.IntegerValue(0)),
            Break(Some(switchLabel.ref)),
          ))
        )
      )
    }

    val rewriter = SpecifyImplicitLabels()
    ColHelper.assertEquals(rewriter.dispatch(before), after)
    assert(rewriter.labelStack.isEmpty)
  }

  it should "use the nearest label possible" in {
    val before = {
      val switchLabelA = new LabelDecl()
      Label(switchLabelA,
        Switch(
          Constant.IntegerValue(33),
          Switch(
            Constant.IntegerValue(5),
            Block(Seq(
              Case(Constant.IntegerValue(0)),
              Break(Some(switchLabelA.ref)),
              Case(Constant.IntegerValue(5)),
              Break(None),
            ))
          )
        )
      )
    }

    val after = {
      val switchLabelA = new LabelDecl()
      val switchLabelB = new LabelDecl()
      Label(switchLabelA,
        Switch(Constant.IntegerValue(33),
          Label(switchLabelB,
            Switch(Constant.IntegerValue(5),
              Block(Seq(
                Case(Constant.IntegerValue(0)),
                Break(Some(switchLabelA.ref)),
                Case(Constant.IntegerValue(5)),
                Break(Some(switchLabelB.ref))
              ))
            )
          )
        )
      )
    }

    val rewriter = SpecifyImplicitLabels()
    ColHelper.assertEquals(rewriter.dispatch(before), after)
    assert(rewriter.labelStack.isEmpty)
  }
}