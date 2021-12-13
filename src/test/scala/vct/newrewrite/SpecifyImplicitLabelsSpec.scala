package vct.newrewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast._
import vct.col.newrewrite.exc.SpecifyImplicitLabels
import vct.col.origin._
import vct.col.rewrite.InitialGeneration
import vct.helper.ColHelper

class SpecifyImplicitLabelsSpec extends AnyFlatSpec with Matchers {
  type G = InitialGeneration
  implicit val o: Origin = DiagnosticOrigin

  it should "add a label to a switch without a label" in {
    val before = {
      Switch(
        IntegerValue(5),
        Block(Seq[Statement[G]](
          Case(IntegerValue(0)),
          Break(None),
          Case(IntegerValue(5)),
          Break(None),
        ))
      )
    }

    val after = {
      val switchLabel = new LabelDecl[G]()
      Label(switchLabel,
        Switch(
          IntegerValue(5),
          Block(Seq[Statement[G]](
            Case(IntegerValue(0)),
            Break(Some(switchLabel.ref)),
            Case(IntegerValue(5)),
            Break(Some(switchLabel.ref)),
          ))
        )
      )
    }

    val rewriter = SpecifyImplicitLabels[G]()
    ColHelper.assertEquals(rewriter.dispatch(before), after)
    assert(rewriter.labelStack.isEmpty)
  }

  it should "reuse labels already present" in {
    val before = {
      val switchLabel = new LabelDecl[G]()
      Label(switchLabel,
        Switch(
          IntegerValue(5),
          Block(Seq[Statement[G]](
            Case(IntegerValue(0)),
            Break(None),
            Case(IntegerValue(5)),
            Break(None),
          ))
        )
      )
    }

    val after = {
      val switchLabel = new LabelDecl[G]()
      Label(switchLabel,
        Switch(
          IntegerValue(5),
          Block(Seq[Statement[G]](
            Case(IntegerValue(0)),
            Break(Some(switchLabel.ref)),
            Case(IntegerValue(5)),
            Break(Some(switchLabel.ref)),
          ))
        )
      )
    }

    val rewriter = SpecifyImplicitLabels[G]()
    ColHelper.assertEquals(rewriter.dispatch(before), after)
    assert(rewriter.labelStack.isEmpty)
  }

  it should "not touch labeled break" in {
    val before = {
      val switchLabel = new LabelDecl[G]()
      Label(switchLabel,
        Switch(
          IntegerValue(5),
          Block(Seq[Statement[G]](
            Case(IntegerValue(0)),
            Break(Some(switchLabel.ref)),
          ))
        )
      )
    }

    val after = {
      val switchLabel = new LabelDecl[G]()
      Label(switchLabel,
        Switch(
          IntegerValue(5),
          Block(Seq[Statement[G]](
            Case(IntegerValue(0)),
            Break(Some(switchLabel.ref)),
          ))
        )
      )
    }

    val rewriter = SpecifyImplicitLabels[G]()
    ColHelper.assertEquals(rewriter.dispatch(before), after)
    assert(rewriter.labelStack.isEmpty)
  }

  it should "use the nearest label possible" in {
    val before = {
      val switchLabelA = new LabelDecl[G]()
      Label(switchLabelA,
        Switch(
          IntegerValue(33),
          Switch(
            IntegerValue(5),
            Block(Seq[Statement[G]](
              Case(IntegerValue(0)),
              Break(Some(switchLabelA.ref)),
              Case(IntegerValue(5)),
              Break(None),
            ))
          )
        )
      )
    }

    val after = {
      val switchLabelA = new LabelDecl[G]()
      val switchLabelB = new LabelDecl[G]()
      Label(switchLabelA,
        Switch(IntegerValue(33),
          Label(switchLabelB,
            Switch(IntegerValue(5),
              Block(Seq[Statement[G]](
                Case(IntegerValue(0)),
                Break(Some(switchLabelA.ref)),
                Case(IntegerValue(5)),
                Break(Some(switchLabelB.ref))
              ))
            )
          )
        )
      )
    }

    val rewriter = SpecifyImplicitLabels[G]()
    ColHelper.assertEquals(rewriter.dispatch(before), after)
    assert(rewriter.labelStack.isEmpty)
  }
}