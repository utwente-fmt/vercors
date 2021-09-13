package vct.newrewrite

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers._
import vct.col.ast.{DiagnosticOrigin, LabelDecl}

class ContinueToBreakSpec extends AnyFlatSpec with should.Matchers {
  implicit val o = DiagnosticOrigin

  "labeldecls" should "be structurally equal" in {
    val l1 = new LabelDecl()
    val l2 = new LabelDecl()
    l1 should be (l2)
  }

//  it should "convert continue into break" {
//    val labelDecl = new LabelDecl
//    val pIn = Block(Seq(
//      Label(labelDecl),
//      Loop(Block(Nil), true, Block(Nil), true, Block(Seq(Continue(Some(labelDecl.ref)))))
//    ))
//
//    pIn should be pOut
//  }

}
