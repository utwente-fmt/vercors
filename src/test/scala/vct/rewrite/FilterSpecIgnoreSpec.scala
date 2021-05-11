package vct.rewrite

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.col.ast.{ClassDeclaration, Program}
import vct.col.newrewrite.FilterSpecIgnore

class FilterSpecIgnoreSpec extends AnyFlatSpec with Matchers {

  //Iets van een rewrite gebruiken om alle nodes langs te gaan.

  "2" should "be equal to 2" in {
    var i = 2;
    assert(i==2);
  }

  it should "not change anything given tree without filterSpecIgnore" in {
    var implicit origin =
    var classNode = new vct.col.ast.Class(Seq());
    var tree = new Program({classNode});

    var rewriter = new FilterSpecIgnore();
    var rewriteTree = rewriter.

    assert(i==2);
  }

}
