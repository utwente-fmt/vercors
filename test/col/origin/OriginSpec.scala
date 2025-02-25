package origin

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import vct.col.origin.SourceName

class OriginSpec extends AnyFlatSpec {
  behavior of "SourceName.stringToName"

  it should "come up with a reasonable name for any identifier" in {
    val cases = Table(
      ("string", "snake", "camel"),
      ("foo", "foo", "foo"),
      ("foo_bar", "foo_bar", "fooBar"),
      ("foo_bar_baz", "foo_bar_baz", "fooBarBaz"),
      ("montAiguille", "mont_aiguille", "montAiguille"),
      ("GERVANNE", "gervanne", "gervanne"),
      ("VeRnAiSoN", "ve_rn_ai_so_n", "veRnAiSoN"),
      ("_", "_", "_"),
      ("___", "___", "___"),
      ("_a", "a", "a"),
      ("a_", "a", "a"),
      ("a_b_", "a_b", "aB"),
      ("_a_b", "a_b", "aB"),
      ("do__ob", "do_ob", "doOb"),
      ("do____ob", "do_ob", "doOb"),
    )

    forAll(cases) { (string, snake, camel) =>
      val name = SourceName.stringToName(string)
      assert(name.snake == snake)
      assert(name.camel == camel)
    }
  }
}
