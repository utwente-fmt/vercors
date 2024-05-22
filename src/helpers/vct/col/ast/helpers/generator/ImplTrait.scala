package vct.col.ast.helpers.generator

import java.nio.file.{Files, Path}
import vct.col.ast.structure.ImplTraitGenerator

import scala.util.Using
import scala.util.matching.Regex

class ImplTrait extends ImplTraitGenerator {
  val w = s"(?:\\s*?)"

  // Token parts
  val letter = s"(?:[a-zA-Z])"
  val digit = s"(?:[0-9])"
  val letter_digit = s"(?:$letter|$digit)"

  // Tokens: must allow surrounding whitespace
  val id = s"(?:$w$letter$letter_digit*$w)"
  val period = s"(?:$w\\.$w)"
  val comma = s"(?:$w,$w)"
  val underscore = s"(?:${w}_$w)"
  val acc_open = s"(?:$w\\{$w)"
  val acc_close = s"(?:$w\\}$w)"
  val brack_open = s"(?:$w\\[$w)"
  val brack_close = s"(?:$w\\]$w)"
  val kwd_import = s"(?:${w}import$w)"
  val kwd_trait = s"(?:${w}trait$w)"
  val kwd_extends = s"(?:${w}extends$w)"
  val kwd_with = s"(?:${w}with$w)"
  val kwd_gen = s"(?:${w}G$w)"

  // Parsing rules
  val stableId = s"(?:$id(?:$period$id)*)"
  val selectorRule = s"(?:$underscore|$id)"
  val selectors = s"(?:$selectorRule|$acc_open$selectorRule(?:$comma$selectorRule)*$acc_close)"
  val importExprRule = s"(?:$stableId$period$selectors)"
  val importRule = s"(?:$kwd_import($importExprRule(?:$comma$importExprRule)*))"

  val lazyTyp = s"(?:$w[^\\s]+$w)"
  val traitParents = s"(?:$lazyTyp($kwd_with$lazyTyp)*)"
  val traitRule = s"(?:$kwd_trait$id$brack_open$kwd_gen$brack_close(($kwd_extends$traitParents)?$acc_open))"

  val `import`: Regex = importRule.r
  val `trait`: Regex = traitRule.r
  val importExpr: Regex = importExprRule.r
  val selector: Regex = selectorRule.r

  /**
   * This method is fragile: we attempt to do some useful fixing up by applying
   * a regex to find import lines and the declaration line. If the file becomes
   * invalid or we make wrong conclusions that's ok: the user can fix it. The
   * method won't do anything if the string {NodeName}Ops already occurs.
   *
   * I think this method should be removed once all forks/branches are in line
   * with extending the correct Ops traits, and only leave generate (which
   * should always be correct).
   */
  def fix(p: Path, opsNames: Seq[String]): Unit = {
    if(opsNames.isEmpty)
      return // obsolete or renamed impl, don't try anything.

    val data = Files.readString(p)

    if(opsNames.forall(name => data.contains(name)))
      return // exit early if most likely the file is already fixed

    val maybeTraitMatch = `trait`.findFirstMatchIn(data)

    if(maybeTraitMatch.isEmpty)
      return // silently fail if we can't detect a trait

    val traitMatch = maybeTraitMatch.get

    val exts: Seq[String] =
      Option(traitMatch.group(2))
        .map(
          _.strip()
            .split("\\s+")
            .to(List)
            .map(_.strip())
            .filterNot(_ == "extends")
            .filterNot(_ == "with")
        )
        .getOrElse(Nil)

    val opsExts = opsNames.map(_ + "[G]")

    if(opsExts.forall(ext => exts.contains(ext)))
      return

    val allImportLines = `import`.findAllMatchIn(data).toSeq

    if(traitMatch.start < allImportLines.lastOption.map(_.end).getOrElse(-1))
      return // the trait is defined before the last import: give up

    // Decide whether the ops thing is already included. Might happen if you
    // accidentally delete the with NodeOps somehow
    var mkImport = true

    allImportLines.foreach { line =>
      importExpr.findAllIn(line.group(1)).foreach { imp =>
        val parts = imp.split('.').toSeq.map(_.strip())
        val pkg = parts.init
        val select = selector.findAllIn(parts.last).map(_.strip()).toSeq

        if(pkg == Seq("vct", "col", "ast", "ops") && (select.contains("_") || opsNames.forall(name => select.contains(name)))) {
          mkImport = false
        }
      }
    }

    val outExts = exts ++ opsExts.filterNot(ext => exts.contains(ext))
    val outImport =
      if(mkImport) {
        val selector = if(opsNames.size == 1) opsNames.head else opsNames.mkString("{", ", ", "}")
        s"import vct.col.ast.ops.$selector"
      } else ""

    // Time to be careful: re-output the entire file except bits we for sure
    // can do ourselves

    val out = new StringBuilder()

    if (allImportLines.isEmpty) {
      out.append(data.subSequence(0, traitMatch.start))
      if (outImport.nonEmpty) {
        out.append(outImport)
        out.append("\n")
        out.append("\n")
      }
      out.append(data.subSequence(traitMatch.start, traitMatch.start(1)))
      out.append(outExts.mkString(" extends ", " with ", " {"))
      out.append(data.subSequence(traitMatch.end(1), data.length))
    } else {
      out.append(data.subSequence(0, allImportLines.last.end))
      out.append("\n")
      out.append(outImport)
      out.append(data.subSequence(allImportLines.last.end, traitMatch.start(1)))
      out.append(outExts.mkString(" extends ", " with ", " {"))
      out.append(data.subSequence(traitMatch.end(1), data.length))
    }

    // Only actually overwrite the file if everything has succeeded

    Using(Files.newBufferedWriter(p)) { writer =>
      writer.append(out.toString())
    }
  }

  def generate(p: Path, node: String, concrete: Boolean, family: Boolean): Unit = {
    Using(Files.newBufferedWriter(p)) { writer =>
      val ext =
        if (concrete && family) s"${node}Ops[G] with ${node}FamilyOps[G]"
        else if (concrete) s"${node}Ops[G]"
        else if (family) s"${node}FamilyOps[G]"
        else "???"

      val opsImport =
        if (concrete && family) s"{${node}Ops, ${node}FamilyOps}"
        else if(concrete) s"${node}Ops"
        else if(family) s"${node}FamilyOps"
        else "???"

      writer.write(
        s"""|package vct.col.ast.unsorted
            |
            |import vct.col.ast.$node
            |import vct.col.ast.ops.$opsImport
            |import vct.col.print._
            |
            |trait ${node}Impl[G] extends $ext { this: $node[G] =>
            |  // override def layout(implicit ctx: Ctx): Doc = ???
            |}
            |""".stripMargin
      )
    }
  }
}
