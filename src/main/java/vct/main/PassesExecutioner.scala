package vct.main

import hre.ast.FileOrigin
import hre.lang.System.{Abort, Fail, Output, Progress, Verdict}
import vct.col.ast.stmt.decl.{ProgramUnit, SpecificationFormat}
import vct.col.features.{Feature, RainbowVisitor}
import vct.logging.PassReport
import vct.main.options.CommandLineOptions
import vct.main.passes.Passes.BY_KEY
import vct.main.passes.{AbstractPass, Parsers}
import vct.silver.ErrorDisplayVisitor

import java.io.{FileOutputStream, PrintWriter}
import java.nio.file.Paths

trait PassesExecutionerTrait{
  def doPasses(passes: Seq[AbstractPass]): Unit
}

class PassesExecutioner extends PassesExecutionerTrait {

  def doPasses(passes: Seq[AbstractPass]): Unit = {
    for((pass, i) <- passes.zipWithIndex) {
      if (debugBefore.has(pass.key)) report.getOutput.dump()
      if (show_before.contains(pass.key)) show(pass)

      val featuresIn = if(strictInternalConditions.get()) {
        Feature.scan(report.getInput)
      } else { Set.empty }

      tk.show
      report = pass.apply_pass(report, Array())

      if(report.getFatal > 0) {
        Verdict("The final verdict is Fail")
        return
      }

      Progress("[%02d%%] %s took %d ms", Int.box(100 * (i+1) / passes.size), pass.key, Long.box(tk.show))

      if (debugAfter.has(pass.key)) report.getOutput.dump()
      if (show_after.contains(pass.key)) show(pass)
      if (stopAfter.contains(pass.key)) Fail("exit after pass %s", pass)

      report = BY_KEY("checkTypesJava").apply_pass(report, Array())

      if(report.getFatal > 0) {
        Verdict("The final verdict is Fail")
        return
      }

      if(strictInternalConditions.get()) {
        val scanner = new RainbowVisitor(report.getOutput)
        scanner.source().accept(scanner)
        val featuresOut = scanner.features.toSet

        val notRemoved = featuresOut.intersect(pass.removes)
        val extraIntro = (featuresOut -- featuresIn) -- pass.introduces

        val permissiveFeatures = Set(
          vct.col.features.QuantifierWithoutTriggers,
          vct.col.features.NestedQuantifiers,
          vct.col.features.ExceptionalReturn,
          vct.col.features.ContextEverywhere,
        )

        if (notRemoved.nonEmpty) {
          notRemoved.foreach(feature => {
            Output("Pass %s did not remove %s:", pass, feature)
            scanner.logBlameExamples(feature)
          })
        }

        if (extraIntro.nonEmpty) {
          extraIntro.foreach(feature => {
            Output("Pass %s introduced %s", pass, feature)
            scanner.logBlameExamples(feature)
          })
        }

        if((notRemoved -- permissiveFeatures).nonEmpty || (extraIntro -- permissiveFeatures).nonEmpty) {
          Abort("Halting, because strict internal conditions are enabled.")
        }
      }

    }

    Verdict("The final verdict is Pass")
  }

  private def show(pass: AbstractPass): Unit = {
    val name = CommandLineOptions.show_file.get
    if (name != null) {
      val file = String.format(name, pass.key)
      val out = new PrintWriter(new FileOutputStream(file))
      vct.col.ast.util.Configuration.getDiagSyntax.print(out, report.getOutput)
      out.close()
    }
    else {
      val out = hre.lang.System.getLogLevelOutputWriter(hre.lang.System.LogLevel.Info)
      vct.col.ast.util.Configuration.getDiagSyntax.print(out, report.getOutput)
      out.close()
    }
  }

}
