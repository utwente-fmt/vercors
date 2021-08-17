package vct.main

import hre.lang.System.{Abort, Fail, Output, Progress, Verdict}
import hre.tools.TimeKeeper
import vct.col.features.{Feature, RainbowVisitor}
import vct.logging.PassReport
import vct.main.options.CommandLineOptions
import vct.main.passes.Passes.BY_KEY
import vct.main.passes.AbstractPass

import java.io.{FileOutputStream, PrintWriter}

trait PassesExecutionerTrait{
  def doPasses(passes: Seq[AbstractPass], report: PassReport, timeKeeper: TimeKeeper): Unit
}

class PassesExecutioner extends PassesExecutionerTrait {

  def doPasses(passes: Seq[AbstractPass], report: PassReport, timeKeeper: TimeKeeper): Unit = {
    var currentReport = report
    for((pass, i) <- passes.zipWithIndex) {
      if (CommandLineOptions.debugBefore.has(pass.key)) currentReport.getOutput.dump()
      if (CommandLineOptions.showBefore.contains(pass.key)) show(pass,currentReport)

      val featuresIn = if(CommandLineOptions.strictInternalConditions.get()) {
        Feature.scan(currentReport.getInput)
      } else { Set.empty }

      timeKeeper.show
      currentReport = pass.apply_pass(currentReport, Array())

      if(currentReport.getFatal > 0) {
        Verdict("The final verdict is Fail")
        return
      }

      Progress("[%02d%%] %s took %d ms", Int.box(100 * (i+1) / passes.size), pass.key, Long.box(timeKeeper.show))

      if (CommandLineOptions.debugAfter.has(pass.key)) currentReport.getOutput.dump()
      if (CommandLineOptions.showAfter.contains(pass.key)) show(pass,currentReport)
      if (CommandLineOptions.stopAfter.contains(pass.key)) Fail("exit after pass %s", pass)

      currentReport = BY_KEY("checkTypesJava").apply_pass(currentReport, Array())

      if(currentReport.getFatal > 0) {
        Verdict("The final verdict is Fail")
        return
      }

      if(CommandLineOptions.strictInternalConditions.get()) {
        val scanner = new RainbowVisitor(currentReport.getOutput)
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

  private def show(pass: AbstractPass, report: PassReport): Unit = {
    val name = CommandLineOptions.showFile.get
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
