package vct.main

import hre.ast.FileOrigin
import hre.lang.System.Progress
import vct.col.ast.stmt.decl.{ProgramUnit, SpecificationFormat}
import vct.logging.PassReport
import vct.main.passes.Parsers
import vct.silver.ErrorDisplayVisitor

import java.nio.file.Paths

trait FileParserTrait {
  def parseInputs(inputPaths: Array[String]): Unit
}

class FileParser extends FileParserTrait {
  def parseInputs(inputPaths: Array[String]): Unit = {
    Progress("parsing inputs...")
    report = new PassReport(new ProgramUnit)
    report.setOutput(report.getInput)
    report.add(new ErrorDisplayVisitor)

    tk.show
    for (pathName <- inputPaths) {
      val path = Paths.get(pathName);
      if (!no_context.get) FileOrigin.add(path, gui_context.get)
      report.getOutput.add(Parsers.parseFile(path))
    }

    Progress("Parsed %d file(s) in: %dms", Int.box(inputPaths.length), Long.box(tk.show))

    if (sequential_spec.get)
      report.getOutput.setSpecificationFormat(SpecificationFormat.Sequential)
  }
}
