package vct.main.stages

import hre.io.Writeable
import hre.stages.Stage
import vct.col.ast.{Declaration, Program, Verification}
import vct.col.origin.{FileSpanningOrigin, Origin}
import vct.col.print.{PVL, Printer}
import vct.col.rewrite.Generation
import vct.options.Options
import vct.options.types.PathOrStd.Path
import vct.parsers.ParseResult
import vct.parsers.transform.BlameProvider

import java.nio.file.Files
import java.nio.file.Paths

case object Output {
  def ofOptions(options: Options, blameProvider: BlameProvider): Stage[ParseResult[_ <: Generation], Unit] = {
    Output(options.vesuvOutput, blameProvider)
  }
}

case class Output(writeable : Writeable, blameProvider: BlameProvider) extends Stage[ParseResult[_<: Generation], Unit] {
  override def friendlyName: String = "Saving File..."

  override def progressWeight: Int = 1

  override def run(in: ParseResult[_ <: Generation]): Unit = {
    // If possible (if a directory is given as output), print all classes to separate files
    if (Files.isDirectory(Paths.get(writeable.fileName))) {
      in.decls.foreach(decl => Path(getFilePath(decl)).write(w => Printer(w, syntax = PVL, unsafeNaming = true).print(decl)))
    }
    // Otherwise create one big program from the parse result and write it to the provided file directly
    else {
      writeable.write(w => {
          implicit val o: Origin = FileSpanningOrigin
          val printer = Printer(w, syntax = PVL, unsafeNaming = true)
          printer.print(Program(in.decls)(blameProvider()))
        }
      )
    }
  }

  def getFilePath(decl: Declaration[_<: Generation]): java.nio.file.Path =
    Paths.get(writeable.fileName, decl.o.preferredName + ".pvl")
}