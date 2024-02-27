package vct.main.stages

import hre.io.Readable
import hre.stages.{Stage, Stages}
import vct.col.ast.{Declaration, Node, Program, Verification}
import vct.col.origin.DiagnosticOrigin
import vct.col.print.{Ctx, Namer}
import vct.col.rewrite.{Generation, InitialGeneration}
import vct.options.Options
import vct.parsers.ParseResult

import java.nio.file.{Files, Path}

// TODO (RR): I messed this up for VeyMont, need to coordinate with Philip to get it in a nice shape

case object Output {
  def vesuvOfOptions[G <: Generation](options: Options): Stages[ParseResult[G], Unit] = {
//    FunctionStage((pr: ParseResult[_ <: Generation]) => Program(pr.decls)(DiagnosticOrigin)(DiagnosticOrigin))
//      .thenRun(??? /* Output(options.vesuvOutput, Ctx.PVL) */)
    ???
  }

  def veymontOfOptions(options: Options): Stage[Verification[_ <: Generation], Seq[StringReadable]] =
    Output(options.veymontOutput, Ctx.Java)
}

case class Output(out: Path, syntax: Ctx.Syntax) extends Stage[Verification[_ <: Generation], Seq[StringReadable]] {
  override def friendlyName: String = "Saving Output"

  override def progressWeight: Int = 1

  override def run(in: Verification[_ <: Generation]): Seq[StringReadable] = {
    val namer = Namer[G](syntax)
    namer.name(in)
    val names = namer.finish
    val ctx = Ctx(syntax = syntax, names = names.asInstanceOf[Map[Declaration[_], String]])

    // If possible (if a directory is given as output), print all classes to separate files
    if (in.isInstanceOf[Program[G]] && Files.isDirectory(out)) {
      in.asInstanceOf[Program[G]].declarations.zipWithIndex.foreach { case (decl, i) =>
        val name = names.getOrElse(decl, s"unknown$i")
        val f = out.resolve(name + ".pvl")
        hre.io.RWFile(f.toFile).write(w => decl.write(w)(ctx))
      }
    }
    // Otherwise create one big program from the parse result and write it to the provided file directly
    else {
      hre.io.RWFile(out.toFile).write(w => in.write(w)(ctx))
    }

    Seq(???)
  }
}