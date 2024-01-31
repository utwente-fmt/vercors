package vct.main.stages

import hre.stages.{Stage, Stages}
import vct.col.ast.{Declaration, Node, Program}
import vct.col.origin.DiagnosticOrigin
import vct.col.print.{Ctx, Namer}
import vct.col.rewrite.{Generation, InitialGeneration}
import vct.options.Options
import vct.parsers.ParseResult

import java.nio.file.{Files, Path}

case object Output {
  def runtimeOfOptions(options: Options) = {
    Output(options.runtimeOutput, Ctx.Java)
  }

  def vesuvOfOptions(options: Options): Stages[ParseResult[_ <: Generation], Unit] =
    FunctionStage((pr: ParseResult[_ <: Generation]) => Program(pr.decls)(DiagnosticOrigin)(DiagnosticOrigin))
      .thenRun(Output(options.vesuvOutput, Ctx.PVL))

  def veymontOfOptions(options: Options): Stage[Node[_ <: Generation], Unit] =
    Output(options.veymontOutput, Ctx.Java)
}

case class Output(out: Path, syntax: Ctx.Syntax) extends Stage[Node[_ <: Generation], Unit] {
  override def friendlyName: String = "Saving Output"

  override def progressWeight: Int = 1

  override def run(in1: Node[_ <: Generation]): Unit = {
    type G = InitialGeneration
    val in = in1.asInstanceOf[Node[G]]
    val namer = Namer[G](syntax)
    namer.name(in)
    val names = namer.finish
    val ctx = Ctx(syntax = syntax, width=250, names = names.asInstanceOf[Map[Declaration[_], String]])

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
  }
}