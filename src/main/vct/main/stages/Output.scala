package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.io.{LiteralReadable, Readable}
import hre.stages.{FunctionStage, Stage, Stages}
import vct.col.ast.{
  Declaration,
  Node,
  Program,
  Verification,
  VerificationContext,
}
import vct.col.origin.{DiagnosticOrigin, Origin}
import vct.col.print.{Ctx, Namer}
import vct.col.rewrite.{Generation, InitialGeneration}
import vct.options.Options
import vct.parsers.ParseResult

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/* TODO (RR): I messed this up for VeyMont & VeSUV, need to coordinate with Philip to get it in a nice shape
      Philip does use the splitting up feature, so I'd need to rework that either into Output or into a separate stage
      Also the writing to file feature he relies on.
 */

case object Output {
  def vesuvOfOptions[G <: Generation](
      options: Options
  ): Stages[ParseResult[G], Unit] = {
    implicit val o: Origin = DiagnosticOrigin
    FunctionStage((pr: ParseResult[_ <: Generation]) =>
      Verification(
        Seq(VerificationContext(Program(pr.decls)(DiagnosticOrigin))),
        Seq(),
      )
    ).thenRun(Output(
      Some(options.vesuvOutput),
      Ctx.PVL,
      Files.isDirectory(options.vesuvOutput),
    )).thenRun(FunctionStage((_: Seq[LiteralReadable]) =>
        ()
      )) // TODO: Not the prettiest, but I have no time for this. I blame Bob.
  }

  def veymontOfOptions(
      options: Options
  ): Stage[Verification[_ <: Generation], Seq[LiteralReadable]] =
    Output(options.veymontOutput, Ctx.Java, false)
}

case class Output(out: Option[Path], syntax: Ctx.Syntax, splitDecls: Boolean)
    extends Stage[Verification[_ <: Generation], Seq[LiteralReadable]]
    with LazyLogging {
  override def friendlyName: String = "Saving Output"

  override def progressWeight: Int = 1

  def extension(syntax: Ctx.Syntax): String =
    syntax match {
      case Ctx.PVL => "pvl"
      case Ctx.Silver => "vpr"
      case Ctx.Java => "java"
      case Ctx.C => "c"
      case Ctx.CPP => "cpp"
      case Ctx.Cuda => "cu"
      case Ctx.OpenCL => "cl"
    }

  override def run(in: Verification[_ <: Generation]): Seq[LiteralReadable] = {
    val namer = Namer[Generation](syntax)
    in.tasks
      .foreach(t => namer.name(t.program.asInstanceOf[Program[Generation]]))
    val names = namer.finish
    val ctx = Ctx(
      syntax = syntax,
      names = names.asInstanceOf[Map[Declaration[_], String]],
    )

    val txts: Seq[LiteralReadable] =
      if (splitDecls) {
        in.tasks.map(t => t.program).flatMap(p => p.declarations).zipWithIndex
          .map { case (decl, i) =>
            val name = names.getOrElse(
              decl.asInstanceOf[Declaration[Generation]],
              s"unknown$i",
            )
            val fileName = s"${name}.${extension(syntax)}"
            val buf = new StringBuffer()
            decl.write(buf)(ctx)
            LiteralReadable(fileName, buf.toString)
          }
      } else {
        val buf = new StringBuffer()
        in.write(buf)(ctx)
        val path = s"unknown.${extension(syntax)}"
        Seq(LiteralReadable(path, buf.toString))
      }

    (out, txts) match {
      case (Some(p), Seq()) =>
        logger
          .warn("Output stage was executed without any declarations to print")
        Files.write(p, "".getBytes(StandardCharsets.UTF_8))
      case (Some(p), Seq(txt)) =>
        logger.info(s"Writing ${txt.fileName} to $p")
        Files.write(p, txt.data.getBytes(StandardCharsets.UTF_8))
      case (Some(p), txts) =>
        txts.foreach { txt =>
          logger.info(s"Writing ${txt.fileName} to $p")
          Files.write(
            p.resolve(txt.fileName),
            txt.data.getBytes(StandardCharsets.UTF_8),
          )
        }
      case _ =>
    }

    txts
  }
}
