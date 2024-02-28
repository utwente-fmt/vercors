package vct.main.stages

import com.typesafe.scalalogging.LazyLogging
import hre.io.Readable
import hre.stages.{Stage, Stages}
import vct.col.ast.{Declaration, Node, Program, Verification}
import vct.col.origin.DiagnosticOrigin
import vct.col.print.{Ctx, Namer}
import vct.col.rewrite.{Generation, InitialGeneration}
import vct.options.Options
import vct.parsers.ParseResult

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

// TODO (RR): I messed this up for VeyMont, need to coordinate with Philip to get it in a nice shape

case object Output {
  def vesuvOfOptions[G <: Generation](options: Options): Stages[ParseResult[G], Unit] = {
//    FunctionStage((pr: ParseResult[_ <: Generation]) => Program(pr.decls)(DiagnosticOrigin)(DiagnosticOrigin))
//      .thenRun(??? /* Output(options.vesuvOutput, Ctx.PVL) */)
    ???
  }

  def veymontOfOptions(options: Options): Stage[Verification[_ <: Generation], Seq[StringReadable]] =
    Output(options.veymontOutput, Ctx.Java, false)
}

case class Output(out: Option[Path], syntax: Ctx.Syntax, splitDecls: Boolean) extends Stage[Verification[_ <: Generation], Seq[StringReadable]] with LazyLogging {
  override def friendlyName: String = "Saving Output"

  override def progressWeight: Int = 1

  def extension(syntax: Ctx.Syntax): String = syntax match {
    case Ctx.PVL => "pvl"
    case Ctx.Silver => "vpr"
    case Ctx.Java => "java"
    case Ctx.C => "c"
    case Ctx.CPP => "cpp"
    case Ctx.Cuda => "cu"
    case Ctx.OpenCL => "cl"
  }

  override def run(in: Verification[_ <: Generation]): Seq[StringReadable] = {
    val namer = Namer[InitialGeneration](syntax)
    namer.name(in.asInstanceOf)
    val names = namer.finish
    val ctx = Ctx(syntax = syntax, names = names.asInstanceOf[Map[Declaration[_], String]])

    val txts: Seq[StringReadable] = if (splitDecls) {
      in.asInstanceOf[Program[_]].declarations.zipWithIndex.map { case (decl, i) =>
        val name = names.getOrElse(decl.asInstanceOf, s"unknown$i")
        val fileName = s"${name}.${extension(syntax)}"
        val buf = new StringBuffer()
        decl.write(buf)(ctx)
        StringReadable(buf.toString, fileName)
      }
    } else {
      val buf = new StringBuffer()
      in.write(buf)(ctx)
      val path = s"unknown.${extension(syntax)}"
      Seq(StringReadable(buf.toString, path))
    }

    (out, txts) match {
      case (Some(p), Seq()) =>
        logger.warn("Output stage was executed without any declarations to print")
        Files.write(p, "".getBytes(StandardCharsets.UTF_8))
      case (Some(p), Seq(txt)) =>
        Files.write(p, txt.data.getBytes(StandardCharsets.UTF_8))
      case (Some(p), txts) => txts.foreach { txt =>
        Files.write(p.resolve(txt.fileName), txt.data.getBytes(StandardCharsets.UTF_8))
      }
      case _ =>
    }

    txts
  }
}