package vct.main.stages

import hre.io.LiteralReadable
import hre.stages.Stage
import vct.options.Options

import java.nio.file.Path

case object PrintCFile {
  def ofOptions(options: Options): Stage[Seq[LiteralReadable], Unit] = {
    PrintCFile(options.cOutput)
  }
}

case class PrintCFile(out: Option[Path])
    extends Stage[Seq[LiteralReadable], Unit] {

  override def friendlyName: String = "write modifications to file"

  override def progressWeight: Int = 0

  override def run(in: Seq[LiteralReadable]): Unit = {
    out match {
      case Some(path) =>
        hre.io.RWFile(path)
          .write(w => in.foreach(lr => w.write(lr.readToCompletion())))
    }
  }
}
