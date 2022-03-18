package vct.parsers

import hre.io.RWFile
import vct.col.ast.Program
import vct.col.newrewrite.ImportADTImporter
import vct.main.util.Util

import java.nio.file.Path

case class PathAdtImporter(basePath: Path) extends ImportADTImporter {
  override def loadAdt[G](name: String): Program[G] =
    Util.loadPVLLibraryFile(RWFile(basePath.resolve(name + ".pvl").toFile))
}
