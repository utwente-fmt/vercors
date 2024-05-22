package vct.importer

import hre.io.RWFile
import vct.col.ast.Program
import vct.col.rewrite.adt.ImportADTImporter
import vct.parsers.debug.DebugOptions

import java.nio.file.Path

case class PathAdtImporter(basePath: Path, debugOptions: DebugOptions) extends ImportADTImporter {
  override def loadAdt[G](name: String): Program[G] =
    Util.loadPVLLibraryFile(RWFile(basePath.resolve(name + ".pvl")), debugOptions)
}
