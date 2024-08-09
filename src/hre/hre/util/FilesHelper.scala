package hre.util

import java.io.IOException
import java.nio.file
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

case object FilesHelper {
  def withTempDir[T](f: Path => T): T = {
    val p = Files.createTempDirectory("java-veymont")
    try { f(p) }
    finally { deleteDirectory(p) }
  }

  def deleteDirectory(p: Path) = {
    require(file.Files.isDirectory(p))
    // Copied from the java standard library documentation
    Files.walkFileTree(
      p,
      new SimpleFileVisitor[Path]() {
        @throws[IOException]
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes,
        ): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        @throws[IOException]
        override def postVisitDirectory(
            dir: Path,
            e: IOException,
        ): FileVisitResult =
          if (e == null) {
            Files.delete(dir)
            FileVisitResult.CONTINUE
          } else {
            // directory iteration failed
            throw e
          }
      },
    )
  }
}
