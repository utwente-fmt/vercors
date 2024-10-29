import mill._
import os.Path

package object util {
  case class DataPoint(base: Path, coordinate: os.SubPath) {
    lazy val mtime = os.mtime(base / coordinate)

    override def equals(obj: Any): Boolean = obj match {
      case other: DataPoint => coordinate == other.coordinate
    }

    private lazy val _hashCode = coordinate.hashCode()
    override def hashCode(): Int = _hashCode

    def copyTo(dest: Path): Unit = {
      os.copy(base / coordinate, dest / coordinate, createFolders = true)
    }

    def copyOver(dest: Path): Unit =
      if(DataPoint(dest, coordinate).mtime != mtime)
        os.copy.over(base / coordinate, dest / coordinate)

    def delete(): Unit =
      os.remove(base / coordinate)
  }

  def quickCopy(target: Path, sourcePaths: Seq[PathRef]): Unit = {
    val sources =
      sourcePaths
        .map(_.path)
        .flatMap { base =>
          os.walk.stream(base)
            .filter(os.isFile(_))
            .map(p => DataPoint(base, p.subRelativeTo(base)))
            .toSeq
        }
        .toSet

    val targets =
      os.walk.stream(target)
        .filter(os.isFile(_))
        .map(p => DataPoint(target, p.subRelativeTo(target)))
        .toSet

    for(toRemove <- targets if !sources.contains(toRemove)) {
      toRemove.delete()
    }

    for(toWrite <- sources if !targets.contains(toWrite)) {
      toWrite.copyTo(target)
    }

    for(toWriteOver <- sources if targets.contains(toWriteOver)) {
      toWriteOver.copyOver(target)
    }
  }
}
