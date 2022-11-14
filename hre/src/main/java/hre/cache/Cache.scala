package hre.cache

import java.nio.file.Path
import net.harawata.appdirs.AppDirsFactory

case object Cache {
  def baseDirectory: Path =
    AppDirsFactory.getInstance().getUserCacheDir("vercors", BuildInfo)
}
