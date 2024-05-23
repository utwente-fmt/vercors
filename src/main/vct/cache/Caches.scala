package vct.cache

import com.typesafe.scalalogging.LazyLogging
import hre.cache.Cache
import vct.main.BuildInfo

import java.nio.file.Path

case object Caches extends LazyLogging {
  // Val's, so that multiple VerCors runs in one JVM (e.g. tests) benefit from a shared cache even when the cache is
  // variate.
  lazy val getLibraryCache: Path = getDirectory(BuildInfo.currentCommit)
    .resolve("library")

  lazy val getCarbonDirectory: Path = getViperDirectory(BuildInfo.carbonCommit)
    .resolve("carbon").resolve("verified")
  lazy val getSiliconDirectory: Path = getViperDirectory(
    BuildInfo.siliconCommit
  ).resolve("silicon").resolve("verified")

  private def getDirectory(keys: String*): Path = {
    val variate = BuildInfo.gitHasChanges != "false"

    if (variate) {
      logger.warn(
        "Caching is enabled, but results will be discarded, since there were uncommitted changes at compilation time."
      )
    }

    Cache.getDirectory(variate, keys)
  }

  private def getViperDirectory(backendCommit: String): Path =
    getDirectory(BuildInfo.currentCommit, BuildInfo.silverCommit, backendCommit)
}
