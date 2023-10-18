package vct.cache

import com.typesafe.scalalogging.LazyLogging
import hre.cache.Cache
import vct.main.BuildInfo

import java.nio.file.Path

case object VerificationCache extends LazyLogging {
  def getCarbonDirectory: Path =
    getViperDirectory(BuildInfo.carbonCommit).resolve("carbon")
      .resolve("verified")
  def getSiliconDirectory: Path =
    getViperDirectory(BuildInfo.siliconCommit).resolve("silicon")
      .resolve("verified")

  private def getViperDirectory(backendCommit: String): Path = {
    val variate = BuildInfo.gitHasChanges == "false"

    if (variate) {
      logger.warn(
        "Caching is enabled, but results will be discarded, since there were uncommitted changes at compilation time."
      )
    }

    Cache.getDirectory(
      variate = variate,
      keys = Seq(BuildInfo.currentCommit, BuildInfo.silverCommit, backendCommit),
    )
  }
}
