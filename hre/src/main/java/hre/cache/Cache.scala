package hre.cache

import java.nio.file.{Files, Path, Paths}
import net.harawata.appdirs.AppDirsFactory

import java.security.MessageDigest

case object Cache {
  private def sha256(data: Array[Byte]): Array[Byte] = {
    val inst = MessageDigest.getInstance("SHA-256")
    inst.digest(data)
  }

  private def hash(keys: Seq[String]): String = {
    val hashes = ("" +: keys).map(_.getBytes).map(sha256)

    val res = hashes.tail.foldLeft(hashes.head) {
      case (acc, hash) => sha256(acc ++ hash)
    }

    res.map("%02x" format _).mkString("")
  }

  private var variateTempDir: Option[Path] = None

  /**
   * Get or create a platform-appropriate cache data directory, keyed by a sequence of keys. If variate is set, the
   * returned directory will be unique for this JVM instance / VerCors run, rather than unique for the set of keys.
   * @param variate Whether or not this process should be considered unique in the cache
   * @param keys The set of keys to determine the cache directory by.
   * @return The path to the cache directory
   */
  def getDirectory(variate: Boolean, keys: Seq[String]): Path = {
    if(variate) {
      if(variateTempDir.isEmpty)
        variateTempDir = Some(Files.createTempDirectory("vercors").toAbsolutePath)
      variateTempDir.get
    } else {
      Paths.get(AppDirsFactory.getInstance().getUserCacheDir("vercors", hash(keys), "utwente"))
    }
  }
}