package hre.io

import vct.result.VerificationError.SystemError

import java.io.Reader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.{MessageDigest, NoSuchAlgorithmException}

case class UnknownChecksumKind(checksumKind: String) extends SystemError {
  override def text: String =
    s"Attempted to calculate checksum using unsupported algorithm: $checksumKind"
}

case class ChecksumReadableFile(
    file: Path,
    doWatch: Boolean = true,
    checksumKind: String,
) extends InMemoryCachedReadable {
  override def underlyingPath: Option[Path] = Some(file)
  override def fileName: String = file.toString
  override def isRereadable: Boolean = true
  private var checksumCache: Option[String] = None

  override protected def getReaderImpl: Reader = {
    val bytes = Files.readAllBytes(file)
    try {
      val digest = MessageDigest.getInstance(checksumKind)
      checksumCache = Some(
        digest.digest(bytes).map(_.asInstanceOf[Int] & 0xff).map { b =>
          if (b > 0xf) { Integer.toHexString(b) }
          else { "0" + Integer.toHexString(b) }
        }.mkString
      )
    } catch {
      case _: NoSuchAlgorithmException =>
        throw UnknownChecksumKind(checksumKind)
    }
    Files.newBufferedReader(file, StandardCharsets.UTF_8)
  }

  def getChecksum: String = {
    if (checksumCache.isEmpty) {
      // Calls ensureCache
      super.readToCompletion()
    }
    checksumCache.get
  }

  override def enroll(watch: Watch): Unit = {
    if (doWatch)
      watch.enroll(file)
    watch.invalidate(this)
  }
}
