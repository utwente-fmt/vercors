package vct.test.integration.features

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import vct.parsers.parser.ColCParser
import vct.resources.Resources

import java.io.{InputStreamReader, OutputStreamWriter, Reader, StringWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

class CHeadersSpec extends AnyFlatSpec with should.Matchers{
  private val cc: Path = Resources.getCcPath
  private val systemInclude: Path = Resources.getCIncludePath
  // Excluding 32-bit linux target since we might not have the include sources on this computer (windows target is fine since this falls back to the headers included with clang anyway)
  private val TARGETS_TO_TEST = Seq("x86_64-linux-unknown", "x86_64-windows-unknown", "i686-windows-unknown")

  private def findMarkers(str: String): String = {
    val start = str.indexOf("//@ START_MARKER")
    if (start == -1) fail("Failed to find start marker")
    val end = str.indexOf("//@ END_MARKER")
    if (end == -1) fail("Failed to find end marker")
    // Removing all spaces because we don't care if the formatting differs slightly
    str.substring(start, end).replace(" ", "")
  }

  private def readFromProcess(process: Process, reader: Reader): String = {
    new Thread(
      () => {
        val writer =
          new OutputStreamWriter(
            process.getOutputStream,
            StandardCharsets.UTF_8,
          )
        try {
          val written = reader.transferTo(writer)
        } finally { writer.close() }
      },
      "[VerCors] clang stdout writer",
    ).start()
    process.waitFor()
    val writer = new StringWriter()
    new InputStreamReader(process.getInputStream).transferTo(writer)
    writer.close()

    if (process.exitValue() != 0) {
      // We ignore this case since if the 32-bit libraries are not installed we might not have all includes working (in my case I got 'bits/libc-header-start.h' file not found as an error but all the defines we look for are still present)
    }

    writer.toString
  }

  private def interpretVerCors(reader: Reader, targetString: String): String = {
    val process = ColCParser(null, null, cc, systemInclude, Nil, Map.empty, Some(targetString)).interpret("-", "-")

    findMarkers(readFromProcess(process, reader))
  }

  private def interpretSystem(reader: Reader, targetString: String): String = {
    val process = new ProcessBuilder(cc.toString, "-CC", "-E", "-target", targetString, "-std=c23", "-o", "-", "-").start()


    findMarkers(readFromProcess(process, reader))
  }

  TARGETS_TO_TEST.foreach { target =>
    it should s"define the same variables in stdint.h (target = $target)" in {
      val reader = () => Files.newBufferedReader(Path.of("examples", "technical", "c-headers", "stdint.h"), StandardCharsets.UTF_8)
      // We don't care about the extra U in UINT8_MAX and UINT16_MAX and writing -127-1 instead of -128

      val replacements = (s: String) => s.replaceAll("(?m)^\\(([^-\n\r]*)\\)$", "$1").replace("255U", "255").replace("65535U", "65535").replace("-127-1", "-128")
      replacements(interpretVerCors(reader(), target)) shouldBe replacements(interpretSystem(reader(), target))
    }
  }

  TARGETS_TO_TEST.foreach { target =>
    it should s"define the same variables in limits.h (target = $target)" in {
      val reader = () => Files.newBufferedReader(Path.of("examples", "technical", "c-headers", "limits.h"), StandardCharsets.UTF_8)
      val replacements = (s: String) => s.replaceAll("(?m)^\\(([^-\n\r]*)\\)$", "$1").replace("65535U", "65535").replace("127*2+1", "255").replace("32767*2+1", "65535").replace("9223372036854775807LL*2ULL+1ULL", "18446744073709551615ULL").replace("2147483647*2U+1U", "4294967295U").replace("2147483647L*2UL+1UL", "4294967295UL").replace("-1LL", "-1").replace("-1L", "-1").replace("9223372036854775807L*2UL+1UL", "18446744073709551615UL")
      replacements(interpretVerCors(reader(), target)) shouldBe replacements(interpretSystem(reader(), target))
    }
  }
}
