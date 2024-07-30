package vct.test.integration.examples.veymont

import vct.test.integration.helper.{VercorsSpec, VeyMontSpec}

import java.io.{
  ByteArrayOutputStream,
  File,
  FileDescriptor,
  FileOutputStream,
  PrintStream,
}
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import javax.tools.ToolProvider

class VeyMontPermissionsPaperSpec extends VeyMontSpec {
  val wd = Paths.get("examples/concepts/veymont/FM2024 - VeyMont")

  veymontTest(
    "TTT case study (choreographic verification)",
    Seq(
      wd.resolve("0-TTT/Move.pvl"),
      wd.resolve("0-TTT/Player.pvl"),
      wd.resolve("0-TTT/0-TTT.pvl"),
    ),
    "--veymont",
    "--veymont-generate-permissions",
    "--veymont-skip-implementation-verification",
  )

  {
//    val root: Path = Files.createTempDirectory("java-veymont")
//    val source = root.resolve("TTTmsg.java")

    veymontTest(
      "TTTmsg case study (choreographic verification)",
      Seq(
        wd.resolve("1-TTTmsg/Move.pvl"),
        wd.resolve("1-TTTmsg/Player.pvl"),
        wd.resolve("1-TTTmsg/1-TTTmsg.pvl"),
      ),
      "--dev-unsafe-optimization",
      "--veymont",
      "--veymont-skip-implementation-verification",
    )

    veymontTest(
      "TTTmsg case study (implementation verification)",
      Seq(
        wd.resolve("1-TTTmsg/Move.pvl"),
        wd.resolve("1-TTTmsg/Player.pvl"),
        wd.resolve("1-TTTmsg/1-TTTmsg.pvl"),
      ),
      "--dev-unsafe-optimization",
      "--veymont",
      "--veymont-skip-choreography-verification",
    )

    // TODO: Do patching, running, output checking
  }

  {
    val root: Path = Files.createTempDirectory("java-veymont")
    val source = root.resolve("TTTlast.java")

    veymontTest(
      "TTTlast case study (choreographic verification)",
      Seq(
        wd.resolve("2-TTTlast/Move.pvl"),
        wd.resolve("2-TTTlast/Player.pvl"),
        wd.resolve("2-TTTlast/2-TTTlast.pvl"),
      ),
      "--dev-unsafe-optimization",
      "--veymont",
      "--veymont-skip-implementation-verification",
      "--veymont-output",
      source.toString,
    )

    veymontTest(
      "TTTlast case study (implementation verification)",
      Seq(
        wd.resolve("2-TTTlast/Move.pvl"),
        wd.resolve("2-TTTlast/Player.pvl"),
        wd.resolve("2-TTTlast/2-TTTlast.pvl"),
      ),
      "--dev-unsafe-optimization",
      "--veymont",
      "--veymont-skip-choreographic-verification",
      "--veymont-output",
      source.toString,
    )

    // TODO: Do patching, running, output checking
  }

  /** Runs the given Java script and returns the captured standard output.
    * Depends on the JDK being present on the current system.
    *
    * @param script
    *   A series of java statements to be executed. May refer to declarations in
    *   the declarations parameter.
    * @param declarations
    *   A series of non-public java declarations, no package declaration
    */
  def runJava(script: String, declarations: String): String = {
    val cls = "ScriptContainer$"
    val source = s"public class $cls { static { $script } }"

    // Save source in .java file.
    val root: Path = Files.createTempDirectory("java")
    val sourceFile = root.resolve("ScriptContainer$.java")
    Files.createFile(sourceFile)
    Files.write(sourceFile, source.getBytes(StandardCharsets.UTF_8))

    // Compile source file.
    val compiler = ToolProvider.getSystemJavaCompiler
    compiler.run(null, null, null, sourceFile.toString)

    // Capture stdout
    val stdoutCapture = new ByteArrayOutputStream();
    System.setOut(new PrintStream(stdoutCapture));

    // Load and instantiate compiled class.
    val classLoader = URLClassLoader.newInstance(Array(root.toUri.toURL))
    Class.forName(cls, true, classLoader)

    // Reset system stdout
    System.setOut(new PrintStream(new FileOutputStream(FileDescriptor.out)));

    // Return captured stdout
    stdoutCapture.toString()
  }

}
