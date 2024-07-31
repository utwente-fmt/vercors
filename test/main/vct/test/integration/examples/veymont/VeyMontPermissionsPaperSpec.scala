package vct.test.integration.examples.veymont

import hre.util.{FilesHelper, Patch}
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
  val wd = Paths.get("concepts/veymont/FM2024 - VeyMont")

//  veymontTest(
//    "TTT case study (choreographic verification)",
//    Seq(
//      wd.resolve("0-TTT/Move.pvl"),
//      wd.resolve("0-TTT/Player.pvl"),
//      wd.resolve("0-TTT/0-TTT.pvl"),
//    ),
//    "--veymont",
//    "--veymont-generate-permissions",
//    "--veymont-skip-implementation-verification",
//  )
//
//  veymontTest(
//    "TTTmsg case study (choreographic verification)",
//    Seq(
//      wd.resolve("1-TTTmsg/Move.pvl"),
//      wd.resolve("1-TTTmsg/Player.pvl"),
//      wd.resolve("1-TTTmsg/1-TTTmsg.pvl"),
//    ),
//    "--dev-unsafe-optimization",
//    "--veymont",
//    "--veymont-skip-implementation-verification",
//  )
//
//  veymontTest(
//    "TTTmsg case study (implementation verification)",
//    Seq(
//      wd.resolve("1-TTTmsg/Move.pvl"),
//      wd.resolve("1-TTTmsg/Player.pvl"),
//      wd.resolve("1-TTTmsg/1-TTTmsg.pvl"),
//    ),
//    "--dev-unsafe-optimization",
//    "--veymont",
//    "--veymont-skip-choreography-verification",
//  )

  {
    val caseWd = wd.resolve("1-TTTmsg")
    veymontTest(
      desc = "TTTmsg case study (implementation generation)",
      inputs = examplePaths(
        caseWd.resolve("Move.pvl"),
        caseWd.resolve("Player.pvl"),
        caseWd.resolve("1-TTTmsg.pvl"),
      ),
      flags = Seq(
        "--dev-unsafe-optimization",
        "--veymont-skip-choreography-verification",
        "--veymont-skip-implementation-verification",
      ),
      language = Java,
      processImplementation = { p =>
        val source = Files.readString(p)
        val patches = Patch
          .fromFile(example(caseWd.resolve("testFiles/patches.txt")))
        val patched = Patch.applyAll(patches, source)
        val testScript = Files
          .readString(example(caseWd.resolve("testFiles/testScript.txt")))
        val output = runJava(testScript, patched)
        println("== output ==")
        println(output)
        assert(output == """p1:
            |0 1 0
            |1 0 1
            |0 -1 -1
            |p2:
            |0 1 0
            |1 0 1
            |0 -1 -1
            |""".stripMargin)
      },
    )
  }

  // TODO: output checking

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
      Seq(
        "--dev-unsafe-optimization",
        "--veymont",
        "--veymont-skip-implementation-verification",
        "--veymont-output",
        source.toString,
      ),
    )

    veymontTest(
      "TTTlast case study (implementation verification)",
      Seq(
        wd.resolve("2-TTTlast/Move.pvl"),
        wd.resolve("2-TTTlast/Player.pvl"),
        wd.resolve("2-TTTlast/2-TTTlast.pvl"),
      ),
      Seq(
        "--dev-unsafe-optimization",
        "--veymont",
        "--veymont-skip-choreographic-verification",
        "--veymont-output",
        source.toString,
      ),
    )

    // TODO: Do patching, running, output checking
  }

  /** Runs the given Java script and returns the captured standard output.
    * Requires the JDK being present on the current system.
    *
    * @param script
    *   A series of java statements to be executed. May refer to declarations in
    *   the declarations parameter.
    * @param declarations
    *   A series of non-public java declarations, no package declaration.
    *   Imports at the beginning are supported.
    */
  def runJava(script: String, declarations: String): String = {
    val cls = "ScriptContainer$"
    val source = s"$declarations\n\npublic class $cls { static { $script } }"

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
