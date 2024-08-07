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

class FM2024VeyMontPermissionsSpec extends VeyMontSpec {
  val wd = Paths.get("publications/2024/FM2024VeyMontPermissions")

//  {
//    val caseWd = wd.resolve("0-TTT")
//    veymontTest(
//      desc = "TTT case study (choreographic verification)",
//      inputs = examplePaths(
//        caseWd.resolve("Move.pvl"),
//        caseWd.resolve("Player.pvl"),
//        caseWd.resolve("0-TTT.pvl"),
//      ),
//      flags = Seq(
//        "--generate-permissions",
//        // Skip implementation verification. This version of 0-TTT does not support that (time constraints).
//        "--veymont-skip-implementation-verification",
//      ),
//    )
//  }
//
  {
    val caseWd = wd.resolve("1-TTTmsg")
//
//    veymontTest(
//      desc = "TTTmsg case study (choreograpy verification)",
//      inputs = examplePaths(
//        caseWd.resolve("Move.pvl"),
//        caseWd.resolve("Player.pvl"),
//        caseWd.resolve("1-TTTmsg.pvl"),
//      ),
//      flags = Seq(
//        "--dev-unsafe-optimization",
//        "--veymont-skip-implementation-verification",
//      ),
//    )

//    veymontTest(
//      desc = "TTTmsg case study (implementation verification)",
//      inputs = examplePaths(
//        caseWd.resolve("Move.pvl"),
//        caseWd.resolve("Player.pvl"),
//        caseWd.resolve("1-TTTmsg.pvl"),
//      ),
//      flags = Seq(
//        "--dev-unsafe-optimization",
//        "--veymont-skip-choreography-verification",
//      ),
//    )

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
      targetLanguage = Java,
      processImplementation = runTttImplementation,
    )
  }

//  {
//    val caseWd = wd.resolve("2-TTTlast")
//
//    veymontTest(
//      desc = "TTTlast case study (choreography verification)",
//      inputs = examplePaths(
//        caseWd.resolve("Move.pvl"),
//        caseWd.resolve("Player.pvl"),
//        caseWd.resolve("2-TTTlast.pvl"),
//      ),
//      flags = Seq(
//        "--dev-unsafe-optimization",
//        "--veymont-skip-implementation-verification",
//      ),
//    )
//
//    veymontTest(
//      desc = "TTTlast case study (implementation verification)",
//      inputs = examplePaths(
//        caseWd.resolve("Move.pvl"),
//        caseWd.resolve("Player.pvl"),
//        caseWd.resolve("2-TTTlast.pvl"),
//      ),
//      flags = Seq(
//        "--dev-unsafe-optimization",
//        "--veymont-skip-choreography-verification",
//      ),
//    )
//
//    veymontTest(
//      desc = "TTTlast case study (implementation execution)",
//      inputs = examplePaths(
//        caseWd.resolve("Move.pvl"),
//        caseWd.resolve("Player.pvl"),
//        caseWd.resolve("2-TTTlast.pvl"),
//      ),
//      flags = Seq(
//        "--dev-unsafe-optimization",
//        "--veymont-skip-implementation-verification",
//        "--veymont-skip-choreography-verification",
//      ),
//      targetLanguage = Java,
//      processImplementation = runTttImplementation,
//    )
//  }

  /////////////////////
  // Testing helpers //
  /////////////////////

  val patchFile = example(wd.resolve("testFiles/patches.txt"))
  val scriptFile = example(wd.resolve("testFiles/testScript.txt"))
  // Given a TTT implementation in java at path p, patches it to be fully executable
  // by adding implementations and adding hooks such that it is possible to inspect the final
  // state with external code, runs it, and asserts that
  // the output expected is equal to each player just picking the first field
  // location that is available.
  def runTttImplementation(p: Path): Unit = {
    val source = Files.readString(p)
    val patches = Patch.fromFile(patchFile)
    val patched = Patch.applyAll(patches, source)
    val testScript = Files.readString(scriptFile)
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

    info(s"Root: $root")
    println(s"Root: $root")
    info(s"Files in there: ${Files.list(root).toArray.toSeq}")
    println(s"Files in there: ${Files.list(root).toArray.toSeq}")

    // Compile source file.
    val compiler = ToolProvider.getSystemJavaCompiler
    compiler.run(null, null, null, sourceFile.toString)

    info(s"Files in there: ${Files.list(root).toArray.toSeq}")
    println(s"Files in there: ${Files.list(root).toArray.toSeq}")

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
