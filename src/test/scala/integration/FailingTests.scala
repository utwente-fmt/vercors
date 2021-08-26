package integration

import hre.util.TestReport.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow
import viper.silver.ast.Int

import java.io.File
import java.net.URLClassLoader

class FailingTests extends AnyFlatSpec with Matchers {
  //This test may take a long time to verify
  ignore should "fail with silicon and examples/openmp/sections-reduced-fail.c" taggedAs Slow in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/openmp/sections-reduced-fail.c")
    configuration.verdict = Verdict.Fail
    configuration.toolSilicon = true
    configuration.progress = true
    IntegrationTestHelper.test(configuration)
  }

  it should "print classPath" in {
    val classpath = System.getProperty("java.class.path")
    val classpathEntries = classpath.split(File.pathSeparator)

    System.out.println("Start classpath")
    for (entry <- classpathEntries) {
      System.out.println(entry)
    }
    System.out.println("end classpath")
    val t = Int
    //assert(false,classpathEntries)
  }

  it should "pass with silicon and examples/basic/AddAssignJava.java" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/basic/AddAssignJava.java")
    configuration.verdict = Verdict.Pass
    configuration.toolSilicon = true
    IntegrationTestHelper.test(configuration)
  }

}
