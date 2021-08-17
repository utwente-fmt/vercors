package integration

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import vct.main.Main

import java.nio.file.Path
import java.util


class BasicSpec extends AnyFlatSpec with Matchers {

  "VerCors" should "add to variables" in {
    val a = 2
    val b = 3
    val expectedResult = 5
    val result = a+b
    assert(result==expectedResult)
  }
  //todo
  //split main
  //stderr out should be allowed for testing
  //listen in system
  //make generic class for testing
  //make integrationTestConfiguration
  //create one place for all the settings (Maybe split it in multiple objects

  "VerCors" should "run basic/AddAssignJava.java" in {
    val file = "examples/basic/AddAssignJava.java"
    val arguments = new util.ArrayList[String]
    arguments.add("--progress")
    arguments.add("--silicon")
    arguments.add("--strict-internal")
    arguments.add("--check-history")
    arguments.add(file)
    val main = new Main()
    val argumentArray = new Array[String](arguments.size)
    arguments.toArray(argumentArray)

    main.run(argumentArray)
  }

}
