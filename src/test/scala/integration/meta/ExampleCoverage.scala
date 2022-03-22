package integration.meta

import integration.`new`.{AbruptExamplesSpec, AlgorithmExamplesSpec, ArrayExamplesSpec, BasicExamplesSpec}
import integration.helper.VercorsSpec
import org.scalatest.flatspec.AnyFlatSpec

class ExampleCoverage extends AnyFlatSpec {
  it should "cover all examples in the examples directory" in {
    val specs: Seq[VercorsSpec] = Seq(
      new AbruptExamplesSpec(),
      new AlgorithmExamplesSpec(),
      new ArrayExamplesSpec(),
      new BasicExamplesSpec(),
    )

    val testedFiles = specs.flatMap(_.coveredExamples).map(_.toFile).toSet

    var shouldFail = false

    for(f <- ExampleFiles.FILES) {
      if(!testedFiles.contains(f)) {
        shouldFail = true
        println(s"Not tested: $f")
      }
    }

    fail("The test suite does not have a test entry that processes the above files.")
  }
}
