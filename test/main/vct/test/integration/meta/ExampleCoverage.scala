package vct.test.integration.meta

import org.scalatest.flatspec.AnyFlatSpec
import vct.test.integration.examples._
import vct.test.integration.helper._

class ExampleCoverage extends AnyFlatSpec {
  it should "cover all examples in the examples directory" in {
    val specs: Seq[VercorsSpec] = Seq(
      new AbruptExamplesSpec(),
      new AlgorithmExamplesSpec(),
      new ArrayExamplesSpec(),
      new BasicExamplesSpec(),
      new CIncludeSpec(),
      new ClassesSpec(),
      new CounterSpec(),
      new CPPSpec(),
      new DemoSpec(),
      new FinalConstExprSpec(),
      new ExtractSpec(),
      new ForkJoinSpec(),
      new GotoSpec(),
      new GpgpuSpec(),
      new JavaBipSpec(),
      new JavaImportSpec(),
      new LoopDependencySpec(),
      new MapsSpec(),
      new ModelsSpec(),
      new OpenMPSpec(),
      new ParallelSpec(),
      new PermissionSpec(),
      new PermutationSpec(),
      new PointerSpec(),
      new PredicatesSpec(),
      new PublicationsSpec(),
      new RefuteSpec(),
      new SequencesSpec(),
      new SetsSpec(),
      new SilverDomainSpec(),
      new SmtSpec(),
      new SummationSpec(),
      new SYCLSpec(),
      new TechnicalAbruptSpec(),
      new TechnicalEnumSpec(),
      new TechnicalFloatSpec(),
      new TechnicalJavaBipSpec(),
      new TechnicalJavaSpec(),
      new TechnicalSpec(),
      new TechnicalStaticSpec(),
      new TechnicalVeyMontSpec(),
      new TerminationSpec(),
      new TypeValuesSpec(),
      new VerifyThisSpec(),
      new VeymontSpec(),
      new WaitNotifySpec(),
      new WandSpec(),
    )

    val testedFiles = specs.flatMap(_.coveredExamples).map(_.toFile).toSet

    var shouldFail = false

    for(f <- ExampleFiles.FILES) {
      if(!testedFiles.contains(f)) {
        shouldFail = true
        println(s"Not tested: $f")
      }
    }

    if(shouldFail) fail("The test suite does not have a test entry that processes the above files.")
  }
}
