package integration.meta

import integration.`new`.{AbruptExamplesSpec, AlgorithmExamplesSpec, ArrayExamplesSpec, BasicExamplesSpec, CIncludeSpec, ClassesSpec, CounterSpec, DemoSpec, ForkJoinSpec, GotoSpec, GpgpuSpec, JavaImportSpec, LoopDependencySpec, MapsSpec, ModelsSpec, OpenMPSpec, ParallelSpec, PermissionSpec, PredicatesSpec, PublicationsSpec, RefuteSpec, SequencesSpec, SetsSpec, SilverDomainSpec, SummationSpec, TechnicalAbruptSpec, TechnicalSpec, TechnicalVeymontSpec, TypeValuesSpec, VerifyThisSpec, VeymontSpec, WaitNotifySpec, WandSpec}
import integration.helper.VercorsSpec
import org.scalatest.flatspec.AnyFlatSpec

class ExampleCoverage extends AnyFlatSpec {
  it should "cover all examples in the examples directory" in {
    val specs: Seq[VercorsSpec] = Seq(
      new AbruptExamplesSpec(),
      new AlgorithmExamplesSpec(),
      new ArrayExamplesSpec(),
      new BasicExamplesSpec(),
      new CIncludeSpec(),
      new ClassesSpec(),
      new DemoSpec(),
      new JavaImportSpec(),
      new SilverDomainSpec(),
      new ForkJoinSpec(),
      new GotoSpec(),
      new ModelsSpec(),
      new GpgpuSpec(),
      new LoopDependencySpec(),
      new CounterSpec(),
      new MapsSpec(),
      new OpenMPSpec(),
      new ParallelSpec(),
      new PermissionSpec(),
      new PredicatesSpec(),
      new PublicationsSpec(),
      new RefuteSpec(),
      new SequencesSpec(),
      new SetsSpec(),
      new SummationSpec(),
      new TechnicalSpec(),
      new TechnicalAbruptSpec(),
      new TechnicalVeymontSpec(),
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
