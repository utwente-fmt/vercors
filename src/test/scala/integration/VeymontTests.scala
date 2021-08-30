package integration

import hre.util.Verdict
import integration.helper.{IntegrationTestConfiguration, IntegrationTestHelper}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers



class VeymontTests extends AnyFlatSpec with Matchers {

  it should "pass with veymont and examples/veymont-check/checkLTS/ltstest.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkLTS/ltstest.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkLTS/simpleifelse.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkLTS/simpleifelse.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkLTS/simplemethodcall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkLTS/simplemethodcall.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkLTS/simplewhile.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkLTS/simplewhile.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/MainConstructorArg.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/MainConstructorArg.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoMain.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoMain.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoMainConstructor.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoMainConstructor.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoMainMethod.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoMainMethod.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoMainMethod2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoMainMethod2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoRunArg.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoRunArg.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/NoRunMethod.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/NoRunMethod.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/OtherMainConstructor.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/OtherMainConstructor.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/TwoMainConstructors.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/TwoMainConstructors.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainClass/WrongRunType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainClass/WrongRunType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorAssignmentWrongType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorAssignmentWrongType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorAssignsNoRole.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorAssignsNoRole.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorBlock.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorBlock.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment3.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorNonRoleAssignment4.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainConstructor/MainConstructorWrongRoleNr.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainConstructor/MainConstructorWrongRoleNr.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/ConstructorCall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/ConstructorCall.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/ConstructorCall2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/ConstructorCall2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/ForLoop.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/ForLoop.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/GuardedRecursion.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/GuardedRecursion.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/GuardedRecursion2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/GuardedRecursion2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/IfCondition.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/IfCondition.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/MainMethodCall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/MainMethodCall.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/NewNonRoleObject.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/NewNonRoleObject.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/NewRoleObject.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/NewRoleObject.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/NonRoleMethodCall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/NonRoleMethodCall.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/PureMethodCall.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/PureMethodCall.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/RoleFieldAssignment.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/RoleFieldAssignment.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WaitStatement.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WaitStatement.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WhileCondition.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WhileCondition.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongAssignment.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongAssignment.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongAssignment2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongAssignment2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongSyntax.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkMainSyntaxAndWellFormedness/WrongSyntax.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/AbsenceRecursion.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/AbsenceRecursion.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/AbsenceRecursion2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/AbsenceRecursion2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTerminationNonMain/AbsenceRecursion3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/AbsenceRecursion3.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/AbsenceRecursion4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/AbsenceRecursion4.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/ForLoop.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/ForLoop.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/LoopStatementInRole.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/LoopStatementInRole.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTerminationNonMain/MainCallFromRole.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/MainCallFromRole.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTerminationNonMain/WaitStatementInRole.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTerminationNonMain/WaitStatementInRole.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/OtherClassFieldType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/OtherClassFieldType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/OtherClassFieldType2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/OtherClassFieldType2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/OtherClassMethodType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/OtherClassMethodType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/OtherClassMethodType2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/OtherClassMethodType2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/RoleFieldType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleFieldType.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTypesNonMain/RoleFieldType2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleFieldType2.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/RoleFieldType3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleFieldType3.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTypesNonMain/RoleFieldType4.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleFieldType4.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "pass with veymont and examples/veymont-check/checkTypesNonMain/RoleMethodType.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleMethodType.pvl")
    configuration.verdict = Verdict.Pass
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/RoleMethodType2.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleMethodType2.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/RoleMethodType3.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/RoleMethodType3.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

  it should "error with veymont and examples/veymont-check/checkTypesNonMain/VoidField.pvl" in {
    val configuration = IntegrationTestConfiguration()
    configuration.files = Array("examples/veymont-check/checkTypesNonMain/VoidField.pvl")
    configuration.verdict = Verdict.Error
    configuration.toolVeymont = true
    IntegrationTestHelper.test(configuration)
  }

}
