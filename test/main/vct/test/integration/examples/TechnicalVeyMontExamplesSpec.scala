package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalVeyMontExamplesSpec extends VercorsSpec {
  val wd = "technical/veymont"

  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkLTS/ltstest.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkLTS/simpleifelse.pvl"

  (vercors
    should error withCode "resolutionError:seqProgInvocation"
    flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/ConstructorCall.pvl")
  (vercors
    should verify
    using silicon flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/IfCondition.pvl")

  (vercors
    should verify
    using silicon flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/MainConstructorWithArgs.pvl")

  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkMainSyntaxAndWellFormedness/MainMethodCall.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkMainSyntaxAndWellFormedness/NewNonRoleObject.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkMainSyntaxAndWellFormedness/NewRoleObject.pvl"

  (vercors
    should error
    withCode "resolutionError:seqProgInvocation"
    flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/NonRoleMethodCall.pvl")

  (vercors should error
    withCode "resolutionError:seqProgInvocation"
    flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/PureMethodCall.pvl")

  (vercors
    should error withCode "resolutionError:seqProgEndpointAssign"
    flags("--veymont-generate-permissions", "--dev-veymont-allow-assign")
    example s"$wd/checkMainSyntaxAndWellFormedness/RoleFieldAssignment.pvl")

  (vercors
    should error withCode "resolutionError:seqProgStatement"
    flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/WaitStatement.pvl")

  (vercors
    should fail withCode "loopUnanimityNotMaintained"
    using silicon flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/WhileCondition.pvl")

  (vercors
    should error
    withCode "resolutionError:seqProgReceivingEndpoint,seqProgReceivingEndpoint"
    flag "--veymont-generate-permissions" example s"$wd/checkMainSyntaxAndWellFormedness/WrongAssignment.pvl")

  (vercors
    should error withCode "assignNotAllowed"
    flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/WrongSyntax.pvl")

  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTerminationNonMain/AbsenceRecursion.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTerminationNonMain/AbsenceRecursion2.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTerminationNonMain/AbsenceRecursion3.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTerminationNonMain/AbsenceRecursion4.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTerminationNonMain/ForLoop.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTerminationNonMain/LoopStatementInRole.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTerminationNonMain/MainCallFromRole.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTerminationNonMain/WaitStatementInRole.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/OtherClassFieldType.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/OtherClassFieldType2.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/OtherClassMethodType.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/OtherClassMethodType2.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleFieldType.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleFieldType2.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleFieldType3.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleFieldType4.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleMethodType.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleMethodType2.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleMethodType3.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleMethodType4.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleMethodType5.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/VoidField.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/various.pvl"
}
