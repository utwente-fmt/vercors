package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

// TODO (RR): Remove the veymont dir from ExampleFiles.scala

class TechnicalVeyMontExamplesSpec extends VercorsSpec {
  val wd = "technical/veymont"

  // TODO (RR): Re-enable tests asap
  // vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkLTS/ltstest.pvl"
  // vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkLTS/simpleifelse.pvl"

  (vercors
    should error withCode "resolutionError:seqProgInvocation"
    flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/ConstructorCall.pvl")
  // (vercors
  //   should verify
  //   using silicon flag "--veymont-generate-permissions"
  //   example s"$wd/checkMainSyntaxAndWellFormedness/IfCondition.pvl")

  (vercors
    should verify
    using silicon flag "--veymont-generate-permissions"
    example s"$wd/checkMainSyntaxAndWellFormedness/MainConstructorWithArgs.pvl")

  // vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkMainSyntaxAndWellFormedness/MainMethodCall.pvl"
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

  // (vercors
  //   should error
  //   withCode "resolutionError:seqProgReceivingEndpoint,seqProgReceivingEndpoint"
  //   flag "--veymont-generate-permissions" example s"$wd/checkMainSyntaxAndWellFormedness/WrongAssignment.pvl")

  // (vercors
  //   should error withCode "assignNotAllowed"
  //   flag "--veymont-generate-permissions"
  //   example s"$wd/checkMainSyntaxAndWellFormedness/WrongSyntax.pvl")

  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleFieldType2.pvl"
  vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/checkTypesNonMain/RoleMethodType4.pvl"

  // vercors should verify using silicon flag "--veymont-generate-permissions" example s"$wd/various.pvl"
}
