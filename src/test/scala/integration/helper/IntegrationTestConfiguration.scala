package integration.helper

import hre.util.TestReport.Verdict

case class IntegrationTestConfiguration(
                                         var files: Array[String] = Array(),
                                         var verdict: Verdict = Verdict.Pass,
                                         var toolSilicon: Boolean = false,
                                         var toolCarbon: Boolean = false,
                                         var toolVeymont: Boolean = false,
                                         var disableSat: Boolean = false,
                                         var checkHistory: Boolean = false,
                                         var checkDefined: Boolean = false,
                                         var checkAxioms: Boolean = false,
                                         var stopBeforeBackend: Boolean = false,
                                         var progress: Boolean = false,
                                       ) {

}
