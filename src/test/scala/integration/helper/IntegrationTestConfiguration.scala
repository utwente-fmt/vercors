package integration.helper

import hre.util.TestReport.Verdict

case class IntegrationTestConfiguration(
                                         var file: String = "",
                                         var verdict: Verdict = Verdict.Pass,
                                         var toolSilicon: Boolean = false,
                                         var toolCarbon: Boolean = false,
                                         var toolVeymont: Boolean = false
                                       ) {

}
