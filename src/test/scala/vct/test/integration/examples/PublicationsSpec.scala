package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class PublicationsSpec extends VercorsSpec {
  // https://github.com/utwente-fmt/vercors/discussions/829
  // vercors should verify using silicon example "publications/2016/layers/LFQ.java"
  // vercors should verify using silicon example "publications/2016/layers/LFQHist.java"

  // https://github.com/utwente-fmt/vercors/issues/815
  // vercors should verify using silicon example "publications/2021/parallelBellman-Ford/ABFS.pvl"
  // vercors should verify using silicon example "publications/2021/parallelBellman-Ford/ASSSP.pvl"
  // vercors should verify using silicon example "publications/2022/PrefixSumCUDA/Blelloch.cu"
  // vercors should verify using silicon example "publications/2022/PrefixSumCUDA/Kogge-Stone.cu"
  // vercors should verify using silicon example "publications/2022/PrefixSumCUDA/StreamCompaction.cu"

  // https://github.com/utwente-fmt/vercors/issues/852
  // vercors should verify using silicon example "publications/2021/parallelBellman-Ford/ASSSP.cu"


  vercors should verify using silicon example "publications/2021/parallelBellman-Ford/ASSSP-Lemmas.pvl"
}
