package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class AtomicsSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/atomics/SingleCell.java"
  vercors should verify using silicon example "concepts/atomics/RBProdCons.java"
  vercors should verify using silicon example "concepts/atomics/RBLock.java"
  vercors should verify using silicon example "concepts/atomics/CountDownLatch.java"
  vercors should verify using silicon example "concepts/atomics/ReentractLock.java"

}
