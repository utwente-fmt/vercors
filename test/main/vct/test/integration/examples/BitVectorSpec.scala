package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class BitVectorSpec extends VercorsSpec {
  vercors should verify using silicon flag "--target" flag "x86_64-linux-unknown" example "concepts/bitvectors/basic.c"
  vercors should verify using silicon flag "--target" flag "x86_64-windows-unknown" example "concepts/bitvectors/basic.c"
  vercors should verify using silicon flag "--target" flag "i686-unknown-unknown" example "concepts/bitvectors/basic.c"
}
