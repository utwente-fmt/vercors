package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class DemoSpec extends VercorsSpec {
  vercors should verify using silicon example "demo/demo1.pvl"
  vercors should verify using silicon example "demo/demo2.pvl"
  vercors should verify using silicon example "demo/demo3a.pvl"
  vercors should verify using silicon example "demo/demo3a-func.pvl"
//  vercors should verify using silicon example "demo/demo3a-func-with-lemmas.pvl"
  vercors should verify using silicon example "demo/demo3b.pvl"
  vercors should verify using silicon example "demo/demo3c.pvl"
  vercors should verify using silicon example "demo/demo3d.pvl"
  vercors should verify using silicon example "demo/demo4.pvl"
}
