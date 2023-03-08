package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class WandSpec extends VercorsSpec {
  vercors should verify using silicon example "concepts/wand/ListAppend.java"
  vercors should verify using silicon example "concepts/wand/ListAppendASyncDef.java"
  vercors should verify using silicon example "concepts/wand/ListAppendASyncDefInline.java"
  vercors should verify using silicon example "concepts/wand/TreeRecursiveSilver.java"
  vercors should verify using silicon example "concepts/wand/TreeWandSilver-e1.java"
  vercors should verify using silicon example "concepts/wand/TreeWandSilver-e2.java"
  vercors should verify using silicon example "concepts/wand/TreeWandSilver.java"
  vercors should verify using silicon example "concepts/wand/WandDemoSilver.java"
}
