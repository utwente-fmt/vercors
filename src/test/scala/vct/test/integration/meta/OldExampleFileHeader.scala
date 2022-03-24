package vct.test.integration.meta

import hre.io.RWFile
import org.scalatest.flatspec.AnyFlatSpec
import vct.test.integration.helper.ExampleFiles

class OldExampleFileHeader extends AnyFlatSpec {
  it must "not mark examples with the old header syntax: it is ignored" in {
    for(f <- ExampleFiles.FILES) {
      if(RWFile(f).readLines().exists(_.stripLeading().startsWith("//::"))) {
        fail(s"File $f contains a line starting with `//::`. This is the old syntax to enter a file into the test suite, but this is no longer used.")
      }
    }
  }
}
