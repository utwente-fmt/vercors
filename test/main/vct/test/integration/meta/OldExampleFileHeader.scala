package vct.test.integration.meta

import hre.io.RWFile
import org.scalatest.flatspec.AnyFlatSpec
import vct.test.integration.helper.ExampleFiles

class OldExampleFileHeader extends AnyFlatSpec {
  it must "not mark examples with the old header syntax: it is ignored" in {
    // PB TODO: We should clean up the example description at some point, but it's not a big priority.
    cancel()

    val wrongFiles = ExampleFiles.FILES.map(RWFile).filter(_.readLines().exists(_.stripLeading().startsWith("//::")))

    for(f <- wrongFiles) {
      println(s"File ${f.fileName} contains a line starting with `//::`. This is the old syntax to enter a file into the test suite, but this is no longer used.")
    }

    if(wrongFiles.nonEmpty) {
      fail("Failing due to errors above")
    }
  }
}
