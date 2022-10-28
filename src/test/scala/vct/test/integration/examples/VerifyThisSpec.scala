package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class VerifyThisSpec extends VercorsSpec {
  vercors should verify using silicon example "verifythis/2012/lcp.pvl"
  vercors should verify using silicon example "verifythis/2015/relaxed_prefix.pvl"
  // vercors should verify using silicon example "verifythis/2017/other_attempts/PairInsertionSort.java"
  // vercors should verify using silicon example "verifythis/2017/other_attempts/kadane.java"
  // vercors should verify using silicon example "verifythis/2017/other_attempts/kadane2D.java"
  // vercors should verify using silicon example "verifythis/2017/submission/VerCors_challenge3.pvl"
  // vercors should verify using silicon example "verifythis/2017/submission/VerCors_challenge4.pvl"
  // vercors should verify using silicon example "verifythis/2017/submission/Vercors_challenge3_2D.pvl"
  vercors should verify using silicon example "verifythis/2017/submission/challenge1.pvl"
  vercors should verify using silicon example "verifythis/2018/challenge2.pvl"
  vercors should verify using silicon example "verifythis/2019/challenge1.pvl"
  vercors should verify using silicon example "verifythis/2021/TeamBlue/Challenge1.pvl"
  // vercors should verify using silicon example "verifythis/2021/TeamBlue/Challenge2.pvl"
  // vercors should verify using silicon example "verifythis/2021/TeamBlue/Challenge3.pvl"
}
