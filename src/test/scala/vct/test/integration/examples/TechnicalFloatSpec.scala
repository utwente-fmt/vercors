package vct.test.integration.examples

import vct.test.integration.helper.VercorsSpec

class TechnicalFloatSpec extends VercorsSpec {
  // https://github.com/utwente-fmt/vercors/issues/853
  // vercors should verify using silicon example "technical/floats.c"
  vercors should verify using silicon example "technical/floats.java"

  vercors should verify using silicon in "literals of floats and casting pvl" pvl """
      float64 m() {
        assert 0.5 > 0.0;
        float64 highNum = 0.5;
        float32 lowNum = highNum.toFloat32();
        float64 otherHighNum = lowNum.toFloat64();
        float32 num = 0.5f;
        return num;
      }
  """

  vercors should verify using silicon in "addition of floats pvl" pvl """
      float64 m() {
        return 0.5 + 0.6;
      }
  """

  vercors should verify using silicon in "subtraction of floats pvl" pvl """
      float64 m() {
        return 0.5 - 0.6;
      }
  """

  vercors should verify using silicon in "multiplication of floats pvl" pvl """
      float64 m() {
        return 0.5 * 0.6;
      }
  """

  vercors should verify using silicon in "inequality of floats pvl" pvl """
      boolean m() {
        assert 0.5 < 0.6;
        assert (0.6 < 0.5) == false;
        assert 0.5 <= 0.5;
        assert (0.5 <= 0.4) == false;
        assert 0.5 != 0.6;
        assert (0.5 == 0.6) == false;
      }
  """

  vercors should verify using silicon in "literals of floats and casting java" java """
      class C { double m() {
        assert 0.5 > 0.0;
        double highNum = 0.5;
        float lowNum = (float) highNum;
        double otherHighNum = (double) lowNum;
        float num = 0.5f;
        return num;
      } }
  """

  vercors should verify using silicon in "addition of floats java" java """

      class C { double m() {
        return 0.5 + 0.6;
      } }
  """

  vercors should verify using silicon in "subtraction of floats java" java """
      class C { double m() {
        return 0.5 - 0.6;
      } }
  """

  vercors should verify using silicon in "multiplication of floats java" java """
      class C { double m() {
        return 0.5 * 0.6;
      } }
  """

  vercors should verify using silicon in "inequality of floats java" java """
      class C { boolean m() {
        assert 0.5 < 0.6;
        assert (0.6 < 0.5) == false;
        assert 0.5 <= 0.5;
        assert (0.5 <= 0.4) == false;
        assert 0.5 != 0.6;
        assert (0.5 == 0.6) == false;
      } }
  """

  vercors should verify using silicon in "literals of floats and casting c" c """
      double m() {
        //@ assert 0.5 > 0.0;
        double highNum = 0.5;
        float lowNum = (float) highNum;
        double otherHighNum = (double) lowNum;
        float num = 0.5f;
        return num;
      }
  """

  vercors should verify using silicon in "addition of floats c" c """
      double m() {
        return 0.5 + 0.6;
      }
  """

  vercors should verify using silicon in "subtraction of floats c" c """
      double m() {
        return 0.5 - 0.6;
      }
  """

  vercors should verify using silicon in "multiplication of floats c" c """
      double m() {
        return 0.5 * 0.6;
      }
  """

  vercors should verify using silicon in "inequality of floats c" c """
      #include "stdbool.h"
      bool m() {
        //@ assert 0.5 < 0.6;
        //@ assert (0.6 < 0.5) == false;
        //@ assert 0.5 <= 0.5;
        //@ assert (0.5 <= 0.4) == false;
        //@ assert 0.5 != 0.6;
        //@ assert (0.5 == 0.6) == false;
      }
  """
}
