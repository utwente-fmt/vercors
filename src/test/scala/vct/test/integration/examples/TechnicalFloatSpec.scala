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

  vercors should verify using silicon in "assigning should work" java
  """
    class floatArray {
        void m() {
            //@ ghost double f = 1;
            //@ ghost float g = 1;
            //@ ghost double f = 1.0;
            //@ ghost float g = 1.0f;
        }
    }
  """

  vercors should verify using silicon in "some float test cases" java
  """
    class C {
      void m() {
        //@ ghost float[] a = {1, 2, 3};
        //@ ghost float[] b = new float[3];
        //@ ghost b[0] = 1;
        //@ ghost float f = 1;
      }
    }
  """

  vercors should verify using silicon in "some more float test cases" java
    """
    class FloatTest {
      float f;
      double d;
      //@ ghost float gf = 1;

      //@ context Perm(gf, write);
      //@ requires Perm(f, write) ** Perm(d, write);
      //@ requires in > 0;
      //@ ensures gf != in;
      void m(float in) {
          f = 1;
          f = 1337;
          f = -1;
          f = 0;
          f = -3.45f;
          f = .42f;
          assert f >= .4f;
          d = 1;
          d = 1337;
          d = -1;
          d = 0;
          d = -3.45;
          d = .42;
          assert d >= .4;
          //@ ghost gf = 1;
          //@ ghost gf = 1337;
          //@ ghost gf = -1;
          //@ ghost gf = 0;
          //@ ghost gf = -3.45f;
          //@ ghost gf = .42f;
          //@ assert gf >= .4f;
          //@ ghost gf = in + 1;
    }
  }
  """
}
