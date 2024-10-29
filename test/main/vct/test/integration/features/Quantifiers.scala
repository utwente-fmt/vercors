package vct.test.integration.features

import vct.test.integration.helper.VercorsSpec

class Quantifiers extends VercorsSpec {

  vercors should verify using silicon in "correct order of quantifier rewrite (issue  #1215)" c """
/*@
  requires ant1 != NULL && \pointer_length(ant1) == _n_vis;
  requires (\forall* int i; 0<=i && i< \pointer_length(ant1); Perm(&ant1[i], write));
  requires (\forall int _0; 0 <= _0 && _0 < _n_vis; 0 <= ant1[_0]);
  requires _n_vis == 230930;
@*/
  int main(int *ant1, int _n_vis) {
  }
  """

}
