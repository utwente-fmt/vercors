// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases SummationReduction
//:: suite problem-fail
//:: tools silicon
//:: verdict Pass

float res;

/*@
  given seq<float> ar_values;
  context \pointer(ar, N, 1/2);
  context Perm(res,write);
  context ar_values == \values(ar, 0, N);
  context_everywhere N>0;
  context_everywhere |ar_values| == N;

  ensures  res==(\sum int k ; 0 <= k && k < N ; ar_values[k] );
@*/
void do_sum(int N,float ar[N]){
  res=(float)0;
  for(int i=0;i<N;i++)
    /*@
      context ar != NULL;
      context Perm(ar[i], 1/2);
      context ar_values[i] == ar[i];

      requires Reducible(res, +);
      ensures  Contribution(res, ar_values[i]);
    */
  {
    res+=ar[i];
  }
}
