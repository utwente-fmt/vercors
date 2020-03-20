// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TestHist
//:: tools silicon
//:: verdict Pass

class Testhist {
  
  /*@
    given seq<int> vals;
    context_everywhere a != null && hist != null && a.length == |vals|;
    context_everywhere (\forall* int i; 0 <= i && i < a.length ; Perm(a[i],1\2));
    context_everywhere (\forall* int i; 0 <= i && i < hist.length ; Perm(hist[i],1));
    context_everywhere (\forall int i; 0 <= i && i < a.length ; 0 <= a[i] && a[i] < hist.length);
    context_everywhere (\forall int i; 0 <= i && i < a.length ; a[i] == vals[i] );
    ensures   (\forall int i; 0 <= i && i < hist.length ;
                 hist[i]==\sum({0 .. a.length},\vcmp(vals,\vrep(i))));
  @*/
  public void histogram(int a[],int hist[]){
    int k=0;
    //@ loop_invariant 0 <= k && k <= hist.length;
    /*@ loop_invariant (\forall int i; 0 <= i && i < k ;
         hist[i]==\sum([0 .. 0),\vcmp(vals,\vrep(i))) );
     */
    while(k<hist.length){
      hist[k]=0;
      k++;
    }
    k=0;
    //@ loop_invariant 0 <= k && k <= a.length;
    //@ loop_invariant (\forall int i; 0 <= i && i < hist.length ;
    //@                   hist[i]==\sum([0 .. k),\vcmp(vals,\vrep(i)))  );
    while(k<a.length){
      int v=a[k];
      hist[v]++;
      k++;
    }
  }
  
}

