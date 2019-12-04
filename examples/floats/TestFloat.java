// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases TestFloat
// tools silicon
// verdict Pass TestFloat.main TestFloat.add TestFloat.prefixsum

class TestFloat {

  public static void main(String args[]){
    float a,b,c,d;
    
    a=(float)1;
    b=(float)2;
    c=a+b;
    d=b+a;
    
    //@ assert c == d;
    
    /*@
      seq<float> s=seq<float>{};
      
      assert \sum([0 .. 0),s)==((float)0);
     
     
      
      seq<seq<float>> M=seq<seq<float>>{seq<float>{(float)1}};
      
      assert \sum( [0 .. 0), M[0] ) == ((float)0);
      assert \sum( [0 .. 1), M[0] ) == ((float)1);
      assert \msum([0 .. 0) * [0 .. 1),M) == ((float)0);
      assert \msum([0 .. 1) * [0 .. 1),M) == ((float)1);
    */
  }

  /*@
    given seq<float> vals;
    context_everywhere a!=null ** (a.length == |vals|);
    context_everywhere (\forall* int i; 0 <= i && i < a.length;PointsTo(a[i],1\2,vals[i]));
    ensures \result == \sum([0 .. a.length),vals);
  @*/
  public float add(float a[]){
    int k=0;
    float res=(float)0;
    //@ loop_invariant 0 <= k && k <= a.length;
    //@ loop_invariant \sum(([0 ..k)),vals) == res;
    while(k<a.length){
      res=res+a[k];
      k=k+1;
    }
    return res;
  }

  /*@
    given seq<float> vals;
    context_everywhere a!=null ** (a.length == |vals|);
    context_everywhere b!=null ** (a.length == b.length);
    context_everywhere (\forall* int i; 0 <= i && i < a.length;PointsTo(a[i],1\2,vals[i]));
    context_everywhere (\forall* int i; 0 <= i && i < a.length;Perm(b[i],1));
    ensures (\forall int i ; 0 <= i && i < b.length ; b[i] == \sum([0 ..i),vals));
  @*/
  public void prefixsum(float a[],float b[]){
    int k=0;
    float res=(float)0;
    //@ loop_invariant 0 <= k && k <= a.length;
    //@ loop_invariant \sum([0 ..k),vals) == res;
    //@ loop_invariant (\forall int i ; 0 <= i && i < k ; b[i] == \sum([0 ..i),vals));
    while(k<a.length){
      b[k]=res;
      res=res+a[k];
      k=k+1;
    }
  }
 
  /*@
    given seq<int> vals;
    context_everywhere a!=null ** (a.length == |vals|);
    context_everywhere b!=null ** (a.length == b.length);
    context_everywhere (\forall* int i; 0 <= i && i < a.length;PointsTo(a[i],1\2,vals[i]));
    context_everywhere (\forall* int i; 0 <= i && i < a.length;Perm(b[i],1));
    ensures (\forall int i ; 0 <= i && i < b.length ; b[i] == \sum([0 ..i),vals));
  @*/
  public void prefixisum(int a[],int b[]){
    int k=0;
    int res=0;
    //@ loop_invariant 0 <= k && k <= a.length;
    //@ loop_invariant \sum([0 ..k),vals) == res;
    //@ loop_invariant (\forall int i ; 0 <= i && i < k ; b[i] == \sum([0 ..i),vals));
    while(k<a.length){
      b[k]=res;
      res=res+a[k];
      k=k+1;
    }
  }
 
}

