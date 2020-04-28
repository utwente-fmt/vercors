// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases HistoryApplication

class History {

  int x;
  
  /*@
  modifies x;
  ensures  x==\old(x)+1;
  process incr();
  
  modifies x;
  requires n>=0;
  ensures  x==\old(x)+n;
  process  single(int n)=n>0?incr()*single(n-1):empty;
  
  modifies x;
  requires n>=0 && m >= 0;
  ensures  x==\old(x)+n+m;
  process  dual(int n,int m)=single(n)||single(m);

  requires n >= 0 ;
  ensures  single(n)==(n>0?single(n-1)*incr():empty);
  void lemma(int n);
  
  requires m>=0 && n >= 0 ;
  ensures single(m)*single(n)==single(m+n);
  void single_axiom(int m,int n);
  @*/
  
}

