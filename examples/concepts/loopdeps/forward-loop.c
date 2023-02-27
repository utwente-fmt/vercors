// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ForwardDep
//:: tools silicon
//:: verdict Pass

/*@
  context_everywhere a != NULL && b != NULL && c != NULL;
  context \pointer(a, len, write);
  context \pointer(b, len, 1\2);
  context \pointer(c, len, write);

  requires (\forall int tid; 0 <= tid && tid < len ; b [ tid ] == tid);

  ensures  (\forall int i; 0 <= i && i < len ;  a[i] == i+1);
  ensures  (\forall int i; 0 <= i && i < len ;  b[i] == i  );
  ensures  (\forall int i; 0  < i && i < len ;  c[i] == i+2);
@*/
void example(int a[],int b[],int c[],int len){
  for(int i=0;i < len;i++) /*@
    requires Perm(&a[i],write);
    requires Perm(&b[i],1\2);
    requires Perm(&c[i],write);

    ensures Perm(&a[i],1\2);
    ensures Perm(&b[i],1\2);
    ensures Perm(&c[i],write);

    requires b[i]==i;

    ensures  i>0 ==> Perm(&a[i-1],1\2);
    ensures  i==len-1 ==> Perm(&a[i],1\2);

    ensures  a[i]==i+1;
    ensures  b[i]==i;
    ensures  i>0 ==> c[i]==i+2;
  @*/ {
    a[i]=b[i]+1;
    //@ send S, 1: 0 <= i ** i < len ** Perm(&a[i],1\2) ** a[i]==i+1;
    //@ recv S;
    S2:if (i>0) {
      c[i]=a[i-1]+2;
    }
  }
}
