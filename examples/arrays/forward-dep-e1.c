// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases forward-dep-err1
//:: tools silicon
//:: verdict Fail

/*@
  context \pointer(a, len, write);
  context \pointer(b, len, 1/2);
  context \pointer(c, len, write);
@*/
void example(int a[],int b[],int c[],int len){
  for(int i=0;i < len;i++)
   /*@
    requires a != NULL && b != NULL && c != NULL;
    requires Perm(a[i],write);
    ensures  Perm(a[i],1/2);
    ensures  i>0 ==> Perm(a[i-1],1/2);
    ensures  i==len-1 ==> Perm(a[i],1/2);

    context  Perm(b[i],1/2);
    context  Perm(c[i],write);
   @*/
    {

    a[i]=b[i]+1;
    S1:if (i< len-1) {
      //@ send a != NULL ** Perm(a[i],1/2) to S2,1;
    }
    S2:if (i>0) {
      //@ recv a != NULL ** Perm(a[i-1],1/2) from S1,1;
      c[i]=a[i+1]+2;
    }
  }
}
