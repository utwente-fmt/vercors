// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases backward-dep-err1
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
    requires a != NULL;
    requires b != NULL;
    requires c != NULL;
    requires Perm(a[i],1/2);
    requires i==0 ==> Perm(a[i],1/2);
    requires i < len-1 ==> Perm(a[i+1],1/2);
    requires Perm(b[i],1/2);
    requires Perm(c[i],write);

    ensures  Perm(a[i],write);
    ensures  Perm(b[i],1/2);
    ensures  Perm(c[i],write);
   @*/
    {
    a[i]=b[i]+1;
    /*@
      S1:if (i>0) {
        recv a!=NULL ** Perm(a[i],1/2) from S2,1;
      }
    @*/
    S2:if (i < len-1) {
      c[i]=a[i+1]+2;
      //@ send a!=NULL ** Perm(a[i+1],1/2) to S1,1;
    }
  }
}
