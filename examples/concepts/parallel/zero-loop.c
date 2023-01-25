// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: cases ZeroLoop-C
//:: tools silicon
//:: verdict Pass

/*@
  context \pointer(ar, len, write);
  ensures  (\forall int k ; 0 <= k && k < len ; ar[k]==0 );
@*/
void zero_array(int ar[],int len){
    for(int i=0;i < len;i++)
       /*@
        context ar != NULL;
        context Perm(&ar[i], write);
        ensures ar[i] == 0;
       @*/
    {
        ar[i]=0;
    }
}
