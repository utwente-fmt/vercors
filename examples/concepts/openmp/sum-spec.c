// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenMPsum
//:: tools


/*

This is correct openMP code, but it is difficult to encode into PVL.
The problem is that there are len logical thread that are
mapped to an unknown number of physical threads.
The local variables tmp are physical thread-local variables.
They satisfy the invariant that the sum of all tmp
variables is the sum of the processed array elements.


*/
#include <omp.h>
#include <stdio.h>

/*@
  context_everywhere a != NULL;
  requires \pointer_length(a) >= len;
@*/
int sum(int len,int a[]){
  int i;
  int res=0;
  #pragma omp parallel private(i) reduction(+:res)
  {
    int tmp=0;
    #pragma omp for nowait
    for(i=0;i<len;i++)
    {
      tmp+=a[i];
      a[i]=0;
    }
    printf("share is %d\n",tmp);
    res+=tmp;
  }
  return res;
}


//@ requires false;
int main(int argc, char *argv[]){
  int a[]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  int i,tmp;

  printf("a: ");
  for(i=0;i<16;i++){printf("%4d",a[i]);}
  printf("\n");

  tmp=sum(16,a);
  printf("sum is %d\n",tmp);
}
