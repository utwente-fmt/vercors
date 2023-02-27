// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenMPinit
//:: tools

/*
  The use of omp_get_thread_num is currently unsupported.
*/

// gcc -fopenmp -c zero-spec.c

#include <stdio.h>
#include <omp.h>


/*@
  requires len > 0;
  context \pointer(a, len, write);
@*/
void init(int len,int a[],int ppid){
  int i;
  #pragma omp parallel for private(i)
  for(i=0;i<len;i++)
  /*@
    context len>0 ** \pointer_index(a, i, write);
  @*/
  {
    int iam = omp_get_thread_num();
    a[i]=ppid*100+iam;
  }
}

//@ requires false;
int main(int argc, char *argv[]){
  int a[]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  int i;
  
  printf("a: ");
  for(i=0;i<16;i++){printf("%4d",a[i]);}
  printf("\n");
  
  printf("init\n");
  init(16,a,1);
  
  printf("a: ");
  for(i=0;i<16;i++){printf("%4d",a[i]);}
  printf("\n");
}


