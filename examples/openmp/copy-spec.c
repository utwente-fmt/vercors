// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenMPcopy
//:: tools silicon

/*
 * Array copy using parallel for loop in OpenMP.
 */

#include <stdio.h>
#include <omp.h>

/*@
  context \pointer(a, len, write) ** \pointer(b, len, 1/2);
  ensures   (\forall  int k;0 <= k && k < len ; a[k]==b[k]);
  ensures   (\forall  int k;0 <= k && k < len ; b[k]==\old(b[k]));
@*/
void copy(int len,int a[],int b[]){
  int i;
  #pragma omp parallel for private(i)
  for(i=0;i<len;i++)
  /*@
    context a != NULL && b != NULL;
    context Perm(a[i],1) ** Perm(b[i],1/4);
    ensures a[i] == b[i];
  @*/
  {
    a[i]=b[i];
  }
}

//@ requires false;
int main(int argc, char *argv[]){
  int a[]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  int b[]={-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16};
  int i;

  printf("a: ");
  for(i=0;i<16;i++){printf("%4d",a[i]);}
  printf("\n");
  printf("b: ");
  for(i=0;i<16;i++){printf("%4d",b[i]);}
  printf("\n");

  printf("copy\n");
  copy(16,a,b);

  printf("a: ");
  for(i=0;i<16;i++){printf("%4d",a[i]);}
  printf("\n");
  printf("b: ");
  for(i=0;i<16;i++){printf("%4d",b[i]);}
  printf("\n");
}
