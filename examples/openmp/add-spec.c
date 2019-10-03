// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenMPadd
//:: suite puptol
//:: tools silicon
/*
 * Demonstrates how two loops that must be fused to be
 * data race free can be specified and verified.
 */
#include <stdio.h>
#include <omp.h>

/*@
  context \pointer(a, len, 1f/2) ** \pointer(b, len, 1f/2) ** \pointer(c, len, write);
  ensures   (\forall  int k;0 <= k && k < len ; c[k]==a[k]+b[k]);
@*/
void add(int len,int a[],int b[],int c[]){
  #pragma omp parallel
  {
    /*@
      context \pointer(a, len, 1f/2) ** \pointer(b, len, 1f/2) ** \pointer(c, len, write);
      ensures (\forall  int k;0 <= k && k < len ; c[k]==a[k]+b[k]);
    @*/
    #pragma omp for schedule(static) nowait
    for(int i=0;i<len;i++)
    /*@
      context a != NULL && c != NULL;
      context Perm(c[i],1) ** Perm(a[i],1f/2);
      ensures c[i] == a[i];
    @*/
    {
      c[i]=a[i];
    }
    #pragma omp for schedule(static)
    for(int i=0;i<len;i++)
    /*@
      context b != NULL && c != NULL;
      context Perm(c[i],1) ** Perm(b[i],1f/2);
      ensures c[i] == \old(c[i]) + b[i];
    @*/
    {
      c[i]=c[i]+b[i];
    }
  }
}

//@ requires false;
int main(int argc, char *argv[]){
  int a[]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  int b[]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  int c[]={-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16};
  int i;

  printf("a: ");
  for(i=0;i<16;i++){printf("%4d",a[i]);}
  printf("\n");
  printf("b: ");
  for(i=0;i<16;i++){printf("%4d",b[i]);}
  printf("\n");

  printf("add\n");
  add(16,a,b,c);

  printf("c: ");
  for(i=0;i<16;i++){printf("%4d",c[i]);}
  printf("\n");
}
