// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenMPSectionReduced
//:: tools silicon
//:: suite puptol
//:: verdict Pass
/*
 * Demonstrates how two loops that must be fused to be
 * data race free can be specified and verified.
 */
#include <stdio.h>
#include <omp.h>


/*@
  context \pointer(a, len, 1\2) ** \pointer(b, len, 1\2);
  context \pointer(c, len, write) ** \pointer(d, len, write);
  ensures   (\forall  int k;0 <= k && k < len ; c[k]==a[k]+b[k]);
  ensures   (\forall  int k;0 <= k && k < len ; d[k]==a[k]*b[k]);
@*/
void addmul(int len,int a[],int b[],int c[], int d[])
{
#pragma omp parallel
{
/*@
  context \pointer(a, len, 1\2) ** \pointer(b, len, 1\2);
  context \pointer(c, len, write) ** \pointer(d, len, write);
  ensures   (\forall  int k;0 <= k && k < len ; c[k]==a[k]+b[k]);
  ensures   (\forall  int k;0 <= k && k < len ; d[k]==a[k]*b[k]);
@*/
#pragma omp sections
{
#pragma omp section
  {
#pragma omp parallel
{
    #pragma omp for schedule(static) nowait
   for(int i=0;i<len;i++)
    /*@
      context \pointer_index(c, i, 1) ** \pointer_index(a, i, 1\4);
      ensures c[i] == a[i];
    @*/
    {
      c[i]=a[i];
    }
    #pragma omp for schedule(static)
    for(int i=0;i<len;i++)
    /*@
      context \pointer_index(c, i, 1) ** \pointer_index(b, i, 1\4);
      ensures c[i] == \old(c[i]) + b[i];
    @*/
    {
      c[i]=c[i]+b[i];
    }
  }//parallel
}//section
#pragma omp section
  {
#pragma omp parallel
{
    #pragma omp for schedule(static) nowait
    for(int i=0;i<len;i++)
    /*@
      context \pointer_index(d, i, 1) ** \pointer_index(a, i, 1\4);
      ensures d[i] == a[i];
    @*/
    {
      d[i]=a[i];
    }
    #pragma omp for schedule(static)
    for(int i=0;i<len;i++)
    /*@
      context \pointer_index(d, i, 1) ** \pointer_index(b, i, 1\4);
      ensures d[i] == \old(d[i]) * b[i];
    @*/
    {
      d[i]=d[i]*b[i];
    }
  } //parallel
}//section
}//sections
}//parallel
}//method

//@ requires false;
int main(int argc, char *argv[]){
  int a[]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  int b[]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  int c[]={-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16};
  int d[]={-1,-2,-3,-4,-5,-6,-7,-8,-9,-10,-11,-12,-13,-14,-15,-16};


  int i;

  printf("a: ");
  for(i=0;i<16;i++){printf("%4d",a[i]);}
  printf("\n");
  printf("b: ");
  for(i=0;i<16;i++){printf("%4d",b[i]);}
  printf("\n");

  addmul(16,a,b,c,d);

  printf("c: ");
  for(i=0;i<16;i++){printf("%4d",c[i]);}
  printf("\n");

 printf("d: ");
  for(i=0;i<16;i++){printf("%4d",d[i]);}
  printf("\n");

}
