// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenMPaddSimd
// suite puptol
// tools silicon

/*
 * Trivial for-simd example.
 * 
 */
#include <stdio.h>
#include <omp.h>

/*@
  context_everywhere a != NULL && b != NULL && c != NULL;
  context_everywhere len>0 && (len % 4 == 0) && \length(a)==len && \length(b)==len && \length(c)==len;
  context   (\forall* int k;0 <= k && k < len ; Perm(a[k],1/2));
  context   (\forall* int k;0 <= k && k < len ; Perm(b[k],1/2));
  context   (\forall* int k;0 <= k && k < len ; Perm(c[k],1));
  ensures   (\forall  int k;0 <= k && k < len ; c[k]==a[k]+b[k]);
@*/
void add(int len,int a[],int b[],int c[]){
  #pragma omp parallel
  {
    /*@
      context (\forall* int k;0 <= k && k < len ; Perm(a[k],1/2));
      context (\forall* int k;0 <= k && k < len ; Perm(b[k],1/2));
      context (\forall* int k;0 <= k && k < len ; Perm(c[k],1));
      ensures (\forall  int k;0 <= k && k < len ; c[k]==a[k]+b[k]);
    @*/
    #pragma omp for simd schedule(static) simdlen(4)
    for(int i=0;i<len;i++)
    /*@
      context Perm(c[i],1) ** Perm(b[i],1/2) ** Perm(a[i],1/2);
      ensures c[i] == a[i] + b[i];
    @*/
    {
      c[i]=a[i]+b[i];
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

