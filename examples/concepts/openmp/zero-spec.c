// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenMPzero
//:: suite puptol
//:: tools silicon
/*
 * Using a parallel for loop in OpenMP to blank an array.
 */
#include <stdio.h>
#include <omp.h>

/*@
  context \pointer(a, len, write);
  ensures   (\forall  int k;0 <= k && k < len ; a[k] == 0 );
@*/
void zero(int len,int a[]){
  int i;
  #pragma omp parallel for private(i)
  for(i=0;i<len;i++)
  /*@
    context a != NULL;
    context Perm(a[i],1);
    ensures a[i] == 0;
  @*/
  {
    a[i]=0;
    //@ spec_ignore {
    printf("item %d done by thread %d.\n",i,omp_get_thread_num());
    //@ spec_ignore }
  }
}

//@ requires false;
int main(int argc, char *argv[]){
  int a[]={1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16};
  int i;

  printf("a: ");
  for(i=0;i<16;i++){printf("%4d",a[i]);}
  printf("\n");

  printf("zero\n");
  zero(16,a);

  printf("a: ");
  for(i=0;i<16;i++){printf("%4d",a[i]);}
  printf("\n");
}
