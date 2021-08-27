// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenMPotherTest
//:: tools

#include <stdio.h>
#include <omp.h>
#include <unistd.h>

extern void zero(int len,int ar[]);
extern void init(int len,int ar[],int ppid);

int main(int argc, char *argv[]){
  int i;
  
  omp_set_nested(2);
  
  #pragma omp parallel num_threads(2) private(i) 
  {
    int iam = omp_get_thread_num();
    printf("init %d\n",iam);
    #pragma omp for private(i)
    for(i=0;i<8;i++){
      printf(" %d doing %d\n",iam,i);
    }
    #pragma omp parallel for num_threads(4) private(i) shared(iam)
    for(i=0;i<8;i++){
      int iam2=omp_get_thread_num();
      printf(" %d/%d/%d doing %d\n",iam,iam2,omp_get_num_threads(),i);
    }
    printf(" %d doing %d\n",iam,omp_get_thread_num());
  }
  return 0;
}


