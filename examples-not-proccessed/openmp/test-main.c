// -*- tab-width:2 ; indent-tabs-mode:nil -*-
//:: case OpenMPmainTest
//:: tools

#include <stdio.h>
#include <omp.h>
#include <unistd.h>

int main(int argc, char *argv[]){
  #pragma omp parallel num_threads(4)
  {
  int iam = omp_get_thread_num();
  printf("init %d\n",iam);
  }
    
  return 0;
}


