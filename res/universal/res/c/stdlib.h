#ifndef STDLIB_H
#define STDLIB_H

#include <stddef.h>

void *malloc(size_t size);

/*@
  requires ptr != NULL ==> \pointer_block_offset(ptr) == 0;
  requires ptr != NULL ==> (\forall* int i; 0 <= i && i < \pointer_block_length(ptr); Perm(&ptr[i], write));
@*/
void free(int *ptr);

/*@
  requires ptr != NULL ==> \pointer_block_offset(ptr) == 0;
  requires ptr != NULL ==> (\forall* int i; 0 <= i && i < \pointer_block_length(ptr); Perm(&ptr[i], write));
@*/
void free(float *ptr);

#endif 


