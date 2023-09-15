#ifndef STDLIB_H
#define STDLIB_H

#include <stddef.h>

void *malloc(size_t size);

/*@
  requires ptr != NULL;
  requires \pointer_block_offset(ptr) == 0;
  requires (\forall* int i; 0 <= i && i < \pointer_block_length(ptr); Perm(&ptr[i], write));
@*/
void free(void *ptr);

#endif 


