#ifndef STDLIB_H
#define STDLIB_H

#include <stddef.h>

#define malloc __vercors_malloc
#define free __vercors_free

void *__vercors_malloc(size_t size);

/*@
  requires ptr != NULL;
  requires \pointer_block_offset(ptr) == 0;
  requires (\forall* int i; 0 <= i && i < \pointer_block_length(ptr); Perm(&ptr[i], write));
@*/
void __vercors_free(void *ptr);

#endif 


