#ifndef STDLIB_H
#define STDLIB_H

#include <stddef.h>

#define malloc __vercors_malloc
#define free __vercors_free

void *__vercors_malloc(size_t size);

// Contract is generated, and make sure to completely remove write permission to all the malloced data.
void __vercors_free(void *ptr);

#endif 


