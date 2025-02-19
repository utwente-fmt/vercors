#ifndef ASSERT_H
#define ASSERT_H
#include<stdbool.h>

/*@
 ensures \result;
@*/
bool alwaystrue();

/*@
 requires alwaystrue() ==> expression;
@*/
void assert(bool expression);

#endif

