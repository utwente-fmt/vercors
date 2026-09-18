#if __LP32__ || _WIN16
char=8
short=16
int=16
long=32
long long=64
intptr_t=32
#elif __ILP32__
char=8
short=16
int=32
long=32
long long=64
intptr_t=32
#elif __LLP64__ || _WIN64
char=8
short=16
int=32
long=32
long long=64
intptr_t=64
#elif __LP64__
char=8
short=16
int=32
long=64
long long=64
intptr_t=64
#else
// We don't support __ILP64__ since its uncommon and we don't have good definition for uint32_t
#error "Target not supported by VerCors"
#endif
