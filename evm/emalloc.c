#include "emalloc.h"

// TMP

// Also make tmps roots when assigned to. Make enough room with GROWROOTS

// When copying, make sure that the addresses stored at the roots are updated.

// Actually, should the root set be the addresses of the local variables? Then
// when they are copied, they can be updated with the new location.

extern ALLOCATOR allocate;
extern REALLOCATOR reallocate;

void* e_malloc(VMState* vm, size_t size) {
    return (VAL)allocate(size);
}

void* e_realloc(VMState* vm, void* ptr, size_t size) {
    return (VAL)reallocate(ptr, size);
}
