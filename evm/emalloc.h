#ifndef _EMALLOC_H
#define _EMALLOC_H

#include "closure.h"

void* e_malloc(VMState* vm, size_t size);
void* e_realloc(VMState* vm, void* ptr, size_t size);

#endif
