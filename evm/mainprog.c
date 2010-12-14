#include <gc_header.h>
#include "closure.h"

void* _do__U_main();

void** _epic_top_of_stack;

VMState* vm;

int main(int argc, char* argv[]) {
    void* stacktop = NULL;
    _epic_top_of_stack = (void**)&stacktop;

    epic_main(argc, argv);

    return 0; 
}
