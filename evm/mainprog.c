#include "closure.h"

void** _epic_top_of_stack;

int main(int argc, char* argv[]) {
    void* stacktop = NULL;
    _epic_top_of_stack = (void**)&stacktop;

    epic_main(argc, argv);

    return 0; 
}
