#include <iostream>

#include "z80Core.h"

int main( int argc, char** argv ) {
    (void)argc;
    (void)argv;
    std::cout << "Entry" << std::endl;

    z80Tick();
    return 0;
}