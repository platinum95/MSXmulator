#include <iostream>

#include "System.h"
#include "z80Core.h"

int main( int argc, char** argv ) {
    (void)argc;
    (void)argv;
    std::cout << "Entry" << std::endl;

    System::Initialise( "bios.rom" );
    while( 1 ) {
        z80Tick();
    }
    return 0;
}