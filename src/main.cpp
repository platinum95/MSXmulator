#include <iostream>

#include "System.h"

int main( int argc, char** argv ) {
    (void)argc;
    (void)argv;
    std::cout << "Entry" << std::endl;

    System::Initialise( "bios.rom" );
    System::Run();

    return 0;
}