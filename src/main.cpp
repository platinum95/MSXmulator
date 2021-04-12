#include <iostream>

#include "AY_3_8910.h"
#include "Input.h"
#include "System.h"
#include "TMS9918A.h"
#include "z80Core.h"

#include <chrono>

int main( int argc, char** argv ) {
    (void)argc;
    (void)argv;
    std::cout << "Entry" << std::endl;

    System::Initialise( "bios.rom" );
    Input::Initialise();
    // 10.738635 MHz base clock, /2 for Pixel Clock, /3 for CPU Clock, /24 for GROMCLK, /5 for PSG
    //constexpr double clockPeriod = 93.1217049467;// ( 1.0 / 10.738635 ) * 1.0e3; // Duration in nanoseconds
    //constexpr double HostClocksPerEmulatedClock = 93.1217;
    while( 1 ) {
        static uint8_t PixelClockDiv = 0;
        static uint8_t CPUClockDiv = 0;
        static uint8_t GROMCLKDiv = 0;
        static uint8_t PSGCLKDiv = 0;
        //auto clockStartTime = std::chrono::high_resolution_clock::now();
        //auto clockEndTime = std::chrono::high_resolution_clock::now() - clockPeriod;
        auto cpuClock = std::clock();
        CLOCKS_PER_SEC;
        
        if ( ++PixelClockDiv == 1 ) {
            VDP::Tick();
            PixelClockDiv = 0;
        }
        if ( ++CPUClockDiv == 2 ) {
            Z80::Tick();
            CPUClockDiv = 0;
        }
        if ( ++GROMCLKDiv == 23 ) {
            GROMCLKDiv = 0;
        }
        if ( ++PSGCLKDiv == 4 ) {
            PSG::Tick();
            PSGCLKDiv = 0;
        }

        //while( std::chrono::high_resolution_clock::now() < clockEndTime );

    }
    return 0;
}