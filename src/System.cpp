#include "System.h"

#include "AY_3_8910.h"
#include "Cartridge.h"
#include "Input.h"
#include "TMS9918A.h"
#include "z80Core.h"

#include <array>
#include <assert.h>
#include <filesystem>
#include <fstream>
#include <iostream>

namespace System {
struct PPI {
    uint8_t RegA{ 0x00 };
    uint8_t SlotASelect{ 0x00 };
    uint8_t SlotBSelect{ 0x00 };
    uint8_t SlotCSelect{ 0x00 };
    uint8_t SlotDSelect{ 0x00 };
    uint8_t RegC{ 0x00 };
    uint8_t KBScan{ 0x00 };
    bool cassetteControl{ false };
    bool cassetteWrite{ false };
    bool capsEnable{ false };
    bool soundEnable{ false };

    inline void PPIAccess( uint8_t port, uint8_t &data, bool writeLine ) {
        switch( port ) {
            case 0x00: {
                // Reg A - Primary slot-select register
                if ( writeLine ) {
                    RegA = data;
                    SlotASelect = data & 0x03;
                    SlotBSelect = ( data >> 2 ) & 0x03;
                    SlotCSelect = ( data >> 4 ) & 0x03;
                    SlotDSelect = ( data >> 6 ) & 0x03;
                }
                else {
                    data = RegA;
                }
                break;
            }
            case 0x01: {
                // Reg B - KB Matrix row-in
                assert( !writeLine );
                data = Input::GetRow( KBScan );
                break;
            }
            case 0x02: {
                // Reg C - KB & Cassette interface
                if ( writeLine ) {
                    RegC = data;
                    updateRegC();
                }
                else {
                    data = RegC;
                }

                break;
            }
            case 0x03: {
                // Command register
                assert( writeLine );
                if ( data & 0x80 ) {
                    // TODO
                    std::cout << "PPI Command-write 0x80\n";
                }
                else {
                    const uint8_t bitMask = 1u << ( ( data >> 1 ) & 0x07 );
                    if ( data & 0x01 ) {
                        RegC |= bitMask;
                    }
                    else {
                        RegC &= ~bitMask;
                    }
                    updateRegC();
                }
                break;
            }
        }
    }

    inline uint8_t GetActiveSlotSelect( uint8_t slot ) {
        switch( slot ) {
            case 0x00: return SlotASelect;
            case 0x01: return SlotBSelect;
            case 0x02: return SlotCSelect;
            case 0x03: return SlotDSelect;
        }
        assert( false );
        return 255;
    }

private:
    void updateRegC() {
        KBScan = RegC & 0x0F;
        cassetteControl = RegC & 0x10;
        cassetteWrite = RegC & 0x20;
        capsEnable = RegC & 0x40;
        soundEnable = RegC & 0x80;
    }

};
static PPI ppi;
static uint8_t ROM[ 0x8000 ];
static uint8_t RAM[ 0x10000 ];
static Cartridge CartA;
static Cartridge CartB;

void Initialise( std::filesystem::path romPath ){
    std::ifstream input( romPath, std::ios::binary | std::ios::in | std::ios::ate );
    assert( input.is_open() );
    auto size = input.tellg();
    input.seekg( std::ios::beg );
    input.read( (char*)ROM, size );
    input.close();

    CartA.loadCartridge( "nemesis.rom" );
    VDP::Reset();
    Input::Initialise();
    PSG::Reset();

}

void IRQ( bool level ) {
    Z80::IRQ( level );
}

void SlotAccess( uint8_t slot, uint16_t addressBus, uint8_t &dataBus, bool writeLine ) {
    const uint8_t slotSelect = ppi.GetActiveSlotSelect( slot );
    
    switch( slotSelect ){
        case 0x00: {
            if ( writeLine ) {
                return;
            }
            else if ( addressBus < 0x8000 ) {
                dataBus = ROM[ addressBus ];
            }
            else {
                // Test -> fallback to RAM
            if ( writeLine ) {
                RAM[ addressBus ] = dataBus;
            }
            else {
                dataBus = RAM[ addressBus ];
            }

            }
            break;
        }
        case 0x01: {
            // 64kb RAM
            if ( writeLine ) {
                RAM[ addressBus ] = dataBus;
            }
            else {
                dataBus = RAM[ addressBus ];
            }
            break;
        }
        case 0x02: {
            CartA.memoryAccess( addressBus, dataBus, writeLine );
            break;
        }
        case 0x03: {
            CartB.memoryAccess( addressBus, dataBus, writeLine );
            break;
        }
    }
}

void SecondarySlotSelect( uint8_t &dataBus, bool writeLine ) {
    const uint8_t slotSelect = ppi.GetActiveSlotSelect( 3 );
    
    switch( slotSelect ){
        case 0x00: {
            // ROM - not expanded and nothing to do since FFFF is OOB
            return;
        }
        case 0x01: {
            // RAM - Not expanded
            if ( writeLine ) {
                RAM[ 0xFFFF ] = dataBus;
            }
            else {
                dataBus = RAM[ 0xFFFF ];
            }
            break;
        }
        case 0x02: {
            // Cartridge 1 - Not expanded for now
            CartA.memoryAccess( 0xFFFF, dataBus, writeLine );
            break;
        }
        case 0x03: {
            // Cartridge 2 - Not expanded for now
            CartB.memoryAccess( 0xFFFF, dataBus, writeLine );
            break;
        }
    }
}

void MemoryAccess( uint16_t addressBus, uint8_t &dataBus, bool writeLine ) {
    (void) dataBus;
    (void) writeLine;
    if ( addressBus < 0x4000 ) {
        // Slot A
        SlotAccess( 0, addressBus, dataBus, writeLine );
    }
    else if ( addressBus < 0x8000 ) {
        SlotAccess( 1, addressBus, dataBus, writeLine );
    }
    else if ( addressBus < 0xC000 ) {
        SlotAccess( 2, addressBus, dataBus, writeLine );
    }
    else if ( addressBus != 0xFFFF ) {
        SlotAccess( 3, addressBus, dataBus, writeLine );
    }
    else {
        SecondarySlotSelect( dataBus, writeLine );
    }

}

uint8_t MemoryAccess( uint16_t addressBus ) {
    uint8_t dataBus = 0;
    System::MemoryAccess( addressBus, dataBus, false );
    return dataBus;
}

void IOAccess( uint16_t addressBus, uint8_t &dataBus, bool writeLine ) {
    uint8_t IOAddress = addressBus & 0x00FF;
    if ( IOAddress < 0x80 ) {
        // Unspecified/reserved
    }
    else if ( IOAddress <= 0x87 ){
        // TODO - RS-232C
        std::cout << "TODO - RS-232C\n";
    }
    else if ( IOAddress < 0x90 ) {
        // TODO - ?
    }
    else if ( IOAddress <= 0x91 ) {
        // TODO - Printer port
        std::cout << "TODO - Printer port\n";
    }
    else if ( IOAddress < 0x98 ) {
        // TODO - ?
    }
    else if ( IOAddress <= 0x99 ) {
        // VDP
        VDP::PortAccess( static_cast<uint8_t>( addressBus - 0x98 ), dataBus, writeLine );
    }
    else if ( IOAddress < 0xA0 ) {
        // TODO - ?
    }
    else if ( IOAddress <= 0xA2 ) {
        // PSG
        PSG::PortAccess( static_cast<uint8_t>( addressBus - 0xA0 ), dataBus, writeLine );
    }
    else if ( IOAddress < 0xA8 ) {
        // TODO - ?
    }
    else if ( IOAddress <= 0xAB ) {
        ppi.PPIAccess( IOAddress - 0xA8, dataBus, writeLine );
    }
    else if ( IOAddress <= 0xB3 ) {
        // TODO - ?
    }
    else if ( IOAddress == 0xB4 ) {
        // TODO - Calendar clock
        std::cout << "TODO - Calendar clock\n";
    }
    else if ( IOAddress < 0xB8 ) {
        // TODO - ?
    }
    else if ( IOAddress <= 0xBB ) {
        // TODO - Light pen
    }
    else if ( IOAddress < 0xD0 ) {
        // TODO - ?
    }
    else if ( IOAddress <= 0xD7 ) {
        // TODO - FDC
    }
    else if ( IOAddress < 0xF7 ) {
        // TODO - ?
    }
    else if ( IOAddress == 0xF7 ) {
        // TODO - A/V Control
        std::cout << "TODO - AV Control\n";
    }
    else if ( IOAddress == 0xF8 ) {
        // TODO - PAL A/V Control
        std::cout << "TODO - PAL A/V Control\n";
    }
    else if ( IOAddress < 0xFB ) {
        // TODO - ?
    }
    else {
        // TODO - Memory Mapping control?
        std::cout << "TODO - ?\n";
    }

}

}