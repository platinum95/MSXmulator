#include "TMS9918A.h"

#include <assert.h>
#include <iostream>

static uint8_t VRAM[ 0x4000 ];

// Port registers
static uint16_t VRAMAddrLatch;

// Internal registers
enum StatusFlags {
    Coincidence = 0x20,
    FifthSprite = 0x40,
    Interrupt   = 0x80
};

static uint8_t Status;

struct Registers {
    uint8_t R0;
    uint8_t R1;
    uint8_t R2_NameAddress;
    uint8_t R3_ColorAddress;
    uint8_t R4_PatternTextMCGenAddress;
    uint8_t R5_SpriteAttributeAddress;
    uint8_t R6_SpritePatternGenAddress;
    uint8_t R7_ColorCode;
};
static Registers portRegisters;

// State
static bool regAddrWriteReady = true; // Initial state
static uint8_t regAddrWriteLatch;
struct State {
    uint8_t mode;
    uint16_t nameTableAddress;
    uint16_t colourTableAddress;
    uint16_t patternTableAddress;
    uint16_t spriteAttribTableAddress;
    uint16_t spritePatternTableAddress;
    uint8_t colour1;
    uint8_t colour0;
};
static State state;

void VDP::Reset() {
    state = {};
    portRegisters = {};
}

void VDP::Tick() {
    // TODO
}

static inline void PortRegisterWrite( uint8_t data ) {
    switch( regAddrWriteLatch ) {
        case 0x00: {
            portRegisters.R0 = data;
            state.mode = ( state.mode & 0x07 ) | ( ( data >> 1 ) & 0x01 );
            break;
        }
        case 0x01: {
            portRegisters.R1 = data;
            state.mode = ( state.mode & 0x04 ) | ( ( data >> 3 ) & 0x03 );
            break;
        }
        case 0x02: {
            portRegisters.R2_NameAddress = data;
            state.nameTableAddress = data * 0x0400;
            break;
        }
        case 0x03: {
            portRegisters.R3_ColorAddress = data;
            state.colourTableAddress = data * 0x0040;
            break;
        }
        case 0x04: {
            portRegisters.R4_PatternTextMCGenAddress = data;
            state.patternTableAddress = data * 0x0800;
            break;
        }
        case 0x05: {
            portRegisters.R5_SpriteAttributeAddress = data;
            state.spriteAttribTableAddress = data * 0x0080;
            break;
        }
        case 0x06: {
            portRegisters.R6_SpritePatternGenAddress = data;
            state.spritePatternTableAddress = data * 0x0800;
            break;
        }
        case 0x07: {
            portRegisters.R7_ColorCode = data;
            state.colour1 = ( data >> 4 ) & 0x0F;
            state.colour0 = data & 0x0F;
            break;
        }
    }
}

void VDP::PortAccess( uint8_t port, uint8_t &dataBus, bool writeLine ) {
    assert( port <= 1 );
    switch ( port ) {
        case 0x00:
            // VRAM data R/W
            assert( static_cast<bool>( ( VRAMAddrLatch & 0x4000 ) ) == writeLine );
            if ( writeLine ) {
                VRAM[ VRAMAddrLatch & 0x3FFF ] = dataBus;
            }
            else {
                dataBus = VRAM[ VRAMAddrLatch & 0x4000 ];
            }
            VRAMAddrLatch = ( VRAMAddrLatch & 0xC000 ) | ( ( VRAMAddrLatch + 1 ) & 0x3FFFF );
            break;
        case 0x01:
            // VDP register write, VRAM addr register, Status register
            if ( writeLine ) {
                if ( regAddrWriteReady ) {
                    regAddrWriteLatch = dataBus;
                    regAddrWriteReady = false;
                }
                else {
                    if ( dataBus & 0x80 ) {
                        PortRegisterWrite( dataBus );
                    }
                    else {
                        // Setting VRAM addr
                        VRAMAddrLatch = ( static_cast<uint16_t>( dataBus ) << 8 ) | static_cast<uint16_t>( regAddrWriteLatch );
                    }
                    regAddrWriteReady = true;
                }
            }
            else {
                dataBus = Status;
                Status &= ~StatusFlags::Interrupt;
            }
            break;
    }
}
