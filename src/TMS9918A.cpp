#include "TMS9918A.h"
#include "System.h"

extern "C" {
#include "gfx.h"
}

#include <assert.h>
#include <iostream>
#include <memory.h>

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
enum class ScanState {
    ActiveArea,
    FrontBorder,
    BackBorder,
    FrontBlanking,
    BackBlanking,
    Sync
};

static bool regAddrWriteReady = true; // Initial state
static uint8_t regAddrWriteLatch;
struct State {
    uint8_t mode;
    uint16_t nameTableAddress;
    uint16_t colourTableAddress;
    uint16_t colourTableBaseAddressMode2;
    uint16_t patternTableAddress;
    uint16_t patternTableBaseAddressMode2;
    uint16_t spriteAttribTableAddress;
    uint16_t spritePatternTableAddress;
    uint8_t colour1;
    uint8_t colour0;
    uint32_t xPos = 0;
    uint32_t yPos = 0;

    ScanState horizontalScanState;
    ScanState verticalScanState;
};
static State state;

// Max x/y pos of beam-active area
#define H_BLANK_BOUNDARY 284
#define V_BLANK_BOUNDARY 243

// Max x/y counter values before wrapping around to 0 post-sync
#define H_MAX 300
#define V_MAX 340


#define H_BORDER_LEFT_TEXT 19
#define H_BORDER_RIGHT_TEXT H_BLANK_BOUNDARY - 25
#define H_BORDER_LEFT 13
#define H_BORDER_RIGHT H_BLANK_BOUNDARY - 15

#define V_BORDER_BOTTOM_BOUNDARY V_BLANK_BOUNDARY - 24
#define V_BORDER_TOP_BOUNDARY 27

void VDP::Blorp() {
    strcpy( (char*)VRAM, "Hello world" );
}

void VDP::Reset() {
    state = {};
    portRegisters = {};

    // Set up temp gfx lib
    gfx_open( H_BLANK_BOUNDARY, V_BLANK_BOUNDARY, "MSXmulator" );
}

static void SetGfxColour( uint8_t colour ) {
    assert( colour < 0x10 );
    switch( colour ) {
        case 0x0:{
            // transparent
            gfx_color(0,0,0);
            break;
        }
        case 0x1:{
            // black
            gfx_color(1,1,1);
            break;
        }
        case 0x2:{
            // medium green
            gfx_color(62,184,73);
            break;
        }
        case 0x3:{
            // light green
            gfx_color(116,208,125);
            break;
        }
        case 0x4:{
            // dark blue
            gfx_color(89,85,224);
            break;
        }
        case 0x5:{
            // light blue
            gfx_color(128,118,241);
            break;
        }
        case 0x6:{
            // dark red
            gfx_color(185,94,81);
            break;
        }
        case 0x7:{
            // cyan
            gfx_color(101,219,239);
            break;
        }
        case 0x8:{
            // medium red
            gfx_color(219,101,89);
            break;
        }
        case 0x9:{
            // light red
            gfx_color(255,137,125);
            break;
        }
        case 0x0A:{
            // dark yellow
            gfx_color(204,195,94);
            break;
        }
        case 0x0B:{
            // light yellow
            gfx_color(222,208,135);
            break;
        }
        case 0x0C:{
            // dark green
            gfx_color(58,162,65);
            break;
        }
        case 0x0D:{
            // magenta
            gfx_color(183,102,181);
            break;
        }
        case 0x0E:{
            // gray
            gfx_color(204,204,204);
            break;
        }
        case 0x0F:{
            // white
            gfx_color(255,255,255);
            break;
        }
    }
}

static void IncrementPixelPosition() {
    if ( ++state.xPos == H_MAX ) {
        state.xPos = 0;
        ++state.yPos;
        if ( state.yPos == V_BLANK_BOUNDARY ) {
            Status |= StatusFlags::Interrupt;
            System::IRQ( false );
        }
        else if ( state.yPos == V_MAX ) {
            state.yPos = 0;
            Status &= ~StatusFlags::Interrupt;
            System::IRQ( true );
        }
    }
}

static inline bool InActivePortion() {
    return 
        ( ( portRegisters.R1 & 0x40 ) != 0 ) &&
        ( state.yPos < V_BORDER_BOTTOM_BOUNDARY && state.yPos >= V_BORDER_TOP_BOUNDARY ) &&
        ( 
            ( state.mode == 0x04 && state.xPos >= H_BORDER_LEFT_TEXT && state.xPos < H_BORDER_RIGHT_TEXT ) ||
            ( state.mode != 0x04 && state.xPos >= H_BORDER_LEFT && state.xPos < H_BORDER_RIGHT )
        );
}

void VDP::Tick() {

    IncrementPixelPosition();
    uint8_t pixelColour = portRegisters.R7_ColorCode & 0x0F;

    if ( InActivePortion() ) {
        const uint8_t adjustedYPos = state.yPos - V_BORDER_TOP_BOUNDARY;
        const uint8_t adjustedXPos = state.xPos - H_BORDER_LEFT;
        if ( state.mode == 0x04 ) {
            // Text mode
            const uint8_t adjustedXTextPos = state.xPos - H_BORDER_LEFT_TEXT;
            const uint8_t currentXCharIdx = adjustedXTextPos / 6;
            const uint8_t currentYCharIdx = adjustedYPos / 8;
            const uint16_t currentCharIdx = ( currentYCharIdx * 40 ) + currentXCharIdx;
            
            const uint8_t patternSelect = VRAM[ state.nameTableAddress + currentCharIdx ];

            const uint16_t patternOffset = state.patternTableAddress + ( patternSelect * 8 * sizeof(uint8_t) );
            const uint8_t currentRow = adjustedYPos % 8;

            const uint8_t rowData = VRAM[ patternOffset + currentRow ];
            const uint8_t currentColumn = adjustedXTextPos % 6;
            // 2 LSBs not used
            const bool pixelSet = static_cast<bool>( ( rowData >> ( 7 - currentColumn ) ) & 0x01 );
            pixelColour = pixelSet ? state.colour1 : state.colour0;
        }
        else if ( state.mode == 0x00 ) {
            // Graphics 1 mode
            // 32x24 grid, 8x8 blocks
            const uint8_t currentXBlockIdx = adjustedXPos / 8;
            const uint8_t currentYBlockIdx = adjustedYPos / 8;
            const uint16_t currentBlockIdx = ( currentYBlockIdx * 32 ) + currentXBlockIdx;
            const uint8_t patternSelect = VRAM[ state.nameTableAddress + currentBlockIdx ];

            const uint16_t patternOffset = state.patternTableAddress + ( patternSelect * 8 * sizeof(uint8_t) );
            const uint8_t colourOffset = ( patternSelect >> 3 ) & 0x1F;

            const uint8_t colourData = VRAM[ state.colourTableAddress + colourOffset ];
            const uint8_t currentRow = adjustedYPos % 8;
            const uint8_t rowData = VRAM[ patternOffset + currentRow ];
            const uint8_t currentColumn = adjustedXPos % 8;
            const bool pixelSet = static_cast<bool>( ( rowData >> ( 7 - currentColumn ) ) & 0x01 );

            pixelColour = pixelSet ? ( colourData >> 4 ) & 0x0F : colourData & 0x0F;

        }
        else if ( state.mode == 0x01 ) {
            // Graphics 2 mode
            // 32x24 grid, 8x8 blocks
            const uint8_t currentXBlockIdx = adjustedXPos / 8;
            const uint8_t currentYBlockIdx = adjustedYPos / 8;
            const uint16_t currentBlockIdx = ( currentYBlockIdx * 32 ) + currentXBlockIdx;
            
            const uint8_t patternTableSelect = currentBlockIdx / 256;
            assert( patternTableSelect < 3 );

            const uint8_t patternSelect = VRAM[ state.nameTableAddress + ( patternTableSelect * 256 * sizeof(uint8_t) ) + ( currentBlockIdx % 256 ) ];

            const uint8_t currentRow = adjustedYPos % 8;

            const uint16_t patternAndColourOffset = 
                ( patternTableSelect * 2048 * sizeof(uint8_t) ) +
                ( patternSelect * 8 * sizeof(uint8_t) ) 
                + currentRow;
            
            const uint8_t rowData = VRAM[ state.patternTableBaseAddressMode2 + patternAndColourOffset ];
            const uint8_t colourData = VRAM[ state.colourTableBaseAddressMode2 + patternAndColourOffset ];

            const uint8_t currentColumn = adjustedXPos % 8;
            const bool pixelSet = static_cast<bool>( ( rowData >> ( 7 - currentColumn ) ) & 0x01 );

            pixelColour = pixelSet ? ( colourData >> 4 ) & 0x0F : colourData & 0x0F;

        }
        else if ( state.mode == 0x02 ) {
            // Multicolour mode
            // 64x48 grid, 4x4 blocks
            const uint8_t currentXBlockIdx = adjustedXPos / 4;
            const uint8_t currentYBlockIdx = adjustedYPos / 4;
            const uint16_t currentBlockIdx = ( currentYBlockIdx * 64 ) + currentXBlockIdx;

            const uint8_t currentYGroupIdx = currentYBlockIdx / 2;

            const uint8_t patternSelect = VRAM[ state.nameTableAddress + currentBlockIdx ];

            const uint8_t currentRow = adjustedYPos % 8;

            uint8_t currentPatternRow = 0;
            for( currentPatternRow = 0; currentPatternRow < 3; ++currentPatternRow ) {
                if( ( currentYGroupIdx - currentPatternRow ) % 4 == 0 ) break;
            }
            const uint8_t currentGroupRow = currentYBlockIdx % 2;

            const uint16_t colourOffset = 
                state.patternTableAddress +
                ( patternSelect * 8 * sizeof(uint8_t) ) 
                + ( currentPatternRow * 2 )
                + currentGroupRow;
            
            const uint8_t colourData = VRAM[ colourOffset ];

            const uint8_t upperBits = static_cast<bool>( ( adjustedXPos % 2 ) == 0 );

            pixelColour = upperBits ? ( colourData >> 4 ) & 0x0F : colourData & 0x0F;

        }
        else {
            std::cout << "Unhandled graphics mode\n";
        }

        // Sprites
        for( uint8_t spriteId = 0; spriteId < 32; ++spriteId ) {
            uint8_t *spriteAttributes = &VRAM[ state.spriteAttribTableAddress + ( spriteId * sizeof(uint8_t) * 4 ) ];
            const uint8_t vPos = spriteAttributes[ 0 ];
            if ( vPos == 208 ) {
                if ( spriteId > 0 ) {
                    int i = 0;
                }
                break;
            }
        }
    }

    gfx_lock();
    SetGfxColour( pixelColour );
    gfx_point( state.xPos, state.yPos );
    gfx_unlock();
}

static inline void PortRegisterWrite( uint8_t registerSelect ) {
    switch( registerSelect ) {
        case 0x00: {
            portRegisters.R0 = regAddrWriteLatch;
            state.mode = ( state.mode & ~0x01 ) | ( ( regAddrWriteLatch >> 1 ) & 0x01 );
            break;
        }
        case 0x01: {
            portRegisters.R1 = regAddrWriteLatch;
            state.mode = ( state.mode & 0x01 ) | ( ( regAddrWriteLatch >> 2 ) & 0x06 );
            break;
        }
        case 0x02: {
            portRegisters.R2_NameAddress = regAddrWriteLatch & 0x0F;
            state.nameTableAddress = static_cast<uint16_t>( portRegisters.R2_NameAddress ) << 10u;
            break;
        }
        case 0x03: {
            portRegisters.R3_ColorAddress = regAddrWriteLatch;
            state.colourTableAddress = regAddrWriteLatch * 0x0040;
            state.colourTableBaseAddressMode2 = regAddrWriteLatch & 0x80 ? 0x2000 : 0x0000;
            break;
        }
        case 0x04: {
            portRegisters.R4_PatternTextMCGenAddress = regAddrWriteLatch;
            state.patternTableAddress = regAddrWriteLatch * 0x0800;
            state.patternTableBaseAddressMode2 = regAddrWriteLatch & 0x04 ? 0x2000 : 0x0000;
            break;
        }
        case 0x05: {
            portRegisters.R5_SpriteAttributeAddress = regAddrWriteLatch;
            state.spriteAttribTableAddress = regAddrWriteLatch * 0x0080;
            break;
        }
        case 0x06: {
            portRegisters.R6_SpritePatternGenAddress = regAddrWriteLatch;
            state.spritePatternTableAddress = regAddrWriteLatch * 0x0800;
            break;
        }
        case 0x07: {
            portRegisters.R7_ColorCode = regAddrWriteLatch;
            state.colour1 = ( regAddrWriteLatch >> 4 ) & 0x0F;
            state.colour0 = regAddrWriteLatch & 0x0F;
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
                uint16_t addr = VRAMAddrLatch & 0x3FFF;
                if ( addr < 960 ) {
                    //std::cout << "Bloop\n";
                }
                VRAM[ addr ] = dataBus;
            }
            else {
                dataBus = VRAM[ VRAMAddrLatch & 0x3FFF ];
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
                        PortRegisterWrite( dataBus & ~0x80 );
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
                System::IRQ( true );
            }
            break;
    }
}
