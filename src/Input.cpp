#include "Input.h"

#include <assert.h>
extern "C" {
#include "gfx.h"
}

#include <assert.h>
#include <chrono>
#include <memory.h>
#include <thread>

struct RowBitValues {
    uint8_t row;
    uint8_t bitmask;
    RowBitValues( uint8_t _row, uint8_t bitpos ) {
        assert( bitpos < 8 );
        row = _row;
        bitmask = 1u << bitpos;
    }
};

uint8_t rowValues[ 11 ];
static std::thread InputThread;

static RowBitValues MapKeycodeToMatrix( uint16_t keyCode ) {
    switch( keyCode ) {
        case  9: return { 7, 2 }; // ESC
        case 10: return { 0, 1 }; // 1
        case 11: return { 0, 2 }; // 2
        case 12: return { 0, 3 }; // 3
        case 13: return { 0, 4 }; // 4
        case 14: return { 0, 5 }; // 5
        case 15: return { 0, 6 }; // 6
        case 16: return { 0, 7 }; // 7
        case 17: return { 1, 0 }; // 8
        case 18: return { 1, 1 }; // 9
        case 19: return { 0, 0 }; // 0
        case 20: return { 1, 2 }; // _-
        case 21: return { 1, 3 }; // +=
        case 22: return { 7, 5 }; // BS

        case 23: return { 7, 3 }; // TAB
        case 24: return { 4, 6 }; // Q
        case 25: return { 5, 4 }; // W
        case 26: return { 3, 2 }; // E
        case 27: return { 4, 7 }; // R
        case 28: return { 5, 1 }; // T
        case 29: return { 5, 6 }; // Y
        case 30: return { 5, 2 }; // U
        case 31: return { 3, 6 }; // I
        case 32: return { 4, 4 }; // O
        case 33: return { 4, 5 }; // P
        case 34: return { 1, 5 }; // [{
        case 35: return { 1, 6 }; // ]}

        case 36: return { 7, 7 }; // Return

        case 37: return { 6, 2 }; // L-CTRL
        case 38: return { 2, 6 }; // A
        case 39: return { 5, 0 }; // S
        case 40: return { 3, 1 }; // D
        case 41: return { 3, 3 }; // F
        case 42: return { 3, 4 }; // G
        case 43: return { 3, 5 }; // H
        case 44: return { 3, 7 }; // J
        case 45: return { 4, 0 }; // K
        case 46: return { 4, 1 }; // L
        case 47: return { 1, 7 }; // ;
        case 48: return { 2, 0 }; // '"
        case 49: return { 2, 1 }; // ~`
        
        case 50: return { 6, 0 }; // L-SHIFT
        case 52: return { 5, 7 }; // Z
        case 53: return { 5, 5 }; // X
        case 54: return { 3, 0 }; // C
        case 55: return { 5, 3 }; // V
        case 56: return { 2, 7 }; // B
        case 57: return { 4, 3 }; // N
        case 58: return { 4, 2 }; // M
        case 59: return { 2, 2 }; // ,<
        case 60: return { 2, 3 }; // .>
        case 61: return { 2, 4 }; // /?
        case 62: return { 6, 0 }; // R-shift

        case 63: return { 9, 0 }; // NUM*
        case 64: return { 6, 2 }; // L-ALT -> GRAPH
        case 65: return { 8, 0 }; // Space
        case 66: return { 6, 3 }; // CAPSLOCK

        case 67: return { 6, 5 }; // F1 - F1/6
        case 68: return { 6, 6 }; // F2 - F2/7
        case 69: return { 6, 7 }; // F3 - F3/8
        case 70: return { 7, 0 }; // F4 - F4/9
        case 71: return { 7, 1 }; // F5 - F5/10
        case 72: return { 8, 1 }; // F6 - HOME
        case 73: return { 8, 2 }; // F7 - INS
        case 74: return { 8, 3 }; // F8 - DEL
        case 75: return { 7, 6 }; // F9 - SELECT
        case 76: return { 7, 4 }; // F10 - STOP

        case 79: return { 10, 2 }; // NUM7
        case 80: return { 10, 3 }; // NUM8
        case 81: return { 10, 4 }; // NUM9
        case 82: return { 10, 5 }; // NUM-
        case 83: return { 7, 7 }; // NUM4
        case 84: return { 10, 0 }; // NUM5
        case 85: return { 10, 1 }; // NUM6
        case 86: return { 9, 1 }; // NUM+
        case 87: return { 9, 4 }; // NUM1
        case 88: return { 9, 5 }; // NUM2
        case 89: return { 9, 6 }; // NUM3
        case 90: return { 9, 3 }; // NUM0
        case 91: return { 10, 7 }; // NUM.
        case 106: return { 9, 2 }; // NUM/
        
        case 108: return { 6, 4 }; // R-ALT - CODE
        
        case 111: return { 8, 5 }; // UP
        case 113: return { 8, 4 }; // LEFT
        case 114: return { 8, 7 }; // RIGHT
        case 115: return { 8, 6 }; // DOWN
        default: return { 2, 5 };
    }
} 

static void UpdateKeyState( uint16_t keyCode, bool pressed ) {
    RowBitValues keyPos = MapKeycodeToMatrix( keyCode );
    if ( pressed ) {
        rowValues[ keyPos.row ] &= ~keyPos.bitmask;
    }
    else {
        rowValues[ keyPos.row ] |= keyPos.bitmask;
    }
}

static void InputLoop() {
    memset( rowValues, 0xFF, 11 );

    while( 1 ) {
        uint16_t keyCode;
        bool keyPressed;
        gfx_lock();
        while( gfx_KeyEvent( &keyCode, &keyPressed ) ) {
            UpdateKeyState( keyCode, keyPressed );
        }
        gfx_unlock();
        std::this_thread::sleep_for( std::chrono::microseconds( 500 ) );
    }
}
void Input::Initialise() {
    InputThread = std::thread( InputLoop );
    InputThread.detach();
}

void Input::Update() {
    
}

uint8_t Input::GetRow( uint8_t rowSelect ) {
    assert( rowSelect < 11 );
    return rowValues[ rowSelect ];
}