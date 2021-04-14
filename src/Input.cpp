#include "Input.h"
#include "KeyboardInterface.h"

#include <assert.h>
#include <memory.h>

uint8_t rowValues[ 11 ];

void Input::Initialise() {
    memset( rowValues, 0xFF, 11 );
}

void Input::Update() {
    for ( const auto& keyEvent : KeyboardInterface::GetKeyEvents() ) {
        if ( keyEvent.keyPressed ) {
            rowValues[ keyEvent.row ] &= ~keyEvent.bitmask;
        }
        else {
            rowValues[ keyEvent.row ] |= keyEvent.bitmask;
        }
    }
}

uint8_t Input::GetRow( uint8_t rowSelect ) {
    assert( rowSelect < 11 );
    Update();
    return rowValues[ rowSelect ];
}