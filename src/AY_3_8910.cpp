#include "AY_3_8910.h"

#include <assert.h>

static uint8_t RegisterSelectLatch;

static uint8_t portRegisters[ 16 ];

void PsgPortAccess( uint8_t port, uint8_t &dataBus, bool writeLine ) {
    switch( port ) {
        case 0x00: {
            assert( writeLine );
            RegisterSelectLatch = dataBus;
            break;
        }
        case 0x01: {
            assert( writeLine );
            assert( RegisterSelectLatch < 16 );
            *( (uint8_t*)&portRegisters + RegisterSelectLatch ) = dataBus;
            break;
        }
        case 0x02: {
            assert( !writeLine );
            assert( RegisterSelectLatch < 16 );
            dataBus = *( (uint8_t*)&portRegisters + RegisterSelectLatch );
            break;
        }
    }
}