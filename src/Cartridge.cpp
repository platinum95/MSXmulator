#include "Cartridge.h"

#include <assert.h>
#include <fstream>

void Cartridge::loadCartridge( std::filesystem::path romPath ) {
    std::ifstream input( romPath, std::ios::binary | std::ios::in | std::ios::ate );
    assert( input.is_open() );
    cartridgeSize = static_cast<size_t>( input.tellg() );
    romData.resize( cartridgeSize );
    input.seekg( std::ios::beg );
    input.read( (char*)romData.data(), cartridgeSize );
    input.close();
}

static uint8_t bankSelect[ 4 ] = { 0, 1, 2, 3 };

static inline void updateMapBank( uint16_t addressBus, uint8_t dataBus ) {
    if ( addressBus >= 0x6000 && addressBus < 0x6800 ) {
        // Bank 1
        bankSelect[ 0 ] = dataBus;
    }
    else if ( addressBus >= 0x6800 && addressBus < 0x7000 ) {
        // Bank 2
        bankSelect[ 1 ] = dataBus;
    }
    else if ( addressBus >= 0x7000 && addressBus < 0x7800 ) {
        // Bank 3
        bankSelect[ 2 ] = dataBus;
    }
    else if ( addressBus >= 0x7800 && addressBus < 0x8000 ) {
        // Bank 4
        bankSelect[ 3 ] = dataBus;
    }
}

static inline uint32_t mapAddress( uint16_t addressBus ) {
    if ( addressBus < 0x4000 ) {
        return static_cast<uint32_t>( addressBus );
    }
    else if ( addressBus < 0x6000 ) {
        // Bank 1
        return static_cast<uint32_t>( addressBus - 0x4000 );
    }
    else if ( addressBus < 0x8000 ) {
        // Bank 2
        return static_cast<uint32_t>( addressBus - 0x6000 ) + ( bankSelect[ 1 ] * 0x2000 );
    }
    else if ( addressBus < 0xA000 ) {
        // Bank 3
        return static_cast<uint32_t>( addressBus - 0x8000 ) + ( bankSelect[ 2 ] * 0x2000 );
    }
    else if ( addressBus < 0xC000 ) {
        // Bank 4
        return static_cast<uint32_t>( addressBus - 0xA000 ) + ( bankSelect[ 3 ] * 0x2000 );
    }
    else {
        return static_cast<uint32_t>( addressBus );
    }
}

void Cartridge::memoryAccess( uint16_t addressBus, uint8_t &dataBus, bool writeLine ) {
    if ( writeLine ) {
        updateMapBank( addressBus, dataBus );
    }
    else {
        uint32_t resolvedAddress = mapAddress( addressBus );
        if ( resolvedAddress < cartridgeSize ) {
            dataBus = romData[ resolvedAddress ];    
        }
        else {
            dataBus = 0xFF;
        }
    }
}