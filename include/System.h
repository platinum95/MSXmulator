#include <filesystem>
#include <stdint.h>

namespace System {
void Initialise( std::filesystem::path romPath );
void MemoryAccess( uint16_t addressBus, uint8_t &dataBus, bool writeLine );
uint8_t MemoryAccess( uint16_t addressBus );
void IOAccess( uint16_t addressBus, uint8_t &dataBus, bool writeLine );
void IRQ( bool level );
}