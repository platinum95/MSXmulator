#include <stdint.h>

namespace VDP {
void Reset();
void PortAccess( uint8_t port, uint8_t &dataBus, bool writeLine );
void Tick();
}