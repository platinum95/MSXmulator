#include <stdint.h>

namespace PSG {
    void Tick();
    void Reset();
    void PortAccess( uint8_t port, uint8_t &dataBus, bool writeLine );
}