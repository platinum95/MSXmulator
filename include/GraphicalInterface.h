#ifndef GRAPHICAL_INTERFACE_H
#define GRAPHICAL_INTERFACE_H

#include <stdint.h>
#include <vector>

namespace GraphicalInterface {
enum class Colour : uint8_t {
    Transparent = 0x0,
    Black       = 0x1,
    MediumGreen = 0x2,
    LightGreen  = 0x3,
    DarkBlue    = 0x4,
    LightBlue   = 0x5,
    DarkRed     = 0x6,
    Cyan        = 0x7,
    MediumRed   = 0x8,
    LightRed    = 0x9,
    DarkYellow  = 0xA,
    LightYellow = 0xB,
    DarkGreen   = 0xC,
    Magenta     = 0xD,
    Grey        = 0xE,
    White       = 0xF
};

void Initialise( uint16_t width, uint16_t height );
void Cleanup();

bool ShouldClose();
void SetPixel( uint32_t x, uint32_t y, Colour colour ) noexcept;
void OutputFrame();
}
#endif //GRAPHICAL_INTERFACE_H