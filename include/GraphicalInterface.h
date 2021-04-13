#ifndef GRAPHICAL_INTERFACE_H
#define GRAPHICAL_INTERFACE_H
#include <stdint.h>
#include <vector>
namespace GraphicalInterface {
enum class Colour : uint8_t {
    Transparent = 0x0,
    Black,
    MediumGreen,
    LightGreen,
    DarkBlue,
    LightBlue,
    DarkRed,
    Cyan,
    MediumRed,
    LightRed,
    DarkYellow,
    LightYellow,
    DarkGreen,
    Magenta,
    Grey,
    White
};

void Initialise( uint16_t width, uint16_t height );
void Cleanup();

void SetPixel( uint32_t x, uint32_t y, Colour colour );
void OutputFrame();
}
#endif //GRAPHICAL_INTERFACE_H