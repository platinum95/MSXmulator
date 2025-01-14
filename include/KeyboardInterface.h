#ifndef KEYBOARD_INTERFACE_H
#define KEYBOARD_INTERFACE_H

#include <optional>
#include <vector>

namespace KeyboardInterface {

struct KeyEvent {
    uint8_t row;
    uint8_t bitmask;
    bool keyPressed;
    KeyEvent( uint8_t _row, uint8_t bitpos, bool pressed );
};

std::optional<std::vector<KeyEvent>> GetKeyEvents();

}

#endif //KEYBOARD_INTERFACE_H