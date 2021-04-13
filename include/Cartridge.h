#include <filesystem>
#include <stdint.h>
#include <vector>

class Cartridge {
public:
    void loadCartridge( std::filesystem::path romPath );
    void memoryAccess( uint16_t addressBus, uint8_t &dataBus, bool writeLine );

private:
    std::vector<uint8_t>    romData;
    size_t                  cartridgeSize{ 0u };
};