#include "z80Core.h"
#include "System.h"

#include <assert.h>
#include <bit>
#include <functional>
#include <iostream>
#include <stdint.h>

#pragma region Concepts

using ReadModeBool = bool(*)();
using ReadMode8BitRegister = uint8_t(*)();
using ReadMode8sBitRegister = int8_t(*)();
using ReadMode16BitRegister = uint16_t(*)();

using WriteMode8BitRegister = void(*)(uint8_t);
using WriteMode16BitRegister = void(*)(uint16_t);

using OpNoParamNoRet            = std::function<void()>;//void(*)();
using Op8BitNoParamWithRet      = std::function<uint8_t()>;// uint8_t(*)();
using Op16BitNoParamWithRet     = std::function<uint16_t()>;//uint16_t(*)();
using Op8BitSingleParamNoRet    = std::function<void(uint8_t)>;//void(*)(uint8_t);
using Op16BitSingleParamNoRet   = std::function<void(uint16_t)>;//void(*)(uint16_t);
using Op8BitSingleParamWithRet  = std::function<uint8_t(uint8_t)>;//uint8_t(*)(uint8_t);
using Op16BitSingleParamWithRet = std::function<uint16_t(uint16_t)>;//uint16_t(*)(uint16_t);
using Op8BitDualParamNoRet      = std::function<void(uint8_t, uint8_t)>;//void(*)(uint8_t, uint8_t);
using Op16BitDualParamNoRet     = std::function<void(uint16_t, uint16_t)>;//void(*)(uint16_t, uint16_t);
using Op8BitDualParamWithRet    = std::function<uint8_t(uint8_t, uint8_t)>;//uint8_t(*)(uint8_t, uint8_t);
using Op16BitDualParamWithRet   = std::function<uint16_t(uint16_t, uint16_t)>;//uint16_t(*)(uint16_t, uint16_t);

template<typename T>
concept IsByteOrWord = std::same_as<T, uint8_t> || std::same_as<T, uint16_t> || std::same_as<T, bool> || std::same_as<T, int8_t>;

template<typename T, typename ...args>
concept ReturnsByteOrWord = IsByteOrWord<std::invoke_result_t<T, args...>>;

template<typename T>
concept WritesByteOrWord = std::same_as<T, Op8BitSingleParamNoRet> || std::same_as<T, Op16BitSingleParamNoRet>;

template<typename opType, typename writeType, typename ...readTypes>
concept WriteMatchesOp = true;// std::is_same<writeType, void(*)(std::invoke_result_t<opType, readTypes...>)>::value;

template<typename opType>
concept IsInvocableOperationNoParamNoRet = std::same_as<opType, OpNoParamNoRet>;

template<typename opType, typename readType>
concept IsInvocableOperationSingleParamNoRet = ReturnsByteOrWord<readType> && std::is_same<opType, void(*)(std::invoke_result_t<readType>)>::value;

template<typename opType, typename readType1, typename readType2>
concept IsInvocableOperationDualParamNoRet = std::is_same<opType, void(*)(std::invoke_result_t<readType1>, std::invoke_result_t<readType2>)>::value;

template<typename opType, typename writeType>
concept IsInvocableOperationNoParamWithRet = WritesByteOrWord<writeType> && WriteMatchesOp<opType, writeType>;

template<typename opType, typename writeType, typename readType>
concept IsInvocableOperationSingleParamWithRet =
    ReturnsByteOrWord<opType, std::invoke_result_t<readType>> && // Operation can be invoked with result of read, and returns a value that can invoke write
    ReturnsByteOrWord<readType> &&  // Read returns either uint8_t or uint16_t
    WritesByteOrWord<writeType> &&  // Write takes either uint8_t or uint16_t
    WriteMatchesOp<opType, writeType, readType>; // Types match between op and write functions


template<typename opType, typename readType1, typename readType2, typename writeType>
concept IsInvocableOperationDualParamWithRet =
    ReturnsByteOrWord<opType, std::invoke_result_t<readType1>, std::invoke_result_t<readType2>> && // Operation can be invoked with result of read, and returns a value that can invoke write
    ReturnsByteOrWord<readType1> &&  // Read returns either uint8_t or uint16_t
    ReturnsByteOrWord<readType2> &&  // Read returns either uint8_t or uint16_t
    WritesByteOrWord<writeType> &&  // Write takes either uint8_t or uint16_t
    WriteMatchesOp<opType, writeType, readType1, readType2>; // Types match between op and write functions


template<auto operation, auto read1>
concept IsSingleParamNoWriteoutOp = IsInvocableOperationSingleParamNoRet<decltype(operation), decltype(read1)>;

template<auto operation, auto write>
concept IsNoParamWriteoutOp = IsInvocableOperationNoParamWithRet<decltype(operation), decltype(write)>;

template<auto operation, auto read1, auto read2>
concept IsDualParamNoWriteoutOp = IsInvocableOperationDualParamNoRet<decltype(operation), decltype(read1), decltype(read2)>;

template<auto operation, auto write, auto read1>
concept IsSingleParamWriteoutOp = IsInvocableOperationSingleParamWithRet<decltype(operation), decltype(write), decltype(read1)>;

template<auto operation, auto read1, auto read2, auto write>
concept IsDualParamWriteoutOp = IsInvocableOperationDualParamWithRet<decltype(operation), decltype(read1), decltype(read2), decltype(write)>;
#pragma endregion

#pragma region OperationBases
template <auto operation>
    requires IsInvocableOperationNoParamNoRet<decltype(operation)>
void OperationBase() {
    operation();
}

template <auto operation, auto readO1_OrWrite>
    requires 
        ( IsSingleParamNoWriteoutOp<operation, readO1_OrWrite> ) ||
        ( IsNoParamWriteoutOp<operation, readO1_OrWrite> )
void OperationBase() {
    if constexpr( IsSingleParamNoWriteoutOp<operation, readO1_OrWrite> ) {
        operation( readO1_OrWrite() );
    }
    else if constexpr( IsNoParamWriteoutOp<operation, readO1_OrWrite> ) {
        readO1_OrWrite( operation() );
    }
    else {
        []<bool flag = false>() {
            static_assert( flag, "Invalid operation combination" );
        }();
    }
}

template <void(operation)(uint8_t, uint8_t), ReadMode8BitRegister readO1, ReadMode8BitRegister readO2>
    requires IsDualParamNoWriteoutOp<operation, readO1, readO2>
void OperationBase() {
    operation( readO1(), readO2() );
}
template <void(operation)(uint16_t, uint16_t), ReadMode16BitRegister readO1, ReadMode16BitRegister readO2>
    requires IsDualParamNoWriteoutOp<operation, readO1, readO2>
void OperationBase() {
    operation( readO1(), readO2() );
}

template <Op8BitSingleParamWithRet Operation, WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
void OperationBase() {
    WriteMode( Operation( ReadMode() ) );
}
template <Op16BitSingleParamWithRet Operation, WriteMode16BitRegister WriteMode, ReadMode16BitRegister ReadMode>
void OperationBase() {
    WriteMode( Operation( ReadMode() ) );
}


template <uint16_t(operation)(uint16_t, uint16_t), WriteMode16BitRegister write, ReadMode16BitRegister readO1, ReadMode16BitRegister readO2>
    requires IsDualParamWriteoutOp<operation, readO1, readO2, write>
void OperationBase() {
    write( operation( readO1(), readO2() ) );
}
template <uint8_t(operation)(uint8_t, uint8_t), WriteMode8BitRegister write, ReadMode8BitRegister readO1, ReadMode8BitRegister readO2>
    requires IsDualParamWriteoutOp<operation, readO1, readO2, write>
void OperationBase() {
    write( operation( readO1(), readO2() ) );
}
#pragma endregion

#pragma region Registers
enum class Flags {
    Carry = 0,
    AddSub = 1,
    ParityOverflow = 2,
    HalfCarry = 4,
    Zero = 6,
    Sign = 7
};


static uint16_t PC;
static uint16_t SP;
static uint16_t IX;
static uint16_t IY;
static uint8_t I;
static uint8_t R;

struct RegisterPair {
    uint16_t word;

    inline uint8_t highByte() const {
        return static_cast<uint8_t>( ( word >> 8 ) & 0x00FF );
    }
    inline void highByte( uint8_t value ) {
        word = ( word & 0x00FF ) | ( static_cast<uint16_t>( value ) << 8 );
    }

    inline uint8_t lowByte() const {
        return static_cast<uint8_t>( word & 0x00FF );
    }
    inline void lowByte( uint8_t value ) {
        word = ( word & 0xFF00 ) | static_cast<uint16_t>( value );
    }
};

struct RegisterSet {
    RegisterPair mainRegister;
    RegisterPair alternateRegister;

    std::string toString() {
        std::stringstream stream;
        stream << "{ " 
            << std::setfill( '0' ) << std::setw( 2 ) << std::hex << static_cast<uint16_t>( mainRegister.highByte() )
            << ", "
            << std::setfill( '0' ) << std::setw( 2 ) << std::hex << static_cast<uint16_t>( mainRegister.lowByte() )
            << " }";
        return stream.str();
    }
};


static RegisterSet A;
static RegisterSet BC;
static RegisterSet DE;
static RegisterSet HL;

static uint16_t nextPC;

static bool IFF1, IFF2;
static uint8_t InterruptMode;
static bool IRQPinLevel = true; // Active-low

template <Flags flag>
struct FlagControl {
    inline void operator=( bool rhs ) {
        constexpr uint8_t mask = 1u << static_cast<uint8_t>( flag );
        if ( rhs ) {
            A.mainRegister.lowByte( A.mainRegister.lowByte() | mask );
        }
        else {
            A.mainRegister.lowByte( A.mainRegister.lowByte() & ~mask );
        }
    }
    inline bool IsSet() const {
        constexpr uint8_t mask = 1u << static_cast<uint8_t>( flag );
        return A.mainRegister.lowByte() & mask;
    }

    inline void Invert() {
        constexpr uint8_t mask = 1u << static_cast<uint8_t>( flag );
        A.mainRegister.lowByte( A.mainRegister.lowByte() ^ mask );
    }
};

FlagControl<Flags::Carry> CarryFlag;
FlagControl<Flags::AddSub> AddSubFlag;
FlagControl<Flags::ParityOverflow> ParityOverflowFlag;
FlagControl<Flags::HalfCarry> HalfCarryFlag;
FlagControl<Flags::Zero> ZeroFlag;
FlagControl<Flags::Sign> SignFlag;

static inline void SetParity( uint8_t value ) {
    uint8_t pCount = 0;
    for( uint8_t i = 0; i < 8; ++i ) {
        if( value & 0x01 ) {
            ++pCount;
        }
        value >>= 1; 
    }

    ParityOverflowFlag = ( ( value % 2 ) == 0 );
}
#pragma endregion

/***********************************
*  Memory access
**********************************/
namespace MemoryAccess {

static inline uint16_t DualByteToWord( uint8_t msb, uint8_t lsb ) {
    return ( static_cast<uint16_t>( msb ) << 8 ) | static_cast<uint16_t>( lsb );
}

template<typename returnType>
    requires IsByteOrWord<returnType>
static inline returnType Read( uint16_t addressBus ) {
    if constexpr ( std::is_same<returnType, uint8_t>::value ) {
        return System::MemoryAccess( addressBus );
    }
    else {
        return DualByteToWord( System::MemoryAccess( addressBus + 1 ), System::MemoryAccess( addressBus ) );
    }
}

template<IsByteOrWord WriteType>
static inline void Write( uint16_t addressBus, WriteType data ) {
    if constexpr ( std::is_same<WriteType, uint8_t>::value ) {
        System::MemoryAccess( addressBus, data, true );
    }
    else {
        uint8_t holder = static_cast<uint8_t>( data & 0x00FF );
        System::MemoryAccess( addressBus, holder, true );
        holder = static_cast<uint8_t>( ( data >> 8 ) & 0x00FF );
        System::MemoryAccess( addressBus + 1, holder, true );
    }
}


}

/***********************************
*  Addressing modes
**********************************/
namespace AddressingModes {

template <uint8_t offset>
static inline uint8_t GetOpcodeByte() {
    uint8_t data = MemoryAccess::Read<uint8_t>( PC + offset );
    return data;
}
static inline uint8_t GetOpcodeByte() {
    uint8_t data = MemoryAccess::Read<uint8_t>( nextPC );
    ++nextPC;
    return data;
}

static inline uint16_t GetOpcodeWord() {
    uint16_t data = MemoryAccess::Read<uint16_t>( nextPC );
    nextPC += 2;
    return data;
}

template <uint8_t offset>
static inline uint8_t Immediate() {
    return GetOpcodeByte<offset>();
}
static inline uint8_t Immediate() {
    return GetOpcodeByte();
}

static inline uint16_t ImmediateExtended() {
    return GetOpcodeWord();
}

static inline int8_t Relative() {
    // TODO - verify the unsigned->signed cast
    return static_cast<int8_t>( Immediate() );
}
template <uint8_t offset>
static inline int8_t Relative() {
    // TODO - verify the unsigned->signed cast
    return static_cast<int8_t>( Immediate<offset>() );
}

static inline uint8_t Bit() {
    // TODO
    return 0;
}

static inline uint8_t EmbeddedBitOpcode() {
    nextPC -= 2;
    uint8_t data = GetOpcodeByte();
    nextPC += 2;
    return data;
}

template <uint8_t offset>
static inline uint16_t IndexedIXAddresso() {
    return IX + Relative<offset>();
}
template <uint8_t offset>
static inline uint16_t IndexedIYAddresso() {
    return IY + Relative<offset>();
}

static inline uint16_t IndexedIXAddress() {
    return IX + Relative();
}
static inline uint16_t IndexedIYAddress() {
    return IY + Relative();
}

// Read high/low byte of given register set
template<const RegisterSet &registerSet, bool highByte>
static inline uint8_t RegisterRead() {
    if constexpr ( highByte ) {
        return registerSet.mainRegister.highByte();
    }
    else {
        return registerSet.mainRegister.lowByte();
    }
}

// Read word of given register set
template<const RegisterSet &registerSet>
static inline uint16_t RegisterRead() {
    return registerSet.mainRegister.word;
}

// Read word of given register
template<uint16_t &reg>
static inline uint16_t RegisterRead() {
    return reg;
}
// Write word of given register
template<uint16_t &reg>
static inline void RegisterWrite( uint16_t data ) {
    reg = data;
}

// Write high/low byte of given register set
template<RegisterSet &registerSet, bool highByte>
static inline void RegisterWrite( uint8_t data ) {
    if constexpr ( highByte ) {
        registerSet.mainRegister.highByte( data );
    }
    else {
        registerSet.mainRegister.lowByte( data );
    }
}

// Write word of given register set
template<RegisterSet &registerSet>
static inline void RegisterWrite( uint16_t data ) {
    registerSet.mainRegister.word = data;
}

// Write byte of given word register
template<uint16_t &reg, bool highByte>
static inline void RegisterWrite( uint8_t data ) {
    if constexpr ( highByte ) {
        reg &= 0x00FF;
        reg |= static_cast<uint16_t>( data ) << 8;
    }
    else {
        reg &= 0xFF00;
        reg |= static_cast<uint16_t>( data );
    }
}

// Read byte of given word register
template<uint16_t &reg, bool highByte>
static inline uint8_t RegisterRead() {
    if constexpr ( highByte ) {
        return static_cast<uint8_t>( ( reg >> 8 ) & 0x00FF );
    }
    else {
        return static_cast<uint8_t>( reg & 0x00FF );
    }
}

// Read byte of given byte register
template<uint8_t &Reg>
static inline uint8_t RegisterRead() {
    return Reg;
}
// Write byte of given byte register
template<uint8_t &Reg>
static inline void RegisterWrite( uint8_t value ) {
    Reg = value;
}

// Read 8/16 bit value from address given by AddressingMode()
template<auto AddressingMode, IsByteOrWord ReturnType>
    requires std::is_same<std::invoke_result_t<decltype(AddressingMode)>, uint16_t>::value
static inline ReturnType AddressRead() {
    return MemoryAccess::Read<ReturnType>( AddressingMode() );
}

// Write 8/16 bit value to address given by AddressingMode()
template<auto AddressingMode, IsByteOrWord WriteType>
    requires std::is_same<std::invoke_result_t<decltype(AddressingMode)>, uint16_t>::value
static inline void AddressWrite( WriteType data ) {
    MemoryAccess::Write( AddressingMode(), data );
}

// Read 8/16 bit value from address given by register set word
template<const RegisterSet &registerSet, IsByteOrWord ReturnType>
static inline ReturnType AddressRead() {
    return AddressRead<RegisterRead<registerSet>, ReturnType>();
}

// Write 8/16 bit value to address given by register set word
template<const RegisterSet &registerSet, IsByteOrWord WriteType>
static inline void AddressWrite( WriteType data ) {
    AddressWrite<RegisterRead<registerSet>>( data );
}

// Read 8/16 bit value from address given by register word
template<const uint16_t &reg, IsByteOrWord ReturnType>
static inline ReturnType AddressRead() {
    return MemoryAccess::Read<ReturnType>( reg );
}

// Write 8/16 bit value to address given by register word
template<const uint16_t &reg, IsByteOrWord WriteType>
static inline void AddressWrite( WriteType data ) {
    MemoryAccess::Write( reg, data );
}



// Read 8 bit value from address given by AddressingMode()
template<uint8_t(AddressingMode)()>
static inline uint8_t PortRead() {
    uint8_t data;
    System::IOAccess( AddressingMode(), data, false );
    return data;
}

// Write 8 bit value to address given by AddressingMode()
template<uint8_t(AddressingMode)()>
static inline void PortWrite( uint8_t data ) {
    System::IOAccess( AddressingMode(), data, true );
}

// Read 8 bit value from address given by BC
template <RegisterSet &registerSet>
static inline uint8_t PortRead() {
    return PortRead<RegisterRead<registerSet, false>>();
}

// Write 8/16 bit value to address given by register word
template <RegisterSet &registerSet>
static inline void PortWrite( uint8_t data ) {
    PortWrite<RegisterRead<registerSet, false>>( data );
}

template<Flags flag>
static inline bool FlagSet() {
    return RegisterRead<A, false>() & ( 1 << static_cast<uint8_t>( flag ) );
}

template<Flags flag>
static inline bool FlagClear() {
    return !FlagSet<flag>();
}
}

#pragma region OpMnemonics

namespace AddressingMnemonics {
static inline void A( uint8_t val ) { AddressingModes::RegisterWrite<::A, true>( val ); }
static inline uint8_t A() { return AddressingModes::RegisterRead<::A, true>(); }
static inline void B( uint8_t val ) { AddressingModes::RegisterWrite<::BC, true>( val ); }
static inline uint8_t B() { return AddressingModes::RegisterRead<::BC, true>(); }
static inline void C( uint8_t val ) { AddressingModes::RegisterWrite<::BC, false>( val ); }
static inline uint8_t C() { return AddressingModes::RegisterRead<::BC, false>(); }
static inline void D( uint8_t val ) { AddressingModes::RegisterWrite<::DE, true>( val ); }
static inline uint8_t D() { return AddressingModes::RegisterRead<::DE, true>(); }
static inline void E( uint8_t val ) { AddressingModes::RegisterWrite<::DE, false>( val ); }
static inline uint8_t E() { return AddressingModes::RegisterRead<::DE, false>(); }
static inline void H( uint8_t val ) { AddressingModes::RegisterWrite<::HL, true>( val ); }
static inline uint8_t H() { return AddressingModes::RegisterRead<::HL, true>(); }
static inline void L( uint8_t val ) { AddressingModes::RegisterWrite<::HL, false>( val ); }
static inline uint8_t L() { return AddressingModes::RegisterRead<::HL, false>(); }
static inline void IXH( uint8_t val ) { AddressingModes::RegisterWrite<::IX, true>( val ); }
static inline uint8_t IXH() { return AddressingModes::RegisterRead<::IX, true>(); }
static inline void IXL( uint8_t val ) { AddressingModes::RegisterWrite<::IX, false>( val ); }
static inline uint8_t IXL() { return AddressingModes::RegisterRead<::IX, false>(); }
static inline void IYH( uint8_t val ) { AddressingModes::RegisterWrite<::IY, true>( val ); }
static inline uint8_t IYH() { return AddressingModes::RegisterRead<::IY, true>(); }
static inline void IYL( uint8_t val ) { AddressingModes::RegisterWrite<::IY, false>( val ); }
static inline uint8_t IYL() { return AddressingModes::RegisterRead<::IY, false>(); }
static inline void I( uint8_t val ) { AddressingModes::RegisterWrite<::I>( val ); }
static inline uint8_t I() { return AddressingModes::RegisterRead<::I>(); }
static inline void R( uint8_t val ) { AddressingModes::RegisterWrite<::R>( val ); }
static inline uint8_t R() { return AddressingModes::RegisterRead<::R>(); }

static inline void AF( uint16_t val ) { AddressingModes::RegisterWrite<::A>( val ); }
static inline uint16_t AF() { return AddressingModes::RegisterRead<::A>(); }
static inline void BC( uint16_t val ) { AddressingModes::RegisterWrite<::BC>( val ); }
static inline uint16_t BC() { return AddressingModes::RegisterRead<::BC>(); }
static inline void DE( uint16_t val ) { AddressingModes::RegisterWrite<::DE>( val ); }
static inline uint16_t DE() { return AddressingModes::RegisterRead<::DE>(); }
static inline void HL( uint16_t val ) { AddressingModes::RegisterWrite<::HL>( val ); }
static inline uint16_t HL() { return AddressingModes::RegisterRead<::HL>(); }
static inline void IX( uint16_t val ) { AddressingModes::RegisterWrite<::IX>( val ); }
static inline uint16_t IX() { return AddressingModes::RegisterRead<::IX>(); }
static inline void IY( uint16_t val ) { AddressingModes::RegisterWrite<::IY>( val ); }
static inline uint16_t IY() { return AddressingModes::RegisterRead<::IY>(); }
static inline void SP( uint16_t val ) { AddressingModes::RegisterWrite<::SP>( val ); }
static inline uint16_t SP() { return AddressingModes::RegisterRead<::SP>(); }
static inline void PC( uint16_t val ) { AddressingModes::RegisterWrite<::PC>( val ); }
static inline uint16_t PC() { return AddressingModes::RegisterRead<::PC>(); }

static inline int8_t r() { return AddressingModes::Relative(); }
static inline uint8_t n() { return AddressingModes::Immediate(); }
static inline uint16_t nn() { return AddressingModes::ImmediateExtended(); }

static inline uint8_t _n_() { return AddressingModes::PortRead<AddressingModes::Immediate>(); }
static inline void _n_( uint8_t val ) { AddressingModes::PortWrite<AddressingModes::Immediate>( val ); }
static inline uint8_t _C_() { return AddressingModes::PortRead<::BC>(); }
static inline void _C_( uint8_t val ) { AddressingModes::PortWrite<::BC>( val ); }
static inline uint8_t _E_() { return AddressingModes::PortRead<::DE>(); }
static inline void _E_( uint8_t val ) { AddressingModes::PortWrite<::DE>( val ); }

template <IsByteOrWord ReturnType>
static inline ReturnType _nn_() { return AddressingModes::AddressRead<AddressingModes::ImmediateExtended, ReturnType>(); }
template <IsByteOrWord T>
static inline void _nn_( T val ) { AddressingModes::AddressWrite<AddressingModes::ImmediateExtended, T>( val ); }
template <IsByteOrWord ReturnType>
static inline ReturnType _BC_() { return AddressingModes::AddressRead<::BC, ReturnType>(); }
template <IsByteOrWord T>
static inline void _BC_( T val ) { AddressingModes::AddressWrite<::BC, T>( val ); }
template <IsByteOrWord ReturnType>
static inline ReturnType _DE_() { return AddressingModes::AddressRead<::DE, ReturnType>(); }
template <IsByteOrWord T>
static inline void _DE_( T val ) { AddressingModes::AddressWrite<::DE, T>( val ); }
template <IsByteOrWord ReturnType>
static inline ReturnType _HL_() { return AddressingModes::AddressRead<::HL, ReturnType>(); }
template <IsByteOrWord T>
static inline void _HL_( T val ) { AddressingModes::AddressWrite<::HL, T>( val ); }
template <IsByteOrWord ReturnType>
static inline ReturnType _SP_() { return AddressingModes::AddressRead<::SP, ReturnType>(); }
template <IsByteOrWord T>
static inline void _SP_( T val ) { AddressingModes::AddressWrite<::SP, T>( val ); }
template <IsByteOrWord ReturnType>
static inline ReturnType _IX_() { return AddressingModes::AddressRead<::IX, ReturnType>(); }
template <IsByteOrWord T>
static inline void _IX_( T val ) { AddressingModes::AddressWrite<::IX, T>( val ); }
template <IsByteOrWord ReturnType>
static inline ReturnType _IY_() { return AddressingModes::AddressRead<::IY, ReturnType>(); }
template <IsByteOrWord T>
static inline void _IY_( T val ) { AddressingModes::AddressWrite<::IY, T>( val ); }

template <IsByteOrWord ReturnType>
static inline ReturnType _IX_d_() { return AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, ReturnType>(); }
template <IsByteOrWord T>
static inline void _IX_d_( T val ) { AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, T>( val ); }

template <IsByteOrWord ReturnType, uint8_t offset>
static inline ReturnType _IX_d_() { return AddressingModes::AddressRead<AddressingModes::IndexedIXAddresso<offset>, ReturnType>(); }
template <IsByteOrWord T, uint8_t offset>
static inline void _IX_d_( T val ) { AddressingModes::AddressWrite<AddressingModes::IndexedIXAddresso<offset>, T>( val ); }
template <IsByteOrWord ReturnType>
static inline ReturnType _IY_d_() { return AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, ReturnType>(); }
template <IsByteOrWord T>
static inline void _IY_d_( T val ) { AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, T>( val ); }
}

namespace FlagMnemonics {
static inline bool C() { return CarryFlag.IsSet(); }
static inline bool NC() { return !C(); }
static inline bool Z() { return ZeroFlag.IsSet(); }
static inline bool NZ() { return !Z(); }
static inline bool PE() { return ParityOverflowFlag.IsSet(); }
static inline bool PO() { return !PE(); }
static inline bool M() { return SignFlag.IsSet(); }
static inline bool P() { return !M(); }
}


#pragma endregion

/***********************************
*  Operations
**********************************/
namespace Operations {

/**********************************
 *  Load & Exchange
 *********************************/
#pragma region LoadAndExchange
static inline uint8_t LD8( uint8_t O1 ) {
    return O1;
}
static inline uint16_t LD16( uint16_t O1 ) {
    return O1;
}

static inline uint8_t POP8() {
    return MemoryAccess::Read<uint8_t>( SP++ );
}
static inline uint16_t POP16() {
    uint16_t value = POP8();
    return value | static_cast<uint16_t>( POP8() ) << 8;
}

static inline void PUSH8( uint8_t data ) {
    MemoryAccess::Write( --SP, data );
}
static inline void PUSH16( uint16_t data ) {
    PUSH8( static_cast<uint8_t>( ( data >> 8 ) & 0x00FF ) );
    PUSH8( static_cast<uint8_t>( data & 0x00FF ) );
}

// Exchange main/alt registers of a given register set
template<RegisterSet &registerSet>
static inline void EX() {
    registerSet.alternateRegister.word = std::exchange( registerSet.mainRegister.word, registerSet.alternateRegister.word );
}

// Exchange main registers of two different sets
template<RegisterSet &registerSet1, RegisterSet &registerSet2>
static inline void EX() {
    registerSet1.mainRegister.word = std::exchange( registerSet2.mainRegister.word, registerSet1.mainRegister.word );
}

// Exchange 16 bits of memory data with a given register set
template<RegisterSet &registerSet>
static inline uint16_t EX_mem( uint16_t memoryData ) {
    // Return value is written to memory
    uint16_t toReturn = AddressingModes::RegisterRead<registerSet>();
    AddressingModes::RegisterWrite<registerSet>( memoryData );
    return toReturn;
}
template<uint16_t &reg>
static inline uint16_t EX_mem( uint16_t memoryData ) {
    // Return value is written to memory
    uint16_t toReturn = AddressingModes::RegisterRead<reg>();
    AddressingModes::RegisterWrite<reg>( memoryData );
    return toReturn;
}

// Exchange main/alt registers of BC, DE, and HL register sets
static inline void EXX() {
    EX<BC>();
    EX<DE>();
    EX<HL>();
}

#pragma endregion

/**********************************
 *  Arithmetic & Logic
 *********************************/
#pragma region ArithmeticAndLogic

template<IsByteOrWord T>
static inline T ADC( T O1, T O2 ) {
    const uint8_t carry = CarryFlag.IsSet() ? 0x01 : 0x00;
    T result = O1 + O2 + carry;
    
    AddSubFlag = false;
    if constexpr ( std::is_same<T, uint8_t>::value ) {
        SignFlag = result & 0x80;
        ZeroFlag = result == 0;
        HalfCarryFlag = ( ( O1 & 0x0F ) + ( O2 & 0x0F ) + carry ) > 0x0F;
        CarryFlag = O1 > 0xFF - O2 - carry;
        ParityOverflowFlag = (~( O1 ^ O2 ) & ( O1 ^ result ) ) & 0x80;
    }
    else {
        SignFlag = result & 0x8000;
        ZeroFlag = result == 0;
        CarryFlag = O1 > 0xFFFF - O2;
        HalfCarryFlag = ( ( O1 & 0x0FFF ) + ( O2 & 0x0FFF ) ) > 0x0FFF;
        ParityOverflowFlag = (~( O1 ^ O2 ) & ( O1 ^ result ) ) & 0x8000;
    }

    return result;
}
static inline uint8_t ADC8( uint8_t O1, uint8_t O2 ) {
    return ADC<uint8_t>( O1, O2 );
}
static inline uint16_t ADC16( uint16_t O1, uint16_t O2 ) {
    return ADC<uint16_t>( O1, O2 );
}
static inline uint8_t ADD8( uint8_t O1, uint8_t O2 ) {
    CarryFlag = false;
    return ADC<uint8_t>( O1, O2 );
}
static inline uint16_t ADD16( uint16_t O1, uint16_t O2 ) {
    CarryFlag = false;
    return ADC<uint16_t>( O1, O2 );
}

static inline uint8_t SUB( uint8_t O1, uint8_t O2 ) {
    // TODO - verify this
    CarryFlag = true;
    uint8_t result = ADC<uint8_t>( O1, ~O2 );
    CarryFlag.Invert();
    HalfCarryFlag.Invert();
    AddSubFlag = true;
    return result;
}

template<IsByteOrWord T>
static inline T SBC( T O1, T O2 ) {
    // TODO - verify
    CarryFlag.Invert();
    T result = ADC<T>( O1, ~O2 );

    CarryFlag.Invert();
    HalfCarryFlag.Invert();
    AddSubFlag = true;
    return result;
}
static inline uint8_t SBC8( uint8_t O1, uint8_t O2 ) {
    return SBC<uint8_t>( O1, O2 );
}
static inline uint16_t SBC16( uint16_t O1, uint16_t O2 ) {
    return SBC<uint16_t>( O1, O2 );
}

template<IsByteOrWord T>
static inline T INC( T O1 ) {
    T result = O1 + 1;
    if constexpr ( std::is_same<T, uint8_t>::value ) {
        ZeroFlag = result == 0;
        SignFlag = result & 0x80;
        HalfCarryFlag = O1 == 0x0F;
        ParityOverflowFlag = O1 == 0x7F;
        AddSubFlag = false;
    }
    return result;
}
static inline uint8_t INC8( uint8_t O1 ) {
    uint8_t result = O1 + 1;

    ZeroFlag = result == 0;
    SignFlag = result & 0x80;
    HalfCarryFlag = O1 == 0x0F;
    ParityOverflowFlag = O1 == 0x7F;
    AddSubFlag = false;

    return result;
}
static inline uint16_t INC16( uint16_t O1 ) {
    return O1 + 1;
}

static inline uint8_t DEC8( uint8_t O1 ) {
    uint8_t result = O1 - 1;

    ZeroFlag = result == 0;
    SignFlag = result & 0x80;
    HalfCarryFlag = O1 == 0x10;
    ParityOverflowFlag = O1 == 0x80;
    AddSubFlag = true;

    return result;
}
static inline uint16_t DEC16( uint16_t O1 ) {
    return O1 - 1;
}

static inline uint8_t AND( uint8_t O1, uint8_t O2 ) {
    uint8_t result = O1 & O2;

    SignFlag = result & 0x80;
    ZeroFlag = result == 0;
    HalfCarryFlag = true;
    AddSubFlag = false;
    CarryFlag = false;
    SetParity( result );
    
    return result;
}

static inline uint8_t OR( uint8_t O1, uint8_t O2 ) {
    uint8_t result = O1 | O2;

    SignFlag = result & 0x80;
    ZeroFlag = result == 0;
    HalfCarryFlag = false;
    AddSubFlag = false;
    CarryFlag = false;
    SetParity( result );
    
    return result;
}

static inline uint8_t XOR( uint8_t O1, uint8_t O2 ) {
    uint8_t result = O1 ^ O2;

    SignFlag = result & 0x80;
    ZeroFlag = result == 0;
    HalfCarryFlag = false;
    AddSubFlag = false;
    CarryFlag = false;
    SetParity( result );
    
    return result;
}

static inline void CP( uint8_t O1, uint8_t O2 ) {
    // TODO - verify
    SUB( O1, O2 );
}

static inline uint8_t DAA( uint8_t O1 ) {
    // TODO - verify
    if ( HalfCarryFlag.IsSet() || ( ( O1 & 0x0F ) > 9 ) ) {
        if ( AddSubFlag.IsSet() ) {
            O1 -= 0x06;
        }
        else {
            O1 += 0x06;
        }
    }
    if ( CarryFlag.IsSet() || ( ( O1 & 0xF0 ) > ( 9 << 4 ) ) ) {
        if ( AddSubFlag.IsSet() ) {
            O1 -= 0x60;
        }
        else {
            O1 += 0x60;
        }
    }
    return O1;
}

static inline void CPL() {
    A.mainRegister.highByte( ~A.mainRegister.highByte() );
    HalfCarryFlag = true;
    AddSubFlag = true;
}

static inline uint8_t NEG( uint8_t O1 ) {
    // TODO - verify
    return SUB( 0u, O1 );
}

static inline void CCF() {
    HalfCarryFlag = CarryFlag.IsSet();
    CarryFlag.Invert();
    AddSubFlag = false;
}

static inline void SCF() {
    AddSubFlag = false;
    HalfCarryFlag = false;
    CarryFlag = true;
}

#pragma endregion

/**********************************
 *  Block Transfer & Search
 *********************************/
#pragma region BlockTransferAndSearch
template<bool repeat>
static inline uint8_t LDN( uint8_t O1 ) {
    --BC.mainRegister.word;

    if constexpr( repeat ) {
        if ( BC.mainRegister.word > 0 ) {
            nextPC = PC;
        }
        else {
            ParityOverflowFlag = false;
            AddSubFlag = false;
        }
    }
    else {
        ParityOverflowFlag = BC.mainRegister.word > 0;
        AddSubFlag = false;
    }

    return O1;
}
template <RegisterSet &RegisterSet>
static inline uint16_t BlockExchangeGetWriteAddress() {
    return RegisterSet.mainRegister.word - 1u;
}
template<bool increment, bool repeat>
static inline void OperationLDN() {
    OperationBase<LDN<repeat>, AddressingModes::AddressWrite<DE, uint8_t>, AddressingModes::AddressRead<HL, uint8_t>>();
    if constexpr ( increment ) {
        ++DE.mainRegister.word;
        ++HL.mainRegister.word;
    }
    else {
        --DE.mainRegister.word;
        --HL.mainRegister.word;
    }
}

template<bool increment, bool repeat>
static inline void CPN( uint8_t O1 ) {
    bool prevCarry = CarryFlag.IsSet();
    CP( A.mainRegister.highByte(), O1 );
    CarryFlag = prevCarry;

    --BC.mainRegister.word;
    if constexpr ( increment ) {
        ++HL.mainRegister.word;
    }
    else {
        --HL.mainRegister.word;
    }
    if constexpr( repeat ) {
        if ( BC.mainRegister.word > 0 && !ZeroFlag.IsSet() ) {
            nextPC = PC;
        }
        else {
            ParityOverflowFlag = false;
            AddSubFlag = true;
        }
    }
    else {
        ParityOverflowFlag = BC.mainRegister.word > 0;
        AddSubFlag = true;
    }
}
template<bool increment, bool repeat>
static inline void OperationCPN() {
    OperationBase<CPN<increment, repeat>, AddressingModes::AddressRead<HL, uint8_t>>();
}
#pragma endregion

/**********************************
 *  Rotate & Shift
 *********************************/
#pragma region RotateAndShift
static inline uint8_t RLC( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    const bool carrySet = O1 & 0x80;
    CarryFlag = carrySet;
    
    uint8_t result = ( O1 << 1 ) | ( carrySet ? 0x01 : 0x00 );
    ZeroFlag = result == 0x00;
    SignFlag = ( result & 0x80 ) == 0x80;
    SetParity( result );

    return result;
}

static inline uint8_t RLCA( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    const bool carrySet = O1 & 0x80;
    CarryFlag = carrySet;
    return ( O1 << 1 ) | ( carrySet ? 0x01 : 0x00 );
}

template<auto ExtraWriteRegisterMode>
static inline uint8_t RLC1( uint8_t value ) {
    value = RLC( value );
    ExtraWriteRegisterMode( value );
    return value;
}
template<RegisterSet &registerSet, bool highByte>
static inline void OperationRLC() {
    OperationBase<RLC, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<RegisterSet &registerSet>
static inline void OperationRLC() {
    OperationBase<RLC, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}
template<auto ExtraWriteRegisterMode, auto WriteMode, auto ReadMode>
static inline void OperationRLC() {
    OperationBase<RLC1<ExtraWriteRegisterMode>, WriteMode, ReadMode>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationRLC() {
    OperationBase<RLC, WriteMode, ReadMode>();
}

static inline uint8_t RRC( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    const bool carrySet = O1 & 0x01;
    CarryFlag = carrySet;

    uint8_t result = ( O1 >> 1 ) | ( carrySet ? 0x80 : 0x00 );
    ZeroFlag = result == 0x00;
    SignFlag = ( result & 0x80 ) == 0x80;
    SetParity( result );

    return result;
}
static inline uint8_t RRCA( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    const bool carrySet = O1 & 0x01;
    CarryFlag = carrySet;
    return ( O1 >> 1 ) | ( carrySet ? 0x80 : 0x00 );
}
template<auto ExtraWriteRegisterMode>
static inline uint8_t RRC1( uint8_t value ) {
    value = RRC( value );
    ExtraWriteRegisterMode( value );
    return value;
}
template<RegisterSet &registerSet, bool highByte>
static inline void OperationRRC() {
    OperationBase<RRC, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<RegisterSet &registerSet>
static inline void OperationRRC() {
    OperationBase<RRC, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}
template<auto ExtraWriteRegisterMode, auto WriteMode, auto ReadMode>
static inline void OperationRRC() {
    OperationBase<RRC1<ExtraWriteRegisterMode>, WriteMode, ReadMode>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationRRC() {
    OperationBase<RRC, WriteMode, ReadMode>();
}

static inline uint8_t RL( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    const uint8_t prevCarryAdd = CarryFlag.IsSet() ? 0x01 : 0x00;
    CarryFlag = O1 & 0x80;

    uint8_t result = ( O1 << 1 ) | prevCarryAdd;
    ZeroFlag = result == 0x00;
    SignFlag = ( result & 0x80 ) == 0x80;
    SetParity( result );
    return result;
    
}
static inline uint8_t RLA( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    const uint8_t prevCarryAdd = CarryFlag.IsSet() ? 0x01 : 0x00;
    CarryFlag = O1 & 0x80;

    return ( O1 << 1 ) | prevCarryAdd;
}
template<auto ExtraWriteRegisterMode>
static inline uint8_t RL1( uint8_t value ) {
    value = RL( value );
    ExtraWriteRegisterMode( value );
    return value;
}
template<RegisterSet &registerSet, bool highByte>
static inline void OperationRL() {
    OperationBase<RL, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<RegisterSet &registerSet>
static inline void OperationRL() {
    OperationBase<RL, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}
template<auto ExtraWriteRegisterMode, auto WriteMode, auto ReadMode>
static inline void OperationRL() {
    OperationBase<RL1<ExtraWriteRegisterMode>, WriteMode, ReadMode>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationRL() {
    OperationBase<RL, WriteMode, ReadMode>();
}

static inline uint8_t RR( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    const uint8_t prevCarryAdd = CarryFlag.IsSet() ? 0x80 : 0x00;
    CarryFlag = O1 & 0x01;

    uint8_t result = ( O1 >> 1 ) | prevCarryAdd;
    ZeroFlag = result == 0x00;
    SignFlag = ( result & 0x80 ) == 0x80;
    SetParity( result );

    return result;
}
static inline uint8_t RRA( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    const uint8_t prevCarryAdd = CarryFlag.IsSet() ? 0x80 : 0x00;
    CarryFlag = O1 & 0x01;

    return ( O1 >> 1 ) | prevCarryAdd;
}
template<auto ExtraWriteRegisterMode>
static inline uint8_t RR1( uint8_t value ) {
    value = RR( value );
    ExtraWriteRegisterMode( value );
    return value;
}
template<RegisterSet &registerSet, bool highByte>
static inline void OperationRR() {
    OperationBase<RR, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<RegisterSet &registerSet>
static inline void OperationRR() {
    OperationBase<RR, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}
template<auto ExtraWriteRegisterMode, auto WriteMode, auto ReadMode>
static inline void OperationRR() {
    OperationBase<RR1<ExtraWriteRegisterMode>, WriteMode, ReadMode>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationRR() {
    OperationBase<RR, WriteMode, ReadMode>();
}

static inline uint8_t SLA( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    CarryFlag = O1 & 0x80;

    uint8_t result = O1 << 1;
    SignFlag = result & 0x80;
    ZeroFlag = result == 0;
    HalfCarryFlag = false;
    SetParity( result );
    AddSubFlag = false;
    return result;
}
template<auto ExtraWriteRegisterMode>
static inline uint8_t SLA1( uint8_t value ) {
    value = SLA( value );
    ExtraWriteRegisterMode( value );
    return value;
}
template<RegisterSet &registerSet, bool highByte>
static inline void OperationSLA() {
    OperationBase<SLA, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<RegisterSet &registerSet>
static inline void OperationSLA() {
    OperationBase<SLA, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}

template<auto ExtraWriteRegisterMode, auto WriteMode, auto ReadMode>
static inline void OperationSLA() {
    OperationBase<SLA1<ExtraWriteRegisterMode>, WriteMode, ReadMode>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationSLA() {
    OperationBase<SLA, WriteMode, ReadMode>();
}

static inline uint8_t SRA( uint8_t O1 ) {
    uint8_t neg = O1 & 0x80;
    CarryFlag = O1 & 0x01;

    uint8_t result = ( O1 >> 1 ) | neg;
    SignFlag = neg;
    ZeroFlag = result == 0;
    HalfCarryFlag = false;
    SetParity( result );
    AddSubFlag = false;
    return result;
}
template<auto ExtraWriteRegisterMode>
static inline uint8_t SRA1( uint8_t value ) {
    value = SRA( value );
    ExtraWriteRegisterMode( value );
    return value;
}
template<RegisterSet &registerSet, bool highByte>
static inline void OperationSRA() {
    OperationBase<SRA, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<RegisterSet &registerSet>
static inline void OperationSRA() {
    OperationBase<SRA, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}
template<auto ExtraWriteRegisterMode, auto WriteMode, auto ReadMode>
static inline void OperationSRA() {
    OperationBase<SRA1<ExtraWriteRegisterMode>, WriteMode, ReadMode>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationSRA() {
    OperationBase<SRA, WriteMode, ReadMode>();
}

static inline uint8_t SLL( uint8_t O1 ) {
    HalfCarryFlag = false;
    AddSubFlag = false;
    CarryFlag = O1 & 0x80;

    uint8_t result = ( O1 << 1 ) | 0x01;
    SignFlag = result & 0x80;
    ZeroFlag = false;
    HalfCarryFlag = false;
    SetParity( result );
    AddSubFlag = false;
    return result;
}
template<auto ExtraWriteRegisterMode>
static inline uint8_t SLL1( uint8_t value ) {
    value = SLL( value );
    ExtraWriteRegisterMode( value );
    return value;
}
template<RegisterSet &registerSet, bool highByte>
static inline void OperationSLL() {
    OperationBase<SLL, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<RegisterSet &registerSet>
static inline void OperationSLL() {
    OperationBase<SLL, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}
template<auto ExtraWriteRegisterMode, auto WriteMode, auto ReadMode>
static inline void OperationSLL() {
    OperationBase<SLL1<ExtraWriteRegisterMode>, WriteMode, ReadMode>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationSLL() {
    OperationBase<SLL, WriteMode, ReadMode>();
}

static inline uint8_t SRL( uint8_t O1 ) {
    CarryFlag = O1 & 0x01;

    uint8_t result = ( O1 >> 1 ) & ~0x80;
    SignFlag = false;
    ZeroFlag = result == 0;
    HalfCarryFlag = false;
    SetParity( result );
    AddSubFlag = false;
    return result;
}
template<auto ExtraWriteRegisterMode>
static inline uint8_t SRL1( uint8_t value ) {
    value = SRL( value );
    ExtraWriteRegisterMode( value );
    return value;
}
template<auto ExtraWriteRegisterMode, auto WriteMode, auto ReadMode>
static inline void OperationSRL() {
    OperationBase<SRL1<ExtraWriteRegisterMode>, WriteMode, ReadMode>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationSRL() {
    OperationBase<SRL, WriteMode, ReadMode>();
}
template<RegisterSet &registerSet, bool highByte>
static inline void OperationSRL() {
    OperationBase<SRL, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<RegisterSet &registerSet>
static inline void OperationSRL() {
    OperationBase<SRL, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}

static inline uint8_t RLD( uint8_t O1 ) {
    uint8_t result = 0;

    uint8_t accLowOrder = A.mainRegister.highByte() & 0x0F;
    uint8_t lowOrderMem = O1 & 0x0F;
    uint8_t highOrderMem = O1 & 0xF0;

    result |= accLowOrder;
    result |= lowOrderMem << 4;

    A.mainRegister.highByte( ( A.mainRegister.highByte() & 0xF0 ) | ( highOrderMem >> 4 ) );
    
    SignFlag = A.mainRegister.highByte() & 0x80;
    ZeroFlag = A.mainRegister.highByte() == 0;
    HalfCarryFlag = false;
    SetParity( A.mainRegister.highByte() );
    AddSubFlag = false;
    return result;
}

static inline uint8_t RRD( uint8_t O1 ) {
    uint8_t result = 0;

    uint8_t accLowOrder = A.mainRegister.highByte() & 0x0F;
    uint8_t lowOrderMem = O1 & 0x0F;
    uint8_t highOrderMem = O1 & 0xF0;

    result |= accLowOrder << 4;
    result |= lowOrderMem;

    A.mainRegister.highByte( ( A.mainRegister.highByte() & 0xF0 ) | lowOrderMem );
    
    SignFlag = A.mainRegister.highByte() & 0x80;
    ZeroFlag = A.mainRegister.highByte() == 0;
    HalfCarryFlag = false;
    SetParity( A.mainRegister.highByte() );
    AddSubFlag = false;
    return result;
}


#pragma endregion
/**********************************
 *  Bit Manipulation
 *********************************/
#pragma region BitManipulation
template <uint8_t BitPos>
static inline void BIT( uint8_t O1 ) {
    HalfCarryFlag = false;
    ZeroFlag = ( O1 & ( 1u << BitPos ) ) == 0x00;
}
template<uint8_t BitPos, RegisterSet &registerSet, bool highByte>
static inline void OperationBIT() {
    OperationBase<BIT<BitPos>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<uint8_t BitPos, RegisterSet &registerSet>
static inline void OperationBIT() {
    OperationBase<BIT<BitPos>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}
template<uint8_t BitPos, auto ReadMode>
static inline void OperationBIT() {
    OperationBase<BIT<BitPos>, ReadMode>();
}

template <uint8_t BitPos>
static inline uint8_t RES( uint8_t O1 ) {
    return O1 & ~( 1u << BitPos );
}
template <uint8_t BitPos, auto ExtraRegisterWriteMode>
static inline uint8_t RES1( uint8_t O1 ) {
    uint8_t result = O1 & ~( 1u << BitPos );
    ExtraRegisterWriteMode( result );
    return result;
}
template<uint8_t BitPos, RegisterSet &registerSet, bool highByte>
static inline void OperationRES() {
    OperationBase<RES<BitPos>, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<uint8_t BitPos, RegisterSet &registerSet>
static inline void OperationRES() {
    OperationBase<RES<BitPos>, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}
template<uint8_t BitPos, auto WriteMode, auto ReadMode>
static inline void OperationRES() {
    OperationBase<RES<BitPos>, WriteMode, ReadMode>();
}
template<uint8_t BitPos, auto ExtraRegisterWriteMode, auto WriteMode, auto ReadMode>
static inline void OperationRES() {
    OperationBase<RES1<BitPos, ExtraRegisterWriteMode>, WriteMode, ReadMode>();
}

template <uint8_t BitPos>
static inline uint8_t SET( uint8_t O1 ) {
    return O1 | ( 1u << BitPos );
}
template <uint8_t BitPos, auto ExtraRegisterWriteMode>
static inline uint8_t SET1( uint8_t O1 ) {
    uint8_t result = O1 | ( 1u << BitPos );
    ExtraRegisterWriteMode( result );
    return result;
}
template<uint8_t BitPos, RegisterSet &registerSet, bool highByte>
static inline void OperationSET() {
    OperationBase<SET<BitPos>, AddressingModes::RegisterWrite<registerSet, highByte>, AddressingModes::RegisterRead<registerSet, highByte>>();
}
template<uint8_t BitPos, RegisterSet &registerSet>
static inline void OperationSET() {
    OperationBase<SET<BitPos>, AddressingModes::AddressWrite<registerSet, uint8_t>, AddressingModes::AddressRead<registerSet, uint8_t>>();
}
template<uint8_t BitPos, auto WriteMode, auto ReadMode>
static inline void OperationSET() {
    OperationBase<SET<BitPos>, WriteMode, ReadMode>();
}
template<uint8_t BitPos, auto ExtraRegisterWriteMode, auto WriteMode, auto ReadMode>
static inline void OperationSET() {
    OperationBase<SET1<BitPos, ExtraRegisterWriteMode>, WriteMode, ReadMode>();
}

#pragma endregion

/**********************************
 *  Jump, Call, & Return
 *********************************/
#pragma region JumpCallAndReturn
template<ReadModeBool Condition>
static inline void JR( int8_t O1 ) {
    if ( Condition() ) {
        nextPC = nextPC + O1;
    }
}
static inline void JR1( int8_t O1 ) {
    nextPC = nextPC + O1;
}
static inline void DJNZ( int8_t O1 ) {
    uint8_t B = AddressingModes::RegisterRead<BC, true>() - 1u;
    AddressingModes::RegisterWrite<BC, true>( B );
    if ( B != 0 ) {
        nextPC = nextPC + O1;
    }
}

template <ReadModeBool Condition>
static inline void JP( uint16_t O1 ) {
    if ( Condition() ) {
        nextPC = O1;
    }
}
static inline void JP1( uint16_t O1 ) {
    nextPC = O1;
}

template<ReadModeBool Condition>
static inline void CALL( uint16_t addr ) {
    if( Condition() ) {
        PUSH16( nextPC );
        nextPC = addr;
    }
}
static inline void CALL1( uint16_t addr ) {
    PUSH16( nextPC );
    nextPC = addr;
}

template<auto ConditionOp>
static inline void RET() {
    if ( ConditionOp() ) {
        nextPC = POP16();
    }
}
static inline void RET1() {
    nextPC = POP16();
}

static inline void RETN() {
    IFF1 = IFF2;
    nextPC = POP16();
}

static inline void RETI( ) {
    nextPC = POP16();
    // TODO - signal IO that IRQ is complete
}

template<uint16_t resetVector>
static inline void RST() {
    PUSH16( nextPC );
    nextPC = resetVector;
}

#pragma endregion

/**********************************
 *  Input/Output
 *********************************/
#pragma region InputOutput
static inline uint8_t INA( uint8_t data ) {
    return data;
}
static inline uint8_t INR( uint8_t data ) {
    SignFlag = data & 0x80;
    ZeroFlag = data == 0;
    HalfCarryFlag = 0;
    SetParity( data );
    AddSubFlag = false;

    return data;
}
static inline uint8_t OUT( uint8_t data ) {
    return data;
}
static inline void IN1( uint8_t data ) {
    INR( data );
}
static inline uint8_t OUT1() {
    return 0;
}

template<bool increment, bool repeat>
static inline uint8_t INN( uint8_t O1 ) {
    AddSubFlag = true;
    BC.mainRegister.highByte( BC.mainRegister.highByte() - 1 );

    if constexpr( repeat ) {
        if ( BC.mainRegister.highByte() != 0 ) {
            nextPC = PC;
        }
        else {
            ZeroFlag = true;
        }
    }
    else {
        ZeroFlag = BC.mainRegister.highByte() == 0;
    }
    return O1;
}
template<bool increment, bool repeat>
static inline void OperationINN8() {
    OperationBase<INN<increment, repeat>, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::PortRead<BC>>();
    if constexpr ( increment ) {
        ++HL.mainRegister.word;
    }
    else {
        --HL.mainRegister.word;
    }
}

template<bool increment, bool repeat>
static inline uint8_t OUTN( uint8_t O1 ) {
    AddSubFlag = true;
    BC.mainRegister.highByte( BC.mainRegister.highByte() - 1 );
    if constexpr ( increment ) {
        ++HL.mainRegister.word;
    }
    else {
        --HL.mainRegister.word;
    }

    if constexpr( repeat ) {
        if ( BC.mainRegister.highByte() != 0 ) {
            nextPC = PC;
        }
        else {
            ZeroFlag = true;
        }
    }
    else {
        ZeroFlag = BC.mainRegister.highByte() == 0;
    }
    return O1;
}
template<bool increment, bool repeat>
static inline void OperationOUTN8() {
    OperationBase<OUTN<increment, repeat>, AddressingModes::PortWrite<BC>, AddressingModes::AddressRead<HL, uint8_t>>();
}

#pragma endregion

/**********************************
 *  CPU Control Group
 *********************************/
#pragma region CPUControlGroup
static inline void NOP() {
}

static inline void HALT() {
    nextPC = PC;
}

static inline void EI() {
    IFF1 = IFF2 = true;
}
static inline void DI() {
    IFF1 = IFF2 = false;
}

template<uint8_t IMValue>
static inline void IM() {
    InterruptMode = IMValue;
}

#pragma endregion

}

namespace OpMnemonics {
template<WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void LD() {
    OperationBase<Operations::LD8, WriteMode, ReadMode>();
}

template<WriteMode16BitRegister WriteMode, ReadMode16BitRegister ReadMode>
static inline void LD() {
    OperationBase<Operations::LD16, WriteMode, ReadMode>();
}

static inline void NOP() {
    OperationBase<Operations::NOP>();
}

template<WriteMode16BitRegister WriteMode, ReadMode16BitRegister ReadMode>
static inline void INC() {
    OperationBase<Operations::INC16, WriteMode, ReadMode>();
}
template<WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void INC() {
    OperationBase<Operations::INC8, WriteMode, ReadMode>();
}
template<WriteMode16BitRegister WriteMode, ReadMode16BitRegister ReadMode>
static inline void DEC() {
    OperationBase<Operations::DEC16, WriteMode, ReadMode>();
}
template<WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void DEC() {
    OperationBase<Operations::DEC8, WriteMode, ReadMode>();
}
template<WriteMode16BitRegister WriteMode, ReadMode16BitRegister ReadMode1, ReadMode16BitRegister ReadMode2>
static inline void ADD() {
    OperationBase<Operations::ADD16, WriteMode, ReadMode1, ReadMode2>();
}
template<ReadMode8BitRegister ReadMode>
static inline void ADD() {
    OperationBase<Operations::ADD8, AddressingMnemonics::A, AddressingMnemonics::A, ReadMode>();
}
template<WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void SUB() {
    OperationBase<Operations::SUB, WriteMode, AddressingMnemonics::A, ReadMode>();
}
template<WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void ADC() {
    OperationBase<Operations::ADC8, WriteMode, AddressingMnemonics::A, ReadMode>();
}
template<WriteMode16BitRegister WriteMode, ReadMode16BitRegister ReadO1, ReadMode16BitRegister ReadO2>
static inline void ADC() {
    OperationBase<Operations::ADC16, WriteMode, ReadO1, ReadO2>();
}
template<ReadMode8BitRegister ReadMode>
static inline void SUB() {
    OperationBase<Operations::SUB, AddressingMnemonics::A, AddressingMnemonics::A, ReadMode>();
}
template<ReadMode8BitRegister ReadMode>
static inline void SBC() {
    OperationBase<Operations::SBC8, AddressingMnemonics::A, AddressingMnemonics::A, ReadMode>();
}
template<WriteMode16BitRegister WriteMode, ReadMode16BitRegister ReadO1, ReadMode16BitRegister ReadO2>
static inline void SBC() {
    OperationBase<Operations::SBC16, WriteMode, ReadO1, ReadO2>();
}
template<ReadMode8BitRegister ReadMode>
static inline void AND() {
    OperationBase<Operations::AND, AddressingMnemonics::A, AddressingMnemonics::A, ReadMode>();
}
template<ReadMode8BitRegister ReadMode>
static inline void XOR() {
    OperationBase<Operations::XOR, AddressingMnemonics::A, AddressingMnemonics::A, ReadMode>();
}
template<ReadMode8BitRegister ReadMode>
static inline void OR() {
    OperationBase<Operations::OR, AddressingMnemonics::A, AddressingMnemonics::A, ReadMode>();
}
template<ReadMode8BitRegister ReadMode>
static inline void CP() {
    OperationBase<Operations::CP, AddressingMnemonics::A, ReadMode>();
}
template<ReadMode16BitRegister ReadMode>
static inline void PUSH() {
    OperationBase<Operations::PUSH16, ReadMode>();
}
template<WriteMode16BitRegister WriteMode>
static inline void POP() {
    OperationBase<Operations::POP16, WriteMode>();
}

template<ReadModeBool Condition, ReadMode16BitRegister AddrRead>
static inline void JR() {
    OperationBase<Operations::JR<Condition>, AddrRead>();
}
template<ReadMode16BitRegister AddrRead>
static inline void JR() {
    OperationBase<Operations::JR1, AddrRead>();
}
template<ReadModeBool Condition, ReadMode16BitRegister AddrRead>
static inline void JP() {
    OperationBase<Operations::JP<Condition>, AddrRead>();
}
template<ReadMode16BitRegister AddrRead>
static inline void JP() {
    OperationBase<Operations::JP1, AddrRead>();
}
template<ReadModeBool Condition, ReadMode16BitRegister AddrRead>
static inline void CALL() {
    OperationBase<Operations::CALL<Condition>, AddrRead>();
}
template<ReadMode16BitRegister AddrRead>
static inline void CALL() {
    OperationBase<Operations::CALL1, AddrRead>();
}
template<ReadModeBool Condition>
static inline void RET() {
    OperationBase<Operations::RET<Condition>>();
}
static inline void RET() {
    OperationBase<Operations::RET1>();
}
static inline void RETI() {
    OperationBase<Operations::RETI>();
}

template<uint8_t Vector>
static inline void RST() {
    OperationBase<Operations::RST<Vector>>();
}

template<RegisterSet &RegisterSet>
static inline void EX() {
    OperationBase<Operations::EX<RegisterSet>>();
}

template<RegisterSet &RegisterSet, uint16_t &Address>
static inline void EX() {
    OperationBase<Operations::EX_mem<RegisterSet>, AddressingModes::AddressWrite<Address, uint16_t>, AddressingModes::AddressRead<Address, uint16_t>>();
}
template<uint16_t &Address, uint16_t &Register>
static inline void EX() {
    OperationBase<Operations::EX_mem<Register>, AddressingModes::AddressWrite<Address, uint16_t>, AddressingModes::AddressRead<Address, uint16_t>>();
}

template<WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void OUT() {
    OperationBase<Operations::OUT, WriteMode, ReadMode>();
}
template<WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void IN() {
    OperationBase<Operations::INR, WriteMode, ReadMode>();
}
template<ReadMode8BitRegister ReadMode>
static inline void INA() {
    OperationBase<Operations::INA, AddressingMnemonics::A, ReadMode>();
}
template<ReadMode8BitRegister ReadMode>
static inline void IN() {
    OperationBase<Operations::IN1, ReadMode>();
}
template<WriteMode8BitRegister WriteMode>
static inline void OUT() {
    OperationBase<Operations::OUT1, WriteMode>();
}

template <WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void SLA() {
    OperationBase<Operations::SLA, WriteMode, ReadMode>();
}
template <WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void SRA() {
    OperationBase<Operations::SRA, WriteMode, ReadMode>();
}
template <WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void SLL() {
    OperationBase<Operations::SLL, WriteMode, ReadMode>();
}
template <WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void SRL() {
    OperationBase<Operations::SRL, WriteMode, ReadMode>();
}

template <uint8_t Bit, ReadMode8BitRegister ReadMode>
static inline void BIT() {
    OperationBase<Operations::BIT<Bit>, ReadMode>();
}
template <uint8_t Bit, WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void RES() {
    OperationBase<Operations::RES<Bit>, WriteMode, ReadMode>();
}
template <uint8_t Bit, WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void SET() {
    OperationBase<Operations::SET<Bit>, WriteMode, ReadMode>();
}


static inline void RLCA() {
    OperationBase<Operations::RLCA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
}
template <WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void RLC() {
    OperationBase<Operations::RLC, WriteMode, ReadMode>();
}
static inline void RRCA() {
    OperationBase<Operations::RRCA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
}
template <WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void RRC() {
    OperationBase<Operations::RRC, WriteMode, ReadMode>();
}
static inline void RLA() {
    OperationBase<Operations::RLA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
}
template <WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void RL() {
    OperationBase<Operations::RL, WriteMode, ReadMode>();
}
static inline void RRA() {
    OperationBase<Operations::RRA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
}
template <WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode>
static inline void RR() {
    OperationBase<Operations::RR, WriteMode, ReadMode>();
}
static inline void RRD() {
    OperationBase<Operations::RRD, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::AddressRead<HL, uint8_t>>();
}
static inline void RLD() {
    OperationBase<Operations::RLD,  AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::AddressRead<HL, uint8_t>>();
}

static inline void DAA() {
    OperationBase<Operations::DAA, AddressingMnemonics::A, AddressingMnemonics::A>();
}
static inline void CPL() {
    OperationBase<Operations::CPL>();
}
static inline void CCF() {
    OperationBase<Operations::CCF>();
}
static inline void SCF() {
    OperationBase<Operations::SCF>();
}

template<ReadMode8sBitRegister ReadMode>
static inline void DJNZ() {
    OperationBase<Operations::DJNZ, ReadMode>();
}

template<ReadModeBool Condition, ReadMode8sBitRegister ReadMode>
static inline void JR() {
    OperationBase<Operations::JR<Condition>, ReadMode>();
}
template<ReadMode8sBitRegister ReadMode>
static inline void JR() {
    OperationBase<Operations::JR1, ReadMode>();
}
template<ReadModeBool Condition, ReadMode8sBitRegister ReadMode>
static inline void JP() {
    OperationBase<Operations::JP<Condition>, ReadMode>();
}
template<ReadMode8sBitRegister ReadMode>
static inline void JP() {
    OperationBase<Operations::JP1, ReadMode>();
}
static inline void RETN() {
    OperationBase<RETN>();
}

template <uint8_t InterruptMode>
static inline void IM() {
    OperationBase<Operations::IM<InterruptMode>>();
}

static inline void NEG() {
    OperationBase<Operations::NEG, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
}

static inline void LDI() {
    Operations::OperationLDN<true, false>();
}
static inline void CPI() {
    Operations::OperationCPN<true, false>();
}
static inline void INI() {
    Operations::OperationINN8<true, false>();
}
static inline void OUTI() {
    Operations::OperationOUTN8<true, false>();
}
static inline void LDD() {
    Operations::OperationLDN<false, false>();
}
static inline void CPD() {
    Operations::OperationCPN<false, false>();
}
static inline void IND() {
    Operations::OperationINN8<false, false>();
}
static inline void OUTD() {
    Operations::OperationOUTN8<false, false>();
}
static inline void LDIR() {
    Operations::OperationLDN<true, true>();
}
static inline void CPIR() {
    Operations::OperationCPN<true, true>();
}
static inline void INIR() {
    Operations::OperationINN8<true, true>();
}
static inline void OTIR() {
    Operations::OperationOUTN8<true, true>();
}
static inline void LDDR() {
    Operations::OperationLDN<false, true>();
}
static inline void CPDR() {
    Operations::OperationCPN<false, true>();
}
static inline void INDR() {
    Operations::OperationINN8<false, true>();
}
static inline void OTDR() {
    Operations::OperationOUTN8<false, true>();
}

static inline void EI() {
    OperationBase<Operations::EI>();
}
static inline void DI() {
    OperationBase<Operations::DI>();
}

}

static inline void extendedOpBITS() {
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC++ );

    switch( opcode ) {
        case 0x00: {
            //00: RLC B - 8	2	2
            OpMnemonics::RLC<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x01: {
            //01: RLC C - 8	2	2
            OpMnemonics::RLC<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x02: {
            //02: RLC D - 8	2	2
            OpMnemonics::RLC<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x03: {
            //03: RLC E - 8	2	2
            OpMnemonics::RLC<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x04: {
            //04: RLC H - 8	2	2
            OpMnemonics::RLC<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x05: {
            //05: RLC L - 8	2	2
            OpMnemonics::RLC<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x06: {
            //06: RLC (HL) - 15	4	2
            OpMnemonics::RLC<AddressingMnemonics::_HL_, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x07: {
            //07: RLC A - 8	2	2
            OpMnemonics::RLC<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x08: {
            //08: RRC B - 8	2	2
            OpMnemonics::RRC<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x09: {
            //09: RRC C - 8	2	2
            OpMnemonics::RRC<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x0A: {
            //0A: RRC D - 8	2	2
            OpMnemonics::RRC<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x0B: {
            //0B: RRC E - 8	2	2
            OpMnemonics::RRC<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x0C: {
            //0C: RRC H - 8	2	2
            OpMnemonics::RRC<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x0D: {
            //0D: RRC L - 8	2	2
            OpMnemonics::RRC<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x0E: {
            //0E: RRC (HL) - 15	4	2
            OpMnemonics::RRC<AddressingMnemonics::_HL_, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x0F: {
            //0F: RRC A - 8	2	2
            OpMnemonics::RRC<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x10: {
            //10: RL B - 8	2	2
            OpMnemonics::RL<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x11: {
            //11: RL C - 8	2	2
            OpMnemonics::RL<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x12: {
            //12: RL D - 8	2	2
            OpMnemonics::RL<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x13: {
            //13: RL E - 8	2	2
            OpMnemonics::RL<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x14: {
            //14: RL H - 8	2	2
            OpMnemonics::RL<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x15: {
            //15: RL L - 8	2	2
            OpMnemonics::RL<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x16: {
            //16: RL (HL) - 15	4	2
            OpMnemonics::RL<AddressingMnemonics::_HL_, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x17: {
            //17: RL A - 8	2	2
            OpMnemonics::RL<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x18: {
            //18: RR B - 8	2	2
            OpMnemonics::RR<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x19: {
            //19: RR C - 8	2	2
            OpMnemonics::RR<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x1A: {
            //1A: RR D - 8	2	2
            OpMnemonics::RR<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x1B: {
            //1B: RR E - 8	2	2
            OpMnemonics::RR<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x1C: {
            //1C: RR H - 8	2	2
            OpMnemonics::RR<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x1D: {
            //1D: RR L - 8	2	2
            OpMnemonics::RR<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x1E: {
            //1E: RR (HL) - 15	4	2
            OpMnemonics::RR<AddressingMnemonics::_HL_, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x1F: {
            //1F: RR A - 8	2	2
            OpMnemonics::RR<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x20: {
            //20: SLA B - 8	2	2
            OpMnemonics::SLA<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x21: {
            //21: SLA C - 8	2	2
            OpMnemonics::SLA<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x22: {
            //22: SLA D - 8	2	2
            OpMnemonics::SLA<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x23: {
            //23: SLA E - 8	2	2
            OpMnemonics::SLA<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x24: {
            //24: SLA H - 8	2	2
            OpMnemonics::SLA<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x25: {
            //25: SLA L - 8	2	2
            OpMnemonics::SLA<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x26: {
            //26: SLA (HL) - 15	4	2
            OpMnemonics::SLA<AddressingMnemonics::_HL_, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x27: {
            //27: SLA A - 8	2	2
            OpMnemonics::SLA<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x28: {
            //28: SRA B - 8	2	2
            OpMnemonics::SRA<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x29: {
            //29: SRA C - 8	2	2
            OpMnemonics::SRA<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x2A: {
            //2A: SRA D - 8	2	2
            OpMnemonics::SRA<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x2B: {
            //2B: SRA E - 8	2	2
            OpMnemonics::SRA<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x2C: {
            //2C: SRA H - 8	2	2
            OpMnemonics::SRA<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x2D: {
            //2D: SRA L - 8	2	2
            OpMnemonics::SRA<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x2E: {
            //2E: SRA (HL) - 15	4	2
            OpMnemonics::SRA<AddressingMnemonics::_HL_, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x2F: {
            //2F: SRA A - 8	2	2
            OpMnemonics::SRA<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x30: {
            //30: SLL B - 8	2	2
            OpMnemonics::SLL<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x31: {
            //31: SLL C - 8	2	2
            OpMnemonics::SLL<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x32: {
            //32: SLL D - 8	2	2
            OpMnemonics::SLL<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x33: {
            //33: SLL E - 8	2	2
            OpMnemonics::SLL<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x34: {
            //34: SLL H - 8	2	2
            OpMnemonics::SLL<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x35: {
            //35: SLL L - 8	2	2
            OpMnemonics::SLL<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x36: {
            //36: SLL (HL) - 15	4	2
            OpMnemonics::SLL<AddressingMnemonics::_HL_, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x37: {
            //37: SLL A - 8	2	2
            OpMnemonics::SLL<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x38: {
            //38: SRL B - 8	2	2
            OpMnemonics::SRL<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x39: {
            //39: SRL C - 8	2	2
            OpMnemonics::SRL<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x3A: {
            //3A: SRL D - 8	2	2
            OpMnemonics::SRL<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x3B: {
            //3B: SRL E - 8	2	2
            OpMnemonics::SRL<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x3C: {
            //3C: SRL H - 8	2	2
            OpMnemonics::SRL<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x3D: {
            //3D: SRL L - 8	2	2
            OpMnemonics::SRL<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x3E: {
            //3E: SRL (HL) - 15	4	2
            OpMnemonics::SRL<AddressingMnemonics::_HL_, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x3F: {
            //3F: SRL A - 8	2	2
            OpMnemonics::SRL<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x40: {
            //40: BIT 0,B - 8	2	2
            OpMnemonics::BIT<0, AddressingMnemonics::B>();
            break;
        }
        case 0x41: {
            //41: BIT 0,C - 8	2	2
            OpMnemonics::BIT<0, AddressingMnemonics::C>();
            break;
        }
        case 0x42: {
            //42: BIT 0,D - 8	2	2
            OpMnemonics::BIT<0, AddressingMnemonics::D>();
            break;
        }
        case 0x43: {
            //43: BIT 0,E - 8	2	2
            OpMnemonics::BIT<0, AddressingMnemonics::E>();
            break;
        }
        case 0x44: {
            //44: BIT 0,H - 8	2	2
            OpMnemonics::BIT<0, AddressingMnemonics::H>();
            break;
        }
        case 0x45: {
            //45: BIT 0,L - 8	2	2
            OpMnemonics::BIT<0, AddressingMnemonics::L>();
            break;
        }
        case 0x46: {
            //46: BIT 0,(HL) - 12	3	2
            OpMnemonics::BIT<0, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x47: {
            //47: BIT 0,A - 8	2	2
            OpMnemonics::BIT<0, AddressingMnemonics::A>();
            break;
        }
        case 0x48: {
            //48: BIT 1,B - 8	2	2
            OpMnemonics::BIT<1, AddressingMnemonics::B>();
            break;
        }
        case 0x49: {
            //49: BIT 1,C - 8	2	2
            OpMnemonics::BIT<1, AddressingMnemonics::C>();
            break;
        }
        case 0x4A: {
            //4A: BIT 1,D - 8	2	2
            OpMnemonics::BIT<1, AddressingMnemonics::D>();
            break;
        }
        case 0x4B: {
            //4B: BIT 1,E - 8	2	2
            OpMnemonics::BIT<1, AddressingMnemonics::E>();
            break;
        }
        case 0x4C: {
            //4C: BIT 1,H - 8	2	2
            OpMnemonics::BIT<1, AddressingMnemonics::H>();
            break;
        }
        case 0x4D: {
            //4D: BIT 1,L - 8	2	2
            OpMnemonics::BIT<1, AddressingMnemonics::L>();
            break;
        }
        case 0x4E: {
            //4E: BIT 1,(HL) - 12	3	2
            OpMnemonics::BIT<1, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x4F: {
            //4F: BIT 1,A - 8	2	2
            OpMnemonics::BIT<1, AddressingMnemonics::A>();
            break;
        }
        case 0x50: {
            //50: BIT 2,B - 8	2	2
            OpMnemonics::BIT<2, AddressingMnemonics::B>();
            break;
        }
        case 0x51: {
            //51: BIT 2,C - 8	2	2
            OpMnemonics::BIT<2, AddressingMnemonics::C>();
            break;
        }
        case 0x52: {
            //52: BIT 2,D - 8	2	2
            OpMnemonics::BIT<2, AddressingMnemonics::D>();
            break;
        }
        case 0x53: {
            //53: BIT 2,E - 8	2	2
            OpMnemonics::BIT<2, AddressingMnemonics::E>();
            break;
        }
        case 0x54: {
            //54: BIT 2,H - 8	2	2
            OpMnemonics::BIT<2, AddressingMnemonics::H>();
            break;
        }
        case 0x55: {
            //55: BIT 2,L - 8	2	2
            OpMnemonics::BIT<2, AddressingMnemonics::L>();
            break;
        }
        case 0x56: {
            //56: BIT 2,(HL) - 12	3	2
            OpMnemonics::BIT<2, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x57: {
            //57: BIT 2,A - 8	2	2
            OpMnemonics::BIT<2, AddressingMnemonics::A>();
            break;
        }
        case 0x58: {
            //58: BIT 3,B - 8	2	2
            OpMnemonics::BIT<3, AddressingMnemonics::B>();
            break;
        }
        case 0x59: {
            //59: BIT 3,C - 8	2	2
            OpMnemonics::BIT<3, AddressingMnemonics::C>();
            break;
        }
        case 0x5A: {
            //5A: BIT 3,D - 8	2	2
            OpMnemonics::BIT<3, AddressingMnemonics::D>();
            break;
        }
        case 0x5B: {
            //5B: BIT 3,E - 8	2	2
            OpMnemonics::BIT<3, AddressingMnemonics::E>();
            break;
        }
        case 0x5C: {
            //5C: BIT 3,H - 8	2	2
            OpMnemonics::BIT<3, AddressingMnemonics::H>();
            break;
        }
        case 0x5D: {
            //5D: BIT 3,L - 8	2	2
            OpMnemonics::BIT<3, AddressingMnemonics::L>();
            break;
        }
        case 0x5E: {
            //5E: BIT 3,(HL) - 12	3	2
            OpMnemonics::BIT<3, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x5F: {
            //5F: BIT 3,A - 8	2	2
            OpMnemonics::BIT<3, AddressingMnemonics::A>();
            break;
        }
        case 0x60: {
            //60: BIT 4,B - 8	2	2
            OpMnemonics::BIT<4, AddressingMnemonics::B>();
            break;
        }
        case 0x61: {
            //61: BIT 4,C - 8	2	2
            OpMnemonics::BIT<4, AddressingMnemonics::C>();
            break;
        }
        case 0x62: {
            //62: BIT 4,D - 8	2	2
            OpMnemonics::BIT<4, AddressingMnemonics::D>();
            break;
        }
        case 0x63: {
            //63: BIT 4,E - 8	2	2
            OpMnemonics::BIT<4, AddressingMnemonics::E>();
            break;
        }
        case 0x64: {
            //64: BIT 4,H - 8	2	2
            OpMnemonics::BIT<4, AddressingMnemonics::H>();
            break;
        }
        case 0x65: {
            //65: BIT 4,L - 8	2	2
            OpMnemonics::BIT<4, AddressingMnemonics::L>();
            break;
        }
        case 0x66: {
            //66: BIT 4,(HL) - 12	3	2
            OpMnemonics::BIT<4, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x67: {
            //67: BIT 4,A - 8	2	2
            OpMnemonics::BIT<4, AddressingMnemonics::A>();
            break;
        }
        case 0x68: {
            //68: BIT 5,B - 8	2	2
            OpMnemonics::BIT<5, AddressingMnemonics::B>();
            break;
        }
        case 0x69: {
            //69: BIT 5,C - 8	2	2
            OpMnemonics::BIT<5, AddressingMnemonics::C>();
            break;
        }
        case 0x6A: {
            //6A: BIT 5,D - 8	2	2
            OpMnemonics::BIT<5, AddressingMnemonics::D>();
            break;
        }
        case 0x6B: {
            //6B: BIT 5,E - 8	2	2
            OpMnemonics::BIT<5, AddressingMnemonics::E>();
            break;
        }
        case 0x6C: {
            //6C: BIT 5,H - 8	2	2
            OpMnemonics::BIT<5, AddressingMnemonics::H>();
            break;
        }
        case 0x6D: {
            //6D: BIT 5,L - 8	2	2
            OpMnemonics::BIT<5, AddressingMnemonics::L>();
            break;
        }
        case 0x6E: {
            //6E: BIT 5,(HL) - 12	3	2
            OpMnemonics::BIT<5, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x6F: {
            //6F: BIT 5,A - 8	2	2
            OpMnemonics::BIT<5, AddressingMnemonics::A>();
            break;
        }
        case 0x70: {
            //70: BIT 6,B - 8	2	2
            OpMnemonics::BIT<6, AddressingMnemonics::B>();
            break;
        }
        case 0x71: {
            //71: BIT 6,C - 8	2	2
            OpMnemonics::BIT<6, AddressingMnemonics::C>();
            break;
        }
        case 0x72: {
            //72: BIT 6,D - 8	2	2
            OpMnemonics::BIT<6, AddressingMnemonics::D>();
            break;
        }
        case 0x73: {
            //73: BIT 6,E - 8	2	2
            OpMnemonics::BIT<6, AddressingMnemonics::E>();
            break;
        }
        case 0x74: {
            //74: BIT 6,H - 8	2	2
            OpMnemonics::BIT<6, AddressingMnemonics::H>();
            break;
        }
        case 0x75: {
            //75: BIT 6,L - 8	2	2
            OpMnemonics::BIT<6, AddressingMnemonics::L>();
            break;
        }
        case 0x76: {
            //76: BIT 6,(HL) - 12	3	2
            OpMnemonics::BIT<6, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x77: {
            //77: BIT 6,A - 8	2	2
            OpMnemonics::BIT<6, AddressingMnemonics::A>();
            break;
        }
        case 0x78: {
            //78: BIT 7,B - 8	2	2
            OpMnemonics::BIT<7, AddressingMnemonics::B>();
            break;
        }
        case 0x79: {
            //79: BIT 7,C - 8	2	2
            OpMnemonics::BIT<7, AddressingMnemonics::C>();
            break;
        }
        case 0x7A: {
            //7A: BIT 7,D - 8	2	2
            OpMnemonics::BIT<7, AddressingMnemonics::D>();
            break;
        }
        case 0x7B: {
            //7B: BIT 7,E - 8	2	2
            OpMnemonics::BIT<7, AddressingMnemonics::E>();
            break;
        }
        case 0x7C: {
            //7C: BIT 7,H - 8	2	2
            OpMnemonics::BIT<7, AddressingMnemonics::H>();
            break;
        }
        case 0x7D: {
            //7D: BIT 7,L - 8	2	2
            OpMnemonics::BIT<7, AddressingMnemonics::L>();
            break;
        }
        case 0x7E: {
            //7E: BIT 7,(HL) - 12	3	2
            OpMnemonics::BIT<7, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x7F: {
            //7F: BIT 7,A - 8	2	2
            OpMnemonics::BIT<7, AddressingMnemonics::A>();
            break;
        }
        case 0x80: {
            //80: RES 0,B - 8	2	2
            OpMnemonics::RES<0,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0x81: {
            //81: RES 0,C - 8	2	2
            OpMnemonics::RES<0,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0x82: {
            //82: RES 0,D - 8	2	2
            OpMnemonics::RES<0,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0x83: {
            //83: RES 0,E - 8	2	2
            OpMnemonics::RES<0,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0x84: {
            //84: RES 0,H - 8	2	2
            OpMnemonics::RES<0,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0x85: {
            //85: RES 0,L - 8	2	2
            OpMnemonics::RES<0,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0x86: {
            //86: RES 0,(HL) - 15	4	2
            OpMnemonics::RES<0,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0x87: {
            //87: RES 0,A - 8	2	2
            OpMnemonics::RES<0,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0x88: {
            //88: RES 1,B - 8	2	2
            OpMnemonics::RES<1,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0x89: {
            //89: RES 1,C - 8	2	2
            OpMnemonics::RES<1,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0x8A: {
            //8A: RES 1,D - 8	2	2
            OpMnemonics::RES<1,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0x8B: {
            //8B: RES 1,E - 8	2	2
            OpMnemonics::RES<1,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0x8C: {
            //8C: RES 1,H - 8	2	2
            OpMnemonics::RES<1,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0x8D: {
            //8D: RES 1,L - 8	2	2
            OpMnemonics::RES<1,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0x8E: {
            //8E: RES 1,(HL) - 15	4	2
            OpMnemonics::RES<1,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0x8F: {
            //8F: RES 1,A - 8	2	2
            OpMnemonics::RES<1,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0x90: {
            //90: RES 2,B - 8	2	2
            OpMnemonics::RES<2,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0x91: {
            //91: RES 2,C - 8	2	2
            OpMnemonics::RES<2,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0x92: {
            //92: RES 2,D - 8	2	2
            OpMnemonics::RES<2,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0x93: {
            //93: RES 2,E - 8	2	2
            OpMnemonics::RES<2,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0x94: {
            //94: RES 2,H - 8	2	2
            OpMnemonics::RES<2,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0x95: {
            //95: RES 2,L - 8	2	2
            OpMnemonics::RES<2,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0x96: {
            //96: RES 2,(HL) - 15	4	2
            OpMnemonics::RES<2,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0x97: {
            //97: RES 2,A - 8	2	2
            OpMnemonics::RES<2,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0x98: {
            //98: RES 3,B - 8	2	2
            OpMnemonics::RES<3,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0x99: {
            //99: RES 3,C - 8	2	2
            OpMnemonics::RES<3,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0x9A: {
            //9A: RES 3,D - 8	2	2
            OpMnemonics::RES<3,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0x9B: {
            //9B: RES 3,E - 8	2	2
            OpMnemonics::RES<3,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0x9C: {
            //9C: RES 3,H - 8	2	2
            OpMnemonics::RES<3,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0x9D: {
            //9D: RES 3,L - 8	2	2
            OpMnemonics::RES<3,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0x9E: {
            //9E: RES 3,(HL) - 15	4	2
            OpMnemonics::RES<3,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0x9F: {
            //9F: RES 3,A - 8	2	2
            OpMnemonics::RES<3,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xA0: {
            //A0: RES 4,B - 8	2	2
            OpMnemonics::RES<4,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xA1: {
            //A1: RES 4,C - 8	2	2
            OpMnemonics::RES<4,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xA2: {
            //A2: RES 4,D - 8	2	2
            OpMnemonics::RES<4,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xA3: {
            //A3: RES 4,E - 8	2	2
            OpMnemonics::RES<4,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xA4: {
            //A4: RES 4,H - 8	2	2
            OpMnemonics::RES<4,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xA5: {
            //A5: RES 4,L - 8	2	2
            OpMnemonics::RES<4,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xA6: {
            //A6: RES 4,(HL) - 15	4	2
            OpMnemonics::RES<4,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xA7: {
            //A7: RES 4,A - 8	2	2
            OpMnemonics::RES<4,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xA8: {
            //A8: RES 5,B - 8	2	2
            OpMnemonics::RES<5,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xA9: {
            //A9: RES 5,C - 8	2	2
            OpMnemonics::RES<5,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xAA: {
            //AA: RES 5,D - 8	2	2
            OpMnemonics::RES<5,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xAB: {
            //AB: RES 5,E - 8	2	2
            OpMnemonics::RES<5,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xAC: {
            //AC: RES 5,H - 8	2	2
            OpMnemonics::RES<5,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xAD: {
            //AD: RES 5,L - 8	2	2
            OpMnemonics::RES<5,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xAE: {
            //AE: RES 5,(HL) - 15	4	2
            OpMnemonics::RES<5,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xAF: {
            //AF: RES 5,A - 8	2	2
            OpMnemonics::RES<5,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xB0: {
            //B0: RES 6,B - 8	2	2
            OpMnemonics::RES<6,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xB1: {
            //B1: RES 6,C - 8	2	2
            OpMnemonics::RES<6,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xB2: {
            //B2: RES 6,D - 8	2	2
            OpMnemonics::RES<6,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xB3: {
            //B3: RES 6,E - 8	2	2
            OpMnemonics::RES<6,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xB4: {
            //B4: RES 6,H - 8	2	2
            OpMnemonics::RES<6,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xB5: {
            //B5: RES 6,L - 8	2	2
            OpMnemonics::RES<6,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xB6: {
            //B6: RES 6,(HL) - 15	4	2
            OpMnemonics::RES<6,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xB7: {
            //B7: RES 6,A - 8	2	2
            OpMnemonics::RES<6,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xB8: {
            //B8: RES 7,B - 8	2	2
            OpMnemonics::RES<7,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xB9: {
            //B9: RES 7,C - 8	2	2
            OpMnemonics::RES<7,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xBA: {
            //BA: RES 7,D - 8	2	2
            OpMnemonics::RES<7,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xBB: {
            //BB: RES 7,E - 8	2	2
            OpMnemonics::RES<7,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xBC: {
            //BC: RES 7,H - 8	2	2
            OpMnemonics::RES<7,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xBD: {
            //BD: RES 7,L - 8	2	2
            OpMnemonics::RES<7,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xBE: {
            //BE: RES 7,(HL) - 15	4	2
            OpMnemonics::RES<7,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xBF: {
            //BF: RES 7,A - 8	2	2
            OpMnemonics::RES<7,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xC0: {
            //C0: SET 0,B - 8	2	2
            OpMnemonics::SET<0,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xC1: {
            //C1: SET 0,C - 8	2	2
            OpMnemonics::SET<0,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xC2: {
            //C2: SET 0,D - 8	2	2
            OpMnemonics::SET<0,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xC3: {
            //C3: SET 0,E - 8	2	2
            OpMnemonics::SET<0,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xC4: {
            //C4: SET 0,H - 8	2	2
            OpMnemonics::SET<0,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xC5: {
            //C5: SET 0,L - 8	2	2
            OpMnemonics::SET<0,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xC6: {
            //C6: SET 0,(HL) - 15	4	2
            OpMnemonics::SET<0,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xC7: {
            //C7: SET 0,A - 8	2	2
            OpMnemonics::SET<0,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xC8: {
            //C8: SET 1,B - 8	2	2
            OpMnemonics::SET<1,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xC9: {
            //C9: SET 1,C - 8	2	2
            OpMnemonics::SET<1,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xCA: {
            //CA: SET 1,D - 8	2	2
            OpMnemonics::SET<1,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xCB: {
            //CB: SET 1,E - 8	2	2
            OpMnemonics::SET<1,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xCC: {
            //CC: SET 1,H - 8	2	2
            OpMnemonics::SET<1,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xCD: {
            //CD: SET 1,L - 8	2	2
            OpMnemonics::SET<1,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xCE: {
            //CE: SET 1,(HL) - 15	4	2
            OpMnemonics::SET<1,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xCF: {
            //CF: SET 1,A - 8	2	2
            OpMnemonics::SET<1,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xD0: {
            //D0: SET 2,B - 8	2	2
            OpMnemonics::SET<2,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xD1: {
            //D1: SET 2,C - 8	2	2
            OpMnemonics::SET<2,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xD2: {
            //D2: SET 2,D - 8	2	2
            OpMnemonics::SET<2,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xD3: {
            //D3: SET 2,E - 8	2	2
            OpMnemonics::SET<2,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xD4: {
            //D4: SET 2,H - 8	2	2
            OpMnemonics::SET<2,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xD5: {
            //D5: SET 2,L - 8	2	2
            OpMnemonics::SET<2,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xD6: {
            //D6: SET 2,(HL) - 15	4	2
            OpMnemonics::SET<2,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xD7: {
            //D7: SET 2,A - 8	2	2
            OpMnemonics::SET<2,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xD8: {
            //D8: SET 3,B - 8	2	2
            OpMnemonics::SET<3,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xD9: {
            //D9: SET 3,C - 8	2	2
            OpMnemonics::SET<3,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xDA: {
            //DA: SET 3,D - 8	2	2
            OpMnemonics::SET<3,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xDB: {
            //DB: SET 3,E - 8	2	2
            OpMnemonics::SET<3,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xDC: {
            //DC: SET 3,H - 8	2	2
            OpMnemonics::SET<3,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xDD: {
            //DD: SET 3,L - 8	2	2
            OpMnemonics::SET<3,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xDE: {
            //DE: SET 3,(HL) - 15	4	2
            OpMnemonics::SET<3,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xDF: {
            //DF: SET 3,A - 8	2	2
            OpMnemonics::SET<3,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xE0: {
            //E0: SET 4,B - 8	2	2
            OpMnemonics::SET<4,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xE1: {
            //E1: SET 4,C - 8	2	2
            OpMnemonics::SET<4,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xE2: {
            //E2: SET 4,D - 8	2	2
            OpMnemonics::SET<4,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xE3: {
            //E3: SET 4,E - 8	2	2
            OpMnemonics::SET<4,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xE4: {
            //E4: SET 4,H - 8	2	2
            OpMnemonics::SET<4,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xE5: {
            //E5: SET 4,L - 8	2	2
            OpMnemonics::SET<4,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xE6: {
            //E6: SET 4,(HL) - 15	4	2
            OpMnemonics::SET<4,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xE7: {
            //E7: SET 4,A - 8	2	2
            OpMnemonics::SET<4,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xE8: {
            //E8: SET 5,B - 8	2	2
            OpMnemonics::SET<5,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xE9: {
            //E9: SET 5,C - 8	2	2
            OpMnemonics::SET<5,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xEA: {
            //EA: SET 5,D - 8	2	2
            OpMnemonics::SET<5,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xEB: {
            //EB: SET 5,E - 8	2	2
            OpMnemonics::SET<5,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xEC: {
            //EC: SET 5,H - 8	2	2
            OpMnemonics::SET<5,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xED: {
            //ED: SET 5,L - 8	2	2
            OpMnemonics::SET<5,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xEE: {
            //EE: SET 5,(HL) - 15	4	2
            OpMnemonics::SET<5,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xEF: {
            //EF: SET 5,A - 8	2	2
            OpMnemonics::SET<5,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xF0: {
            //F0: SET 6,B - 8	2	2
            OpMnemonics::SET<6,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xF1: {
            //F1: SET 6,C - 8	2	2
            OpMnemonics::SET<6,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xF2: {
            //F2: SET 6,D - 8	2	2
            OpMnemonics::SET<6,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xF3: {
            //F3: SET 6,E - 8	2	2
            OpMnemonics::SET<6,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xF4: {
            //F4: SET 6,H - 8	2	2
            OpMnemonics::SET<6,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xF5: {
            //F5: SET 6,L - 8	2	2
            OpMnemonics::SET<6,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xF6: {
            //F6: SET 6,(HL) - 15	4	2
            OpMnemonics::SET<6,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xF7: {
            //F7: SET 6,A - 8	2	2
            OpMnemonics::SET<6,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
        case 0xF8: {
            //F8: SET 7,B - 8	2	2
            OpMnemonics::SET<7,  AddressingMnemonics::B,  AddressingMnemonics::B>();
            break;
        }
        case 0xF9: {
            //F9: SET 7,C - 8	2	2
            OpMnemonics::SET<7,  AddressingMnemonics::C,  AddressingMnemonics::C>();
            break;
        }
        case 0xFA: {
            //FA: SET 7,D - 8	2	2
            OpMnemonics::SET<7,  AddressingMnemonics::D,  AddressingMnemonics::D>();
            break;
        }
        case 0xFB: {
            //FB: SET 7,E - 8	2	2
            OpMnemonics::SET<7,  AddressingMnemonics::E,  AddressingMnemonics::E>();
            break;
        }
        case 0xFC: {
            //FC: SET 7,H - 8	2	2
            OpMnemonics::SET<7,  AddressingMnemonics::H,  AddressingMnemonics::H>();
            break;
        }
        case 0xFD: {
            //FD: SET 7,L - 8	2	2
            OpMnemonics::SET<7,  AddressingMnemonics::L,  AddressingMnemonics::L>();
            break;
        }
        case 0xFE: {
            //FE: SET 7,(HL) - 15	4	2
            OpMnemonics::SET<7,  AddressingMnemonics::_HL_,  AddressingMnemonics::_HL_>();
            break;
        }
        case 0xFF: {
            //FF: SET 7,A - 8	2	2
            OpMnemonics::SET<7,  AddressingMnemonics::A,  AddressingMnemonics::A>();
            break;
        }
    }
}

static inline void extendedOpIXBITS() {
    // Prefixed with DDCB
    // Operand follows, then final byte of opcode.
    // TODO - note that for instruction decoding
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC + 1 );
    switch( opcode ) {
        case 0x00:{
            // TODO - LD B,RLC (IX+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x01:{
            // TODO - LD C,RLC (IX+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x02:{
            // TODO - LD D,RLC (IX+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x03:{
            // TODO - LD E,RLC (IX+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x04:{
            // TODO - LD H,RLC (IX+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x05:{
            // TODO - LD L,RLC (IX+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x06:{
            // TODO - RLC (IX+d)
            Operations::OperationRLC<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x07:{
            // TODO - LD A,RLC (IX+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x08:{
            // TODO - LD B,RRC (IX+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x09:{
            // TODO - LD C,RRC (IX+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x0A:{
            // TODO - LD D,RRC (IX+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x0B:{
            // TODO - LD E,RRC (IX+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x0C:{
            // TODO - LD H,RRC (IX+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x0D:{
            // TODO - LD L,RRC (IX+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x0E:{
            // TODO - RRC (IX+d)
            Operations::OperationRRC<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x0F:{
            // TODO - LD A,RRC (IX+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x10:{
            // TODO - LD B,RL (IX+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x11:{
            // TODO - LD C,RL (IX+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x12:{
            // TODO - LD D,RL (IX+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x13:{
            // TODO - LD E,RL (IX+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x14:{
            // TODO - LD H,RL (IX+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x15:{
            // TODO - LD L,RL (IX+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x16:{
            // TODO - RL (IX+d)
            Operations::OperationRL<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x17:{
            // TODO - LD A,RL (IX+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x18:{
            // TODO - LD B,RR (IX+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x19:{
            // TODO - LD C,RR (IX+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x1A:{
            // TODO - LD D,RR (IX+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x1B:{
            // TODO - LD E,RR (IX+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x1C:{
            // TODO - LD H,RR (IX+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x1D:{
            // TODO - LD L,RR (IX+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x1E:{
            // TODO - RR (IX+d)
            Operations::OperationRR<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x1F:{
            // TODO - LD A,RR (IX+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x20:{
            // TODO - LD B,SLA (IX+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x21:{
            // TODO - LD C,SLA (IX+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x22:{
            // TODO - LD D,SLA (IX+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x23:{
            // TODO - LD E,SLA (IX+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x24:{
            // TODO - LD H,SLA (IX+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x25:{
            // TODO - LD L,SLA (IX+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x26:{
            // TODO - SLA (IX+d)
            Operations::OperationSLA<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x27:{
            // TODO - LD A,SLA (IX+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x28:{
            // TODO - LD B,SRA (IX+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x29:{
            // TODO - LD C,SRA (IX+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x2A:{
            // TODO - LD D,SRA (IX+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x2B:{
            // TODO - LD E,SRA (IX+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x2C:{
            // TODO - LD H,SRA (IX+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x2D:{
            // TODO - LD L,SRA (IX+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x2E:{
            // TODO - SRA (IX+d)
            Operations::OperationSRA<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x2F:{
            // TODO - LD A,SRA (IX+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x30:{
            // TODO - LD B,SLL (IX+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x31:{
            // TODO - LD C,SLL (IX+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x32:{
            // TODO - LD D,SLL (IX+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x33:{
            // TODO - LD E,SLL (IX+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x34:{
            // TODO - LD H,SLL (IX+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x35:{
            // TODO - LD L,SLL (IX+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x36:{
            // TODO - SLL (IX+d)
            Operations::OperationSLL<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x37:{
            // TODO - LD A,SLL (IX+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x38:{
            // TODO - LD B,SRL (IX+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x39:{
            // TODO - LD C,SRL (IX+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x3A:{
            // TODO - LD D,SRL (IX+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x3B:{
            // TODO - LD E,SRL (IX+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x3C:{
            // TODO - LD H,SRL (IX+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x3D:{
            // TODO - LD L,SRL (IX+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x3E:{
            // TODO - SRL (IX+d)
            Operations::OperationSRL<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x3F:{
            // TODO - LD A,SRL (IX+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x40:{
            // TODO - BIT 0,(IX+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x41:{
            // TODO - BIT 0,(IX+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x42:{
            // TODO - BIT 0,(IX+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x43:{
            // TODO - BIT 0,(IX+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x44:{
            // TODO - BIT 0,(IX+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x45:{
            // TODO - BIT 0,(IX+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x46:{
            // TODO - BIT 0,(IX+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x47:{
            // TODO - BIT 0,(IX+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x48:{
            // TODO - BIT 1,(IX+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x49:{
            // TODO - BIT 1,(IX+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x4A:{
            // TODO - BIT 1,(IX+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x4B:{
            // TODO - BIT 1,(IX+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x4C:{
            // TODO - BIT 1,(IX+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x4D:{
            // TODO - BIT 1,(IX+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x4E:{
            // TODO - BIT 1,(IX+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x4F:{
            // TODO - BIT 1,(IX+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x50:{
            // TODO - BIT 2,(IX+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x51:{
            // TODO - BIT 2,(IX+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x52:{
            // TODO - BIT 2,(IX+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x53:{
            // TODO - BIT 2,(IX+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x54:{
            // TODO - BIT 2,(IX+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x55:{
            // TODO - BIT 2,(IX+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x56:{
            // TODO - BIT 2,(IX+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x57:{
            // TODO - BIT 2,(IX+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x58:{
            // TODO - BIT 3,(IX+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x59:{
            // TODO - BIT 3,(IX+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x5A:{
            // TODO - BIT 3,(IX+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x5B:{
            // TODO - BIT 3,(IX+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x5C:{
            // TODO - BIT 3,(IX+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x5D:{
            // TODO - BIT 3,(IX+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x5E:{
            // TODO - BIT 3,(IX+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x5F:{
            // TODO - BIT 3,(IX+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x60:{
            // TODO - BIT 4,(IX+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x61:{
            // TODO - BIT 4,(IX+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x62:{
            // TODO - BIT 4,(IX+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x63:{
            // TODO - BIT 4,(IX+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x64:{
            // TODO - BIT 4,(IX+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x65:{
            // TODO - BIT 4,(IX+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x66:{
            // TODO - BIT 4,(IX+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x67:{
            // TODO - BIT 4,(IX+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x68:{
            // TODO - BIT 5,(IX+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x69:{
            // TODO - BIT 5,(IX+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x6A:{
            // TODO - BIT 5,(IX+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x6B:{
            // TODO - BIT 5,(IX+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x6C:{
            // TODO - BIT 5,(IX+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x6D:{
            // TODO - BIT 5,(IX+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x6E:{
            // TODO - BIT 5,(IX+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x6F:{
            // TODO - BIT 5,(IX+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x70:{
            // TODO - BIT 6,(IX+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x71:{
            // TODO - BIT 6,(IX+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x72:{
            // TODO - BIT 6,(IX+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x73:{
            // TODO - BIT 6,(IX+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x74:{
            // TODO - BIT 6,(IX+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x75:{
            // TODO - BIT 6,(IX+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x76:{
            // TODO - BIT 6,(IX+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x77:{
            // TODO - BIT 6,(IX+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x78:{
            // TODO - BIT 7,(IX+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x79:{
            // TODO - BIT 7,(IX+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x7A:{
            // TODO - BIT 7,(IX+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x7B:{
            // TODO - BIT 7,(IX+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x7C:{
            // TODO - BIT 7,(IX+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x7D:{
            // TODO - BIT 7,(IX+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x7E:{
            // TODO - BIT 7,(IX+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x7F:{
            // TODO - BIT 7,(IX+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x80:{
            // TODO - LD B,RES 0,(IX+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x81:{
            // TODO - LD C,RES 0,(IX+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x82:{
            // TODO - LD D,RES 0,(IX+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x83:{
            // TODO - LD E,RES 0,(IX+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x84:{
            // TODO - LD H,RES 0,(IX+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x85:{
            // TODO - LD L,RES 0,(IX+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x86:{
            // TODO - RES 0,(IX+d)
            Operations::OperationRES<0, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x87:{
            // TODO - LD A,RES 0,(IX+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x88:{
            // TODO - LD B,RES 1,(IX+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x89:{
            // TODO - LD C,RES 1,(IX+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x8A:{
            // TODO - LD D,RES 1,(IX+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x8B:{
            // TODO - LD E,RES 1,(IX+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x8C:{
            // TODO - LD H,RES 1,(IX+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x8D:{
            // TODO - LD L,RES 1,(IX+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x8E:{
            // TODO - RES 1,(IX+d)
            Operations::OperationRES<1, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x8F:{
            // TODO - LD A,RES 1,(IX+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x90:{
            // TODO - LD B,RES 2,(IX+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x91:{
            // TODO - LD C,RES 2,(IX+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x92:{
            // TODO - LD D,RES 2,(IX+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x93:{
            // TODO - LD E,RES 2,(IX+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x94:{
            // TODO - LD H,RES 2,(IX+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x95:{
            // TODO - LD L,RES 2,(IX+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x96:{
            // TODO - RES 2,(IX+d)
            Operations::OperationRES<2, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x97:{
            // TODO - LD A,RES 2,(IX+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x98:{
            // TODO - LD B,RES 3,(IX+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x99:{
            // TODO - LD C,RES 3,(IX+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x9A:{
            // TODO - LD D,RES 3,(IX+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x9B:{
            // TODO - LD E,RES 3,(IX+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x9C:{
            // TODO - LD H,RES 3,(IX+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x9D:{
            // TODO - LD L,RES 3,(IX+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x9E:{
            // TODO - RES 3,(IX+d)
            Operations::OperationRES<3, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x9F:{
            // TODO - LD A,RES 3,(IX+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA0:{
            // TODO - LD B,RES 4,(IX+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA1:{
            // TODO - LD C,RES 4,(IX+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA2:{
            // TODO - LD D,RES 4,(IX+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA3:{
            // TODO - LD E,RES 4,(IX+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA4:{
            // TODO - LD H,RES 4,(IX+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA5:{
            // TODO - LD L,RES 4,(IX+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA6:{
            // TODO - RES 4,(IX+d)
            Operations::OperationRES<4, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA7:{
            // TODO - LD A,RES 4,(IX+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA8:{
            // TODO - LD B,RES 5,(IX+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA9:{
            // TODO - LD C,RES 5,(IX+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xAA:{
            // TODO - LD D,RES 5,(IX+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xAB:{
            // TODO - LD E,RES 5,(IX+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xAC:{
            // TODO - LD H,RES 5,(IX+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xAD:{
            // TODO - LD L,RES 5,(IX+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xAE:{
            // TODO - RES 5,(IX+d)
            Operations::OperationRES<5, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xAF:{
            // TODO - LD A,RES 5,(IX+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB0:{
            // TODO - LD B,RES 6,(IX+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB1:{
            // TODO - LD C,RES 6,(IX+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB2:{
            // TODO - LD D,RES 6,(IX+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB3:{
            // TODO - LD E,RES 6,(IX+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB4:{
            // TODO - LD H,RES 6,(IX+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB5:{
            // TODO - LD L,RES 6,(IX+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB6:{
            // TODO - RES 6,(IX+d)
            Operations::OperationRES<6, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB7:{
            // TODO - LD A,RES 6,(IX+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB8:{
            // TODO - LD B,RES 7,(IX+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB9:{
            // TODO - LD C,RES 7,(IX+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xBA:{
            // TODO - LD D,RES 7,(IX+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xBB:{
            // TODO - LD E,RES 7,(IX+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xBC:{
            // TODO - LD H,RES 7,(IX+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xBD:{
            // TODO - LD L,RES 7,(IX+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xBE:{
            // TODO - RES 7,(IX+d)
            Operations::OperationRES<7, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xBF:{
            // TODO - LD A,RES 7,(IX+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC0:{
            // TODO - LD B,SET 0,(IX+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC1:{
            // TODO - LD C,SET 0,(IX+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC2:{
            // TODO - LD D,SET 0,(IX+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC3:{
            // TODO - LD E,SET 0,(IX+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC4:{
            // TODO - LD H,SET 0,(IX+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC5:{
            // TODO - LD L,SET 0,(IX+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC6:{
            // TODO - SET 0,(IX+d)
            Operations::OperationSET<0, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC7:{
            // TODO - LD A,SET 0,(IX+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC8:{
            // TODO - LD B,SET 1,(IX+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xC9:{
            // TODO - LD C,SET 1,(IX+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xCA:{
            // TODO - LD D,SET 1,(IX+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xCB:{
            // TODO - LD E,SET 1,(IX+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xCC:{
            // TODO - LD H,SET 1,(IX+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xCD:{
            // TODO - LD L,SET 1,(IX+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xCE:{
            // TODO - SET 1,(IX+d)
            Operations::OperationSET<1, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xCF:{
            // TODO - LD A,SET 1,(IX+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD0:{
            // TODO - LD B,SET 2,(IX+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD1:{
            // TODO - LD C,SET 2,(IX+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD2:{
            // TODO - LD D,SET 2,(IX+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD3:{
            // TODO - LD E,SET 2,(IX+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD4:{
            // TODO - LD H,SET 2,(IX+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD5:{
            // TODO - LD L,SET 2,(IX+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD6:{
            // TODO - SET 2,(IX+d)
            Operations::OperationSET<2, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD7:{
            // TODO - LD A,SET 2,(IX+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD8:{
            // TODO - LD B,SET 3,(IX+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xD9:{
            // TODO - LD C,SET 3,(IX+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xDA:{
            // TODO - LD D,SET 3,(IX+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xDB:{
            // TODO - LD E,SET 3,(IX+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xDC:{
            // TODO - LD H,SET 3,(IX+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xDD:{
            // TODO - LD L,SET 3,(IX+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xDE:{
            // TODO - SET 3,(IX+d)
            Operations::OperationSET<3, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xDF:{
            // TODO - LD A,SET 3,(IX+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE0:{
            // TODO - LD B,SET 4,(IX+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE1:{
            // TODO - LD C,SET 4,(IX+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE2:{
            // TODO - LD D,SET 4,(IX+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE3:{
            // TODO - LD E,SET 4,(IX+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE4:{
            // TODO - LD H,SET 4,(IX+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE5:{
            // TODO - LD L,SET 4,(IX+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE6:{
            // TODO - SET 4,(IX+d)
            Operations::OperationSET<4, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE7:{
            // TODO - LD A,SET 4,(IX+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE8:{
            // TODO - LD B,SET 5,(IX+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xE9:{
            // TODO - LD C,SET 5,(IX+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xEA:{
            // TODO - LD D,SET 5,(IX+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xEB:{
            // TODO - LD E,SET 5,(IX+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xEC:{
            // TODO - LD H,SET 5,(IX+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xED:{
            // TODO - LD L,SET 5,(IX+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xEE:{
            // TODO - SET 5,(IX+d)
            Operations::OperationSET<5, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xEF:{
            // TODO - LD A,SET 5,(IX+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF0:{
            // TODO - LD B,SET 6,(IX+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF1:{
            // TODO - LD C,SET 6,(IX+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF2:{
            // TODO - LD D,SET 6,(IX+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF3:{
            // TODO - LD E,SET 6,(IX+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF4:{
            // TODO - LD H,SET 6,(IX+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF5:{
            // TODO - LD L,SET 6,(IX+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF6:{
            // TODO - SET 6,(IX+d)
            Operations::OperationSET<6, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF7:{
            // TODO - LD A,SET 6,(IX+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF8:{
            // TODO - LD B,SET 7,(IX+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xF9:{
            // TODO - LD C,SET 7,(IX+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xFA:{
            // TODO - LD D,SET 7,(IX+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xFB:{
            // TODO - LD E,SET 7,(IX+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xFC:{
            // TODO - LD H,SET 7,(IX+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xFD:{
            // TODO - LD L,SET 7,(IX+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xFE:{
            // TODO - SET 7,(IX+d)
            Operations::OperationSET<7, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xFF:{
            // TODO - LD A,SET 7,(IX+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
    }
    nextPC += 2;
}

static inline void extendedOpIX() {
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC++ );
    switch( opcode ) {
        case 0x09:{
            //09: ADD IX,BC	
            OpMnemonics::ADD<AddressingMnemonics::IX, AddressingMnemonics::IX, AddressingMnemonics::BC>();
            break;
        }
        case 0x19:{
            //19: ADD IX,DE
            OpMnemonics::ADD<AddressingMnemonics::IX, AddressingMnemonics::IX, AddressingMnemonics::DE>();
            break;
        }
        case 0x21:{
            //21 n n: LD IX,nn
            OpMnemonics::LD<AddressingMnemonics::IX, AddressingMnemonics::nn>();
            break;
        }
        case 0x22:{
            //22 n n: LD (nn),IX
            OpMnemonics::LD<AddressingMnemonics::_nn_, AddressingMnemonics::IX>();
            break;
        }
        case 0x23:{
            //23: INC IX
            OpMnemonics::INC<AddressingMnemonics::IX, AddressingMnemonics::IX>();
            break;
        }
        case 0x24:{
            //24: INC IXH
            OpMnemonics::INC<AddressingMnemonics::IXH, AddressingMnemonics::IXH>();
            break;
        }
        case 0x25:{
            //25: DEC IXH
            OpMnemonics::DEC<AddressingMnemonics::IXH, AddressingMnemonics::IXH>();
            break;
        }
        case 0x26:{
            //26 n : LD IXH,n
            OpMnemonics::LD<AddressingMnemonics::IXH, AddressingMnemonics::n>();
            break;
        }
        case 0x29:{
            //29: ADD IX,IX
            OpMnemonics::ADD<AddressingMnemonics::IX, AddressingMnemonics::IX, AddressingMnemonics::IX>();
            break;
        }
        case 0x2A:{
            //2A n n: LD IX,(nn)
            OpMnemonics::LD<AddressingMnemonics::IX, AddressingMnemonics::_nn_>();
            break;
        }
        case 0x2B:{
            //2B: DEC IX
            OpMnemonics::DEC<AddressingMnemonics::IX, AddressingMnemonics::IX>();
            break;
        }
        case 0x2C:{
            //2C: INC IXL
            OpMnemonics::INC<AddressingMnemonics::IXL, AddressingMnemonics::IXL>();
            break;
        }
        case 0x2D:{
            //2D: DEC IXL
            OpMnemonics::DEC<AddressingMnemonics::IXL, AddressingMnemonics::IXL>();
            break;
        }
        case 0x2E:{
            //2E n: LD IXL,n
            OpMnemonics::LD<AddressingMnemonics::IXL, AddressingMnemonics::n>();
            break;
        }
        case 0x34:{
            //34 d: INC (IX+d)
            OpMnemonics::INC<AddressingMnemonics::_IX_d_<uint8_t, 2>, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x35:{
            //35 d: DEC (IX+d)
            OpMnemonics::DEC<AddressingMnemonics::_IX_d_<uint8_t, 2>, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x36:{
            //36 d n: LD (IX+d),n
            nextPC += 1;
            OpMnemonics::LD<AddressingMnemonics::_IX_d_<uint8_t, 2>, AddressingMnemonics::n>();
            break;
        }
        case 0x39:{
            //39: ADD IX,SP
            OpMnemonics::ADD<AddressingMnemonics::IX, AddressingMnemonics::IX, AddressingMnemonics::SP>();
            break;
        }
        case 0x44:{
            //44: LD B,IXH
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::IXH>();
            break;
        }
        case 0x45:{
            //45: LD B,IXL
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::IXL>();
            break;
        }
        case 0x46:{
            //46 d: LD B,(IX+d)
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x4C:{
            //4C: LD C,IXH
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::IXH>();
            break;
        }
        case 0x4D:{
            //4D: LD C,IXL
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::IXL>();
            break;
        }
        case 0x4E:{
            //4E d: LD C,(IX+d)
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x54:{
            //54: LD D,IXH
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::IXH>();
            break;
        }
        case 0x55:{
            //55: LD D,IXL
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::IXL>();
            break;
        }
        case 0x56:{
            //56 d: LD D,(IX+d)
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x5C:{
            //5C: LD E,IXH
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::IXH>();
            break;
        }
        case 0x5D:{
            //5D: LD E,IXL
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::IXL>();
            break;
        }
        case 0x5E:{
            //5E d: LD E,(IX+d)
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x60:{
            //60: LD IXH,B
            OpMnemonics::LD<AddressingMnemonics::IXH, AddressingMnemonics::B>();
            break;
        }
        case 0x61:{
            //61: LD IXH,C
            OpMnemonics::LD<AddressingMnemonics::IXH, AddressingMnemonics::C>();
            break;
        }
        case 0x62:{
            //62: LD IXH,D
            OpMnemonics::LD<AddressingMnemonics::IXH, AddressingMnemonics::D>();
            break;
        }
        case 0x63:{
            //63: LD IXH,E
            OpMnemonics::LD<AddressingMnemonics::IXH, AddressingMnemonics::E>();
            break;
        }
        case 0x64:{
            //64: LD IXH,IXH
            OpMnemonics::LD<AddressingMnemonics::IXH, AddressingMnemonics::IXH>();
            break;
        }
        case 0x65:{
            //65: LD IXH,IXL
            OpMnemonics::LD<AddressingMnemonics::IXH, AddressingMnemonics::IXL>();
            break;
        }
        case 0x66:{
            //66 d: LD H,(IX+d)
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x67:{
            //67: LD IXH,A
            OpMnemonics::LD<AddressingMnemonics::IXH, AddressingMnemonics::A>();
            break;
        }
        case 0x68:{
            //68: LD IXL,B
            OpMnemonics::LD<AddressingMnemonics::IXL, AddressingMnemonics::B>();
            break;
        }
        case 0x69:{
            //69: LD IXL,C
            OpMnemonics::LD<AddressingMnemonics::IXL, AddressingMnemonics::C>();
            break;
        }
        case 0x6A:{
            //6A: LD IXL,D
            OpMnemonics::LD<AddressingMnemonics::IXL, AddressingMnemonics::D>();
            break;
        }
        case 0x6B:{
            //6B: LD IXL,E
            OpMnemonics::LD<AddressingMnemonics::IXL, AddressingMnemonics::E>();
            break;
        }
        case 0x6C:{
            //6C: LD IXL,IXH
            OpMnemonics::LD<AddressingMnemonics::IXL, AddressingMnemonics::IXH>();
            break;
        }
        case 0x6D:{
            //6D: LD IXL,IXL
            OpMnemonics::LD<AddressingMnemonics::IXL, AddressingMnemonics::IXL>();
            break;
        }
        case 0x6E:{
            //6E d: LD L,(IX+d)
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x6F:{
            //6F: LD IXL,A
            OpMnemonics::LD<AddressingMnemonics::IXL, AddressingMnemonics::A>();
            break;
        }
        case 0x70:{
            //70 d: LD (IX+d),B
            OpMnemonics::LD<AddressingMnemonics::_IX_d_, AddressingMnemonics::B>();
            break;
        }
        case 0x71:{
            //71 d: LD (IX+d),C
            OpMnemonics::LD<AddressingMnemonics::_IX_d_, AddressingMnemonics::C>();
            break;
        }
        case 0x72:{
            //72 d: LD (IX+d),D
            OpMnemonics::LD<AddressingMnemonics::_IX_d_, AddressingMnemonics::D>();
            break;
        }
        case 0x73:{
            //73 d: LD (IX+d),E
            OpMnemonics::LD<AddressingMnemonics::_IX_d_, AddressingMnemonics::E>();
            break;
        }
        case 0x74:{
            //74 d: LD (IX+d),H
            OpMnemonics::LD<AddressingMnemonics::_IX_d_, AddressingMnemonics::H>();
            break;
        }
        case 0x75:{
            //75 d: LD (IX+d),L
            OpMnemonics::LD<AddressingMnemonics::_IX_d_, AddressingMnemonics::L>();
            break;
        }
        case 0x77:{
            //77 d: LD (IX+d),A
            OpMnemonics::LD<AddressingMnemonics::_IX_d_, AddressingMnemonics::A>();
            break;
        }
        case 0x7C:{
            //7C: LD A,IXH
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::IXH>();
            break;
        }
        case 0x7D:{
            //7D: LD A,IXL
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::IXL>();
            break;
        }
        case 0x7E:{
            //7E d: LD A,(IX+d)
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x84:{
            //84: ADD A,IXH
            OpMnemonics::ADD<AddressingMnemonics::IXH>();
            break;
        }
        case 0x85:{
            //85: ADD A,IXL
            OpMnemonics::ADD<AddressingMnemonics::IXL>();
            break;
        }
        case 0x86:{
            //86 d: ADD A,(IX+d)
            OpMnemonics::ADD<AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x8C:{
            //8C: ADC A,IXH
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::IXH>();
            break;
        }
        case 0x8D:{
            //8D: ADC A,IXL
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::IXL>();
            break;
        }
        case 0x8E:{
            //8E d: ADC A,(IX+d)
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x94:{
            //94: SUB IXH
            OpMnemonics::SUB<AddressingMnemonics::IXH>();
            break;
        }
        case 0x95:{
            //95: SUB IXL
            OpMnemonics::SUB<AddressingMnemonics::IXL>();
            break;
        }
        case 0x96:{
            //96 d: SUB (IX+d)
            OpMnemonics::SUB<AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0x9C:{
            //9C: SBC A,IXH
            OpMnemonics::SBC<AddressingMnemonics::IXH>();
            break;
        }
        case 0x9D:{
            //9D: SBC A,IXL
            OpMnemonics::SBC<AddressingMnemonics::IXL>();
            break;
        }
        case 0x9E:{
            //9E d: SBC A,(IX+d)
            OpMnemonics::SBC<AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0xA4:{
            //A4: AND IXH
            OpMnemonics::AND<AddressingMnemonics::IXH>();
            break;
        }
        case 0xA5:{
            //A5: AND IXL
            OpMnemonics::AND<AddressingMnemonics::IXL>();
            break;
        }
        case 0xA6:{
            //A6 d: AND (IX+d)
            OpMnemonics::AND<AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0xAC:{
            //AC: XOR IXH
            OpMnemonics::XOR<AddressingMnemonics::IXH>();
            break;
        }
        case 0xAD:{
            //AD: XOR IXL
            OpMnemonics::XOR<AddressingMnemonics::IXL>();
            break;
        }
        case 0xAE:{
            //AE d: XOR (IX+d)
            OpMnemonics::XOR<AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0xB4:{
            //B4: OR IXH
            OpMnemonics::OR<AddressingMnemonics::IXH>();
            break;
        }
        case 0xB5:{
            //B5: OR IXL
            OpMnemonics::OR<AddressingMnemonics::IXL>();
            break;
        }
        case 0xB6:{
            //B6 d: OR (IX+d)
            OpMnemonics::OR<AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0xBC:{
            //BC: CP IXH
            OpMnemonics::CP<AddressingMnemonics::IXH>();
            break;
        }
        case 0xBD:{
            //BD: CP IXL
            OpMnemonics::CP<AddressingMnemonics::IXL>();
            break;
        }
        case 0xBE:{
            //BE d: CP (IX+d)
            OpMnemonics::CP<AddressingMnemonics::_IX_d_>();
            break;
        }
        case 0xCB:{
            // TODO - IX-BITS
            extendedOpIXBITS();
            break;
        }
        case 0xE1:{
            //E1: POP IX
            OpMnemonics::POP<AddressingMnemonics::IX>();
            break;
        }
        case 0xE3:{
            //E3: EX (SP),IX
            // TODO - see if there's a better option here
            OpMnemonics::EX<SP, IX>();
            break;
        }
        case 0xE5:{
            //E5: PUSH IX
            OpMnemonics::PUSH<AddressingMnemonics::IX>();
            break;
        }
        case 0xE9:{
            //E9: JP (IX)
            OpMnemonics::JP<AddressingMnemonics::IX>();
            break;
        }
        case 0xF9:{
            //F9: LD SP,IX
            OpMnemonics::LD<AddressingMnemonics::SP, AddressingMnemonics::IX>();
            break;
        }
    }
}


static inline void extendedOpEXTD() {
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC++ );
    switch( opcode ) {
        case 0x40:{
            //40: IN B,(C)
            OpMnemonics::IN<AddressingMnemonics::B, AddressingMnemonics::_C_>();
            break;
        }
        case 0x41:{
            //41: OUT _C_,B
            OpMnemonics::OUT<AddressingMnemonics::_C_, AddressingMnemonics::B>();
            break;
        }
        case 0x42:{
            //42: SBC HL,BC
            OpMnemonics::SBC<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::BC>();
            break;
        }
        case 0x43:{
            //43 n n: LD _nn_,BC
            OpMnemonics::LD<AddressingMnemonics::_nn_, AddressingMnemonics::BC>();
            break;
        }
        case 0x44:{
            //44: NEG
            OpMnemonics::NEG();
            break;
        }
        case 0x45:{
            //45: RETN
            OpMnemonics::RETN();
            break;
        }
        case 0x46:{
            //46: IM 0
            OpMnemonics::IM<0>();
            break;
        }
        case 0x47:{
            //47: LD I,A
            OpMnemonics::LD<AddressingMnemonics::I, AddressingMnemonics::A>();
            break;
        }
        case 0x48:{
            //48: IN C,(C)
            OpMnemonics::IN<AddressingMnemonics::C, AddressingMnemonics::_C_>();
            break;
        }
        case 0x49:{
            //49: OUT _C_,C
            OpMnemonics::OUT<AddressingMnemonics::_C_, AddressingMnemonics::C>();
            break;
        }
        case 0x4A:{
            //4A: ADC HL,BC
            OpMnemonics::ADC<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::BC>();
            break;
        }
        case 0x4B:{
            //4B n n: LD BC,(nn)
            OpMnemonics::LD<AddressingMnemonics::BC, AddressingMnemonics::_nn_>();
            break;
        }
        case 0x4C:{
            //4C: NEG
            OpMnemonics::NEG();
            break;
        }
        case 0x4D:{
            //4D: RETI
            OpMnemonics::RETI();
            break;
        }
        case 0x4E:{
            //4E: IM 0
            OpMnemonics::IM<0>();
            break;
        }
        case 0x4F:{
            //4F: LD R,A
            OpMnemonics::LD<AddressingMnemonics::R, AddressingMnemonics::A>();
            break;
        }
        case 0x50:{
            //50: IN D,(C)
            OpMnemonics::IN<AddressingMnemonics::D, AddressingMnemonics::_C_>();
            break;
        }
        case 0x51:{
            //51: OUT _C_,D
            OpMnemonics::OUT<AddressingMnemonics::_C_, AddressingMnemonics::D>();
            break;
        }
        case 0x52:{
            //52: SBC HL,DE
            OpMnemonics::SBC<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::DE>();
            break;
        }
        case 0x53:{
            //53 n n: LD _nn_,DE
            OpMnemonics::LD<AddressingMnemonics::_nn_, AddressingMnemonics::DE>();
            break;
        }
        case 0x54:{
            //54: NEG
            OpMnemonics::NEG();
            break;
        }
        case 0x55:{
            //55: RETN
            OpMnemonics::RETN();
            break;
        }
        case 0x56:{
            //56: IM 1
            OpMnemonics::IM<1>();
            break;
        }
        case 0x57:{
            //57: LD A,I
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::I>();
            break;
        }
        case 0x58:{
            //58: IN E,(C)
            OpMnemonics::IN<AddressingMnemonics::E, AddressingMnemonics::_C_>();
            break;
        }
        case 0x59:{
            //59: OUT _C_,E
            OpMnemonics::OUT<AddressingMnemonics::_C_, AddressingMnemonics::E>();
            break;
        }
        case 0x5A:{
            //5A: ADC HL,DE
            OpMnemonics::ADC<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::DE>();
            break;
        }
        case 0x5B:{
            //5B n n: LD DE,(nn)
            OpMnemonics::LD<AddressingMnemonics::DE, AddressingMnemonics::_nn_>();
            break;
        }
        case 0x5C:{
            //5C: NEG
            OpMnemonics::NEG();
            break;
        }
        case 0x5D:{
            //5D: RETN
            OpMnemonics::RETN();
            break;
        }
        case 0x5E:{
            //5E: IM 2
            OpMnemonics::IM<2>();
            break;
        }
        case 0x5F:{
            //5F: LD A,R
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::R>();
            break;
        }
        case 0x60:{
            //60: IN H,(C)
            OpMnemonics::IN<AddressingMnemonics::H, AddressingMnemonics::_C_>();
            break;
        }
        case 0x61:{
            //61: OUT _C_,H
            OpMnemonics::OUT<AddressingMnemonics::_C_, AddressingMnemonics::H>();
            break;
        }
        case 0x62:{
            //62: SBC HL,HL
            OpMnemonics::SBC<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::HL>();
            break;
        }
        case 0x63:{
            //63 n n: LD _nn_,HL
            OpMnemonics::LD<AddressingMnemonics::_nn_, AddressingMnemonics::HL>();
            break;
        }
        case 0x64:{
            //64: NEG
            OpMnemonics::NEG();
            break;
        }
        case 0x65:{
            //65: RETN
            OpMnemonics::RETN();
            break;
        }
        case 0x66:{
            //66: IM 0
            OpMnemonics::IM<0>();
            break;
        }
        case 0x67:{
            //67: RRD
            OpMnemonics::RRD();
            break;
        }
        case 0x68:{
            //68: IN L,(C)
            OpMnemonics::IN<AddressingMnemonics::L, AddressingMnemonics::_C_>();
            break;
        }
        case 0x69:{
            //69: OUT _C_,L
            OpMnemonics::OUT<AddressingMnemonics::_C_, AddressingMnemonics::L>();
            break;
        }
        case 0x6A:{
            //6A: ADC HL,HL
            OpMnemonics::ADC<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::HL>();
            break;
        }
        case 0x6B:{
            //6B n n: LD HL,(nn)
            OpMnemonics::LD<AddressingMnemonics::HL, AddressingMnemonics::_nn_>();
            break;
        }
        case 0x6C:{
            //6C: NEG
            OpMnemonics::NEG();
            break;
        }
        case 0x6D:{
            //6D: RETN
            OpMnemonics::RETN();
            break;
        }
        case 0x6E:{
            //6E: IM 0
            OpMnemonics::IM<0>();
            break;
        }
        case 0x6F:{
            //6F: RLD
            OpMnemonics::RLD();
            break;
        }
        case 0x70:{
            //70: IN F,(C) / IN (C)
            OpMnemonics::IN<AddressingMnemonics::_C_>();
            break;
        }
        case 0x71:{
            //71: OUT _C_,0
            OpMnemonics::OUT<AddressingMnemonics::_C_>();
            break;
        }
        case 0x72:{
            //72: SBC HL,SP
            OpMnemonics::SBC<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::SP>();
            break;
        }
        case 0x73:{
            //73 n n: LD _nn_,SP
            OpMnemonics::LD<AddressingMnemonics::_nn_, AddressingMnemonics::SP>();
            break;
        }
        case 0x74:{
            //74: NEG
            OpMnemonics::NEG();
            break;
        }
        case 0x75:{
            //75: RETN
            OpMnemonics::RETN();
            break;
        }
        case 0x76:{
            //76: IM 1
            OpMnemonics::IM<1>();
            break;
        }
        case 0x78:{
            //78: IN A,(C)
            OpMnemonics::INA<AddressingMnemonics::_C_>();
            break;
        }
        case 0x79:{
            //79: OUT _C_,A
            OpMnemonics::OUT<AddressingMnemonics::_C_, AddressingMnemonics::A>();
            break;
        }
        case 0x7A:{
            //7A: ADC HL,SP
            OpMnemonics::ADC<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::SP>();
            break;
        }
        case 0x7B:{
            //7B n n: LD SP,(nn)
            OpMnemonics::LD<AddressingMnemonics::SP, AddressingMnemonics::_nn_>();
            break;
        }
        case 0x7C:{
            //7C: NEG
            OpMnemonics::NEG();
            break;
        }
        case 0x7D:{
            //7D: RETN
            OpMnemonics::RETN();
            break;
        }
        case 0x7E:{
            //7E: IM 2
            OpMnemonics::IM<2>();
            break;
        }
        case 0xA0:{
            //A0: LDI
            OpMnemonics::LDI();
            break;
        }
        case 0xA1:{
            //A1: CPI
            OpMnemonics::CPI();
            break;
        }
        case 0xA2:{
            //A2: INI
            OpMnemonics::INI();
            break;
        }
        case 0xA3:{
            //A3: OUTI
            OpMnemonics::OUTI();
            break;
        }
        case 0xA8:{
            //A8: LDD
            OpMnemonics::LDD();
            break;
        }
        case 0xA9:{
            //A9: CPD
            OpMnemonics::CPD();
            break;
        }
        case 0xAA:{
            //AA: IND
            OpMnemonics::IND();
            break;
        }
        case 0xAB:{
            //AB: OUTD
            OpMnemonics::OUTD();
            break;
        }
        case 0xB0:{
            //B0: LDIR
            OpMnemonics::LDIR();
            break;
        }
        case 0xB1:{
            //B1: CPIR
            OpMnemonics::CPIR();
            break;
        }
        case 0xB2:{
            //B2: INIR
            OpMnemonics::INIR();
            break;
        }
        case 0xB3:{
            //B3: OTIR
            OpMnemonics::OTIR();
            break;
        }
        case 0xB8:{
            //B8: LDDR
            OpMnemonics::LDDR();
            break;
        }
        case 0xB9:{
            //B9: CPDR
            OpMnemonics::CPDR();
            break;
        }
        case 0xBA:{
            //BA: INDR
            OpMnemonics::INDR();
            break;
        }
        case 0xBB:{
            //BB: OTDR
            OpMnemonics::OTDR();
            break;
        }
    }
}

static inline void extendedOpIYBITS() {
    // Prefixed with FDCB
    // Operand follows, then final byte of opcode.
    // TODO - note that for instruction decoding
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC + 1 );
    switch( opcode ) {
        case 0x00:{
            // TODO - LD B,RLC (IY+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x01:{
            // TODO - LD C,RLC (IY+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x02:{
            // TODO - LD D,RLC (IY+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x03:{
            // TODO - LD E,RLC (IY+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x04:{
            // TODO - LD H,RLC (IY+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x05:{
            // TODO - LD L,RLC (IY+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x06:{
            // TODO - RLC (IY+d)
            Operations::OperationRLC<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x07:{
            // TODO - LD A,RLC (IY+d)
            Operations::OperationRLC<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x08:{
            // TODO - LD B,RRC (IY+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x09:{
            // TODO - LD C,RRC (IY+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x0A:{
            // TODO - LD D,RRC (IY+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x0B:{
            // TODO - LD E,RRC (IY+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x0C:{
            // TODO - LD H,RRC (IY+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x0D:{
            // TODO - LD L,RRC (IY+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x0E:{
            // TODO - RRC (IY+d)
            Operations::OperationRRC<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x0F:{
            // TODO - LD A,RRC (IY+d)
            Operations::OperationRRC<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x10:{
            // TODO - LD B,RL (IY+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x11:{
            // TODO - LD C,RL (IY+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x12:{
            // TODO - LD D,RL (IY+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x13:{
            // TODO - LD E,RL (IY+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x14:{
            // TODO - LD H,RL (IY+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x15:{
            // TODO - LD L,RL (IY+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x16:{
            // TODO - RL (IY+d)
            Operations::OperationRL<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x17:{
            // TODO - LD A,RL (IY+d)
            Operations::OperationRL<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x18:{
            // TODO - LD B,RR (IY+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x19:{
            // TODO - LD C,RR (IY+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x1A:{
            // TODO - LD D,RR (IY+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x1B:{
            // TODO - LD E,RR (IY+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x1C:{
            // TODO - LD H,RR (IY+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x1D:{
            // TODO - LD L,RR (IY+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x1E:{
            // TODO - RR (IY+d)
            Operations::OperationRR<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x1F:{
            // TODO - LD A,RR (IY+d)
            Operations::OperationRR<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x20:{
            // TODO - LD B,SLA (IY+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x21:{
            // TODO - LD C,SLA (IY+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x22:{
            // TODO - LD D,SLA (IY+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x23:{
            // TODO - LD E,SLA (IY+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x24:{
            // TODO - LD H,SLA (IY+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x25:{
            // TODO - LD L,SLA (IY+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x26:{
            // TODO - SLA (IY+d)
            Operations::OperationSLA<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x27:{
            // TODO - LD A,SLA (IY+d)
            Operations::OperationSLA<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x28:{
            // TODO - LD B,SRA (IY+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x29:{
            // TODO - LD C,SRA (IY+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x2A:{
            // TODO - LD D,SRA (IY+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x2B:{
            // TODO - LD E,SRA (IY+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x2C:{
            // TODO - LD H,SRA (IY+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x2D:{
            // TODO - LD L,SRA (IY+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x2E:{
            // TODO - SRA (IY+d)
            Operations::OperationSRA<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x2F:{
            // TODO - LD A,SRA (IY+d)
            Operations::OperationSRA<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x30:{
            // TODO - LD B,SLL (IY+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x31:{
            // TODO - LD C,SLL (IY+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x32:{
            // TODO - LD D,SLL (IY+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x33:{
            // TODO - LD E,SLL (IY+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x34:{
            // TODO - LD H,SLL (IY+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x35:{
            // TODO - LD L,SLL (IY+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x36:{
            // TODO - SLL (IY+d)
            Operations::OperationSLL<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x37:{
            // TODO - LD A,SLL (IY+d)
            Operations::OperationSLL<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x38:{
            // TODO - LD B,SRL (IY+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x39:{
            // TODO - LD C,SRL (IY+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x3A:{
            // TODO - LD D,SRL (IY+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x3B:{
            // TODO - LD E,SRL (IY+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x3C:{
            // TODO - LD H,SRL (IY+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x3D:{
            // TODO - LD L,SRL (IY+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x3E:{
            // TODO - SRL (IY+d)
            Operations::OperationSRL<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x3F:{
            // TODO - LD A,SRL (IY+d)
            Operations::OperationSRL<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x40:{
            // TODO - BIT 0,(IY+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x41:{
            // TODO - BIT 0,(IY+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x42:{
            // TODO - BIT 0,(IY+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x43:{
            // TODO - BIT 0,(IY+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x44:{
            // TODO - BIT 0,(IY+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x45:{
            // TODO - BIT 0,(IY+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x46:{
            // TODO - BIT 0,(IY+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x47:{
            // TODO - BIT 0,(IY+d)
            Operations::OperationBIT<0, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x48:{
            // TODO - BIT 1,(IY+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x49:{
            // TODO - BIT 1,(IY+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x4A:{
            // TODO - BIT 1,(IY+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x4B:{
            // TODO - BIT 1,(IY+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x4C:{
            // TODO - BIT 1,(IY+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x4D:{
            // TODO - BIT 1,(IY+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x4E:{
            // TODO - BIT 1,(IY+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x4F:{
            // TODO - BIT 1,(IY+d)
            Operations::OperationBIT<1, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x50:{
            // TODO - BIT 2,(IY+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x51:{
            // TODO - BIT 2,(IY+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x52:{
            // TODO - BIT 2,(IY+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x53:{
            // TODO - BIT 2,(IY+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x54:{
            // TODO - BIT 2,(IY+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x55:{
            // TODO - BIT 2,(IY+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x56:{
            // TODO - BIT 2,(IY+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x57:{
            // TODO - BIT 2,(IY+d)
            Operations::OperationBIT<2, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x58:{
            // TODO - BIT 3,(IY+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x59:{
            // TODO - BIT 3,(IY+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x5A:{
            // TODO - BIT 3,(IY+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x5B:{
            // TODO - BIT 3,(IY+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x5C:{
            // TODO - BIT 3,(IY+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x5D:{
            // TODO - BIT 3,(IY+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x5E:{
            // TODO - BIT 3,(IY+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x5F:{
            // TODO - BIT 3,(IY+d)
            Operations::OperationBIT<3, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x60:{
            // TODO - BIT 4,(IY+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x61:{
            // TODO - BIT 4,(IY+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x62:{
            // TODO - BIT 4,(IY+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x63:{
            // TODO - BIT 4,(IY+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x64:{
            // TODO - BIT 4,(IY+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x65:{
            // TODO - BIT 4,(IY+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x66:{
            // TODO - BIT 4,(IY+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x67:{
            // TODO - BIT 4,(IY+d)
            Operations::OperationBIT<4, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x68:{
            // TODO - BIT 5,(IY+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x69:{
            // TODO - BIT 5,(IY+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x6A:{
            // TODO - BIT 5,(IY+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x6B:{
            // TODO - BIT 5,(IY+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x6C:{
            // TODO - BIT 5,(IY+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x6D:{
            // TODO - BIT 5,(IY+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x6E:{
            // TODO - BIT 5,(IY+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x6F:{
            // TODO - BIT 5,(IY+d)
            Operations::OperationBIT<5, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x70:{
            // TODO - BIT 6,(IY+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x71:{
            // TODO - BIT 6,(IY+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x72:{
            // TODO - BIT 6,(IY+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x73:{
            // TODO - BIT 6,(IY+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x74:{
            // TODO - BIT 6,(IY+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x75:{
            // TODO - BIT 6,(IY+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x76:{
            // TODO - BIT 6,(IY+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x77:{
            // TODO - BIT 6,(IY+d)
            Operations::OperationBIT<6, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x78:{
            // TODO - BIT 7,(IY+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x79:{
            // TODO - BIT 7,(IY+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x7A:{
            // TODO - BIT 7,(IY+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x7B:{
            // TODO - BIT 7,(IY+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x7C:{
            // TODO - BIT 7,(IY+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x7D:{
            // TODO - BIT 7,(IY+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x7E:{
            // TODO - BIT 7,(IY+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x7F:{
            // TODO - BIT 7,(IY+d)
            Operations::OperationBIT<7, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x80:{
            // TODO - LD B,RES 0,(IY+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x81:{
            // TODO - LD C,RES 0,(IY+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x82:{
            // TODO - LD D,RES 0,(IY+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x83:{
            // TODO - LD E,RES 0,(IY+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x84:{
            // TODO - LD H,RES 0,(IY+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x85:{
            // TODO - LD L,RES 0,(IY+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x86:{
            // TODO - RES 0,(IY+d)
            Operations::OperationRES<0, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x87:{
            // TODO - LD A,RES 0,(IY+d)
            Operations::OperationRES<0, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x88:{
            // TODO - LD B,RES 1,(IY+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x89:{
            // TODO - LD C,RES 1,(IY+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x8A:{
            // TODO - LD D,RES 1,(IY+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x8B:{
            // TODO - LD E,RES 1,(IY+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x8C:{
            // TODO - LD H,RES 1,(IY+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x8D:{
            // TODO - LD L,RES 1,(IY+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x8E:{
            // TODO - RES 1,(IY+d)
            Operations::OperationRES<1, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x8F:{
            // TODO - LD A,RES 1,(IY+d)
            Operations::OperationRES<1, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x90:{
            // TODO - LD B,RES 2,(IY+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x91:{
            // TODO - LD C,RES 2,(IY+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x92:{
            // TODO - LD D,RES 2,(IY+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x93:{
            // TODO - LD E,RES 2,(IY+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x94:{
            // TODO - LD H,RES 2,(IY+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x95:{
            // TODO - LD L,RES 2,(IY+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x96:{
            // TODO - RES 2,(IY+d)
            Operations::OperationRES<2, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x97:{
            // TODO - LD A,RES 2,(IY+d)
            Operations::OperationRES<2, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x98:{
            // TODO - LD B,RES 3,(IY+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x99:{
            // TODO - LD C,RES 3,(IY+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x9A:{
            // TODO - LD D,RES 3,(IY+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x9B:{
            // TODO - LD E,RES 3,(IY+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x9C:{
            // TODO - LD H,RES 3,(IY+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x9D:{
            // TODO - LD L,RES 3,(IY+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x9E:{
            // TODO - RES 3,(IY+d)
            Operations::OperationRES<3, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x9F:{
            // TODO - LD A,RES 3,(IY+d)
            Operations::OperationRES<3, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA0:{
            // TODO - LD B,RES 4,(IY+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA1:{
            // TODO - LD C,RES 4,(IY+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA2:{
            // TODO - LD D,RES 4,(IY+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA3:{
            // TODO - LD E,RES 4,(IY+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA4:{
            // TODO - LD H,RES 4,(IY+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA5:{
            // TODO - LD L,RES 4,(IY+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA6:{
            // TODO - RES 4,(IY+d)
            Operations::OperationRES<4, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA7:{
            // TODO - LD A,RES 4,(IY+d)
            Operations::OperationRES<4, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA8:{
            // TODO - LD B,RES 5,(IY+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA9:{
            // TODO - LD C,RES 5,(IY+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xAA:{
            // TODO - LD D,RES 5,(IY+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xAB:{
            // TODO - LD E,RES 5,(IY+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xAC:{
            // TODO - LD H,RES 5,(IY+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xAD:{
            // TODO - LD L,RES 5,(IY+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xAE:{
            // TODO - RES 5,(IY+d)
            Operations::OperationRES<5, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xAF:{
            // TODO - LD A,RES 5,(IY+d)
            Operations::OperationRES<5, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB0:{
            // TODO - LD B,RES 6,(IY+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB1:{
            // TODO - LD C,RES 6,(IY+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB2:{
            // TODO - LD D,RES 6,(IY+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB3:{
            // TODO - LD E,RES 6,(IY+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB4:{
            // TODO - LD H,RES 6,(IY+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB5:{
            // TODO - LD L,RES 6,(IY+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB6:{
            // TODO - RES 6,(IY+d)
            Operations::OperationRES<6, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB7:{
            // TODO - LD A,RES 6,(IY+d)
            Operations::OperationRES<6, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB8:{
            // TODO - LD B,RES 7,(IY+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB9:{
            // TODO - LD C,RES 7,(IY+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xBA:{
            // TODO - LD D,RES 7,(IY+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xBB:{
            // TODO - LD E,RES 7,(IY+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xBC:{
            // TODO - LD H,RES 7,(IY+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xBD:{
            // TODO - LD L,RES 7,(IY+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xBE:{
            // TODO - RES 7,(IY+d)
            Operations::OperationRES<7, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xBF:{
            // TODO - LD A,RES 7,(IY+d)
            Operations::OperationRES<7, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC0:{
            // TODO - LD B,SET 0,(IY+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC1:{
            // TODO - LD C,SET 0,(IY+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC2:{
            // TODO - LD D,SET 0,(IY+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC3:{
            // TODO - LD E,SET 0,(IY+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC4:{
            // TODO - LD H,SET 0,(IY+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC5:{
            // TODO - LD L,SET 0,(IY+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC6:{
            // TODO - SET 0,(IY+d)
            Operations::OperationSET<0, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC7:{
            // TODO - LD A,SET 0,(IY+d)
            Operations::OperationSET<0, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC8:{
            // TODO - LD B,SET 1,(IY+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xC9:{
            // TODO - LD C,SET 1,(IY+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xCA:{
            // TODO - LD D,SET 1,(IY+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xCB:{
            // TODO - LD E,SET 1,(IY+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xCC:{
            // TODO - LD H,SET 1,(IY+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xCD:{
            // TODO - LD L,SET 1,(IY+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xCE:{
            // TODO - SET 1,(IY+d)
            Operations::OperationSET<1, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xCF:{
            // TODO - LD A,SET 1,(IY+d)
            Operations::OperationSET<1, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD0:{
            // TODO - LD B,SET 2,(IY+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD1:{
            // TODO - LD C,SET 2,(IY+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD2:{
            // TODO - LD D,SET 2,(IY+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD3:{
            // TODO - LD E,SET 2,(IY+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD4:{
            // TODO - LD H,SET 2,(IY+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD5:{
            // TODO - LD L,SET 2,(IY+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD6:{
            // TODO - SET 2,(IY+d)
            Operations::OperationSET<2, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD7:{
            // TODO - LD A,SET 2,(IY+d)
            Operations::OperationSET<2, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD8:{
            // TODO - LD B,SET 3,(IY+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xD9:{
            // TODO - LD C,SET 3,(IY+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xDA:{
            // TODO - LD D,SET 3,(IY+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xDB:{
            // TODO - LD E,SET 3,(IY+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xDC:{
            // TODO - LD H,SET 3,(IY+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xDD:{
            // TODO - LD L,SET 3,(IY+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xDE:{
            // TODO - SET 3,(IY+d)
            Operations::OperationSET<3, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xDF:{
            // TODO - LD A,SET 3,(IY+d)
            Operations::OperationSET<3, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE0:{
            // TODO - LD B,SET 4,(IY+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE1:{
            // TODO - LD C,SET 4,(IY+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE2:{
            // TODO - LD D,SET 4,(IY+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE3:{
            // TODO - LD E,SET 4,(IY+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE4:{
            // TODO - LD H,SET 4,(IY+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE5:{
            // TODO - LD L,SET 4,(IY+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE6:{
            // TODO - SET 4,(IY+d)
            Operations::OperationSET<4, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE7:{
            // TODO - LD A,SET 4,(IY+d)
            Operations::OperationSET<4, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE8:{
            // TODO - LD B,SET 5,(IY+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xE9:{
            // TODO - LD C,SET 5,(IY+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xEA:{
            // TODO - LD D,SET 5,(IY+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xEB:{
            // TODO - LD E,SET 5,(IY+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xEC:{
            // TODO - LD H,SET 5,(IY+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xED:{
            // TODO - LD L,SET 5,(IY+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xEE:{
            // TODO - SET 5,(IY+d)
            Operations::OperationSET<5, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xEF:{
            // TODO - LD A,SET 5,(IY+d)
            Operations::OperationSET<5, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF0:{
            // TODO - LD B,SET 6,(IY+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF1:{
            // TODO - LD C,SET 6,(IY+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF2:{
            // TODO - LD D,SET 6,(IY+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF3:{
            // TODO - LD E,SET 6,(IY+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF4:{
            // TODO - LD H,SET 6,(IY+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF5:{
            // TODO - LD L,SET 6,(IY+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF6:{
            // TODO - SET 6,(IY+d)
            Operations::OperationSET<6, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF7:{
            // TODO - LD A,SET 6,(IY+d)
            Operations::OperationSET<6, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF8:{
            // TODO - LD B,SET 7,(IY+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xF9:{
            // TODO - LD C,SET 7,(IY+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xFA:{
            // TODO - LD D,SET 7,(IY+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xFB:{
            // TODO - LD E,SET 7,(IY+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xFC:{
            // TODO - LD H,SET 7,(IY+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xFD:{
            // TODO - LD L,SET 7,(IY+d)
            Operations::OperationSET<7, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xFE:{
            // TODO - SET 7,(IY+d)
            Operations::OperationSET<7, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xFF:{
            // TODO - LD A,SET 7,(IY+d)
            auto a = AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>();
            Operations::OperationSET<7, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
    }
    nextPC += 2;
}
static inline void extendedOpIY() {
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC++ );
    switch( opcode ) {
        case 0x09:{
            //09: ADD IY,BC	
            OpMnemonics::ADD<AddressingMnemonics::IY, AddressingMnemonics::IY, AddressingMnemonics::BC>();
            break;
        }
        case 0x19:{
            //19: ADD IY,DE
            OpMnemonics::ADD<AddressingMnemonics::IY, AddressingMnemonics::IY, AddressingMnemonics::DE>();
            break;
        }
        case 0x21:{
            //21 n n: LD IY,nn
            OpMnemonics::LD<AddressingMnemonics::IY, AddressingMnemonics::nn>();
            break;
        }
        case 0x22:{
            //22 n n: LD (nn),IY
            OpMnemonics::LD<AddressingMnemonics::_nn_, AddressingMnemonics::IY>();
            break;
        }
        case 0x23:{
            //23: INC IY
            OpMnemonics::INC<AddressingMnemonics::IY, AddressingMnemonics::IY>();
            break;
        }
        case 0x24:{
            //24: INC IYH
            OpMnemonics::INC<AddressingMnemonics::IYH, AddressingMnemonics::IYH>();
            break;
        }
        case 0x25:{
            //25: DEC IYH
            OpMnemonics::DEC<AddressingMnemonics::IYH, AddressingMnemonics::IYH>();
            break;
        }
        case 0x26:{
            //26 n : LD IYH,n
            OpMnemonics::LD<AddressingMnemonics::IYH, AddressingMnemonics::n>();
            break;
        }
        case 0x29:{
            //29: ADD IY,IY
            OpMnemonics::ADD<AddressingMnemonics::IY, AddressingMnemonics::IY, AddressingMnemonics::IY>();
            break;
        }
        case 0x2A:{
            //2A n n: LD IY,(nn)
            OpMnemonics::LD<AddressingMnemonics::IY, AddressingMnemonics::_nn_>();
            break;
        }
        case 0x2B:{
            //2B: DEC IY
            OpMnemonics::DEC<AddressingMnemonics::IY, AddressingMnemonics::IY>();
            break;
        }
        case 0x2C:{
            //2C: INC IYL
            OpMnemonics::INC<AddressingMnemonics::IYL, AddressingMnemonics::IYL>();
            break;
        }
        case 0x2D:{
            //2D: DEC IYL
            OpMnemonics::DEC<AddressingMnemonics::IYL, AddressingMnemonics::IYL>();
            break;
        }
        case 0x2E:{
            //2E n: LD IYL,n
            OpMnemonics::LD<AddressingMnemonics::IYL, AddressingMnemonics::n>();
            break;
        }
        case 0x34:{
            //34 d: INC (IY+d)
            OpMnemonics::INC<AddressingMnemonics::_IY_d_<uint8_t>, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x35:{
            //35 d: DEC (IY+d)
            OpMnemonics::DEC<AddressingMnemonics::_IY_d_<uint8_t>, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x36:{
            //36 d n: LD (IY+d),n
            OpMnemonics::LD<AddressingMnemonics::_IY_d_, AddressingMnemonics::n>();
            break;
        }
        case 0x39:{
            //39: ADD IY,SP
            OpMnemonics::ADD<AddressingMnemonics::IY, AddressingMnemonics::IY, AddressingMnemonics::SP>();
            break;
        }
        case 0x44:{
            //44: LD B,IYH
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::IYH>();
            break;
        }
        case 0x45:{
            //45: LD B,IYL
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::IYL>();
            break;
        }
        case 0x46:{
            //46 d: LD B,(IY+d)
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x4C:{
            //4C: LD C,IYH
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::IYH>();
            break;
        }
        case 0x4D:{
            //4D: LD C,IYL
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::IYL>();
            break;
        }
        case 0x4E:{
            //4E d: LD C,(IY+d)
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x54:{
            //54: LD D,IYH
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::IYH>();
            break;
        }
        case 0x55:{
            //55: LD D,IYL
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::IYL>();
            break;
        }
        case 0x56:{
            //56 d: LD D,(IY+d)
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x5C:{
            //5C: LD E,IYH
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::IYH>();
            break;
        }
        case 0x5D:{
            //5D: LD E,IYL
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::IYL>();
            break;
        }
        case 0x5E:{
            //5E d: LD E,(IY+d)
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x60:{
            //60: LD IYH,B
            OpMnemonics::LD<AddressingMnemonics::IYH, AddressingMnemonics::B>();
            break;
        }
        case 0x61:{
            //61: LD IYH,C
            OpMnemonics::LD<AddressingMnemonics::IYH, AddressingMnemonics::C>();
            break;
        }
        case 0x62:{
            //62: LD IYH,D
            OpMnemonics::LD<AddressingMnemonics::IYH, AddressingMnemonics::D>();
            break;
        }
        case 0x63:{
            //63: LD IYH,E
            OpMnemonics::LD<AddressingMnemonics::IYH, AddressingMnemonics::E>();
            break;
        }
        case 0x64:{
            //64: LD IYH,IYH
            OpMnemonics::LD<AddressingMnemonics::IYH, AddressingMnemonics::IYH>();
            break;
        }
        case 0x65:{
            //65: LD IYH,IYL
            OpMnemonics::LD<AddressingMnemonics::IYH, AddressingMnemonics::IYL>();
            break;
        }
        case 0x66:{
            //66 d: LD H,(IY+d)
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x67:{
            //67: LD IYH,A
            OpMnemonics::LD<AddressingMnemonics::IYH, AddressingMnemonics::A>();
            break;
        }
        case 0x68:{
            //68: LD IYL,B
            OpMnemonics::LD<AddressingMnemonics::IYL, AddressingMnemonics::B>();
            break;
        }
        case 0x69:{
            //69: LD IYL,C
            OpMnemonics::LD<AddressingMnemonics::IYL, AddressingMnemonics::C>();
            break;
        }
        case 0x6A:{
            //6A: LD IYL,D
            OpMnemonics::LD<AddressingMnemonics::IYL, AddressingMnemonics::D>();
            break;
        }
        case 0x6B:{
            //6B: LD IYL,E
            OpMnemonics::LD<AddressingMnemonics::IYL, AddressingMnemonics::E>();
            break;
        }
        case 0x6C:{
            //6C: LD IYL,IYH
            OpMnemonics::LD<AddressingMnemonics::IYL, AddressingMnemonics::IYH>();
            break;
        }
        case 0x6D:{
            //6D: LD IYL,IYL
            OpMnemonics::LD<AddressingMnemonics::IYL, AddressingMnemonics::IYL>();
            break;
        }
        case 0x6E:{
            //6E d: LD L,(IY+d)
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x6F:{
            //6F: LD IYL,A
            OpMnemonics::LD<AddressingMnemonics::IYL, AddressingMnemonics::A>();
            break;
        }
        case 0x70:{
            //70 d: LD (IY+d),B
            OpMnemonics::LD<AddressingMnemonics::_IY_d_, AddressingMnemonics::B>();
            break;
        }
        case 0x71:{
            //71 d: LD (IY+d),C
            OpMnemonics::LD<AddressingMnemonics::_IY_d_, AddressingMnemonics::C>();
            break;
        }
        case 0x72:{
            //72 d: LD (IY+d),D
            OpMnemonics::LD<AddressingMnemonics::_IY_d_, AddressingMnemonics::D>();
            break;
        }
        case 0x73:{
            //73 d: LD (IY+d),E
            OpMnemonics::LD<AddressingMnemonics::_IY_d_, AddressingMnemonics::E>();
            break;
        }
        case 0x74:{
            //74 d: LD (IY+d),H
            OpMnemonics::LD<AddressingMnemonics::_IY_d_, AddressingMnemonics::H>();
            break;
        }
        case 0x75:{
            //75 d: LD (IY+d),L
            OpMnemonics::LD<AddressingMnemonics::_IY_d_, AddressingMnemonics::L>();
            break;
        }
        case 0x77:{
            //77 d: LD (IY+d),A
            OpMnemonics::LD<AddressingMnemonics::_IY_d_, AddressingMnemonics::A>();
            break;
        }
        case 0x7C:{
            //7C: LD A,IYH
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::IYH>();
            break;
        }
        case 0x7D:{
            //7D: LD A,IYL
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::IYL>();
            break;
        }
        case 0x7E:{
            //7E d: LD A,(IY+d)
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x84:{
            //84: ADD A,IYH
            OpMnemonics::ADD<AddressingMnemonics::IYH>();
            break;
        }
        case 0x85:{
            //85: ADD A,IYL
            OpMnemonics::ADD<AddressingMnemonics::IYL>();
            break;
        }
        case 0x86:{
            //86 d: ADD A,(IY+d)
            OpMnemonics::ADD<AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x8C:{
            //8C: ADC A,IYH
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::IYH>();
            break;
        }
        case 0x8D:{
            //8D: ADC A,IYL
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::IYL>();
            break;
        }
        case 0x8E:{
            //8E d: ADC A,(IY+d)
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x94:{
            //94: SUB IYH
            OpMnemonics::SUB<AddressingMnemonics::IYH>();
            break;
        }
        case 0x95:{
            //95: SUB IYL
            OpMnemonics::SUB<AddressingMnemonics::IYL>();
            break;
        }
        case 0x96:{
            //96 d: SUB (IY+d)
            OpMnemonics::SUB<AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0x9C:{
            //9C: SBC A,IYH
            OpMnemonics::SBC<AddressingMnemonics::IYH>();
            break;
        }
        case 0x9D:{
            //9D: SBC A,IYL
            OpMnemonics::SBC<AddressingMnemonics::IYL>();
            break;
        }
        case 0x9E:{
            //9E d: SBC A,(IY+d)
            OpMnemonics::SBC<AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0xA4:{
            //A4: AND IYH
            OpMnemonics::AND<AddressingMnemonics::IYH>();
            break;
        }
        case 0xA5:{
            //A5: AND IYL
            OpMnemonics::AND<AddressingMnemonics::IYL>();
            break;
        }
        case 0xA6:{
            //A6 d: AND (IY+d)
            OpMnemonics::AND<AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0xAC:{
            //AC: XOR IYH
            OpMnemonics::XOR<AddressingMnemonics::IYH>();
            break;
        }
        case 0xAD:{
            //AD: XOR IYL
            OpMnemonics::XOR<AddressingMnemonics::IYL>();
            break;
        }
        case 0xAE:{
            //AE d: XOR (IY+d)
            OpMnemonics::XOR<AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0xB4:{
            //B4: OR IYH
            OpMnemonics::OR<AddressingMnemonics::IYH>();
            break;
        }
        case 0xB5:{
            //B5: OR IYL
            OpMnemonics::OR<AddressingMnemonics::IYL>();
            break;
        }
        case 0xB6:{
            //B6 d: OR (IY+d)
            OpMnemonics::OR<AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0xBC:{
            //BC: CP IYH
            OpMnemonics::CP<AddressingMnemonics::IYH>();
            break;
        }
        case 0xBD:{
            //BD: CP IYL
            OpMnemonics::CP<AddressingMnemonics::IYL>();
            break;
        }
        case 0xBE:{
            //BE d: CP (IY+d)
            OpMnemonics::CP<AddressingMnemonics::_IY_d_>();
            break;
        }
        case 0xCB:{
            // TODO - IX-BITS
            extendedOpIYBITS();
            break;
        }
        case 0xE1:{
            //E1: POP IY
            OpMnemonics::POP<AddressingMnemonics::IY>();
            break;
        }
        case 0xE3:{
            //E3: EX (SP),IY
            // TODO - see if there's a better option here
            OpMnemonics::EX<SP, IY>();
            break;
        }
        case 0xE5:{
            //E5: PUSH IY
            OpMnemonics::PUSH<AddressingMnemonics::IY>();
            break;
        }
        case 0xE9:{
            //E9: JP (IY)
            OpMnemonics::JP<AddressingMnemonics::IY>();
            break;
        }
        case 0xF9:{
            //F9: LD SP,IY
            OpMnemonics::LD<AddressingMnemonics::SP, AddressingMnemonics::IY>();
            break;
        }
    }
}

static inline void operationTick() {
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC++ );
    switch( opcode ) {
        case 0x00: {
            OpMnemonics::NOP();
            break;
        }
        case 0x01: {
            OpMnemonics::LD<AddressingMnemonics::BC, AddressingMnemonics::nn>();
            break;
        }
        case 0x02: {
            OpMnemonics::LD<AddressingMnemonics::_BC_, AddressingMnemonics::A>();
            break;
        }
        case 0x03: {
            OpMnemonics::INC<AddressingMnemonics::BC, AddressingMnemonics::BC>();
            break;
        }
        case 0x04: {
            OpMnemonics::INC<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x05: {
            OpMnemonics::DEC<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x06: {
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::n>();
            break;
        }
        case 0x07: {
            OpMnemonics::RLCA();
            break;
        }
        case 0x08: {
            OpMnemonics::EX<A>();
            break;
        }
        case 0x09: {
            OpMnemonics::ADD<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::BC>();
            break;
        }
        case 0x0A: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::_BC_>();
            break;
        }
        case 0x0B: {
            OpMnemonics::DEC<AddressingMnemonics::BC, AddressingMnemonics::BC>();
            break;
        }
        case 0x0C: {
            OpMnemonics::INC<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x0D: {
            OpMnemonics::DEC<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x0E: {
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::n>();
            break;
        }
        case 0x0F: {
            OpMnemonics::RRCA();
            break;
        }
        case 0x10: {
            OpMnemonics::DJNZ<AddressingMnemonics::r>();
            break;
        }
        case 0x11: {
            OpMnemonics::LD<AddressingMnemonics::DE, AddressingMnemonics::nn>();
            break;
        }
        case 0x12: {
            OpMnemonics::LD<AddressingMnemonics::_DE_, AddressingMnemonics::A>();
            break;
        }
        case 0x13: {
            OpMnemonics::INC<AddressingMnemonics::DE, AddressingMnemonics::DE>();
            break;
        }
        case 0x14: {
            OpMnemonics::INC<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x15: {
            OpMnemonics::DEC<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x16: {
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::n>();
            break;
        }
        case 0x17: {
            OpMnemonics::RLA();
            break;
        }
        case 0x18: {
            OpMnemonics::JR<AddressingMnemonics::r>();
            break;
        }
        case 0x19: {
            OpMnemonics::ADD<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::DE>();
            break;
        }
        case 0x1A: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::_DE_>();
            break;
        }
        case 0x1B: {
            OpMnemonics::DEC<AddressingMnemonics::DE, AddressingMnemonics::DE>();
            break;
        }
        case 0x1C: {
            OpMnemonics::INC<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x1D: {
            OpMnemonics::DEC<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x1E: {
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::n>();
            break;
        }
        case 0x1F: {
            OpMnemonics::RRA();
            break;
        }
        case 0x20: {
            OpMnemonics::JR<FlagMnemonics::NZ, AddressingMnemonics::r>();
            break;
        }
        case 0x21: {
            OpMnemonics::LD<AddressingMnemonics::HL, AddressingMnemonics::nn>();
            break;
        }
        case 0x22: {
            OpMnemonics::LD<AddressingMnemonics::_nn_, AddressingMnemonics::HL>();
            break;
        }
        case 0x23: {
            OpMnemonics::INC<AddressingMnemonics::HL, AddressingMnemonics::HL>();
            break;
        }
        case 0x24: {
            OpMnemonics::INC<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x25: {
            OpMnemonics::DEC<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x26: {
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::n>();
            break;
        }
        case 0x27: {
            OpMnemonics::DAA();
            break;
        }
        case 0x28: {
            OpMnemonics::JR<FlagMnemonics::Z, AddressingMnemonics::r>();
            break;
        }
        case 0x29: {
            OpMnemonics::ADD<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::HL>();
            break;
        }
        case 0x2A: {
            OpMnemonics::LD<AddressingMnemonics::HL, AddressingMnemonics::_nn_>();
            break;
        }
        case 0x2B: {
            OpMnemonics::DEC<AddressingMnemonics::HL, AddressingMnemonics::HL>();
            break;
        }
        case 0x2C: {
            OpMnemonics::INC<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x2D: {
            OpMnemonics::DEC<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x2E: {
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::n>();
            break;
        }
        case 0x2F: {
            OpMnemonics::CPL();
            break;
        }
        case 0x30: {
            OpMnemonics::JR<FlagMnemonics::NC, AddressingMnemonics::r>();
            break;
        }
        case 0x31: {
            OpMnemonics::LD<AddressingMnemonics::SP, AddressingMnemonics::nn>();
            break;
        }
        case 0x32: {
            OpMnemonics::LD<AddressingMnemonics::_nn_, AddressingMnemonics::A>();
            break;
        }
        case 0x33: {
            OpMnemonics::INC<AddressingMnemonics::SP, AddressingMnemonics::SP>();
            break;
        }
        case 0x34: {
            OpMnemonics::INC<AddressingMnemonics::_HL_<uint8_t>, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x35: {
            OpMnemonics::DEC<AddressingMnemonics::_HL_<uint8_t>, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x36: {
            OpMnemonics::LD<AddressingMnemonics::_HL_, AddressingMnemonics::n>();
            break;
        }
        case 0x37: {
            OpMnemonics::SCF();
            break;
        }
        case 0x38: {
            OpMnemonics::JR<FlagMnemonics::C, AddressingMnemonics::r>();
            break;
        }
        case 0x39: {
            OpMnemonics::ADD<AddressingMnemonics::HL, AddressingMnemonics::HL, AddressingMnemonics::SP>();
            break;
        }
        case 0x3A: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::_nn_>();
            break;
        }
        case 0x3B: {
            OpMnemonics::DEC<AddressingMnemonics::SP, AddressingMnemonics::SP>();
            break;
        }
        case 0x3C: {
            OpMnemonics::INC<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x3D: {
            OpMnemonics::DEC<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x3E: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::n>();
            break;
        }
        case 0x3F: {
            OpMnemonics::CCF();
            break;
        }
        case 0x40: {
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::B>();
            break;
        }
        case 0x41: {
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::C>();
            break;
        }
        case 0x42: {
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::D>();
            break;
        }
        case 0x43: {
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::E>();
            break;
        }
        case 0x44: {
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::H>();
            break;
        }
        case 0x45: {
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::L>();
            break;
        }
        case 0x46: {
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x47: {
            OpMnemonics::LD<AddressingMnemonics::B, AddressingMnemonics::A>();
            break;
        }
        case 0x48: {
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::B>();
            break;
        }
        case 0x49: {
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::C>();
            break;
        }
        case 0x4A: {
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::D>();
            break;
        }
        case 0x4B: {
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::E>();
            break;
        }
        case 0x4C: {
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::H>();
            break;
        }
        case 0x4D: {
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::L>();
            break;
        }
        case 0x4E: {
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x4F: {
            OpMnemonics::LD<AddressingMnemonics::C, AddressingMnemonics::A>();
            break;
        }
        case 0x50: {
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::B>();
            break;
        }
        case 0x51: {
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::C>();
            break;
        }
        case 0x52: {
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::D>();
            break;
        }
        case 0x53: {
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::E>();
            break;
        }
        case 0x54: {
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::H>();
            break;
        }
        case 0x55: {
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::L>();
            break;
        }
        case 0x56: {
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x57: {
            OpMnemonics::LD<AddressingMnemonics::D, AddressingMnemonics::A>();
            break;
        }
        case 0x58: {
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::B>();
            break;
        }
        case 0x59: {
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::C>();
            break;
        }
        case 0x5A: {
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::D>();
            break;
        }
        case 0x5B: {
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::E>();
            break;
        }
        case 0x5C: {
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::H>();
            break;
        }
        case 0x5D: {
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::L>();
            break;
        }
        case 0x5E: {
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x5F: {
            OpMnemonics::LD<AddressingMnemonics::E, AddressingMnemonics::A>();
            break;
        }
        case 0x60: {
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::B>();
            break;
        }
        case 0x61: {
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::C>();
            break;
        }
        case 0x62: {
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::D>();
            break;
        }
        case 0x63: {
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::E>();
            break;
        }
        case 0x64: {
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::H>();
            break;
        }
        case 0x65: {
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::L>();
            break;
        }
        case 0x66: {
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x67: {
            OpMnemonics::LD<AddressingMnemonics::H, AddressingMnemonics::A>();
            break;
        }
        case 0x68: {
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::B>();
            break;
        }
        case 0x69: {
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::C>();
            break;
        }
        case 0x6A: {
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::D>();
            break;
        }
        case 0x6B: {
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::E>();
            break;
        }
        case 0x6C: {
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::H>();
            break;
        }
        case 0x6D: {
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::L>();
            break;
        }
        case 0x6E: {
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x6F: {
            OpMnemonics::LD<AddressingMnemonics::L, AddressingMnemonics::A>();
            break;
        }
        case 0x70: {
            OpMnemonics::LD<AddressingMnemonics::_HL_, AddressingMnemonics::B>();
            break;
        }
        case 0x71: {
            OpMnemonics::LD<AddressingMnemonics::_HL_, AddressingMnemonics::C>();
            break;
        }
        case 0x72: {
            OpMnemonics::LD<AddressingMnemonics::_HL_, AddressingMnemonics::D>();
            break;
        }
        case 0x73: {
            OpMnemonics::LD<AddressingMnemonics::_HL_, AddressingMnemonics::E>();
            break;
        }
        case 0x74: {
            OpMnemonics::LD<AddressingMnemonics::_HL_, AddressingMnemonics::H>();
            break;
        }
        case 0x75: {
            OpMnemonics::LD<AddressingMnemonics::_HL_, AddressingMnemonics::L>();
            break;
        }
        case 0x76: {
            OperationBase<Operations::HALT>();
            break;
        }
        case 0x77: {
            OpMnemonics::LD<AddressingMnemonics::_HL_, AddressingMnemonics::A>();
            break;
        }
        case 0x78: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::B>();
            break;
        }
        case 0x79: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::C>();
            break;
        }
        case 0x7A: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::D>();
            break;
        }
        case 0x7B: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::E>();
            break;
        }
        case 0x7C: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::H>();
            break;
        }
        case 0x7D: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::L>();
            break;
        }
        case 0x7E: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x7F: {
            OpMnemonics::LD<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x80: {
            OpMnemonics::ADD<AddressingMnemonics::B>();
            break;
        }
        case 0x81: {
            OpMnemonics::ADD<AddressingMnemonics::C>();
            break;
        }
        case 0x82: {
            OpMnemonics::ADD<AddressingMnemonics::D>();
            break;
        }
        case 0x83: {
            OpMnemonics::ADD<AddressingMnemonics::E>();
            break;
        }
        case 0x84: {
            OpMnemonics::ADD<AddressingMnemonics::H>();
            break;
        }
        case 0x85: {
            OpMnemonics::ADD<AddressingMnemonics::L>();
            break;
        }
        case 0x86: {
            OpMnemonics::ADD<AddressingMnemonics::_HL_>();
            break;
        }
        case 0x87: {
            OpMnemonics::ADD<AddressingMnemonics::A>();
            break;
        }
        case 0x88: {
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::B>();
            break;
        }
        case 0x89: {
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::C>();
            break;
        }
        case 0x8A: {
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::D>();
            break;
        }
        case 0x8B: {
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::E>();
            break;
        }
        case 0x8C: {
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::H>();
            break;
        }
        case 0x8D: {
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::L>();
            break;
        }
        case 0x8E: {
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::_HL_>();
            break;
        }
        case 0x8F: {
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::A>();
            break;
        }
        case 0x90: {
            OpMnemonics::SUB<AddressingMnemonics::B>();
            break;
        }
        case 0x91: {
            OpMnemonics::SUB<AddressingMnemonics::C>();
            break;
        }
        case 0x92: {
            OpMnemonics::SUB<AddressingMnemonics::D>();
            break;
        }
        case 0x93: {
            OpMnemonics::SUB<AddressingMnemonics::E>();
            break;
        }
        case 0x94: {
            OpMnemonics::SUB<AddressingMnemonics::H>();
            break;
        }
        case 0x95: {
            OpMnemonics::SUB<AddressingMnemonics::L>();
            break;
        }
        case 0x96: {
            OpMnemonics::SUB<AddressingMnemonics::_HL_>();
            break;
        }
        case 0x97: {
            OpMnemonics::SUB<AddressingMnemonics::A>();
            break;
        }
        case 0x98: {
            OpMnemonics::SBC<AddressingMnemonics::B>();
            break;
        }
        case 0x99: {
            OpMnemonics::SBC<AddressingMnemonics::C>();
            break;
        }
        case 0x9A: {
            OpMnemonics::SBC<AddressingMnemonics::D>();
            break;
        }
        case 0x9B: {
            OpMnemonics::SBC<AddressingMnemonics::E>();
            break;
        }
        case 0x9C: {
            OpMnemonics::SBC<AddressingMnemonics::H>();
            break;
        }
        case 0x9D: {
            OpMnemonics::SBC<AddressingMnemonics::L>();
            break;
        }
        case 0x9E: {
            OpMnemonics::SBC<AddressingMnemonics::_HL_>();
            break;
        }
        case 0x9F: {
            OpMnemonics::SBC<AddressingMnemonics::A>();
            break;
        }
        case 0xA0: {
            OpMnemonics::AND<AddressingMnemonics::B>();
            break;
        }
        case 0xA1: {
            OpMnemonics::AND<AddressingMnemonics::C>();
            break;
        }
        case 0xA2: {
            OpMnemonics::AND<AddressingMnemonics::D>();
            break;
        }
        case 0xA3: {
            OpMnemonics::AND<AddressingMnemonics::E>();
            break;
        }
        case 0xA4: {
            OpMnemonics::AND<AddressingMnemonics::H>();
            break;
        }
        case 0xA5: {
            OpMnemonics::AND<AddressingMnemonics::L>();
            break;
        }
        case 0xA6: {
            OpMnemonics::AND<AddressingMnemonics::_HL_>();
            break;
        }
        case 0xA7: {
            OpMnemonics::AND<AddressingMnemonics::A>();
            break;
        }
        case 0xA8: {
            OpMnemonics::XOR<AddressingMnemonics::B>();
            break;
        }
        case 0xA9: {
            OpMnemonics::XOR<AddressingMnemonics::C>();
            break;
        }
        case 0xAA: {
            OpMnemonics::XOR<AddressingMnemonics::D>();
            break;
        }
        case 0xAB: {
            OpMnemonics::XOR<AddressingMnemonics::E>();
            break;
        }
        case 0xAC: {
            OpMnemonics::XOR<AddressingMnemonics::H>();
            break;
        }
        case 0xAD: {
            OpMnemonics::XOR<AddressingMnemonics::L>();
            break;
        }
        case 0xAE: {
            OpMnemonics::XOR<AddressingMnemonics::_HL_>();
            break;
        }
        case 0xAF: {
            OpMnemonics::XOR<AddressingMnemonics::A>();
            break;
        }
        case 0xB0: {
            OpMnemonics::OR<AddressingMnemonics::B>();
            break;
        }
        case 0xB1: {
            OpMnemonics::OR<AddressingMnemonics::C>();
            break;
        }
        case 0xB2: {
            OpMnemonics::OR<AddressingMnemonics::D>();
            break;
        }
        case 0xB3: {
            OpMnemonics::OR<AddressingMnemonics::E>();
            break;
        }
        case 0xB4: {
            OpMnemonics::OR<AddressingMnemonics::H>();
            break;
        }
        case 0xB5: {
            OpMnemonics::OR<AddressingMnemonics::L>();
            break;
        }
        case 0xB6: {
            OpMnemonics::OR<AddressingMnemonics::_HL_>();
            break;
        }
        case 0xB7: {
            OpMnemonics::OR<AddressingMnemonics::A>();
            break;
        }
        case 0xB8: {
            OpMnemonics::CP<AddressingMnemonics::B>();
            break;
        }
        case 0xB9: {
            OpMnemonics::CP<AddressingMnemonics::C>();
            break;
        }
        case 0xBA: {
            OpMnemonics::CP<AddressingMnemonics::D>();
            break;
        }
        case 0xBB: {
            OpMnemonics::CP<AddressingMnemonics::E>();
            break;
        }
        case 0xBC: {
            OpMnemonics::CP<AddressingMnemonics::H>();
            break;
        }
        case 0xBD: {
            OpMnemonics::CP<AddressingMnemonics::L>();
            break;
        }
        case 0xBE: {
            OpMnemonics::CP<AddressingMnemonics::_HL_>();
            break;
        }
        case 0xBF: {
            OpMnemonics::CP<AddressingMnemonics::A>();
            break;
        }
        case 0xC0: {
            OpMnemonics::RET<FlagMnemonics::NZ>();
            break;
        }
        case 0xC1: {
            OpMnemonics::POP<AddressingMnemonics::BC>();
            break;
        }
        case 0xC2: {
            OpMnemonics::JP<FlagMnemonics::NZ, AddressingMnemonics::nn>();
            break;
        }
        case 0xC3: {
            OpMnemonics::JP<AddressingMnemonics::nn>();
            break;
        }
        case 0xC4: {
            OpMnemonics::CALL<FlagMnemonics::NZ, AddressingMnemonics::nn>();
            break;
        }
        case 0xC5: {
            OpMnemonics::PUSH<AddressingMnemonics::BC>();
            break;
        }
        case 0xC6: {
            OpMnemonics::ADD<AddressingMnemonics::n>();
            break;
        }
        case 0xC7: {
            OpMnemonics::RST<0x0>();
            break;
        }
        case 0xC8: {
            OpMnemonics::RET<FlagMnemonics::Z>();
            break;
        }
        case 0xC9: {
            OpMnemonics::RET();
            break;
        }
        case 0xCA: {
            OpMnemonics::JP<FlagMnemonics::Z, AddressingMnemonics::nn>();
            break;
        }
        case 0xCB: {
            extendedOpBITS();
            break;
        }
        case 0xCC: {
            OpMnemonics::CALL<FlagMnemonics::Z, AddressingMnemonics::nn>();
            break;
        }
        case 0xCD: {
            OpMnemonics::CALL<AddressingMnemonics::nn>();
            break;
        }
        case 0xCE: {
            OpMnemonics::ADC<AddressingMnemonics::A, AddressingMnemonics::n>();
            break;
        }
        case 0xCF: {
            OpMnemonics::RST<0x8>();
            break;
        }
        case 0xD0: {
            OpMnemonics::RET<FlagMnemonics::NC>();
            break;
        }
        case 0xD1: {
            OpMnemonics::POP<AddressingMnemonics::DE>();
            break;
        }
        case 0xD2: {
            OpMnemonics::JP<FlagMnemonics::NC, AddressingMnemonics::nn>();
            break;
        }
        case 0xD3: {
            OpMnemonics::OUT<AddressingMnemonics::_n_, AddressingMnemonics::A>();
            break;
        }
        case 0xD4: {
            OpMnemonics::CALL<FlagMnemonics::NC, AddressingMnemonics::nn>();
            break;
        }
        case 0xD5: {
            OpMnemonics::PUSH<AddressingMnemonics::DE>();
            break;
        }
        case 0xD6: {
            OpMnemonics::SUB<AddressingMnemonics::n>();
            break;
        }
        case 0xD7: {
            OpMnemonics::RST<0x10>();
            break;
        }
        case 0xD8: {
            OpMnemonics::RET<FlagMnemonics::C>();
            break;
        }
        case 0xD9: {
            Operations::EXX();
            break;
        }
        case 0xDA: {
            OpMnemonics::JP<FlagMnemonics::C, AddressingMnemonics::nn>();
            break;
        }
        case 0xDB: {
            OpMnemonics::INA<AddressingMnemonics::_n_>();
            break;
        }
        case 0xDC: {
            OpMnemonics::CALL<FlagMnemonics::C, AddressingMnemonics::nn>();
            break;
        }
        case 0xDD:{
            extendedOpIX();
            break;
        }
        case 0xDE: {
            OpMnemonics::SBC<AddressingMnemonics::n>();
            break;
        }
        case 0xDF: {
            OpMnemonics::RST<0x18>();
            break;
        }
        case 0xE0: {
            OpMnemonics::RET<FlagMnemonics::PO>();
            break;
        }
        case 0xE1: {
            OpMnemonics::POP<AddressingMnemonics::HL>();
            break;
        }
        case 0xE2: {
            OpMnemonics::JP<FlagMnemonics::PO, AddressingMnemonics::nn>();
            break;
        }
        case 0xE3: {
            OpMnemonics::EX<HL, SP>();
            break;
        }
        case 0xE4: {
            OpMnemonics::CALL<FlagMnemonics::PO, AddressingMnemonics::nn>();
            break;
        }
        case 0xE5: {
            OpMnemonics::PUSH<AddressingMnemonics::HL>();
            break;
        }
        case 0xE6: {
            OpMnemonics::AND<AddressingMnemonics::n>();
            break;
        }
        case 0xE7: {
            OpMnemonics::RST<0x20>();
            break;
        }
        case 0xE8: {
            OpMnemonics::RET<FlagMnemonics::PE>();
            break;
        }
        case 0xE9: {
            OpMnemonics::JP<AddressingMnemonics::HL>();
            break;
        }
        case 0xEA: {
            OpMnemonics::JP<FlagMnemonics::PE, AddressingMnemonics::nn>();
            break;
        }
        case 0xEB: {
            Operations::EX<DE, HL>();
            break;
        }
        case 0xEC: {
            OpMnemonics::CALL<FlagMnemonics::PE, AddressingMnemonics::nn>();
            break;
        }
        case 0xED: {
            extendedOpEXTD();
            break;
        }
        case 0xEE: {
            OpMnemonics::XOR<AddressingMnemonics::n>();
            break;
        }
        case 0xEF: {
            OpMnemonics::RST<0x28>();
            break;
        }
        case 0xF0: {
            OpMnemonics::RET<FlagMnemonics::P>();
            break;
        }
        case 0xF1: {
            OpMnemonics::POP<AddressingMnemonics::AF>();
            break;
        }
        case 0xF2: {
            OpMnemonics::JP<FlagMnemonics::P, AddressingMnemonics::nn>();
            break;
        }
        case 0xF3: {
            // DI
            OpMnemonics::DI();
            break;
        }
        case 0xF4: {
            OpMnemonics::CALL<FlagMnemonics::P, AddressingMnemonics::nn>();
            break;
        }
        case 0xF5: {
            OpMnemonics::PUSH<AddressingMnemonics::AF>();
            break;
        }
        case 0xF6: {
            OpMnemonics::OR<AddressingMnemonics::n>();
            break;
        }
        case 0xF7: {
            OpMnemonics::RST<0x30>();
            break;
        }
        case 0xF8: {
            OpMnemonics::RET<FlagMnemonics::M>();
            break;
        }
        case 0xF9: {
            OpMnemonics::LD<AddressingMnemonics::SP, AddressingMnemonics::HL>();
            break;
        }
        case 0xFA: {
            OpMnemonics::JP<FlagMnemonics::M, AddressingMnemonics::nn>();
            break;
        }
        case 0xFB: {
            OpMnemonics::EI();
            break;
        }
        case 0xFC: {
            OpMnemonics::CALL<FlagMnemonics::M, AddressingMnemonics::nn>();
            break;
        }
        case 0xFD:{
            extendedOpIY();
            break;
        }
        case 0xFE: {
            OpMnemonics::CP<AddressingMnemonics::n>();
            break;
        }
        case 0xFF: {
            OpMnemonics::RST<0x38>();
            break;
        }
    }
}

static inline void debugPrintState() {
    static uint64_t opCount = 0;

    std::cout << std::setfill( '0' ) << std::setw( 4 )
        << ++opCount
        << " PC:" << std::setfill( '0' ) << std::setw( 4 ) << std::hex << PC
        << " SP: " << std::setfill( '0' ) << std::setw( 4 ) << std::hex << SP
        << " IX: " << std::setfill( '0' ) << std::setw( 4 ) << std::hex << IX
        << " IY: " << std::setfill( '0' ) << std::setw( 4 ) << std::hex << IY
        << " I: " << std::setfill( '0' ) << std::setw( 4 ) << std::hex << I
        << " R: " << std::setfill( '0' ) << std::setw( 4 ) << std::hex << R
        << " AF: " << A.toString()
        << " BC: " << BC.toString()
        << " DE: " << DE.toString()
        << " HL: " << HL.toString()
        << " <--> " 
            << std::setw( 2 ) << std::hex << static_cast<uint16_t>( MemoryAccess::Read<uint8_t>( PC ) ) << " "
            << std::setw( 2 ) << std::hex << static_cast<uint16_t>( MemoryAccess::Read<uint8_t>( PC + 1 ) ) << " "
            << std::setw( 2 ) << std::hex << static_cast<uint16_t>( MemoryAccess::Read<uint8_t>( PC + 2 ) ) << " "
            << std::setw( 2 ) << std::hex << static_cast<uint16_t>( MemoryAccess::Read<uint8_t>( PC + 3 ) )
        << std::endl;

    // printf( "PC:%04X AF:{%02X,%02X} BC:{%02X,%02X} DE:{%02X,%02X} HL:{%02X,%02X}\n",
    //     PC,
    //     A.mainRegister.highByte(), A.mainRegister.lowByte(),
    //     BC.mainRegister.highByte(), BC.mainRegister.lowByte(),
    //     DE.mainRegister.highByte(), DE.mainRegister.lowByte(),
    //     HL.mainRegister.highByte(), HL.mainRegister.lowByte() );
        
}

void acceptInterrupt() {
    assert( InterruptMode < 3 );
    switch( InterruptMode ) {
        case 0x00: {
            std::cout << "TODO - IRQ Mode 0\n";
            break;
        }
        case 0x01: {
            OpMnemonics::RST<0x38>();
            break;
        }
        case 0x02: {
            std::cout << "TODO - IRQ Mode 2\n";
            break;
        }
    }
    IFF1 = IFF2 = false;
}

void Z80::Tick() {
    static bool debugPrint = false;
    if ( PC == 0x82FC ) {
//        std::cout << "beep\n";
//        debugPrint = true;
    }

    if ( debugPrint ) {
        debugPrintState();
    }
    nextPC = PC;
    operationTick();

    if ( IFF1 & !IRQPinLevel ) { // TODO - unlikely
        acceptInterrupt();
    }
    
    PC = nextPC;
}

void Z80::IRQ( bool level ) {
    IRQPinLevel = level;
}