#include "z80Core.h"
#include "System.h"

#include <bit>
#include <functional>
#include <iostream>
#include <stdint.h>

#pragma region Concepts
template<typename T>
concept IsByteOrWord = std::is_same<T, uint8_t>::value || std::is_same<T, uint16_t>::value || std::is_same<T, bool>::value || std::is_same<T, int8_t>::value;

template<typename T, typename ...args>
concept ReturnsByteOrWord = IsByteOrWord<std::invoke_result_t<T, args...>>;

template<typename T>
concept WritesByteOrWord = std::is_same<T, void(*)(uint8_t)>::value || std::is_same<T, void(*)(uint16_t)>::value;

template<typename opType, typename writeType, typename ...readTypes>
concept WriteMatchesOp = true;// std::is_same<writeType, void(*)(std::invoke_result_t<opType, readTypes...>)>::value;

template<typename opType>
concept IsInvocableOperationNoParamNoRet = std::is_same<opType, void(*)()>::value;

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

template <auto operation, auto readO1, auto readO2>
    requires IsDualParamNoWriteoutOp<operation, readO1, readO2>
void OperationBase() {
    operation( readO1(), readO2() );
}

template <auto operation, auto write, auto readO1>
    requires IsSingleParamWriteoutOp<operation, write, readO1>
void OperationBase() {
    write( operation( readO1() ) );
}

template <auto operation, auto write, auto readO1, auto readO2>
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
static uint16_t I;
static uint16_t R;

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



// Read 8/16 bit value from address given by AddressingMode()
template<uint8_t(AddressingMode)()>
static inline uint8_t PortRead() {
    uint8_t data;
    System::IOAccess( AddressingMode(), data, false );
    return data;
}

// Write 8/16 bit value to address given by AddressingMode()
template<uint8_t(AddressingMode)()>
static inline void PortWrite( uint8_t data ) {
    System::IOAccess( AddressingMode(), data, true );
}

// Read 8/16 bit value from address given by BC
template <RegisterSet &registerSet = BC>
static inline uint8_t PortRead() {
    return PortRead<RegisterRead<registerSet, false>>();
}

// Write 8/16 bit value to address given by register word
template <RegisterSet &registerSet = BC>
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

enum class AddressingModeMnemonics {
    _A,
    _F,
    _B,
    _C,
    _D,
    _E,
    _H,
    _L,
    _IXl,
    _IXh,
    _IYl,
    _IYh,
    _SPl,
    _SPh,
    _AF,
    _BC,
    _DE,
    _HL,
    _IX,
    _IY,
    _SP,
    _n,
    _nn,
    _r,   // TODO - get actual mnemonic, might be 'l'
    __BC_,
    __DE_,
    __HL_,
    __nn_
};

constexpr bool IsRegister8BitAddressingMode( AddressingModeMnemonics mnemonic ) {
    return 
        mnemonic == AddressingModeMnemonics::_A ||
        mnemonic == AddressingModeMnemonics::_F ||
        mnemonic == AddressingModeMnemonics::_B ||
        mnemonic == AddressingModeMnemonics::_C ||
        mnemonic == AddressingModeMnemonics::_D ||
        mnemonic == AddressingModeMnemonics::_E ||
        mnemonic == AddressingModeMnemonics::_H ||
        mnemonic == AddressingModeMnemonics::_L ||
        mnemonic == AddressingModeMnemonics::_IXl ||
        mnemonic == AddressingModeMnemonics::_IXh ||
        mnemonic == AddressingModeMnemonics::_IYl ||
        mnemonic == AddressingModeMnemonics::_IYh ||
        mnemonic == AddressingModeMnemonics::_SPl ||
        mnemonic == AddressingModeMnemonics::_SPh;
}

constexpr bool IsRegister16BitAddressingMode( AddressingModeMnemonics mnemonic ) {
    return 
        mnemonic == AddressingModeMnemonics::_AF ||
        mnemonic == AddressingModeMnemonics::_BC ||
        mnemonic == AddressingModeMnemonics::_DE ||
        mnemonic == AddressingModeMnemonics::_HL ||
        mnemonic == AddressingModeMnemonics::_IX ||
        mnemonic == AddressingModeMnemonics::_IY ||
        mnemonic == AddressingModeMnemonics::_SP;
}

constexpr bool IsImmediateAddressingMode( AddressingModeMnemonics mnemonic ) {
    return 
        mnemonic == AddressingModeMnemonics::_n ||
        mnemonic == AddressingModeMnemonics::_nn ||
        mnemonic == AddressingModeMnemonics::_r;
}

constexpr bool IsMemoryAddressingMode( AddressingModeMnemonics mnemonic ) {
    return 
        mnemonic == AddressingModeMnemonics::__BC_ ||
        mnemonic == AddressingModeMnemonics::__DE_ ||
        mnemonic == AddressingModeMnemonics::__HL_ ||
        mnemonic == AddressingModeMnemonics::__nn_;
}

using ReadMode8BitRegister = uint8_t(*)();
using ReadMode16BitRegister = uint16_t(*)();

using WriteMode8BitRegister = void(*)(uint8_t);
using WriteMode16BitRegister = void(*)(uint16_t);

template <AddressingModeMnemonics mnemonic>
    requires( IsRegister8BitAddressingMode( mnemonic ) )
constexpr ReadMode8BitRegister GetReadAddressingMode() {
    if constexpr( mnemonic == AddressingModeMnemonics::_A ) { return AddressingModes::RegisterRead<A, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_F ) { return AddressingModes::RegisterRead<A, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_B ) { return AddressingModes::RegisterRead<BC, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_C ) { return AddressingModes::RegisterRead<BC, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_D ) { return AddressingModes::RegisterRead<DE, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_E ) { return AddressingModes::RegisterRead<DE, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_H ) { return AddressingModes::RegisterRead<HL, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_L ) { return AddressingModes::RegisterRead<HL, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IXh ) { return AddressingModes::RegisterRead<IX, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IXl ) { return AddressingModes::RegisterRead<IX, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IYh ) { return AddressingModes::RegisterRead<IY, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IYl ) { return AddressingModes::RegisterRead<IY, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_SPh ) { return AddressingModes::RegisterRead<SP, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_SPl ) { return AddressingModes::RegisterRead<SP, false>; }
    else {}
}
template <AddressingModeMnemonics mnemonic>
    requires( IsRegister8BitAddressingMode( mnemonic ) )
constexpr WriteMode8BitRegister GetWriteAddressingMode() {
    if constexpr( mnemonic == AddressingModeMnemonics::_A ) { return AddressingModes::RegisterWrite<A, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_F ) { return AddressingModes::RegisterWrite<A, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_B ) { return AddressingModes::RegisterWrite<BC, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_C ) { return AddressingModes::RegisterWrite<BC, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_D ) { return AddressingModes::RegisterWrite<DE, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_E ) { return AddressingModes::RegisterWrite<DE, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_H ) { return AddressingModes::RegisterWrite<HL, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_L ) { return AddressingModes::RegisterWrite<HL, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IXh ) { return AddressingModes::RegisterWrite<IX, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IXl ) { return AddressingModes::RegisterWrite<IX, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IYh ) { return AddressingModes::RegisterWrite<IY, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IYl ) { return AddressingModes::RegisterWrite<IY, false>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_SPh ) { return AddressingModes::RegisterWrite<SP, true>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_SPl ) { return AddressingModes::RegisterWrite<SP, false>; }
    else {}
}

template <AddressingModeMnemonics mnemonic>
    requires( IsRegister16BitAddressingMode( mnemonic ) )
constexpr ReadMode16BitRegister GetAddressingMode2() {
    if constexpr( mnemonic == AddressingModeMnemonics::_AF ) { return AddressingModes::RegisterRead<A>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_BC ) { return AddressingModes::RegisterRead<BC>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_DE ) { return AddressingModes::RegisterRead<DE>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_HL ) { return AddressingModes::RegisterRead<HL>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IX ) { return AddressingModes::RegisterRead<IX>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IY ) { return AddressingModes::RegisterRead<IY>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_SP ) { return AddressingModes::RegisterRead<SP>; }
    else {}
}
template <AddressingModeMnemonics mnemonic>
    requires( IsRegister16BitAddressingMode( mnemonic ) )
constexpr WriteMode16BitRegister GetWriteAddressingMode2() {
    if constexpr( mnemonic == AddressingModeMnemonics::_AF ) { return AddressingModes::RegisterWrite<A>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_BC ) { return AddressingModes::RegisterWrite<BC>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_DE ) { return AddressingModes::RegisterWrite<DE>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_HL ) { return AddressingModes::RegisterWrite<HL>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IX ) { return AddressingModes::RegisterWrite<IX>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_IY ) { return AddressingModes::RegisterWrite<IY>; }
    else if constexpr( mnemonic == AddressingModeMnemonics::_SP ) { return AddressingModes::RegisterWrite<SP>; }
    else {}
}

namespace AddressingMnemonics {
void A( uint8_t val ) { AddressingModes::RegisterWrite<::A, true>( val ); }
uint8_t A() { return AddressingModes::RegisterRead<::A, true>(); }
void B( uint8_t val ) { AddressingModes::RegisterWrite<::BC, true>( val ); }
uint8_t B() { return AddressingModes::RegisterRead<::BC, true>(); }
void C( uint8_t val ) { AddressingModes::RegisterWrite<::BC, false>( val ); }
uint8_t C() { return AddressingModes::RegisterRead<::BC, false>(); }
void D( uint8_t val ) { AddressingModes::RegisterWrite<::DE, true>( val ); }
uint8_t D() { return AddressingModes::RegisterRead<::DE, true>(); }
void E( uint8_t val ) { AddressingModes::RegisterWrite<::DE, false>( val ); }
uint8_t E() { return AddressingModes::RegisterRead<::DE, false>(); }
void H( uint8_t val ) { AddressingModes::RegisterWrite<::HL, true>( val ); }
uint8_t H() { return AddressingModes::RegisterRead<::HL, true>(); }
void L( uint8_t val ) { AddressingModes::RegisterWrite<::HL, false>( val ); }
uint8_t L() { return AddressingModes::RegisterRead<::HL, false>(); }
void IXH( uint8_t val ) { AddressingModes::RegisterWrite<::IX, true>( val ); }
uint8_t IXH() { return AddressingModes::RegisterRead<::IX, true>(); }
void IXL( uint8_t val ) { AddressingModes::RegisterWrite<::IX, false>( val ); }
uint8_t IXL() { return AddressingModes::RegisterRead<::IX, false>(); }


void AF( uint16_t val ) { AddressingModes::RegisterWrite<::A>( val ); }
uint16_t AF() { return AddressingModes::RegisterRead<::A>(); }
void BC( uint16_t val ) { AddressingModes::RegisterWrite<::BC>( val ); }
uint16_t BC() { return AddressingModes::RegisterRead<::BC>(); }
void DE( uint16_t val ) { AddressingModes::RegisterWrite<::DE>( val ); }
uint16_t DE() { return AddressingModes::RegisterRead<::DE>(); }
void HL( uint16_t val ) { AddressingModes::RegisterWrite<::HL>( val ); }
uint16_t HL() { return AddressingModes::RegisterRead<::HL>(); }

}

enum class OpTypes {
    LDA
};
template<WriteMode8BitRegister WriteMode, ReadMode8BitRegister ReadMode> 
static inline void LDA_Op2() {
    WriteMode( ReadMode() );
}

template<AddressingModeMnemonics WriteMnemonic, AddressingModeMnemonics ReadMnemonic>
static inline void LDA_Op() {
    LDA_Op2<GetWriteAddressingMode<WriteMnemonic>, GetReadAddressingMode<ReadMnemonic>>();
}

template <OpTypes OpType, auto ...args>
static inline void Operation() {
    LDA_Op2<args...>();
}

static inline void test() {
    //Operation<OpTypes::LDA, t::_A, t::_B>();
    LDA_Op2< t::A, t::B >();
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
template<IsByteOrWord T>
static inline T LD( T O1 ) {
    return O1;
}
static inline uint8_t LD8( uint8_t O1 ) {
    return LD<uint8_t>( O1 );
}
static inline uint16_t LD16( uint16_t O1 ) {
    return LD<uint16_t>( O1 );
}

template<uint16_t &reg1, uint16_t &reg2>
static inline void OperationLD() {
    OperationBase<LD16, AddressingModes::RegisterWrite<reg1>, AddressingModes::RegisterRead<reg2>>();
}
template<uint16_t &reg, auto ReadMode>
static inline void OperationLD() {
    OperationBase<LD16, AddressingModes::RegisterWrite<reg>, ReadMode>();
}
template<auto WriteMode, uint16_t &reg>
static inline void OperationLD() {
    OperationBase<LD16, WriteMode, AddressingModes::RegisterRead<reg>>();
}
template<auto WriteMode, auto ReadMode>
    requires std::is_same< std::invoke_result_t<decltype(ReadMode)>, uint8_t>::value
static inline void OperationLD() {
    OperationBase<LD8, WriteMode, ReadMode>();
}
template<auto WriteMode, auto ReadMode>
    requires std::is_same< std::invoke_result_t<decltype(ReadMode)>, uint16_t>::value
static inline void OperationLD() {
    OperationBase<LD16, WriteMode, ReadMode>();
}

static inline uint8_t POP8() {
    return MemoryAccess::Read<uint8_t>( SP++ );
}
static inline uint16_t POP16() {
    uint16_t value = POP8();
    return value | static_cast<uint16_t>( POP8() ) << 8;
}
template<RegisterSet &registerSet>
static inline void OperationPop() {
    OperationBase<POP16, AddressingModes::RegisterWrite<registerSet>>();
}
template<uint16_t &reg>
static inline void OperationPOP() {
    OperationBase<POP16, AddressingModes::RegisterWrite<reg>>();
}

static inline void PUSH8( uint8_t data ) {
    MemoryAccess::Write( --SP, data );
}
static inline void PUSH16( uint16_t data ) {
    PUSH8( static_cast<uint8_t>( ( data >> 8 ) & 0x00FF ) );
    PUSH8( static_cast<uint8_t>( data & 0x00FF ) );
}
template<RegisterSet &registerSet>
static inline void OperationPush() {
    OperationBase<PUSH16, AddressingModes::RegisterRead<registerSet>>();
}
template<uint16_t &reg>
static inline void OperationPUSH() {
    OperationBase<PUSH16, AddressingModes::RegisterRead<reg>>();
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
template<auto WriteMode, auto ReadMode, uint16_t &reg>
static inline void OperationEX() {
    OperationBase<EX_mem<reg>, WriteMode, ReadMode>();
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
static inline T ADD( T O1, T O2 ) {
    T result = O1 + O2;
    
    AddSubFlag = false;

    if constexpr ( std::is_same<T, uint8_t>::value ) {
        ZeroFlag = result == 0;
        SignFlag = result & 0x80;
        CarryFlag = O1 > 0xFF - O2;
        HalfCarryFlag = ( ( O1 & 0x0F ) + ( O2 & 0x0F ) ) > 0x0F;
        ParityOverflowFlag = (~( O1 ^ O2 ) & ( O1 ^ result ) ) & 0x80;
    }
    else {
        CarryFlag = O1 > 0xFFFF - O2;
        HalfCarryFlag = ( ( O1 & 0x0FFF ) + ( O2 & 0x0FFF ) ) > 0x0FFF;
    }
    
    return result;
}
static inline uint8_t ADD8( uint8_t O1, uint8_t O2 ) {
    return ADD<uint8_t>( O1, O2 );
}
static inline uint16_t ADD16( uint16_t O1, uint16_t O2 ) {
    return ADD<uint16_t>( O1, O2 );
}
template<uint16_t &reg, RegisterSet &registerSet>
static inline void OperationADD() {
    OperationBase<ADD16, AddressingModes::RegisterWrite<reg>, AddressingModes::RegisterRead<reg>, AddressingModes::RegisterRead<registerSet>>();
}
template<uint16_t &reg1, uint16_t &reg2>
static inline void OperationADD() {
    OperationBase<ADD16, AddressingModes::RegisterWrite<reg1>, AddressingModes::RegisterRead<reg1>, AddressingModes::RegisterRead<reg2>>();
}
template<auto ReadMode>
static inline void OperationADD() {
    OperationBase<ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, ReadMode>();
}

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
template<auto ReadMode>
static inline void OperationADC() {
    OperationBase<ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, ReadMode>();
}

static inline uint8_t SUB( uint8_t O1, uint8_t O2 ) {
    // TODO - verify this
    uint8_t negO2 = ( ~O2 ) + 1u;
    uint8_t result = ADD<uint8_t>( O1, negO2 );
    CarryFlag.Invert();
    HalfCarryFlag.Invert();
    AddSubFlag = true;
    return result;
}
template<auto ReadMode>
static inline void OperationSUB() {
    OperationBase<SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, ReadMode>();
}

template<IsByteOrWord T>
static inline T SBC( T O1, T O2 ) {
    // TODO - verify
    CarryFlag.Invert();
    T negO2 = ~O2 + 1u;

    T result = ADC<T>( O1, negO2 );

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
template<auto ReadMode>
static inline void OperationSBC() {
    OperationBase<SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, ReadMode>();
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
    return INC<uint8_t>( O1 );
}
static inline uint16_t INC16( uint16_t O1 ) {
    return INC<uint16_t>( O1 );
}
template<uint16_t &reg>
static inline void OperationINC() {
    OperationBase<INC16, AddressingModes::RegisterWrite<reg>, AddressingModes::RegisterRead<reg>>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationINC() {
    OperationBase<INC8, WriteMode, ReadMode>();
}

template<IsByteOrWord T>
static inline T DEC( T O1 ) {
    T result = O1 - 1;
    if constexpr ( std::is_same<T, uint8_t>::value ) {
        ZeroFlag = result == 0;
        SignFlag = result & 0x80;
        HalfCarryFlag = O1 == 0x10;
        ParityOverflowFlag = O1 == 0x80;
        AddSubFlag = true;
    }
    return result;
}
static inline uint8_t DEC8( uint8_t O1 ) {
    return DEC<uint8_t>( O1 );
}
static inline uint16_t DEC16( uint16_t O1 ) {
    return DEC<uint16_t>( O1 );
}
template<uint16_t &reg>
static inline void OperationDEC() {
    OperationBase<DEC16, AddressingModes::RegisterWrite<reg>, AddressingModes::RegisterRead<reg>>();
}
template<auto WriteMode, auto ReadMode>
static inline void OperationDEC() {
    OperationBase<DEC8, WriteMode, ReadMode>();
}

static inline uint8_t AND( uint8_t O1, uint8_t O2 ) {
    uint8_t result = O1 & O2;

    SignFlag = result & 0x80;
    ZeroFlag = result == 0;
    HalfCarryFlag = true;
    AddSubFlag = false;
    CarryFlag = false;
    // TODO - ParityOverflowFlag;
    
    return result;
}
template<auto ReadMode>
static inline void OperationAND() {
    OperationBase<AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, ReadMode>();
}

static inline uint8_t OR( uint8_t O1, uint8_t O2 ) {
    uint8_t result = O1 | O2;

    SignFlag = result & 0x80;
    ZeroFlag = result == 0;
    HalfCarryFlag = true;
    AddSubFlag = false;
    CarryFlag = false;
    // TODO - ParityOverflowFlag;
    
    return result;
}
template<auto ReadMode>
static inline void OperationOR() {
    OperationBase<OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, ReadMode>();
}

static inline uint8_t XOR( uint8_t O1, uint8_t O2 ) {
    uint8_t result = O1 ^ O2;

    SignFlag = result & 0x80;
    ZeroFlag = result == 0;
    HalfCarryFlag = false;
    AddSubFlag = false;
    CarryFlag = false;
    // TODO - ParityOverflowFlag;
    
    return result;
}
template<auto ReadMode>
static inline void OperationXOR() {
    OperationBase<XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, ReadMode>();
}

static inline void CP( uint8_t O1, uint8_t O2 ) {
    // TODO - verify
    SUB( O1, O2 );
}
template<auto ReadMode>
static inline void OperationCP() {
    OperationBase<CP, AddressingModes::RegisterRead<A, true>, ReadMode>();
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
static inline void OperationNEG() {
    OperationBase<NEG, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
}

static inline uint8_t CCF( uint8_t O1 ) {
    AddSubFlag = false;
    return O1 ^ ( 1u << static_cast<uint8_t>( Flags::Carry ) );
}

static inline uint8_t SCF( uint8_t O1 ) {
    AddSubFlag = false;
    HalfCarryFlag = false;
    return O1 | ( 1 << static_cast<uint8_t>( Flags::Carry ) );
}

#pragma endregion

/**********************************
 *  Block Transfer & Search
 *********************************/
#pragma region BlockTransferAndSearch
template<bool increment, bool repeat>
static inline uint8_t LDN( uint8_t O1 ) {
    --BC.mainRegister.word;
    if constexpr ( increment ) {
        ++DE.mainRegister.word;
        ++HL.mainRegister.word;
    }
    else {
        --DE.mainRegister.word;
        --HL.mainRegister.word;
    }

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
template<bool increment, bool repeat>
static inline void OperationLDN() {
    OperationBase<LDN<increment, repeat>, AddressingModes::AddressWrite<DE, uint8_t>, AddressingModes::AddressRead<HL, uint8_t>>();
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
    return ( O1 << 1 ) | ( carrySet ? 0x01 : 0x00 );
}

static inline uint8_t RLCA( uint8_t O1 ) {
    return RLC( O1 );
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
    return ( O1 >> 1 ) | ( carrySet ? 0x80 : 0x00 );
}
static inline uint8_t RRCA( uint8_t O1 ) {
    return RRC( O1 );
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
    return ( O1 << 1 ) | prevCarryAdd;
}
static inline uint8_t RLA( uint8_t O1 ) {
    return RL( O1 );
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
    return ( O1 >> 1 ) | prevCarryAdd;
}
static inline uint8_t RRA( uint8_t O1 ) {
    return RR( O1 );
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
    // TODO - parity
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
    // TODO - parity
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
    // TODO - parity
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

    uint8_t result = ( O1 >> 1 ) & 0x7F;
    SignFlag = false;
    ZeroFlag = result == 0;
    HalfCarryFlag = false;
    // TODO - parity
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
    // TODO - parity
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
    // TODO - parity
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
    HalfCarryFlag = true;
    ZeroFlag = O1 & ( 1u << BitPos );
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
static inline void JR( int8_t O1, bool condition ) {
    if ( condition ) {
        nextPC = nextPC + O1;
    }
}
static inline void JR1( int8_t O1 ) {
    JR( O1, true );
}
static inline void DJNZ( int8_t O1 ) {
    uint8_t B = AddressingModes::RegisterRead<BC, true>() - 1u;
    AddressingModes::RegisterWrite<BC, true>( B );
    JR( O1, B != 0 );
}

static inline void JP( uint16_t O1, bool condition ) {
    if ( condition ) {
        nextPC = O1;
    }
}
static inline void JP1( uint16_t O1 ) {
    JP( O1, true );
}
template<auto ConditionOp>
    requires std::is_same<std::invoke_result_t<decltype(ConditionOp)>, bool>::value
static inline void OperationJP() {
    OperationBase<JP, AddressingModes::ImmediateExtended, ConditionOp>();
}
static inline void OperationJP() {
    OperationBase<JP1, AddressingModes::ImmediateExtended>();
}
template<uint16_t &reg>
static inline void OperationJP() {
    OperationBase<JP1, AddressingModes::RegisterRead<reg>>();
}

static inline void CALL( uint16_t addr, bool condition ) {
    if( condition ) {
        PUSH16( nextPC );
        nextPC = addr;
    }
}
static inline void CALL1( uint16_t addr ) {
    return CALL( addr, true );
}
template<auto ConditionOp>
    requires std::is_same<std::invoke_result_t<decltype(ConditionOp)>, bool>::value
static inline void OperationCall() {
    OperationBase<CALL, AddressingModes::ImmediateExtended, ConditionOp>();
}
static inline void OperationCall() {
    OperationBase<CALL1, AddressingModes::ImmediateExtended>();
}

static inline void RET( bool condition ) {
    if ( condition ) {
        nextPC = POP16();
    }
}
static inline void RET1() {
    RET( true );
}

static inline void RETN() {
    IFF1 = IFF2;
    nextPC = POP16();
}
static inline void OperationRETN() {
    OperationBase<RETN>();
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
template<uint16_t resetVector>
static inline void OperationRst() {
    OperationBase<RST<resetVector>>();
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
    // TODO - parity
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
static inline uint8_t INNOUTN8( uint8_t O1 ) {
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
            ZeroFlag = BC.mainRegister.highByte() == 0;
        }
    }
    else {
        ZeroFlag = BC.mainRegister.highByte() == 0;
    }
    return O1;
}
template<bool increment, bool repeat>
static inline void OperationINN8() {
    OperationBase<INNOUTN8<increment, repeat>, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::PortRead<BC>>();
}
template<bool increment, bool repeat>
static inline void OperationOUTN8() {
    OperationBase<INNOUTN8<increment, repeat>, AddressingModes::PortWrite<BC>, AddressingModes::AddressRead<HL, uint8_t>>();
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

template<bool enable>
static inline void EI_DI() {
    IFF1 = IFF2 = enable;
}

template<uint8_t IMValue>
static inline void IM() {
    InterruptMode = IMValue;
}
template<uint8_t IMValue>
static inline void OperationIM() {
    OperationBase<IM<IMValue>>();
}

#pragma endregion

}

static inline void extendedOpBITS() {
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC++ );

    switch( opcode ) {
        case 0x00:{
            // TODO - RLC B	8	2	2
            Operations::OperationRLC<BC, true>();
            break;
        }
        case 0x01:{
            // TODO - RLC C	8	2	2
            Operations::OperationRLC<BC, false>();
            break;
        }
        case 0x02:{
            // TODO - RLC D	8	2	2
            Operations::OperationRLC<DE, true>();
            break;
        }
        case 0x03:{
            // TODO - RLC E	8	2	2
            Operations::OperationRLC<DE, false>();
            break;
        }
        case 0x04:{
            // TODO - RLC H	8	2	2
            Operations::OperationRLC<HL, true>();
            break;
        }
        case 0x05:{
            // TODO - RLC L	8	2	2
            Operations::OperationRLC<HL, false>();
            break;
        }
        case 0x06:{
            // TODO - RLC (HL)	15	4	2
            Operations::OperationRLC<HL>();
            break;
        }
        case 0x07:{
            // TODO - RLC A	8	2	2
            Operations::OperationRLC<A, true>();
            break;
        }
        case 0x08:{
            // TODO - RRC B	8	2	2
            Operations::OperationRRC<BC, true>();
            break;
        }
        case 0x09:{
            // TODO - RRC C	8	2	2
            Operations::OperationRRC<BC, false>();
            break;
        }
        case 0x0A:{
            // TODO - RRC D	8	2	2
            Operations::OperationRRC<DE, true>();
            break;
        }
        case 0x0B:{
            // TODO - RRC E	8	2	2
            Operations::OperationRRC<DE, false>();
            break;
        }
        case 0x0C:{
            // TODO - RRC H	8	2	2
            Operations::OperationRRC<HL, true>();
            break;
        }
        case 0x0D:{
            // TODO - RRC L	8	2	2
            Operations::OperationRRC<HL, false>();
            break;
        }
        case 0x0E:{
            // TODO - RRC (HL)	15	4	2
            Operations::OperationRRC<HL>();
            break;
        }
        case 0x0F:{
            // TODO - RRC A	8	2	2
            Operations::OperationRRC<A, true>();
            break;
        }
        case 0x10:{
            // TODO - RL B	8	2	2
            Operations::OperationRL<BC, true>();
            break;
        }
        case 0x11:{
            // TODO - RL C	8	2	2
            Operations::OperationRL<BC, false>();
            break;
        }
        case 0x12:{
            // TODO - RL D	8	2	2
            Operations::OperationRL<DE, true>();
            break;
        }
        case 0x13:{
            // TODO - RL E	8	2	2
            Operations::OperationRL<DE, false>();
            break;
        }
        case 0x14:{
            // TODO - RL H	8	2	2
            Operations::OperationRL<HL, true>();
            break;
        }
        case 0x15:{
            // TODO - RL L	8	2	2
            Operations::OperationRL<HL, false>();
            break;
        }
        case 0x16:{
            // TODO - RL (HL)	15	4	2
            Operations::OperationRL<HL>();
            break;
        }
        case 0x17:{
            // TODO - RL A	8	2	2
            Operations::OperationRL<A, true>();
            break;
        }
        case 0x18:{
            // TODO - RR B	8	2	2
            Operations::OperationRR<BC, true>();
            break;
        }
        case 0x19:{
            // TODO - RR C	8	2	2
            Operations::OperationRR<BC, false>();
            break;
        }
        case 0x1A:{
            // TODO - RR D	8	2	2
            Operations::OperationRR<DE, true>();
            break;
        }
        case 0x1B:{
            // TODO - RR E	8	2	2
            Operations::OperationRR<DE, false>();
            break;
        }
        case 0x1C:{
            // TODO - RR H	8	2	2
            Operations::OperationRR<HL, true>();
            break;
        }
        case 0x1D:{
            // TODO - RR L	8	2	2
            Operations::OperationRR<HL, false>();
            break;
        }
        case 0x1E:{
            // TODO - RR (HL)	15	4	2
            Operations::OperationRR<HL>();
            break;
        }
        case 0x1F:{
            // TODO - RR A	8	2	2
            Operations::OperationRR<A, true>();
            break;
        }
        case 0x20:{
            // TODO - SLA B	8	2	2
            Operations::OperationSLA<BC, true>();
            break;
        }
        case 0x21:{
            // TODO - SLA C	8	2	2
            Operations::OperationSLA<BC, false>();
            break;
        }
        case 0x22:{
            // TODO - SLA D	8	2	2
            Operations::OperationSLA<DE, true>();
            break;
        }
        case 0x23:{
            // TODO - SLA E	8	2	2
            Operations::OperationSLA<DE, false>();
            break;
        }
        case 0x24:{
            // TODO - SLA H	8	2	2
            Operations::OperationSLA<HL, true>();
            break;
        }
        case 0x25:{
            // TODO - SLA L	8	2	2
            Operations::OperationSLA<HL, false>();
            break;
        }
        case 0x26:{
            // TODO - SLA (HL)	15	4	2
            Operations::OperationSLA<HL>();
            break;
        }
        case 0x27:{
            // TODO - SLA A	8	2	2
            Operations::OperationSLA<A, true>();
            break;
        }
        case 0x28:{
            // TODO - SRA B	8	2	2
            Operations::OperationSRA<BC, true>();
            break;
        }
        case 0x29:{
            // TODO - SRA C	8	2	2
            Operations::OperationSRA<BC, false>();
            break;
        }
        case 0x2A:{
            // TODO - SRA D	8	2	2
            Operations::OperationSRA<DE, true>();
            break;
        }
        case 0x2B:{
            // TODO - SRA E	8	2	2
            Operations::OperationSRA<DE, false>();
            break;
        }
        case 0x2C:{
            // TODO - SRA H	8	2	2
            Operations::OperationSRA<HL, true>();
            break;
        }
        case 0x2D:{
            // TODO - SRA L	8	2	2
            Operations::OperationSRA<HL, false>();
            break;
        }
        case 0x2E:{
            // TODO - SRA (HL)	15	4	2
            Operations::OperationSRA<HL>();
            break;
        }
        case 0x2F:{
            // TODO - SRA A	8	2	2
            Operations::OperationSRA<A, true>();
            break;
        }
        case 0x30:{
            // TODO - SLL B	8	2	2
            Operations::OperationSLL<BC, true>();
            break;
        }
        case 0x31:{
            // TODO - SLL C	8	2	2
            Operations::OperationSLL<BC, false>();
            break;
        }
        case 0x32:{
            // TODO - SLL D	8	2	2
            Operations::OperationSLL<DE, true>();
            break;
        }
        case 0x33:{
            // TODO - SLL E	8	2	2
            Operations::OperationSLL<DE, false>();
            break;
        }
        case 0x34:{
            // TODO - SLL H	8	2	2
            Operations::OperationSLL<HL, true>();
            break;
        }
        case 0x35:{
            // TODO - SLL L	8	2	2
            Operations::OperationSLL<HL, false>();
            break;
        }
        case 0x36:{
            // TODO - SLL (HL)	15	4	2
            Operations::OperationSLL<HL>();
            break;
        }
        case 0x37:{
            // TODO - SLL A	8	2	2
            Operations::OperationSLL<A, true>();
            break;
        }
        case 0x38:{
            // TODO - SRL B	8	2	2
            Operations::OperationSRL<BC, true>();
            break;
        }
        case 0x39:{
            // TODO - SRL C	8	2	2
            Operations::OperationSRL<BC, false>();
            break;
        }
        case 0x3A:{
            // TODO - SRL D	8	2	2
            Operations::OperationSRL<DE, true>();
            break;
        }
        case 0x3B:{
            // TODO - SRL E	8	2	2
            Operations::OperationSRL<DE, false>();
            break;
        }
        case 0x3C:{
            // TODO - SRL H	8	2	2
            Operations::OperationSRL<HL, true>();
            break;
        }
        case 0x3D:{
            // TODO - SRL L	8	2	2
            Operations::OperationSRL<HL, false>();
            break;
        }
        case 0x3E:{
            // TODO - SRL (HL)	15	4	2
            Operations::OperationSRL<HL>();
            break;
        }
        case 0x3F:{
            // TODO - SRL A	8	2	2
            Operations::OperationSRL<A, true>();
            break;
        }
        case 0x40:{
            // TODO - BIT 0,B	8	2	2
            Operations::OperationBIT<0, BC, true>();
            break;
        }
        case 0x41:{
            // TODO - BIT 0,C	8	2	2
            Operations::OperationBIT<0, BC, false>();
            break;
        }
        case 0x42:{
            // TODO - BIT 0,D	8	2	2
            Operations::OperationBIT<0, DE, true>();
            break;
        }
        case 0x43:{
            // TODO - BIT 0,E	8	2	2
            Operations::OperationBIT<0, DE, false>();
            break;
        }
        case 0x44:{
            // TODO -  	BIT 0,H	8	2	2
            Operations::OperationBIT<0, HL, true>();
            break;
        }
        case 0x45:{
            // TODO -  	BIT 0,L	8	2	2
            Operations::OperationBIT<0, HL, false>();
            break;
        }
        case 0x46:{
            // TODO -  	BIT 0,(HL)	12	3	2
            Operations::OperationBIT<0, HL>();
            break;
        }
        case 0x47:{
            // TODO -  	BIT 0,A	8	2	2
            Operations::OperationBIT<0, A, true>();
            break;
        }
        case 0x48:{
            // TODO - BIT 1,B	8	2	2
            Operations::OperationBIT<1, BC, true>();
            break;
        }
        case 0x49:{
            // TODO -  	BIT 1,C	8	2	2
            Operations::OperationBIT<1, BC, false>();
            break;
        }
        case 0x4A:{
            // TODO -  	BIT 1,D	8	2	2
            Operations::OperationBIT<1, DE, true>();
            break;
        }
        case 0x4B:{
            // TODO -  	BIT 1,E	8	2	2
            Operations::OperationBIT<1, DE, false>();
            break;
        }
        case 0x4C:{
            // TODO -  	BIT 1,H	8	2	2
            Operations::OperationBIT<1, HL, true>();
            break;
        }
        case 0x4D:{
            // TODO -  	BIT 1,L	8	2	2
            Operations::OperationBIT<1, HL, false>();
            break;
        }
        case 0x4E:{
            // TODO -  	BIT 1,(HL)	12	3	2
            Operations::OperationBIT<1, HL>();
            break;
        }
        case 0x4F:{
            // TODO -  	BIT 1,A	8	2	2
            Operations::OperationBIT<1, A, true>();
            break;
        }
        case 0x50:{
            // TODO - BIT 2,B	8	2	2
            Operations::OperationBIT<2, BC, true>();
            break;
        }
        case 0x51:{
            // TODO -  	BIT 2,C	8	2	2
            Operations::OperationBIT<2, BC, false>();
            break;
        }
        case 0x52:{
            // TODO -  	BIT 2,D	8	2	2
            Operations::OperationBIT<2, DE, true>();
            break;
        }
        case 0x53:{
            // TODO -  	BIT 2,E	8	2	2
            Operations::OperationBIT<2, DE, false>();
            break;
        }
        case 0x54:{
            // TODO -  	BIT 2,H	8	2	2
            Operations::OperationBIT<2, HL, true>();
            break;
        }
        case 0x55:{
            // TODO -  	BIT 2,L	8	2	2
            Operations::OperationBIT<2, HL, false>();
            break;
        }
        case 0x56:{
            // TODO -  	BIT 2,(HL)	12	3	2
            Operations::OperationBIT<2, HL>();
            break;
        }
        case 0x57:{
            // TODO -  	BIT 2,A	8	2	2
            Operations::OperationBIT<2, A, true>();
            break;
        }
        case 0x58:{
            // TODO - BIT 3,B	8	2	2
            Operations::OperationBIT<3, BC, true>();
            break;
        }
        case 0x59:{
            // TODO -  	BIT 3,C	8	2	2
            Operations::OperationBIT<3, BC, false>();
            break;
        }
        case 0x5A:{
            // TODO -  	BIT 3,D	8	2	2
            Operations::OperationBIT<3, DE, true>();
            break;
        }
        case 0x5B:{
            // TODO -  	BIT 3,E	8	2	2
            Operations::OperationBIT<3, DE, false>();
            break;
        }
        case 0x5C:{
            // TODO -  	BIT 3,H	8	2	2
            Operations::OperationBIT<3, HL, true>();
            break;
        }
        case 0x5D:{
            // TODO -  	BIT 3,L	8	2	2
            Operations::OperationBIT<3, HL, false>();
            break;
        }
        case 0x5E:{
            // TODO -  	BIT 3,(HL)	12	3	2
            Operations::OperationBIT<3, HL>();
            break;
        }
        case 0x5F:{
            // TODO -  	BIT 3,A	8	2	2
            Operations::OperationBIT<3, A, true>();
            break;
        }
        case 0x60:{
            // TODO - BIT 4,B	8	2	2
            Operations::OperationBIT<4, BC, true>();
            break;
        }
        case 0x61:{
            // TODO -  	BIT 4,C	8	2	2
            Operations::OperationBIT<4, BC, false>();
            break;
        }
        case 0x62:{
            // TODO -  	BIT 4,D	8	2	2
            Operations::OperationBIT<4, DE, true>();
            break;
        }
        case 0x63:{
            // TODO -  	BIT 4,E	8	2	2
            Operations::OperationBIT<4, DE, false>();
            break;
        }
        case 0x64:{
            // TODO -  	BIT 4,H	8	2	2
            Operations::OperationBIT<4, HL, true>();
            break;
        }
        case 0x65:{
            // TODO -  	BIT 4,L	8	2	2
            Operations::OperationBIT<4, HL, false>();
            break;
        }
        case 0x66:{
            // TODO -  	BIT 4,(HL)	12	3	2
            Operations::OperationBIT<4, HL>();
            break;
        }
        case 0x67:{
            // TODO -  	BIT 4,A	8	2	2
            Operations::OperationBIT<4, A, true>();
            break;
        }
        case 0x68:{
            // TODO - BIT 5,B	8	2	2
            Operations::OperationBIT<5, BC, true>();
            break;
        }
        case 0x69:{
            // TODO -  	BIT 5,C	8	2	2
            Operations::OperationBIT<5, BC, false>();
            break;
        }
        case 0x6A:{
            // TODO -  	BIT 5,D	8	2	2
            Operations::OperationBIT<5, DE, true>();
            break;
        }
        case 0x6B:{
            // TODO -  	BIT 5,E	8	2	2
            Operations::OperationBIT<5, DE, false>();
            break;
        }
        case 0x6C:{
            // TODO -  	BIT 5,H	8	2	2
            Operations::OperationBIT<5, HL, true>();
            break;
        }
        case 0x6D:{
            // TODO -  	BIT 5,L	8	2	2
            Operations::OperationBIT<5, HL, false>();
            break;
        }
        case 0x6E:{
            // TODO -  	BIT 5,(HL)	12	3	2
            Operations::OperationBIT<5, HL>();
            break;
        }
        case 0x6F:{
            // TODO -  	BIT 5,A	8	2	2
            Operations::OperationBIT<5, A, true>();
            break;
        }
        case 0x70:{
            // TODO - BIT 6,B	8	2	2
            Operations::OperationBIT<6, BC, true>();
            break;
        }
        case 0x71:{
            // TODO -  	BIT 6,C	8	2	2
            Operations::OperationBIT<6, BC, false>();
            break;
        }
        case 0x72:{
            // TODO -  	BIT 6,D	8	2	2
            Operations::OperationBIT<6, DE, true>();
            break;
        }
        case 0x73:{
            // TODO -  	BIT 6,E	8	2	2
            Operations::OperationBIT<6, DE, false>();
            break;
        }
        case 0x74:{
            // TODO -  	BIT 6,H	8	2	2
            Operations::OperationBIT<6, HL, true>();
            break;
        }
        case 0x75:{
            // TODO -  	BIT 6,L	8	2	2
            Operations::OperationBIT<6, HL, false>();
            break;
        }
        case 0x76:{
            // TODO -  	BIT 6,(HL)	12	3	2
            Operations::OperationBIT<6, HL>();
            break;
        }
        case 0x77:{
            // TODO -  	BIT 6,A	8	2	2
            Operations::OperationBIT<6, A, true>();
            break;
        }
        case 0x78:{
            // TODO - BIT 7,B	8	2	2
            Operations::OperationBIT<7, BC, true>();
            break;
        }
        case 0x79:{
            // TODO -  	BIT 7,C	8	2	2
            Operations::OperationBIT<7, BC, false>();
            break;
        }
        case 0x7A:{
            // TODO -  	BIT 7,D	8	2	2
            Operations::OperationBIT<7, DE, true>();
            break;
        }
        case 0x7B:{
            // TODO -  	BIT 7,E	8	2	2
            Operations::OperationBIT<7, DE, false>();
            break;
        }
        case 0x7C:{
            // TODO -  	BIT 7,H	8	2	2
            Operations::OperationBIT<7, HL, true>();
            break;
        }
        case 0x7D:{
            // TODO -  	BIT 7,L	8	2	2
            Operations::OperationBIT<7, HL, false>();
            break;
        }
        case 0x7E:{
            // TODO -  	BIT 7,(HL)	12	3	2
            Operations::OperationBIT<7, HL>();
            break;
        }
        case 0x7F:{
            // TODO -  	BIT 7,A	8	2	2
            Operations::OperationBIT<7, A, true>();
            break;
        }
        case 0x80:{
            // TODO - RES 0,B	8	2	2
            Operations::OperationRES<0, BC, true>();
            break;
        }
        case 0x81:{
            // TODO - RES 0,C	8	2	2
            Operations::OperationRES<0, BC, false>();
            break;
        }
        case 0x82:{
            // TODO - RES 0,D	8	2	2
            Operations::OperationRES<0, DE, true>();
            break;
        }
        case 0x83:{
            // TODO - RES 0,E	8	2	2
            Operations::OperationRES<0, DE, false>();
            break;
        }
        case 0x84:{
            // TODO -  	RES 0,H	8	2	2
            Operations::OperationRES<0, HL, true>();
            break;
        }
        case 0x85:{
            // TODO -  	RES 0,L	8	2	2
            Operations::OperationRES<0, HL, false>();
            break;
        }
        case 0x86:{
            // TODO -  	RES 0,(HL)	15	4	2
            Operations::OperationRES<0, HL>();
            break;
        }
        case 0x87:{
            // TODO -  	RES 0,A	8	2	2
            Operations::OperationRES<0, A, true>();
            break;
        }
        case 0x88:{
            // TODO - RES 1,B	8	2	2
            Operations::OperationRES<1, BC, true>();
            break;
        }
        case 0x89:{
            // TODO -  	RES 1,C	8	2	2
            Operations::OperationRES<1, BC, false>();
            break;
        }
        case 0x8A:{
            // TODO -  	RES 1,D	8	2	2
            Operations::OperationRES<1, DE, true>();
            break;
        }
        case 0x8B:{
            // TODO -  	RES 1,E	8	2	2
            Operations::OperationRES<1, DE, false>();
            break;
        }
        case 0x8C:{
            // TODO -  	RES 1,H	8	2	2
            Operations::OperationRES<1, HL, true>();
            break;
        }
        case 0x8D:{
            // TODO -  	RES 1,L	8	2	2
            Operations::OperationRES<1, HL, false>();
            break;
        }
        case 0x8E:{
            // TODO -  	RES 1,(HL)	15	4	2
            Operations::OperationRES<1, HL>();
            break;
        }
        case 0x8F:{
            // TODO -  	RES 1,A	8	2	2
            Operations::OperationRES<1, A, true>();
            break;
        }
        case 0x90:{
            // TODO - RES 2,B	8	2	2
            Operations::OperationRES<2, BC, true>();
            break;
        }
        case 0x91:{
            // TODO -  	RES 2,C	8	2	2
            Operations::OperationRES<2, BC, false>();
            break;
        }
        case 0x92:{
            // TODO -  	RES 2,D	8	2	2
            Operations::OperationRES<2, DE, true>();
            break;
        }
        case 0x93:{
            // TODO -  	RES 2,E	8	2	2
            Operations::OperationRES<2, DE, false>();
            break;
        }
        case 0x94:{
            // TODO -  	RES 2,H	8	2	2
            Operations::OperationRES<2, HL, true>();
            break;
        }
        case 0x95:{
            // TODO -  	RES 2,L	8	2	2
            Operations::OperationRES<2, HL, false>();
            break;
        }
        case 0x96:{
            // TODO -  	RES 2,(HL)	15	4	2
            Operations::OperationRES<2, HL>();
            break;
        }
        case 0x97:{
            // TODO -  	RES 2,A	8	2	2
            Operations::OperationRES<2, A, true>();
            break;
        }
        case 0x98:{
            // TODO - RES 3,B	8	2	2
            Operations::OperationRES<3, BC, true>();
            break;
        }
        case 0x99:{
            // TODO -  	RES 3,C	8	2	2
            Operations::OperationRES<3, BC, false>();
            break;
        }
        case 0x9A:{
            // TODO -  	RES 3,D	8	2	2
            Operations::OperationRES<3, DE, true>();
            break;
        }
        case 0x9B:{
            // TODO -  	RES 3,E	8	2	2
            Operations::OperationRES<3, DE, false>();
            break;
        }
        case 0x9C:{
            // TODO -  	RES 3,H	8	2	2
            Operations::OperationRES<3, HL, true>();
            break;
        }
        case 0x9D:{
            // TODO -  	RES 3,L	8	2	2
            Operations::OperationRES<3, HL, false>();
            break;
        }
        case 0x9E:{
            // TODO -  	RES 3,(HL)	15	4	2
            Operations::OperationRES<3, HL>();
            break;
        }
        case 0x9F:{
            // TODO -  	RES 3,A	8	2	2
            Operations::OperationRES<3, A, true>();
            break;
        }
        case 0xA0:{
            // TODO - RES 4,B	8	2	2
            Operations::OperationRES<4, BC, true>();
            break;
        }
        case 0xA1:{
            // TODO -  	RES 4,C	8	2	2
            Operations::OperationRES<4, BC, false>();
            break;
        }
        case 0xA2:{
            // TODO -  	RES 4,D	8	2	2
            Operations::OperationRES<4, DE, true>();
            break;
        }
        case 0xA3:{
            // TODO -  	RES 4,E	8	2	2
            Operations::OperationRES<4, DE, false>();
            break;
        }
        case 0xA4:{
            // TODO -  	RES 4,H	8	2	2
            Operations::OperationRES<4, HL, true>();
            break;
        }
        case 0xA5:{
            // TODO -  	RES 4,L	8	2	2
            Operations::OperationRES<4, HL, false>();
            break;
        }
        case 0xA6:{
            // TODO -  	RES 4,(HL)	15	4	2
            Operations::OperationRES<4, HL>();
            break;
        }
        case 0xA7:{
            // TODO -  	RES 4,A	8	2	2
            Operations::OperationRES<4, A, true>();
            break;
        }
        case 0xA8:{
            // TODO - RES 5,B	8	2	2
            Operations::OperationRES<5, BC, true>();
            break;
        }
        case 0xA9:{
            // TODO -  	RES 5,C	8	2	2
            Operations::OperationRES<5, BC, false>();
            break;
        }
        case 0xAA:{
            // TODO -  	RES 5,D	8	2	2
            Operations::OperationRES<5, DE, true>();
            break;
        }
        case 0xAB:{
            // TODO -  	RES 5,E	8	2	2
            Operations::OperationRES<5, DE, false>();
            break;
        }
        case 0xAC:{
            // TODO -  	RES 5,H	8	2	2
            Operations::OperationRES<5, HL, true>();
            break;
        }
        case 0xAD:{
            // TODO -  	RES 5,L	8	2	2
            Operations::OperationRES<5, HL, false>();
            break;
        }
        case 0xAE:{
            // TODO -  	RES 5,(HL)	15	4	2
            Operations::OperationRES<5, HL>();
            break;
        }
        case 0xAF:{
            // TODO -  	RES 5,A	8	2	2
            Operations::OperationRES<5, A, true>();
            break;
        }
        case 0xB0:{
            // TODO - RES 6,B	8	2	2
            Operations::OperationRES<6, BC, true>();
            break;
        }
        case 0xB1:{
            // TODO -  	RES 6,C	8	2	2
            Operations::OperationRES<6, BC, false>();
            break;
        }
        case 0xB2:{
            // TODO -  	RES 6,D	8	2	2
            Operations::OperationRES<6, DE, true>();
            break;
        }
        case 0xB3:{
            // TODO -  	RES 6,E	8	2	2
            Operations::OperationRES<6, DE, false>();
            break;
        }
        case 0xB4:{
            // TODO -  	RES 6,H	8	2	2
            Operations::OperationRES<6, HL, true>();
            break;
        }
        case 0xB5:{
            // TODO -  	RES 6,L	8	2	2
            Operations::OperationRES<6, HL, false>();
            break;
        }
        case 0xB6:{
            // TODO -  	RES 6,(HL)	15	4	2
            Operations::OperationRES<6, HL>();
            break;
        }
        case 0xB7:{
            // TODO -  	RES 6,A	8	2	2
            Operations::OperationRES<6, A, true>();
            break;
        }
        case 0xB8:{
            // TODO - RES 7,B	8	2	2
            Operations::OperationRES<7, BC, true>();
            break;
        }
        case 0xB9:{
            // TODO -  	RES 7,C	8	2	2
            Operations::OperationRES<7, BC, false>();
            break;
        }
        case 0xBA:{
            // TODO -  	RES 7,D	8	2	2
            Operations::OperationRES<7, DE, true>();
            break;
        }
        case 0xBB:{
            // TODO -  	RES 7,E	8	2	2
            Operations::OperationRES<7, DE, false>();
            break;
        }
        case 0xBC:{
            // TODO -  	RES 7,H	8	2	2
            Operations::OperationRES<7, HL, true>();
            break;
        }
        case 0xBD:{
            // TODO -  	RES 7,L	8	2	2
            Operations::OperationRES<7, HL, false>();
            break;
        }
        case 0xBE:{
            // TODO -  	RES 7,(HL)	15	4	2
            Operations::OperationRES<7, HL>();
            break;
        }
        case 0xBF:{
            // TODO -  	RES 7,A	8	2	2
            Operations::OperationRES<7, A, true>();
            break;
        }
        case 0xC0:{
            // TODO - SET 0,B	8	2	2
            Operations::OperationSET<0, BC, true>();
            break;
        }
        case 0xC1:{
            // TODO - SET 0,C	8	2	2
            Operations::OperationSET<0, BC, false>();
            break;
        }
        case 0xC2:{
            // TODO - SET 0,D	8	2	2
            Operations::OperationSET<0, DE, true>();
            break;
        }
        case 0xC3:{
            // TODO - SET 0,E	8	2	2
            Operations::OperationSET<0, DE, false>();
            break;
        }
        case 0xC4:{
            // TODO -  	SET 0,H	8	2	2
            Operations::OperationSET<0, HL, true>();
            break;
        }
        case 0xC5:{
            // TODO -  	SET 0,L	8	2	2
            Operations::OperationSET<0, HL, false>();
            break;
        }
        case 0xC6:{
            // TODO -  	SET 0,(HL)	15	4	2
            Operations::OperationSET<0, HL>();
            break;
        }
        case 0xC7:{
            // TODO -  	SET 0,A	8	2	2
            Operations::OperationSET<0, A, true>();
            break;
        }
        case 0xC8:{
            // TODO - SET 1,B	8	2	2
            Operations::OperationSET<1, BC, true>();
            break;
        }
        case 0xC9:{
            // TODO -  	SET 1,C	8	2	2
            Operations::OperationSET<1, BC, false>();
            break;
        }
        case 0xCA:{
            // TODO -  	SET 1,D	8	2	2
            Operations::OperationSET<1, DE, true>();
            break;
        }
        case 0xCB:{
            // TODO -  	SET 1,E	8	2	2
            Operations::OperationSET<1, DE, false>();
            break;
        }
        case 0xCC:{
            // TODO -  	SET 1,H	8	2	2
            Operations::OperationSET<1, HL, true>();
            break;
        }
        case 0xCD:{
            // TODO -  	SET 1,L	8	2	2
            Operations::OperationSET<1, HL, false>();
            break;
        }
        case 0xCE:{
            // TODO -  	SET 1,(HL)	15	4	2
            Operations::OperationSET<1, HL>();
            break;
        }
        case 0xCF:{
            // TODO -  	SET 1,A	8	2	2
            Operations::OperationSET<1, A, true>();
            break;
        }
        case 0xD0:{
            // TODO - SET 2,B	8	2	2
            Operations::OperationSET<2, BC, true>();
            break;
        }
        case 0xD1:{
            // TODO -  	SET 2,C	8	2	2
            Operations::OperationSET<2, BC, false>();
            break;
        }
        case 0xD2:{
            // TODO -  	SET 2,D	8	2	2
            Operations::OperationSET<2, DE, true>();
            break;
        }
        case 0xD3:{
            // TODO -  	SET 2,E	8	2	2
            Operations::OperationSET<2, DE, false>();
            break;
        }
        case 0xD4:{
            // TODO -  	SET 2,H	8	2	2
            Operations::OperationSET<2, HL, true>();
            break;
        }
        case 0xD5:{
            // TODO -  	SET 2,L	8	2	2
            Operations::OperationSET<2, HL, false>();
            break;
        }
        case 0xD6:{
            // TODO -  	SET 2,(HL)	15	4	2
            Operations::OperationSET<2, HL>();
            break;
        }
        case 0xD7:{
            // TODO -  	SET 2,A	8	2	2
            Operations::OperationSET<2, A, true>();
            break;
        }
        case 0xD8:{
            // TODO - SET 3,B	8	2	2
            Operations::OperationSET<3, BC, true>();
            break;
        }
        case 0xD9:{
            // TODO -  	SET 3,C	8	2	2
            Operations::OperationSET<3, BC, false>();
            break;
        }
        case 0xDA:{
            // TODO -  	SET 3,D	8	2	2
            Operations::OperationSET<3, DE, true>();
            break;
        }
        case 0xDB:{
            // TODO -  	SET 3,E	8	2	2
            Operations::OperationSET<3, DE, false>();
            break;
        }
        case 0xDC:{
            // TODO -  	SET 3,H	8	2	2
            Operations::OperationSET<3, HL, true>();
            break;
        }
        case 0xDD:{
            // TODO -  	SET 3,L	8	2	2
            Operations::OperationSET<3, HL, false>();
            break;
        }
        case 0xDE:{
            // TODO -  	SET 3,(HL)	15	4	2
            Operations::OperationSET<3, HL>();
            break;
        }
        case 0xDF:{
            // TODO -  	SET 3,A	8	2	2
            Operations::OperationSET<3, A, true>();
            break;
        }
        case 0xE0:{
            // TODO - SET 4,B	8	2	2
            Operations::OperationSET<4, BC, true>();
            break;
        }
        case 0xE1:{
            // TODO -  	SET 4,C	8	2	2
            Operations::OperationSET<4, BC, false>();
            break;
        }
        case 0xE2:{
            // TODO -  	SET 4,D	8	2	2
            Operations::OperationSET<4, DE, true>();
            break;
        }
        case 0xE3:{
            // TODO -  	SET 4,E	8	2	2
            Operations::OperationSET<4, DE, false>();
            break;
        }
        case 0xE4:{
            // TODO -  	SET 4,H	8	2	2
            Operations::OperationSET<4, HL, true>();
            break;
        }
        case 0xE5:{
            // TODO -  	SET 4,L	8	2	2
            Operations::OperationSET<4, HL, false>();
            break;
        }
        case 0xE6:{
            // TODO -  	SET 4,(HL)	15	4	2
            Operations::OperationSET<4, HL>();
            break;
        }
        case 0xE7:{
            // TODO -  	SET 4,A	8	2	2
            Operations::OperationSET<4, A, true>();
            break;
        }
        case 0xE8:{
            // TODO - SET 5,B	8	2	2
            Operations::OperationSET<5, BC, true>();
            break;
        }
        case 0xE9:{
            // TODO -  	SET 5,C	8	2	2
            Operations::OperationSET<5, BC, false>();
            break;
        }
        case 0xEA:{
            // TODO -  	SET 5,D	8	2	2
            Operations::OperationSET<5, DE, true>();
            break;
        }
        case 0xEB:{
            // TODO -  	SET 5,E	8	2	2
            Operations::OperationSET<5, DE, false>();
            break;
        }
        case 0xEC:{
            // TODO -  	SET 5,H	8	2	2
            Operations::OperationSET<5, HL, true>();
            break;
        }
        case 0xED:{
            // TODO -  	SET 5,L	8	2	2
            Operations::OperationSET<5, HL, false>();
            break;
        }
        case 0xEE:{
            // TODO -  	SET 5,(HL)	15	4	2
            Operations::OperationSET<5, HL>();
            break;
        }
        case 0xEF:{
            // TODO -  	SET 5,A	8	2	2
            Operations::OperationSET<5, A, true>();
            break;
        }
        case 0xF0:{
            // TODO - SET 6,B	8	2	2
            Operations::OperationSET<6, BC, true>();
            break;
        }
        case 0xF1:{
            // TODO -  	SET 6,C	8	2	2
            Operations::OperationSET<6, BC, false>();
            break;
        }
        case 0xF2:{
            // TODO -  	SET 6,D	8	2	2
            Operations::OperationSET<6, DE, true>();
            break;
        }
        case 0xF3:{
            // TODO -  	SET 6,E	8	2	2
            Operations::OperationSET<6, DE, false>();
            break;
        }
        case 0xF4:{
            // TODO -  	SET 6,H	8	2	2
            Operations::OperationSET<6, HL, true>();
            break;
        }
        case 0xF5:{
            // TODO -  	SET 6,L	8	2	2
            Operations::OperationSET<6, HL, false>();
            break;
        }
        case 0xF6:{
            // TODO -  	SET 6,(HL)	15	4	2
            Operations::OperationSET<6, HL>();
            break;
        }
        case 0xF7:{
            // TODO -  	SET 6,A	8	2	2
            Operations::OperationSET<6, A, true>();
            break;
        }
        case 0xF8:{
            // TODO - SET 7,B	8	2	2
            Operations::OperationSET<7, BC, true>();
            break;
        }
        case 0xF9:{
            // TODO -  	SET 7,C	8	2	2
            Operations::OperationSET<7, BC, false>();
            break;
        }
        case 0xFA:{
            // TODO -  	SET 7,D	8	2	2
            Operations::OperationSET<7, DE, true>();
            break;
        }
        case 0xFB:{
            // TODO -  	SET 7,E	8	2	2
            Operations::OperationSET<7, DE, false>();
            break;
        }
        case 0xFC:{
            // TODO -  	SET 7,H	8	2	2
            Operations::OperationSET<7, HL, true>();
            break;
        }
        case 0xFD:{
            // TODO -  	SET 7,L	8	2	2
            Operations::OperationSET<7, HL, false>();
            break;
        }
        case 0xFE:{
            // TODO -  	SET 7,(HL)	15	4	2
            Operations::OperationSET<7, HL>();
            break;
        }
        case 0xFF:{
            // TODO -  	SET 7,A	8	2	2
            Operations::OperationSET<7, A, true>();
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
            // TODO - 09	ADD IX,BC
            Operations::OperationADD<IX, BC>();
            break;
        }
        case 0x19:{
            // TODO - 19	ADD IX,DE
            Operations::OperationADD<IX, DE>();
            break;
        }
        case 0x21:{
            // TODO - 21 n n	LD IX,nn
            Operations::OperationLD<IX, AddressingModes::ImmediateExtended>();
            break;
        }
        case 0x22:{
            // TODO - 22 n n	LD (nn),IX
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::ImmediateExtended, uint16_t>, IX>();
            break;
        }
        case 0x23:{
            // TODO - 23	INC IX
            Operations::OperationINC<IX>();
            break;
        }
        case 0x24:{
            // TODO - 24	INC IXH
            Operations::OperationINC<AddressingModes::RegisterWrite<IX, true>, AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x25:{
            // TODO - 25	DEC IXH
            Operations::OperationDEC<AddressingModes::RegisterWrite<IX, true>, AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x26:{
            // TODO - 26 n 	LD IXH,n
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, true>,  AddressingModes::Immediate>();
            break;
        }
        case 0x29:{
            // TODO - 29	ADD IX,IX
            Operations::OperationADD<IX, IX>();
            break;
        }
        case 0x2A:{
            // TODO - 2A n n	LD IX,(nn)
            Operations::OperationLD<IX, AddressingModes::AddressRead<AddressingModes::ImmediateExtended, uint8_t>>();
            break;
        }
        case 0x2B:{
            // TODO - 2B	DEC IX
            Operations::OperationDEC<IX>();
            break;
        }
        case 0x2C:{
            // TODO - 2C	INC IXL
            Operations::OperationINC<AddressingModes::RegisterWrite<IX, false>, AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x2D:{
            // TODO - 2D	DEC IXL
            Operations::OperationDEC<AddressingModes::RegisterWrite<IX, false>, AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x2E:{
            // TODO - 2E n	LD IXL,n
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, false>, AddressingModes::Immediate>();
            break;
        }
        case 0x34:{
            // TODO - 34 d	INC (IX+d)
            Operations::OperationINC<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x35:{
            // TODO - 35 d	DEC (IX+d)
            Operations::OperationDEC<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x36:{
            // TODO - 36 d n	LD (IX+d),n
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::Immediate>();
            break;
        }
        case 0x39:{
            // TODO - 39	ADD IX,SP
            Operations::OperationADD<IX, SP>();
            break;
        }
        case 0x44:{
            // TODO - 44	LD B,IXH
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, true>,  AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x45:{
            // TODO - 45	LD B,IXL
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, true>,  AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x46:{
            // TODO - 46 d	LD B,(IX+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, true>,  AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x4C:{
            // TODO - 4C	LD C,IXH
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, false>,  AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x4D:{
            // TODO - 4D	LD C,IXL
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, false>,  AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x4E:{
            // TODO - 4E d	LD C,(IX+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, false>,  AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x54:{
            // TODO - 54	LD D,IXH
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, true>,  AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x55:{
            // TODO - 55	LD D,IXL
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, true>,  AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x56:{
            // TODO - 56 d	LD D,(IX+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, true>,  AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x5C:{
            // TODO - 5C	LD E,IXH
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, false>,  AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x5D:{
            // TODO - 5D	LD E,IXL
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, false>,  AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x5E:{
            // TODO - 5E d	LD E,(IX+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, false>,  AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x60:{
            // TODO - 60	LD IXH,B
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, true>,  AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x61:{
            // TODO - 61	LD IXH,C
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, true>,  AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x62:{
            // TODO - 62	LD IXH,D
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, true>,  AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x63:{
            // TODO - 63	LD IXH,E
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, true>,  AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x64:{
            // TODO - 64	LD IXH,IXH
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, true>,  AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x65:{
            // TODO - 65	LD IXH,IXL
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, true>,  AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x66:{
            // TODO - 66 d	LD H,(IX+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x67:{
            // TODO - 67	LD IXH,A
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, true>,  AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x68:{
            // TODO - 68	LD IXL,B
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, false>,  AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x69:{
            // TODO - 69	LD IXL,C
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, false>,  AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x6A:{
            // TODO - 6A	LD IXL,D
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, false>,  AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x6B:{
            // TODO - 6B	LD IXL,E
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, false>,  AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x6C:{
            // TODO - 6C	LD IXL,IXH
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, false>,  AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x6D:{
            // TODO - 6D	LD IXL,IXL
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, false>,  AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x6E:{
            // TODO - 6E d	LD L,(IX+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<HL, false>,  AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x6F:{
            // TODO - 6F	LD IXL,A
            Operations::OperationLD<AddressingModes::RegisterWrite<IX, false>,  AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x70:{
            // TODO - 70 d	LD (IX+d),B
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x71:{
            // TODO - 71 d	LD (IX+d),C
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x72:{
            // TODO - 72 d	LD (IX+d),D
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x73:{
            // TODO - 73 d	LD (IX+d),E
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x74:{
            // TODO - 74 d	LD (IX+d),H
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x75:{
            // TODO - 75 d	LD (IX+d),L
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x77:{
            // TODO - 77 d	LD (IX+d),A
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIXAddress, uint8_t>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x7C:{
            // TODO - 7C	LD A,IXH
            Operations::OperationLD<AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x7D:{
            // TODO - 7D	LD A,IXL
            Operations::OperationLD<AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x7E:{
            // TODO - 7E d	LD A,(IX+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x84:{
            // TODO - 84	ADD A,IXH
            Operations::OperationADD<AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x85:{
            // TODO - 85	ADD A,IXL
            Operations::OperationADD<AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x86:{
            // TODO - 86 d	ADD A,(IX+d)
            Operations::OperationADD<AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x8C:{
            // TODO - 8C	ADC A,IXH
            Operations::OperationADC<AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x8D:{
            // TODO - 8D	ADC A,IXL
            Operations::OperationADC<AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x8E:{
            // TODO - 8E d	ADC A,(IX+d)
            Operations::OperationADC<AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x94:{
            // TODO - 94	SUB IXH
            Operations::OperationSUB<AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x95:{
            // TODO - 95	SUB IXL
            Operations::OperationSUB<AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x96:{
            // TODO - 96 d	SUB (IX+d)
            Operations::OperationSUB<AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0x9C:{
            // TODO - 9C	SBC A,IXH
            Operations::OperationSBC<AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0x9D:{
            // TODO - 9D	SBC A,IXL
            Operations::OperationSBC<AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0x9E:{
            // TODO - 9E d	SBC A,(IX+d)
            Operations::OperationSBC<AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xA4:{
            // TODO - A4	AND IXH
            Operations::OperationAND<AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0xA5:{
            // TODO - A5	AND IXL
            Operations::OperationAND<AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0xA6:{
            // TODO - A6 d	AND (IX+d)
            Operations::OperationAND<AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xAC:{
            // TODO - AC	XOR IXH
            Operations::OperationXOR<AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0xAD:{
            // TODO - AD	XOR IXL
            Operations::OperationXOR<AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0xAE:{
            // TODO - AE d	XOR (IX+d)
            Operations::OperationXOR<AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xB4:{
            // TODO - B4	OR IXH
            Operations::OperationOR<AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0xB5:{
            // TODO - B5	OR IXL
            Operations::OperationOR<AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0xB6:{
            // TODO - B6 d	OR (IX+d)
            Operations::OperationOR<AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xBC:{
            // TODO - BC	CP IXH
            Operations::OperationCP<AddressingModes::RegisterRead<IX, true>>();
            break;
        }
        case 0xBD:{
            // TODO - BD	CP IXL
            Operations::OperationCP<AddressingModes::RegisterRead<IX, false>>();
            break;
        }
        case 0xBE:{
            // TODO - BE d	CP (IX+d)
            Operations::OperationCP<AddressingModes::AddressRead<AddressingModes::IndexedIXAddress, uint8_t>>();
            break;
        }
        case 0xCB:{
            // TODO - IX-BITS
            extendedOpIXBITS();
            break;
        }
        case 0xE1:{
            // TODO - E1	POP IX
            Operations::OperationPOP<IX>();
            break;
        }
        case 0xE3:{
            // TODO - E3	EX (SP),IX
            Operations::OperationEX<AddressingModes::AddressWrite<SP, uint16_t>, AddressingModes::AddressRead<SP, uint16_t>, IX>();
            break;
        }
        case 0xE5:{
            // TODO - E5	PUSH IX
            Operations::OperationPUSH<IX>();
            break;
        }
        case 0xE9:{
            // TODO - E9	JP (IX)
            Operations::OperationJP<IX>();
            break;
        }
        case 0xF9:{
            // TODO - F9	LD SP,IX
            Operations::OperationLD<SP, IX>();
            break;
        }
    }
}

static inline void extendedOpEXTD() {
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC++ );
    switch( opcode ) {
        case 0x40:{
            // TODO - IN B,(C);
            OperationBase<Operations::INR, AddressingModes::RegisterWrite<BC, true>, AddressingModes::PortRead<BC>>();
            break;
        }
        case 0x41:{
            // TODO - OUT (C),B
            OperationBase<Operations::OUT, AddressingModes::PortWrite<BC>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x42:{
            // TODO - SBC HL,BC
            OperationBase<Operations::SBC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<BC>>();
            break;
        }
        case 0x43:{
            // TODO - LD (nn),BC
            OperationBase<Operations::LD16, AddressingModes::AddressWrite<AddressingModes::ImmediateExtended, uint16_t>, AddressingModes::RegisterRead<BC>>();
            break;
        }
        case 0x44:{
            // TODO - NEG
            Operations::OperationNEG();
            break;
        }
        case 0x45:{
            // TODO - RETN
            Operations::OperationRETN();
            break;
        }
        case 0x46:{
            // TODO - IM 0
            Operations::OperationIM<0>();
            break;
        }
        case 0x47:{
            // TODO - LD I,A
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<I>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x48:{
            // TODO - IN C,(C);
            OperationBase<Operations::INR, AddressingModes::RegisterWrite<BC, false>, AddressingModes::PortRead<BC>>();
            break;
        }
        case 0x49:{
            // TODO - OUT (C),C
            OperationBase<Operations::OUT, AddressingModes::PortWrite<BC>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x4A:{
            // TODO - ADC HL,BC
            OperationBase<Operations::ADC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<BC>>();
            break;
        }
        case 0x4B:{
            // TODO - LD BC,(nn);
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<BC>, AddressingModes::AddressRead<AddressingModes::ImmediateExtended, uint8_t>>();
            break;
        }
        case 0x4C:{
            // TODO - NEG
            Operations::OperationNEG();
            break;
        }
        case 0x4D:{
            // TODO - RETI
            OperationBase<Operations::RETI>();
            break;
        }
        case 0x4E:{
            // TODO - IM 0
            Operations::OperationIM<0>();
            break;
        }
        case 0x4F:{
            // TODO - LD R,A
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<R>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x50:{
            // TODO - IN D,(C);
            OperationBase<Operations::INR, AddressingModes::RegisterWrite<DE, true>, AddressingModes::PortRead<BC>>();
            break;
        }
        case 0x51:{
            // TODO - OUT (C),D
            OperationBase<Operations::OUT, AddressingModes::PortWrite<BC>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x52:{
            // TODO - SBC HL,DE
            OperationBase<Operations::SBC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<DE>>();
            break;
        }
        case 0x53:{
            // TODO - LD (nn),DE
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<AddressingModes::ImmediateExtended, uint16_t>, AddressingModes::RegisterRead<DE>>();
            break;
        }
        case 0x54:{
            // TODO - NEG
            Operations::OperationNEG();
            break;
        }
        case 0x55:{
            // TODO - RETN
            Operations::OperationRETN();
            break;
        }
        case 0x56:{
            // TODO - IM 1
            Operations::OperationIM<1>();
            break;
        }
        case 0x57:{
            // TODO - LD A,I
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<I>>();
            break;
        }
        case 0x58:{
            // TODO - IN E,(C);
            OperationBase<Operations::INR, AddressingModes::RegisterWrite<DE, false>, AddressingModes::PortRead<BC>>();
            break;
        }
        case 0x59:{
            // TODO - OUT (C),E
            OperationBase<Operations::OUT, AddressingModes::PortWrite<BC>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x5A:{
            // TODO - ADC HL,DE
            OperationBase<Operations::ADC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<DE>>();
            break;
        }
        case 0x5B:{
            // TODO - LD DE,(nn);
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<DE>, AddressingModes::AddressRead<AddressingModes::ImmediateExtended, uint8_t>>();
            break;
        }
        case 0x5C:{
            // TODO - NEG
            Operations::OperationNEG();
            break;
        }
        case 0x5D:{
            // TODO - RETN
            Operations::OperationRETN();
            break;
        }
        case 0x5E:{
            // TODO - IM 2
            Operations::OperationIM<2>();
            break;
        }
        case 0x5F:{
            // TODO - LD A,R
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<R>>();
            break;
        }
        case 0x60:{
            // TODO - IN H,(C);
            OperationBase<Operations::INR, AddressingModes::RegisterWrite<HL, true>, AddressingModes::PortRead<BC>>();
            break;
        }
        case 0x61:{
            // TODO - OUT (C),H
            OperationBase<Operations::OUT, AddressingModes::PortWrite<BC>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x62:{
            // TODO - SBC HL,HL
            OperationBase<Operations::SBC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<HL>>();
            break;
        }
        case 0x63:{
            // TODO - LD (nn),HL
            OperationBase<Operations::LD16, AddressingModes::AddressWrite<AddressingModes::ImmediateExtended, uint16_t>, AddressingModes::RegisterRead<HL>>();
            break;
        }
        case 0x64:{
            // TODO - NEG
            Operations::OperationNEG();
            break;
        }
        case 0x65:{
            // TODO - RETN
            Operations::OperationRETN();
            break;
        }
        case 0x66:{
            // TODO - IM 0
            Operations::OperationIM<0>();
            break;
        }
        case 0x67:{
            // TODO - RRD
            OperationBase<Operations::RRD, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x68:{
            // TODO - IN L,(C);
            OperationBase<Operations::INR, AddressingModes::RegisterWrite<HL, false>, AddressingModes::PortRead<BC>>();
            break;
        }
        case 0x69:{
            // TODO - OUT (C),L
            OperationBase<Operations::OUT, AddressingModes::PortWrite<BC>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x6A:{
            // TODO - ADC HL,HL
            OperationBase<Operations::ADC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<HL>>();
            break;
        }
        case 0x6B:{
            // TODO - LD HL,(nn);
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<HL>, AddressingModes::AddressRead<AddressingModes::ImmediateExtended, uint8_t>>();
            break;
        }
        case 0x6C:{
            // TODO - NEG
            Operations::OperationNEG();
            break;
        }
        case 0x6D:{
            // TODO - RETN
            Operations::OperationRETN();
            break;
        }
        case 0x6E:{
            // TODO - IM 0
            Operations::OperationIM<0>();
            break;
        }
        case 0x6F:{
            // TODO - RLD
            OperationBase<Operations::RLD,  AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x70:{
            // TODO - IN F,(C) / IN (C);
            OperationBase<Operations::IN1, AddressingModes::PortRead<BC>>();
            break;
        }
        case 0x71:{
            // TODO - OUT (C),0
            OperationBase<Operations::OUT1, AddressingModes::PortWrite<BC>>();
            break;
        }
        case 0x72:{
            // TODO - SBC HL,SP
            OperationBase<Operations::SBC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<SP>>();
            break;
        }
        case 0x73:{
            // TODO - LD (nn),SP	
            OperationBase<Operations::LD16, AddressingModes::AddressWrite<AddressingModes::ImmediateExtended, uint16_t>, AddressingModes::RegisterRead<SP>>();	
            break;
        }
        case 0x74:{
            // TODO - NEG
            Operations::OperationNEG();
            break;
        }
        case 0x75:{
            // TODO - RETN
            Operations::OperationRETN();
            break;
        }
        case 0x76:{
            // TODO - IM 1
            Operations::OperationIM<1>();
            break;
        }
        case 0x78:{
            // TODO - IN A,(C);
            OperationBase<Operations::INA, AddressingModes::RegisterWrite<A, true>, AddressingModes::PortRead<BC>>();
            break;
        }
        case 0x79:{
            // TODO - OUT (C),A
            OperationBase<Operations::OUT, AddressingModes::PortWrite<BC>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x7A:{
            // TODO - ADC HL,SP
            OperationBase<Operations::ADC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<SP>>();
            break;
        }
        case 0x7B:{
            // TODO - LD SP,(nn);
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<SP>, AddressingModes::AddressRead<AddressingModes::ImmediateExtended, uint16_t>>();
            break;
        }
        case 0x7C:{
            // TODO - NEG
            Operations::OperationNEG();
            break;
        }
        case 0x7D:{
            // TODO - RETN
            Operations::OperationRETN();
            break;
        }
        case 0x7E:{
            // TODO - IM 2
            Operations::OperationIM<2>();
            break;
        }
        case 0xA0:{
            // TODO - LDI
            Operations::OperationLDN<true, false>();
            break;
        }
        case 0xA1:{
            // TODO - CPI
            Operations::OperationCPN<true, false>();
            break;
        }
        case 0xA2:{
            // TODO - INI
            Operations::OperationINN8<true, false>();
            break;
        }
        case 0xA3:{
            // TODO - OUTI
            Operations::OperationOUTN8<true, false>();
            break;
        }
        case 0xA8:{
            // TODO - LDD
            Operations::OperationLDN<false, false>();
            break;
        }
        case 0xA9:{
            // TODO - CPD
            Operations::OperationCPN<false, false>();
            break;
        }
        case 0xAA:{
            // TODO - IND
            Operations::OperationINN8<false, false>();
            break;
        }
        case 0xAB:{
            // TODO - OUTD
            Operations::OperationOUTN8<false, false>();
            break;
        }
        case 0xB0:{
            // TODO - LDIR
            Operations::OperationLDN<true, true>();
            break;
        }
        case 0xB1:{
            // TODO - CPIR
            Operations::OperationCPN<true, true>();
            break;
        }
        case 0xB2:{
            // TODO - INIR
            Operations::OperationINN8<true, true>();
            break;
        }
        case 0xB3:{
            // TODO - OTIR
            Operations::OperationOUTN8<true, true>();
            break;
        }
        case 0xB8:{
            // TODO - LDDR
            Operations::OperationLDN<false, true>();
            break;
        }
        case 0xB9:{
            // TODO - CPDR
            Operations::OperationCPN<false, true>();
            break;
        }
        case 0xBA:{
            // TODO - INDR
            Operations::OperationINN8<false, true>();
            break;
        }
        case 0xBB:{
            // TODO - OTDR
            Operations::OperationOUTN8<false, true>();
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
            // TODO - 09	ADD IY,BC
            Operations::OperationADD<IY, BC>();
            break;
        }
        case 0x19:{
            // TODO - 19	ADD IY,DE
            Operations::OperationADD<IY, DE>();
            break;
        }
        case 0x21:{
            // TODO - 21 n n	LD IY,nn
            Operations::OperationLD<IY, AddressingModes::ImmediateExtended>();
            break;
        }
        case 0x22:{
            // TODO - 22 n n	LD (nn),IY
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::ImmediateExtended, uint16_t>, IY>();
            break;
        }
        case 0x23:{
            // TODO - 23	INC IY
            Operations::OperationINC<IY>();
            break;
        }
        case 0x24:{
            // TODO - 24	INC IYH
            Operations::OperationINC<AddressingModes::RegisterWrite<IY, true>, AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x25:{
            // TODO - 25	DEC IYH
            Operations::OperationDEC<AddressingModes::RegisterWrite<IY, true>, AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x26:{
            // TODO - 26 n 	LD IYH,n
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, true>,  AddressingModes::Immediate>();
            break;
        }
        case 0x29:{
            // TODO - 29	ADD IY,IY
            Operations::OperationADD<IY, IY>();
            break;
        }
        case 0x2A:{
            // TODO - 2A n n	LD IY,(nn)
            Operations::OperationLD<IY, AddressingModes::AddressRead<AddressingModes::ImmediateExtended, uint8_t>>();
            break;
        }
        case 0x2B:{
            // TODO - 2B	DEC IY
            Operations::OperationDEC<IY>();
            break;
        }
        case 0x2C:{
            // TODO - 2C	INC IYL
            Operations::OperationINC<AddressingModes::RegisterWrite<IY, false>, AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x2D:{
            // TODO - 2D	DEC IYL
            Operations::OperationDEC<AddressingModes::RegisterWrite<IY, false>, AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x2E:{
            // TODO - 2E n	LD IYL,n
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, false>, AddressingModes::Immediate>();
            break;
        }
        case 0x34:{
            // TODO - 34 d	INC (IY+d)
            Operations::OperationINC<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x35:{
            // TODO - 35 d	DEC (IY+d)
            Operations::OperationDEC<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x36:{
            // TODO - 36 d n	LD (IY+d),n
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::Immediate>();
            break;
        }
        case 0x39:{
            // TODO - 39	ADD IY,SP
            Operations::OperationADD<IY, SP>();
            break;
        }
        case 0x44:{
            // TODO - 44	LD B,IYH
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, true>,  AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x45:{
            // TODO - 45	LD B,IYL
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, true>,  AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x46:{
            // TODO - 46 d	LD B,(IY+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, true>,  AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x4C:{
            // TODO - 4C	LD C,IYH
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, false>,  AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x4D:{
            // TODO - 4D	LD C,IYL
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, false>,  AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x4E:{
            // TODO - 4E d	LD C,(IY+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<BC, false>,  AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x54:{
            // TODO - 54	LD D,IYH
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, true>,  AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x55:{
            // TODO - 55	LD D,IYL
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, true>,  AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x56:{
            // TODO - 56 d	LD D,(IY+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, true>,  AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x5C:{
            // TODO - 5C	LD E,IYH
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, false>,  AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x5D:{
            // TODO - 5D	LD E,IYL
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, false>,  AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x5E:{
            // TODO - 5E d	LD E,(IY+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<DE, false>,  AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x60:{
            // TODO - 60	LD IYH,B
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, true>,  AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x61:{
            // TODO - 61	LD IYH,C
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, true>,  AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x62:{
            // TODO - 62	LD IYH,D
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, true>,  AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x63:{
            // TODO - 63	LD IYH,E
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, true>,  AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x64:{
            // TODO - 64	LD IYH,IYH
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, true>,  AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x65:{
            // TODO - 65	LD IYH,IYL
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, true>,  AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x66:{
            // TODO - 66 d	LD H,(IY+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x67:{
            // TODO - 67	LD IYH,A
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, true>,  AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x68:{
            // TODO - 68	LD IYL,B
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, false>,  AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x69:{
            // TODO - 69	LD IYL,C
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, false>,  AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x6A:{
            // TODO - 6A	LD IYL,D
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, false>,  AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x6B:{
            // TODO - 6B	LD IYL,E
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, false>,  AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x6C:{
            // TODO - 6C	LD IYL,IYH
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, false>,  AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x6D:{
            // TODO - 6D	LD IYL,IYL
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, false>,  AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x6E:{
            // TODO - 6E d	LD L,(IY+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<HL, false>,  AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x6F:{
            // TODO - 6F	LD IYL,A
            Operations::OperationLD<AddressingModes::RegisterWrite<IY, false>,  AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x70:{
            // TODO - 70 d	LD (IY+d),B
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x71:{
            // TODO - 71 d	LD (IY+d),C
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x72:{
            // TODO - 72 d	LD (IY+d),D
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x73:{
            // TODO - 73 d	LD (IY+d),E
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x74:{
            // TODO - 74 d	LD (IY+d),H
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x75:{
            // TODO - 75 d	LD (IY+d),L
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x77:{
            // TODO - 77 d	LD (IY+d),A
            Operations::OperationLD<AddressingModes::AddressWrite<AddressingModes::IndexedIYAddress, uint8_t>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x7C:{
            // TODO - 7C	LD A,IYH
            Operations::OperationLD<AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x7D:{
            // TODO - 7D	LD A,IYL
            Operations::OperationLD<AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x7E:{
            // TODO - 7E d	LD A,(IY+d)
            Operations::OperationLD<AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x84:{
            // TODO - 84	ADD A,IYH
            Operations::OperationADD<AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x85:{
            // TODO - 85	ADD A,IYL
            Operations::OperationADD<AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x86:{
            // TODO - 86 d	ADD A,(IY+d)
            Operations::OperationADD<AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x8C:{
            // TODO - 8C	ADC A,IYH
            Operations::OperationADC<AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x8D:{
            // TODO - 8D	ADC A,IYL
            Operations::OperationADC<AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x8E:{
            // TODO - 8E d	ADC A,(IY+d)
            Operations::OperationADC<AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x94:{
            // TODO - 94	SUB IYH
            Operations::OperationSUB<AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x95:{
            // TODO - 95	SUB IYL
            Operations::OperationSUB<AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x96:{
            // TODO - 96 d	SUB (IY+d)
            Operations::OperationSUB<AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0x9C:{
            // TODO - 9C	SBC A,IYH
            Operations::OperationSBC<AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0x9D:{
            // TODO - 9D	SBC A,IYL
            Operations::OperationSBC<AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0x9E:{
            // TODO - 9E d	SBC A,(IY+d)
            Operations::OperationSBC<AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xA4:{
            // TODO - A4	AND IYH
            Operations::OperationAND<AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0xA5:{
            // TODO - A5	AND IYL
            Operations::OperationAND<AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0xA6:{
            // TODO - A6 d	AND (IY+d)
            Operations::OperationAND<AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xAC:{
            // TODO - AC	XOR IYH
            Operations::OperationXOR<AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0xAD:{
            // TODO - AD	XOR IYL
            Operations::OperationXOR<AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0xAE:{
            // TODO - AE d	XOR (IY+d)
            Operations::OperationXOR<AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xB4:{
            // TODO - B4	OR IYH
            Operations::OperationOR<AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0xB5:{
            // TODO - B5	OR IYL
            Operations::OperationOR<AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0xB6:{
            // TODO - B6 d	OR (IY+d)
            Operations::OperationOR<AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xBC:{
            // TODO - BC	CP IYH
            Operations::OperationCP<AddressingModes::RegisterRead<IY, true>>();
            break;
        }
        case 0xBD:{
            // TODO - BD	CP IYL
            Operations::OperationCP<AddressingModes::RegisterRead<IY, false>>();
            break;
        }
        case 0xBE:{
            // TODO - BE d	CP (IY+d)
            Operations::OperationCP<AddressingModes::AddressRead<AddressingModes::IndexedIYAddress, uint8_t>>();
            break;
        }
        case 0xCB:{
            // TODO - IY-BITS
            extendedOpIYBITS();
            break;
        }
        case 0xE1:{
            // TODO - E1	POP IY
            Operations::OperationPOP<IY>();
            break;
        }
        case 0xE3:{
            // TODO - E3	EX (SP),IY
            Operations::OperationEX<AddressingModes::AddressWrite<SP, uint16_t>, AddressingModes::AddressRead<SP, uint16_t>, IY>();
            break;
        }
        case 0xE5:{
            // TODO - E5	PUSH IY
            Operations::OperationPUSH<IY>();
            break;
        }
        case 0xE9:{
            // TODO - E9	JP (IY)
            Operations::OperationJP<IY>();
            break;
        }
        case 0xF9:{
            // TODO - F9	LD SP,IY
            Operations::OperationLD<SP, IY>();
            break;
        }
    }
}

static inline void operationTick() {
    uint8_t opcode = MemoryAccess::Read<uint8_t>( nextPC++ );
    switch( opcode ) {
        case 0x00:{
	        // 00,NOP,4,1,1
            OperationBase<Operations::NOP>();
	        break;
        }
        case 0x01:{
            //TODO - 01 n n,LD BC,nn,10,3,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<BC>, AddressingModes::ImmediateExtended>();
            break;
        }
        case 0x02:{
            //TODO - 02,LD (BC),A,7,2,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<BC, uint8_t>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x03:{
            //TODO - 03,INC BC,6,1,1
            OperationBase<Operations::INC16, AddressingModes::RegisterWrite<BC>, AddressingModes::RegisterRead<BC>>();
            break;
        }
        case 0x04:{
            //TODO - 04,INC B,4,1,1
            OperationBase<Operations::INC8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x05:{
            //TODO - 05,DEC B,4,1,1
            OperationBase<Operations::DEC8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x06:{
            //TODO - 06 n,LD B,n,7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::Immediate>();
            break;
        }
        case 0x07:{
            //TODO - 07,RLCA,4,1,1
            OperationBase<Operations::RLCA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x08:{
            //TODO - 08,EX AF,AF’,4,1,1
            OperationBase<Operations::EX<A>>();
            break;
        }
        case 0x09:{
            //TODO - 09,ADD HL,BC,11,3,1
            OperationBase<Operations::ADD16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<BC>>();
            break;
        }
        case 0x0A:{
            //TODO - 0A,LD A,(BC),7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressRead<BC, uint8_t>>();
            break;
        }
        case 0x0B:{
            //TODO - 0B,DEC BC,6,1,1
            OperationBase<Operations::DEC16, AddressingModes::RegisterWrite<BC>, AddressingModes::RegisterRead<BC>>();
            break;
        }
        case 0x0C:{
            //TODO - 0C,INC C,4,1,1
            OperationBase<Operations::INC8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x0D:{
            //TODO - 0D,DEC C,4,1,1
            OperationBase<Operations::DEC8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x0E:{
            //TODO - 0E n,LD C,n,7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::Immediate>();
            break;
        }
        case 0x0F:{
            //TODO - 0F,RRCA,4,1,1
            OperationBase<Operations::RRCA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x10:{
            //TODO - 10 e,DJNZ (PC+e),8/13,2/3,1/1,(met/not met)
            OperationBase<Operations::DJNZ, AddressingModes::Relative>();
            break;
        }
        case 0x11:{
            //TODO - 11 n n,LD DE,nn,10,3,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<DE>, AddressingModes::ImmediateExtended>();
            break;
        }
        case 0x12:{
            //TODO - 12,LD (DE),A,7,2,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<DE, uint8_t>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x13:{
            //TODO - 13,INC DE,6,1,1
            OperationBase<Operations::INC16, AddressingModes::RegisterWrite<DE>, AddressingModes::RegisterRead<DE>>();
            break;
        }
        case 0x14:{
            //TODO - 14,INC D,4,1,1
            OperationBase<Operations::INC8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x15:{
            //TODO - 15,DEC D,4,1,1
            OperationBase<Operations::DEC8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x16:{
            //TODO - 16 n,LD D,n,7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::Immediate>();
            break;
        }
        case 0x17:{
            //TODO - 17,RLA,4,1,1
            OperationBase<Operations::RLA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x18:{
            //TODO - 18 e,JR (PC+e),12,3,1
            OperationBase<Operations::JR1, AddressingModes::Relative>();
            break;
        }
        case 0x19:{
            //TODO - 19,ADD HL,DE,11,3,1
            OperationBase<Operations::ADD16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<DE>>();
            break;
        }
        case 0x1A:{
            //TODO - 1A,LD A,(DE),7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressRead<DE, uint8_t>>();
            break;
        }
        case 0x1B:{
            //TODO - 1B,DEC DE,6,1,1
            OperationBase<Operations::DEC16, AddressingModes::RegisterWrite<DE>, AddressingModes::RegisterRead<DE>>();
            break;
        }
        case 0x1C:{
            //TODO - 1C,INC E,4,1,1
            OperationBase<Operations::INC8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x1D:{
            //TODO - 1D,DEC E,4,1,1
            OperationBase<Operations::DEC8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x1E:{
            //TODO - 1E n,LD E,n,7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::Immediate>();
            break;
        }
        case 0x1F:{
            //TODO - 1F,RRA,4,1,1
            OperationBase<Operations::RRA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x20:{
            //TODO - 20 e,JR NZ,(PC+e),12/7,3/2,1/1,(met/not met)
            OperationBase<Operations::JR, AddressingModes::Relative, AddressingModes::FlagClear<Flags::Zero>>();
            break;
        }
        case 0x21:{
            //TODO - 21 n n,LD HL,nn,10,3,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<HL>, AddressingModes::ImmediateExtended>();
            break;
        }
        case 0x22:{
            //TODO - 22 n n,LD (nn),HL,16,5,3
            OperationBase<Operations::LD16, AddressingModes::AddressWrite<AddressingModes::ImmediateExtended, uint16_t>, AddressingModes::RegisterRead<HL>>();
            break;
        }
        case 0x23:{
            //TODO - 23,INC HL,6,1,1
            OperationBase<Operations::INC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>>();
            break;
        }
        case 0x24:{
            //TODO - 24,INC H,4,1,1
            OperationBase<Operations::INC8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x25:{
            //TODO - 25,DEC H,4,1,1
            OperationBase<Operations::DEC8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x26:{
            //TODO - 26 n,LD H,n,7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::Immediate>();
            break;
        }
        case 0x27:{
            //TODO - 27,DAA,4,1,1
            OperationBase<Operations::DAA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x28:{
            //TODO - 28 e,JR Z,(PC+e),12/7,3/2,1/1,(met/not met)
            OperationBase<Operations::JR, AddressingModes::Relative, AddressingModes::FlagSet<Flags::Zero>>();
            break;
        }
        case 0x29:{
            //TODO - 29,ADD HL,HL,11,3,1
            OperationBase<Operations::ADD16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<HL>>();
            break;
        }
        case 0x2A:{
            //TODO - 2A n n,LD HL,(nn),16,5,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<HL>, AddressingModes::AddressRead<AddressingModes::ImmediateExtended, uint16_t>>();
            break;
        }
        case 0x2B:{
            //TODO - 2B,DEC HL,6,1,1
            OperationBase<Operations::DEC16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>>();
            break;
        }
        case 0x2C:{
            //TODO - 2C,INC L,4,1,1
            OperationBase<Operations::INC8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x2D:{
            //TODO - 2D,DEC L,4,1,1
            OperationBase<Operations::DEC8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x2E:{
            //TODO - 2E n,LD L,n,7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::Immediate>();
            break;
        }
        case 0x2F:{
            //TODO - 2F,CPL,4,1,1
            OperationBase<Operations::CPL>();
            break;
        }
        case 0x30:{
            //TODO - 30 e,JR NC,(PC+e),12/7,3/2,1/1,(met/not met)
            OperationBase<Operations::JR, AddressingModes::Relative, AddressingModes::FlagClear<Flags::Carry>>();
            break;
        }
        case 0x31:{
            //TODO - 31 n n,LD SP,nn,10,3,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<SP>, AddressingModes::ImmediateExtended>();
            break;
        }
        case 0x32:{
            //TODO - 32 n n,LD (nn),A,13,4,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<AddressingModes::ImmediateExtended, uint8_t>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x33:{
            //TODO - 33,INC SP,6,1,1
            OperationBase<Operations::INC16, AddressingModes::RegisterWrite<SP>, AddressingModes::RegisterRead<SP>>();
            break;
        }
        case 0x34:{
            //TODO - 34,INC (HL),11,3,1
            OperationBase<Operations::INC8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x35:{
            //TODO - 35,DEC (HL),11,3,1
            OperationBase<Operations::DEC8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x36:{
            //TODO - 36 n,LD (HL),n,10,3,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::Immediate>();
            break;
        }
        case 0x37:{
            //TODO - 37,SCF,4,1,1
            OperationBase<Operations::SCF, AddressingModes::RegisterWrite<A, false>, AddressingModes::RegisterRead<A, false>>();
            break;
        }
        case 0x38:{
            //TODO - 38 e,JR C,(PC+e),12/7,3/2,1/1,(met/not met)
            OperationBase<Operations::JR, AddressingModes::Relative, AddressingModes::FlagSet<Flags::Carry>>();
            break;
        }
        case 0x39:{
            //TODO - 39,ADD HL,SP,11,3,1
            OperationBase<Operations::ADD16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<SP>>();
            break;
        }
        case 0x3A:{
            //TODO - 3A n n,LD A,(nn),13,4,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressRead<AddressingModes::ImmediateExtended, uint8_t>>();
            break;
        }
        case 0x3B:{
            //TODO - 3B,DEC SP,6,1,1
            OperationBase<Operations::DEC16, AddressingModes::RegisterWrite<SP>, AddressingModes::RegisterRead<SP>>();
            break;
        }
        case 0x3C:{
            //TODO - 3C,INC A,4,1,1
            OperationBase<Operations::INC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x3D:{
            //TODO - 3D,DEC A,4,1,1
            OperationBase<Operations::DEC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x3E:{
            //TODO - 3E n,LD A,n,7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::Immediate>();
            break;
        }
        case 0x3F:{
            //TODO - 3F,CCF,4,1,1
            OperationBase<Operations::CCF, AddressingModes::RegisterWrite<A, false>, AddressingModes::RegisterRead<A, false>>();
            break;
        }
        case 0x40:{
            //TODO - 40,LD B,B,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x41:{
            //TODO - 41,LD B,C,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x42:{
            //TODO - 42,LD B,D,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x43:{
            //TODO - 43,LD B,E,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x44:{
            //TODO - 44,LD B,H,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x45:{
            //TODO - 45,LD B,L,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x46:{
            //TODO - 46,LD B,(HL),7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x47:{
            //TODO - 47,LD B,A,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x48:{
            //TODO - 48,LD C,B,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x49:{
            //TODO - 49,LD C,C,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x4A:{
            //TODO - 4A,LD C,D,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x4B:{
            //TODO - 4B,LD C,E,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x4C:{
            //TODO - 4C,LD C,H,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x4D:{
            //TODO - 4D,LD C,L,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x4E:{
            //TODO - 4E,LD C,(HL),7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x4F:{
            //TODO - 4F,LD C,A,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x50:{
            //TODO - 50,LD D,B,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x51:{
            //TODO - 51,LD D,C,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x52:{
            //TODO - 52,LD D,D,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x53:{
            //TODO - 53,LD D,E,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x54:{
            //TODO - 54,LD D,H,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x55:{
            //TODO - 55,LD D,L,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x56:{
            //TODO - 56,LD D,(HL),7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x57:{
            //TODO - 57,LD D,A,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x58:{
            //TODO - 58,LD E,B,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x59:{
            //TODO - 59,LD E,C,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x5A:{
            //TODO - 5A,LD E,D,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x5B:{
            //TODO - 5B,LD E,E,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x5C:{
            //TODO - 5C,LD E,H,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x5D:{
            //TODO - 5D,LD E,L,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x5E:{
            //TODO - 5E,LD E,(HL),7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x5F:{
            //TODO - 5F,LD E,A,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x60:{
            //TODO - 60,LD H,B,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x61:{
            //TODO - 61,LD H,C,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x62:{
            //TODO - 62,LD H,D,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x63:{
            //TODO - 63,LD H,E,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x64:{
            //TODO - 64,LD H,H,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x65:{
            //TODO - 65,LD H,L,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x66:{
            //TODO - 66,LD H,(HL),7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x67:{
            //TODO - 67,LD H,A,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x68:{
            //TODO - 68,LD L,B,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x69:{
            //TODO - 69,LD L,C,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x6A:{
            //TODO - 6A,LD L,D,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x6B:{
            //TODO - 6B,LD L,E,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x6C:{
            //TODO - 6C,LD L,H,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x6D:{
            //TODO - 6D,LD L,L,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x6E:{
            //TODO - 6E,LD L,(HL),7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x6F:{
            //TODO - 6F,LD L,A,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x70:{
            //TODO - 70,LD (HL),B,7,2,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x71:{
            //TODO - 71,LD (HL),C,7,2,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x72:{
            //TODO - 72,LD (HL),D,7,2,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x73:{
            //TODO - 73,LD (HL),E,7,2,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x74:{
            //TODO - 74,LD (HL),H,7,2,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x75:{
            //TODO - 75,LD (HL),L,7,2,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x76:{
            //TODO - 76,HALT,4,1,1,(repeated till next int)
            OperationBase<Operations::HALT>();
            break;
        }
        case 0x77:{
            //TODO - 77,LD (HL),A,7,2,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x78:{
            //TODO - 78,LD A,B,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x79:{
            //TODO - 79,LD A,C,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x7A:{
            //TODO - 7A,LD A,D,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x7B:{
            //TODO - 7B,LD A,E,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x7C:{
            //TODO - 7C,LD A,H,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x7D:{
            //TODO - 7D,LD A,L,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x7E:{
            //TODO - 7E,LD A,(HL),7,2,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x7F:{
            //TODO - 7F,LD A,A,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x80:{
            //TODO - 80,ADD A,B,4,1,1
            OperationBase<Operations::ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x81:{
            //TODO - 81,ADD A,C,4,1,1
            OperationBase<Operations::ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x82:{
            //TODO - 82,ADD A,D,4,1,1
            OperationBase<Operations::ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x83:{
            //TODO - 83,ADD A,E,4,1,1
            OperationBase<Operations::ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x84:{
            //TODO - 84,ADD A,H,4,1,1
            OperationBase<Operations::ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x85:{
            //TODO - 85,ADD A,L,4,1,1
            OperationBase<Operations::ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x86:{
            //TODO - 86,ADD A,(HL),7,2,1
            OperationBase<Operations::ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x87:{
            //TODO - 87,ADD A,A,4,1,1
            OperationBase<Operations::ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x88:{
            //TODO - 88,ADC A,B,4,1,1
            OperationBase<Operations::ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x89:{
            //TODO - 89,ADC A,C,4,1,1
            OperationBase<Operations::ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x8A:{
            //TODO - 8A,ADC A,D,4,1,1
            OperationBase<Operations::ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x8B:{
            //TODO - 8B,ADC A,E,4,1,1
            OperationBase<Operations::ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x8C:{
            //TODO - 8C,ADC A,H,4,1,1
            OperationBase<Operations::ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x8D:{
            //TODO - 8D,ADC A,L,4,1,1
            OperationBase<Operations::ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x8E:{
            //TODO - 8E,ADC A,(HL),7,2,1
            OperationBase<Operations::ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x8F:{
            //TODO - 8F,ADC A,A,4,1,1
            OperationBase<Operations::ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x90:{
            //TODO - 90,SUB B,4,1,1
            OperationBase<Operations::SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x91:{
            //TODO - 91,SUB C,4,1,1
            OperationBase<Operations::SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x92:{
            //TODO - 92,SUB D,4,1,1
            OperationBase<Operations::SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x93:{
            //TODO - 93,SUB E,4,1,1
            OperationBase<Operations::SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x94:{
            //TODO - 94,SUB H,4,1,1
            OperationBase<Operations::SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x95:{
            //TODO - 95,SUB L,4,1,1
            OperationBase<Operations::SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x96:{
            //TODO - 96,SUB (HL),7,2,1
            OperationBase<Operations::SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x97:{
            //TODO - 97,SUB A,4,1,1
            OperationBase<Operations::SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x98:{
            //TODO - 98,SBC A,B,4,1,1
            OperationBase<Operations::SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0x99:{
            //TODO - 99,SBC A,C,4,1,1
            OperationBase<Operations::SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0x9A:{
            //TODO - 9A,SBC A,D,4,1,1
            OperationBase<Operations::SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0x9B:{
            //TODO - 9B,SBC A,E,4,1,1
            OperationBase<Operations::SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0x9C:{
            //TODO - 9C,SBC A,H,4,1,1
            OperationBase<Operations::SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0x9D:{
            //TODO - 9D,SBC A,L,4,1,1
            OperationBase<Operations::SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0x9E:{
            //TODO - 9E,SBC A,(HL),7,2,1
            OperationBase<Operations::SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0x9F:{
            //TODO - 9F,SBC A,A,4,1,1
            OperationBase<Operations::SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0xA0:{
            //TODO - A0,AND B,4,1,1
            OperationBase<Operations::AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0xA1:{
            //TODO - A1,AND C,4,1,1
            OperationBase<Operations::AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0xA2:{
            //TODO - A2,AND D,4,1,1
            OperationBase<Operations::AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0xA3:{
            //TODO - A3,AND E,4,1,1
            OperationBase<Operations::AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0xA4:{
            //TODO - A4,AND H,4,1,1
            OperationBase<Operations::AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0xA5:{
            //TODO - A5,AND L,4,1,1
            OperationBase<Operations::AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0xA6:{
            //TODO - A6,AND (HL),7,2,1
            OperationBase<Operations::AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0xA7:{
            //TODO - A7,AND A,4,1,1
            OperationBase<Operations::AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0xA8:{
            //TODO - A8,XOR B,4,1,1
            OperationBase<Operations::XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0xA9:{
            //TODO - A9,XOR C,4,1,1
            OperationBase<Operations::XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0xAA:{
            //TODO - AA,XOR D,4,1,1
            OperationBase<Operations::XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0xAB:{
            //TODO - AB,XOR E,4,1,1
            OperationBase<Operations::XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0xAC:{
            //TODO - AC,XOR H,4,1,1
            OperationBase<Operations::XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0xAD:{
            //TODO - AD,XOR L,4,1,1
            OperationBase<Operations::XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0xAE:{
            //TODO - AE,XOR (HL),7,2,1
            OperationBase<Operations::XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0xAF:{
            //TODO - AF,XOR A,4,1,1
            OperationBase<Operations::XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0xB0:{
            //TODO - B0,OR B,4,1,1
            OperationBase<Operations::OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0xB1:{
            //TODO - B1,OR C,4,1,1
            OperationBase<Operations::OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0xB2:{
            //TODO - B2,OR D,4,1,1
            OperationBase<Operations::OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0xB3:{
            //TODO - B3,OR E,4,1,1
            OperationBase<Operations::OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0xB4:{
            //TODO - B4,OR H,4,1,1
            OperationBase<Operations::OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0xB5:{
            //TODO - B5,OR L,4,1,1
            OperationBase<Operations::OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0xB6:{
            //TODO - B6,OR (HL),7,2,1
            OperationBase<Operations::OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0xB7:{
            //TODO - B7,OR A,4,1,1
            OperationBase<Operations::OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0xB8:{
            //TODO - B8,CP B,4,1,1
            OperationBase<Operations::CP, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, true>>();
            break;
        }
        case 0xB9:{
            //TODO - B9,CP C,4,1,1
            OperationBase<Operations::CP, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<BC, false>>();
            break;
        }
        case 0xBA:{
            //TODO - BA,CP D,4,1,1
            OperationBase<Operations::CP, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, true>>();
            break;
        }
        case 0xBB:{
            //TODO - BB,CP E,4,1,1
            OperationBase<Operations::CP, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<DE, false>>();
            break;
        }
        case 0xBC:{
            //TODO - BC,CP H,4,1,1
            OperationBase<Operations::CP, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, true>>();
            break;
        }
        case 0xBD:{
            //TODO - BD,CP L,4,1,1
            OperationBase<Operations::CP, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<HL, false>>();
            break;
        }
        case 0xBE:{
            //TODO - BE,CP (HL),7,2,1
            OperationBase<Operations::CP, AddressingModes::RegisterRead<A, true>, AddressingModes::AddressRead<HL, uint8_t>>();
            break;
        }
        case 0xBF:{
            //TODO - BF,CP A,4,1,1
            OperationBase<Operations::CP, AddressingModes::RegisterRead<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0xC0:{
            //TODO - C0,RET NZ,11/5,3/1,1/1,(met/not met)
            OperationBase<Operations::RET, AddressingModes::FlagClear<Flags::Zero>>();
            break;
        }
        case 0xC1:{
            //TODO - C1,POP BC,10,3,1
            Operations::OperationPop<BC>();
            break;
        }
        case 0xC2:{
            //TODO - C2 n n,JP NZ,(nn),10,3,1,(met or not)
            Operations::OperationJP<AddressingModes::FlagClear<Flags::Zero>>();
            break;
        }
        case 0xC3:{
            //TODO - C3 n n,JP (nn),10,3,1
            Operations::OperationJP();
            break;
        }
        case 0xC4:{
            //TODO - C4 n n,CALL NZ,(nn),17/10,5/3,1/1,(met/not met)
            Operations::OperationCall<AddressingModes::FlagClear<Flags::Zero>>();
            break;
        }
        case 0xC5:{
            //TODO - C5,PUSH BC,11,3,1
            Operations::OperationPush<BC>();
            break;
        }
        case 0xC6:{
            //TODO - C6 n,ADD A,n,7,2,1
            OperationBase<Operations::ADD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::Immediate>();
            break;
        }
        case 0xC7:{
            //TODO - C7,RST 0H,11,3,1
            Operations::OperationRst<0x00>();
            break;
        }
        case 0xC8:{
            //TODO - C8,RET Z,11/5,3/1,1/1,(met/not met)
            OperationBase<Operations::RET, AddressingModes::FlagSet<Flags::Zero>>();
            break;
        }
        case 0xC9:{
            //TODO - C9,RET,10,3,1
            OperationBase<Operations::RET1>();
            break;
        }
        case 0xCA:{
            //TODO - CA n n,JP Z,(nn),10,3,1,(always same)
            Operations::OperationJP<AddressingModes::FlagSet<Flags::Zero>>();
            break;
        }
        case 0xCB:{
            extendedOpBITS();
            break;
        }
        case 0xCC:{
            //TODO - CC n n,CALL Z,(nn),17/10,5/3,1/1,(met/not met)
            Operations::OperationCall<AddressingModes::FlagSet<Flags::Zero>>();
            break;
        }
        case 0xCD:{
            //TODO - CD n n,CALL (nn),17,5,1
            Operations::OperationCall();
            break;
        }
        case 0xCE:{
            //TODO - CE n,ADC A,n,7,2,1
            OperationBase<Operations::ADC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::Immediate>();
            break;
        }
        case 0xCF:{
            //TODO - CF,RST 8H,11,3,1
            Operations::OperationRst<0x08>();
            break;
        }
        case 0xD0:{
            //TODO - D0,RET NC,5,1,1
            OperationBase<Operations::RET, AddressingModes::FlagClear<Flags::Carry>>();
            break;
        }
        case 0xD1:{
            //TODO - D1,POP DE,10,3,1
            Operations::OperationPop<DE>();
            break;
        }
        case 0xD2:{
            //TODO - D2 n n,JP NC,(nn),10,3,1,(met or not)
            Operations::OperationJP<AddressingModes::FlagClear<Flags::Carry>>();
            break;
        }
        case 0xD3:{
            //TODO - D3 n,OUT (n),A,11,3,1
            OperationBase<Operations::OUT, AddressingModes::PortWrite<AddressingModes::Immediate>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0xD4:{
            //TODO - D4 n n,CALL NC,(nn),17/10,5/3,1/1,(met/not met)
            Operations::OperationCall<AddressingModes::FlagClear<Flags::Carry>>();
            break;
        }
        case 0xD5:{
            //TODO - D5,PUSH DE,11,3,1
            Operations::OperationPush<DE>();
            break;
        }
        case 0xD6:{
            //TODO - D6 n,SUB n,7,2,1
            OperationBase<Operations::SUB, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::Immediate>();
            break;
        }
        case 0xD7:{
            //TODO - D7,RST 10H,11,3,1
            Operations::OperationRst<0x10>();
            break;
        }
        case 0xD8:{
            //TODO - D8,RET C,5,1,1
            OperationBase<Operations::RET, AddressingModes::FlagSet<Flags::Carry>>();
            break;
        }
        case 0xD9:{
            //TODO - D9,EXX,4,1,1
            OperationBase<Operations::EXX>();
            break;
        }
        case 0xDA:{
            //TODO - DA n n,JP C,(nn),10,3,1,(met or not)
            Operations::OperationJP<AddressingModes::FlagSet<Flags::Carry>>();
            break;
        }
        case 0xDB:{
            //TODO - DB n,IN A,(n),11,3,1
            OperationBase<Operations::INA, AddressingModes::RegisterWrite<A, true>, AddressingModes::PortRead<AddressingModes::Immediate>>();
            break;
        }
        case 0xDC:{
            //TODO - DC n n,CALL C,(nn),17/10,5/3,1
            Operations::OperationCall<AddressingModes::FlagSet<Flags::Carry>>();
            break;
        }
        case 0xDD:{
            extendedOpIX();
            break;
        }
        case 0xDE:{
            //TODO - DE n,SBC A,n
            OperationBase<Operations::SBC8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::Immediate>();
            break;
        }
        case 0xDF:{
            //TODO - DF,RST 18H
            Operations::OperationRst<0x18>();
            break;
        }
        case 0xE0:{
            //TODO - E0,RET PO
            // Parity clear for ODD
            OperationBase<Operations::RET, AddressingModes::FlagClear<Flags::ParityOverflow>>();
            break;
        }
        case 0xE1:{
            //TODO - E1,POP HL
            Operations::OperationPop<HL>();
            break;
        }
        case 0xE2:{
            //TODO - E2 n n,JP PO,(nn)
            Operations::OperationJP<AddressingModes::FlagClear<Flags::ParityOverflow>>();
            break;
        }
        case 0xE3:{
            //TODO - E3,EX (SP),HL
            OperationBase<Operations::EX_mem<HL>, AddressingModes::AddressWrite<SP, uint8_t>, AddressingModes::AddressRead<SP, uint8_t>>();
            break;
        }
        case 0xE4:{
            //TODO - E4 n n,CALL PO,(nn)
            Operations::OperationCall<AddressingModes::FlagClear<Flags::ParityOverflow>>();
            break;
        }
        case 0xE5:{
            //TODO - E5,PUSH HL
            Operations::OperationPush<HL>();
            break;
        }
        case 0xE6:{
            //TODO - E6 n,AND n
            OperationBase<Operations::AND, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::Immediate>();
            break;
        }
        case 0xE7:{
            //TODO - E7,RST 20H
            Operations::OperationRst<0x20>();
            break;
        }
        case 0xE8:{
            //TODO - E8,RET PE
            OperationBase<Operations::RET, AddressingModes::FlagSet<Flags::ParityOverflow>>();
            break;
        }
        case 0xE9:{
            //TODO - E9,JP (HL)
            OperationBase<Operations::JP1, AddressingModes::AddressRead<HL, uint16_t>>();
            break;
        }
        case 0xEA:{
            //TODO - EA n n,JP PE,(nn)
            Operations::OperationJP<AddressingModes::FlagSet<Flags::ParityOverflow>>();
            break;
        }
        case 0xEB:{
            //TODO - EB,EX DE,HL
            OperationBase<Operations::EX<DE, HL>>();
            break;
        }
        case 0xEC:{
            //TODO - EC n n,CALL PE,(nn)
            Operations::OperationCall<AddressingModes::FlagSet<Flags::ParityOverflow>>();
            break;
        }
        case 0xED:{
            extendedOpEXTD();
            break;
        }
        case 0xEE:{
            //TODO - EE n,XOR n
            OperationBase<Operations::XOR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::Immediate>();
            break;
        }
        case 0xEF:{
            //TODO - EF,RST 28H
            Operations::OperationRst<0x28>();
            break;
        }
        case 0xF0:{
            //TODO - F0,RET P
            OperationBase<Operations::RET, AddressingModes::FlagClear<Flags::Sign>>();
            break;
        }
        case 0xF1:{
            //TODO - F1,POP AF
            Operations::OperationPop<A>();
            break;
        }
        case 0xF2:{
            //TODO - F2 n n,JP P,(nn)
            Operations::OperationJP<AddressingModes::FlagClear<Flags::Sign>>();
            break;
        }
        case 0xF3:{
            //TODO - F3,DI
            OperationBase<Operations::EI_DI<false>>();
            break;
        }
        case 0xF4:{
            //TODO - F4 n n,CALL P,(nn)
            Operations::OperationCall<AddressingModes::FlagClear<Flags::Sign>>();
            break;
        }
        case 0xF5:{
            //TODO - F5,PUSH AF
            Operations::OperationPush<A>();
            break;
        }
        case 0xF6:{
            //TODO - F6 n,OR n
            OperationBase<Operations::OR, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>, AddressingModes::Immediate>();
            break;
        }
        case 0xF7:{
            //TODO - F7,RST 30H
            Operations::OperationRst<0x30>();
            break;
        }
        case 0xF8:{
            //TODO - F8,RET M
            OperationBase<Operations::RET, AddressingModes::FlagSet<Flags::Sign>>();
            break;
        }
        case 0xF9:{
            //TODO - F9,LD SP,HL
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<SP>, AddressingModes::RegisterRead<HL>>();
            break;
        }
        case 0xFA:{
            //TODO - FA n n,JP M,(nn)
            Operations::OperationJP<AddressingModes::FlagSet<Flags::Sign>>();
            break;
        }
        case 0xFB:{
            //TODO - FB,EI
            OperationBase<Operations::EI_DI<true>>();
            break;
        }
        case 0xFC:{
            //TODO - FC n n,CALL M,(nn)
            Operations::OperationCall<AddressingModes::FlagSet<Flags::Sign>>();
            break;
        }
        case 0xFD:{
            extendedOpIY();
            break;
        }
        case 0xFE:{
            //TODO - FE n,CP n
            OperationBase<Operations::CP, AddressingModes::RegisterRead<A, true>, AddressingModes::Immediate>();
            break;
        }
        case 0xFF:{
            //TODO - FF,RST 38H
            Operations::OperationRst<0x38>();
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
        
}

void z80Tick() {
    debugPrintState();
    test();
    nextPC = PC;
    operationTick();
    PC = nextPC;
}