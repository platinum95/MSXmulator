#include <functional>
#include <iostream>
#include <stdint.h>

/* Template bases */

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


template <auto operation>
    requires IsInvocableOperationNoParamNoRet<decltype(operation)>
void OperationBase() {
    std::cout << "No Param, no ret\n";
    operation();
}

template <auto operation, auto readO1_OrWrite>
    requires 
        ( IsSingleParamNoWriteoutOp<operation, readO1_OrWrite> ) ||
        ( IsNoParamWriteoutOp<operation, readO1_OrWrite> )
void OperationBase() {
    if constexpr( IsSingleParamNoWriteoutOp<operation, readO1_OrWrite> ) {
        std::cout << "Single param, no ret\n";
        operation( readO1_OrWrite() );
    }
    else if constexpr( IsNoParamWriteoutOp<operation, readO1_OrWrite> ) {
        readO1_OrWrite( operation() );
        std::cout << "No param, with ret\n";
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
    std::cout << "Dual param, no ret\n";
}

template <auto operation, auto write, auto readO1>
    requires IsSingleParamWriteoutOp<operation, write, readO1>
void OperationBase() {
    write( operation( readO1() ) );
    std::cout << "Single param, with ret\n";
}

template <auto operation, auto write, auto readO1, auto readO2>
    requires IsDualParamWriteoutOp<operation, readO1, readO2, write>
void OperationBase() {
    write( operation( readO1(), readO2() ) );
    std::cout << "Dual param, with ret\n";
}

static inline uint8_t readSomething() {
    return 0;
}

static inline uint16_t readSomething_invalid() {
    return 0;
}

static inline void operateSomethingNoRetNoParam() {
}

static inline uint8_t operateSomethingRetNoParam() {
    return 0;
}

static inline uint8_t operateSomethingRetSingleParam( uint8_t a ) {
    (void) a;
    return 0;
}
static inline uint8_t operateSomethingRetDualParam( uint8_t a, uint8_t b ) {
    (void) a;
    (void) b;
    return 0;
}

static inline void operateSomethingNoRetSingleParam( uint8_t a ) {
    (void) a;
}

static inline void operateSomethingNoRetDualParam( uint8_t a, uint8_t b ) {
    (void) a;
    (void) b;
}

static inline void writeSomething( uint8_t a ) {
    (void) a;
}

static inline void writeSomething_invalid( uint16_t a ) {
    (void) a;
}

static uint8_t imaginaryAccumulator;
static inline uint8_t LDA_Op( uint8_t O1 ) {
    std::cout << "Executing LDA Op\n";
    return O1;
}

static inline void LDA_Write( uint8_t value ) {
    imaginaryAccumulator = value;
    std::cout << "Executing LDA writeout to acc\n";
}

static inline void operationTick();

void z80Tick() {
    OperationBase<operateSomethingNoRetNoParam>();
    OperationBase<operateSomethingRetNoParam, writeSomething>();
    OperationBase<operateSomethingRetSingleParam, writeSomething, readSomething>();
    OperationBase<operateSomethingRetDualParam, writeSomething, readSomething, readSomething>();
    OperationBase<operateSomethingNoRetSingleParam, readSomething>();
    OperationBase<operateSomethingNoRetDualParam, readSomething, readSomething>();

    operationTick();
}

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

struct RegisterPair2 {
    uint8_t r1;
    uint8_t r2;

    uint16_t GetWord() const { /* TODO */ return 0; }
    void StoreWord() {/* TODO */ }
};

union RegisterPair {
    struct ByteRegisters {
        uint8_t r1;
        uint8_t r2;
    } byteRegisters;
    uint16_t word;
};

struct RegisterSet {
    RegisterPair mainRegister;
    RegisterPair alternateRegister;
};

struct RegisterSet2 {
    RegisterPair mainRegister;
    RegisterPair alternateRegister;
};

static RegisterSet A;
static RegisterSet BC;
static RegisterSet DE;
static RegisterSet HL;

/***********************************
*  Memory access
**********************************/
namespace MemoryAccess {

static inline uint16_t DualByteToWord( uint8_t msb, uint8_t lsb ) {
    return ( static_cast<uint16_t>( msb ) << 8 ) | static_cast<uint16_t>( lsb );
}

static inline void MemoryAccess( uint16_t addressBus, uint8_t &dataBus, bool writeLine ) {
    // TODO
    (void) addressBus;
    (void) dataBus;
    (void) writeLine;
}

static inline uint8_t MemoryAccess( uint16_t addressBus ) {
    // TODO
    uint8_t dataBus = 0;
    MemoryAccess( addressBus, dataBus, false );
    return dataBus;
}

static inline uint8_t ReadU8( uint16_t addressBus ) {
    return MemoryAccess( addressBus );
}

static inline uint16_t ReadU16( uint16_t addressBus ) {
    return DualByteToWord( MemoryAccess( addressBus + 1 ), MemoryAccess( addressBus ) );
}


template<typename returnType>
    requires IsByteOrWord<returnType>
static inline returnType Read( uint16_t addressBus ) {
    if constexpr ( std::is_same<returnType, uint8_t>::value ) {
        return ReadU8( addressBus );
    }
    else {
        return ReadU16( addressBus );
    }
}

template<IsByteOrWord WriteType>
static inline void Write( uint16_t addressBus, WriteType data ) {
    if constexpr ( std::is_same<WriteType, uint8_t>::value ) {
        MemoryAccess( addressBus, data, true );
    }
    else {
        uint8_t holder = static_cast<uint8_t>( data & 0x00FF );
        MemoryAccess( addressBus, holder, true );
        holder = static_cast<uint8_t>( ( data >> 8 ) & 0x00FF );
        MemoryAccess( addressBus, holder, true );
    }
}


}

/***********************************
*  Addressing modes
**********************************/
namespace AddressingModes {

template<uint8_t offset>
static inline uint8_t GetOpcodeByte() {
    return MemoryAccess::ReadU8( PC + offset );
}

template<uint8_t offset>
static inline uint16_t GetOpcodeWord() {
    return MemoryAccess::ReadU16( PC + offset );
}

// Actual addressing modes
template<uint8_t offset=0>
static inline uint8_t Immediate() {
    return GetOpcodeByte<offset>();
}

template<uint8_t offset=0>
static inline uint16_t ImmediateExtended() {
    return GetOpcodeWord<offset>();
}

static inline uint8_t ModifiedPageZero() {
    // TODO
    return 0;
}

template<uint8_t offset=0>
static inline int8_t Relative() {
    // TODO - verify the unsigned->signed cast
    return static_cast<int8_t>( Immediate<offset>() );
}

static inline uint8_t Bit() {
    // TODO
    return 0;
}

template<uint8_t offset=0>
const auto ExtendedAddress = ImmediateExtended<offset>;


template<uint8_t offset=0>
static inline uint16_t IndexedIXAddress() {
    return IX + Relative<offset>();
}

template<uint8_t offset=0>
static inline uint16_t IndexedIYAddress() {
    return IY + Relative<offset>();
}

// Read high/low byte of given register set
template<RegisterSet &registerSet, bool lowByte>
static inline uint8_t RegisterRead() {
    if constexpr ( lowByte ) {
        return registerSet.mainRegister.byteRegisters.r1;
    }
    else {
        return registerSet.mainRegister.byteRegisters.r2;
    }
}

// Read word of given register set
template<RegisterSet &registerSet>
static inline uint16_t RegisterRead() {
    return registerSet.mainRegister.word;
}

// Read word of given register
template<uint16_t &reg>
static inline uint16_t RegisterRead() {
    return reg;
}

// Write high/low byte of given register set
template<RegisterSet &registerSet, bool lowByte>
static inline void RegisterWrite( uint8_t data ) {
    if constexpr ( lowByte ) {
        registerSet.mainRegister.byteRegisters.r1 = data;
    }
    else {
        registerSet.mainRegister.byteRegisters.r2 = data;
    }
}

// Write word of given register set
template<RegisterSet &registerSet>
static inline void RegisterWrite( uint16_t data ) {
    registerSet.mainRegister.word = data;
}

// Write word of given register
template<uint16_t &reg>
static inline void RegisterWrite( uint16_t data ) {
    reg = data;
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

// Read 8/16 bit value from address given by register word
template<RegisterSet &registerSet, IsByteOrWord ReturnType>
static inline ReturnType AddressRead() {
    return AddressRead<RegisterRead<registerSet>, ReturnType>();
}

// Write 8/16 bit value to address given by register word
template<RegisterSet &registerSet, IsByteOrWord WriteType>
static inline void AddressWrite( WriteType data ) {
    AddressWrite<RegisterRead<registerSet>>( data );
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

namespace Operations {
static inline void NOP() {
}

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

template<IsByteOrWord T>
static inline T INC( T O1 ) {
    return O1 + 1;
}
static inline uint8_t INC8( uint8_t O1 ) {
    return INC<uint8_t>( O1 );
}
static inline uint16_t INC16( uint16_t O1 ) {
    return INC<uint16_t>( O1 );
}

template<IsByteOrWord T>
static inline T DEC( T O1 ) {
    return O1 - 1;
}
static inline uint8_t DEC8( uint8_t O1 ) {
    return DEC<uint8_t>( O1 );
}
static inline uint16_t DEC16( uint16_t O1 ) {
    return DEC<uint16_t>( O1 );
}

template<IsByteOrWord T>
static inline T ADD( T O1, T O2 ) {
    return O1 + O2;
}
static inline uint8_t ADD8( uint8_t O1, uint8_t O2 ) {
    return ADD<uint8_t>( O1, O2 );
}
static inline uint16_t ADD16( uint16_t O1, uint16_t O2 ) {
    return ADD<uint16_t>( O1, O2 );
}

template<IsByteOrWord T>
static inline T ADC( T O1, T O2 ) {
    return O1 + O2 + 0; // TODO - get carry flag
}
static inline uint8_t ADC8( uint8_t O1, uint8_t O2 ) {
    return ADC<uint8_t>( O1, O2 );
}
static inline uint16_t ADC16( uint16_t O1, uint16_t O2 ) {
    return ADC<uint16_t>( O1, O2 );
}

static inline uint16_t EX( uint16_t O1 ) {
    // TODO - toggle register select flag
    (void) O1;
    return 01;
}

static inline uint8_t RLCA( uint8_t O1 ) {
    // TODO
    return O1 << 1;
}

static inline uint8_t RRCA( uint8_t O1 ) {
    // TODO
    return O1 >> 1;
}

static inline uint16_t DJNZ( int8_t O1 ) {
    // TODO
    return PC + O1;
}

static inline uint8_t RLA( uint8_t O1 ) {
    // TODO
    return O1 << 1;
}

static inline uint16_t JR( int8_t O1, bool condition=true ) {
    // TODO
    if ( condition ) {
        return PC + O1;
    }
    else {
        return PC;
    }
}

static inline uint8_t RRA( uint8_t O1 ) {
    // TODO
    return O1 >> 1;
}

static inline uint8_t DAA( uint8_t O1 ) {
    // TODO
    (void) O1;
    return O1;
}

static inline uint8_t SCF( uint8_t O1 ) {
    // TODO
    (void) O1;
    return O1 | ( 1 << static_cast<uint8_t>( Flags::Carry ) );
}

static inline uint8_t CCF( uint8_t O1 ) {
    // TODO
    return O1 ^ ( 1u << static_cast<uint8_t>( Flags::Carry ) );
}

static inline void HALT() {
    // TODO
}

}

static inline void extendedOpBITS() {
    uint8_t opcode = MemoryAccess::ReadU8( PC++ );

    switch( opcode ) {
        // TODO
    }
}

static inline void extendedOpIX() {
    uint8_t opcode = MemoryAccess::ReadU8( PC++ );
    switch( opcode ) {
        // TODO 
    }
}

static inline void extendedOpEXTD() {
    uint8_t opcode = MemoryAccess::ReadU8( PC++ );
    switch( opcode ) {
        // TODO
    }
}

static inline void extendedOpIY() {
    uint8_t opcode = MemoryAccess::ReadU8( PC++ );
    switch( opcode ) {
        // TODO
    }
}

static inline void operationTick() {
    uint8_t opcode = MemoryAccess::ReadU8( PC++ );
    switch( opcode ) {
        case 0x00:{
	        // 00,NOP,4,1,1
            OperationBase<Operations::NOP>();
	        break;
        }
        case 0x01:{
            //TODO - 01 n n,LD BC,nn,10,3,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<BC>, AddressingModes::AddressRead<AddressingModes::ImmediateExtended<0>, uint16_t>>();
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
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::Immediate<0>>();
            break;
        }
        case 0x07:{
            //TODO - 07,RLCA,4,1,1
            OperationBase<Operations::RLCA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x08:{
            //TODO - 08,EX AF,AF’,4,1,1
            OperationBase<Operations::EX, AddressingModes::RegisterWrite<A>, AddressingModes::RegisterRead<A>>();
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
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, false>, AddressingModes::Immediate<0>>();
            break;
        }
        case 0x0F:{
            //TODO - 0F,RRCA,4,1,1
            OperationBase<Operations::RLCA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x10:{
            //TODO - 10 e,DJNZ (PC+e),8/13,2/3,1/1,(met/not met)
            OperationBase<Operations::DJNZ, AddressingModes::RegisterWrite<PC>, AddressingModes::Relative>();
            break;
        }
        case 0x11:{
            //TODO - 11 n n,LD DE,nn,10,3,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<DE>, AddressingModes::ImmediateExtended<0>>();
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
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, true>, AddressingModes::Immediate<0>>();
            break;
        }
        case 0x17:{
            //TODO - 17,RLA,4,1,1
            OperationBase<Operations::RLA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x18:{
            //TODO - 18 e,JR (PC+e),12,3,1
            OperationBase<Operations::JR, AddressingModes::RegisterWrite<PC>, AddressingModes::Relative, [](){return true;}>();
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
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<DE, false>, AddressingModes::Immediate<0>>();
            break;
        }
        case 0x1F:{
            //TODO - 1F,RRA,4,1,1
            OperationBase<Operations::RRA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x20:{
            //TODO - 20 e,JR NZ,(PC+e),12/7,3/2,1/1,(met/not met)
            OperationBase<Operations::JR, AddressingModes::RegisterWrite<PC>, AddressingModes::FlagClear<Flags::Zero>, AddressingModes::Relative>();
            break;
        }
        case 0x21:{
            //TODO - 21 n n,LD HL,nn,10,3,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<HL>, AddressingModes::ImmediateExtended<0>>();
            break;
        }
        case 0x22:{
            //TODO - 22 n n,LD (nn),HL,16,5,3
            OperationBase<Operations::LD16, AddressingModes::AddressWrite<AddressingModes::ImmediateExtended<0>, uint16_t>, AddressingModes::RegisterRead<HL>>();
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
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, true>, AddressingModes::Immediate<0>>();
            break;
        }
        case 0x27:{
            //TODO - 27,DAA,4,1,1
            OperationBase<Operations::DAA, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x28:{
            //TODO - 28 e,JR Z,(PC+e),12/7,3/2,1/1,(met/not met)
            OperationBase<Operations::JR, AddressingModes::RegisterWrite<PC>, AddressingModes::FlagSet<Flags::Zero>, AddressingModes::Relative>();
            break;
        }
        case 0x29:{
            //TODO - 29,ADD HL,HL,11,3,1
            OperationBase<Operations::ADD16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<HL>>();
            break;
        }
        case 0x2A:{
            //TODO - 2A n n,LD HL,(nn),16,5,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<HL>, AddressingModes::ImmediateExtended<0>>();
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
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<HL, false>, AddressingModes::Immediate<0>>();
            break;
        }
        case 0x2F:{
            //TODO - 2F,CPL,4,1,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::RegisterRead<A, true>>();
            break;
        }
        case 0x30:{
            //TODO - 30 e,JR NC,(PC+e),12/7,3/2,1/1,(met/not met)
            OperationBase<Operations::JR, AddressingModes::RegisterWrite<PC>, AddressingModes::FlagClear<Flags::Carry>, AddressingModes::Relative>();
            break;
        }
        case 0x31:{
            //TODO - 31 n n,LD SP,nn,10,3,1
            OperationBase<Operations::LD16, AddressingModes::RegisterWrite<SP>, AddressingModes::ImmediateExtended<0>>();
            break;
        }
        case 0x32:{
            //TODO - 32 n n,LD (nn),A,13,4,1
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<AddressingModes::ImmediateExtended<0>, uint8_t>, AddressingModes::RegisterRead<A, true>>();
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
            OperationBase<Operations::LD8, AddressingModes::AddressWrite<HL, uint8_t>, AddressingModes::Immediate<0>>();
            break;
        }
        case 0x37:{
            //TODO - 37,SCF,4,1,1
            OperationBase<Operations::SCF, AddressingModes::RegisterWrite<A, false>, AddressingModes::RegisterRead<A, false>>();
            break;
        }
        case 0x38:{
            //TODO - 38 e,JR C,(PC+e),12/7,3/2,1/1,(met/not met)
            OperationBase<Operations::JR, AddressingModes::RegisterWrite<PC>, AddressingModes::FlagSet<Flags::Carry>, AddressingModes::Relative>();
            break;
        }
        case 0x39:{
            //TODO - 39,ADD HL,SP,11,3,1
            OperationBase<Operations::ADD16, AddressingModes::RegisterWrite<HL>, AddressingModes::RegisterRead<HL>, AddressingModes::RegisterRead<SP>>();
            break;
        }
        case 0x3A:{
            //TODO - 3A n n,LD A,(nn),13,4,1
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::AddressRead<AddressingModes::ImmediateExtended<0>, uint8_t>>();
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
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<A, true>, AddressingModes::Immediate<0>>();
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
            OperationBase<Operations::LD8, AddressingModes::RegisterWrite<BC, true>, AddressingModes::RegisterRead<HL, true>>();
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
            break;
        }
        case 0x81:{
            //TODO - 81,ADD A,C,4,1,1
            break;
        }
        case 0x82:{
            //TODO - 82,ADD A,D,4,1,1
            break;
        }
        case 0x83:{
            //TODO - 83,ADD A,E,4,1,1
            break;
        }
        case 0x84:{
            //TODO - 84,ADD A,H,4,1,1
            break;
        }
        case 0x85:{
            //TODO - 85,ADD A,L,4,1,1
            break;
        }
        case 0x86:{
            //TODO - 86,ADD A,(HL),7,2,1
            break;
        }
        case 0x87:{
            //TODO - 87,ADD A,A,4,1,1
            break;
        }
        case 0x88:{
            //TODO - 88,ADC A,B,4,1,1
            break;
        }
        case 0x89:{
            //TODO - 89,ADC A,C,4,1,1
            break;
        }
        case 0x8A:{
            //TODO - 8A,ADC A,D,4,1,1
            break;
        }
        case 0x8B:{
            //TODO - 8B,ADC A,E,4,1,1
            break;
        }
        case 0x8C:{
            //TODO - 8C,ADC A,H,4,1,1
            break;
        }
        case 0x8D:{
            //TODO - 8D,ADC A,L,4,1,1
            break;
        }
        case 0x8E:{
            //TODO - 8E,ADC A,(HL),7,2,1
            break;
        }
        case 0x8F:{
            //TODO - 8F,ADC A,A,4,1,1
            break;
        }
        case 0x90:{
            //TODO - 90,SUB B,4,1,1
            break;
        }
        case 0x91:{
            //TODO - 91,SUB C,4,1,1
            break;
        }
        case 0x92:{
            //TODO - 92,SUB D,4,1,1
            break;
        }
        case 0x93:{
            //TODO - 93,SUB E,4,1,1
            break;
        }
        case 0x94:{
            //TODO - 94,SUB H,4,1,1
            break;
        }
        case 0x95:{
            //TODO - 95,SUB L,4,1,1
            break;
        }
        case 0x96:{
            //TODO - 96,SUB (HL),7,2,1
            break;
        }
        case 0x97:{
            //TODO - 97,SUB A,4,1,1
            break;
        }
        case 0x98:{
            //TODO - 98,SBC A,B,4,1,1
            break;
        }
        case 0x99:{
            //TODO - 99,SBC A,C,4,1,1
            break;
        }
        case 0x9A:{
            //TODO - 9A,SBC A,D,4,1,1
            break;
        }
        case 0x9B:{
            //TODO - 9B,SBC A,E,4,1,1
            break;
        }
        case 0x9C:{
            //TODO - 9C,SBC A,H,4,1,1
            break;
        }
        case 0x9D:{
            //TODO - 9D,SBC A,L,4,1,1
            break;
        }
        case 0x9E:{
            //TODO - 9E,SBC A,(HL),7,2,1
            break;
        }
        case 0x9F:{
            //TODO - 9F,SBC A,A,4,1,1
            break;
        }
        case 0xA0:{
            //TODO - A0,AND B,4,1,1
            break;
        }
        case 0xA1:{
            //TODO - A1,AND C,4,1,1
            break;
        }
        case 0xA2:{
            //TODO - A2,AND D,4,1,1
            break;
        }
        case 0xA3:{
            //TODO - A3,AND E,4,1,1
            break;
        }
        case 0xA4:{
            //TODO - A4,AND H,4,1,1
            break;
        }
        case 0xA5:{
            //TODO - A5,AND L,4,1,1
            break;
        }
        case 0xA6:{
            //TODO - A6,AND (HL),7,2,1
            break;
        }
        case 0xA7:{
            //TODO - A7,AND A,4,1,1
            break;
        }
        case 0xA8:{
            //TODO - A8,XOR B,4,1,1
            break;
        }
        case 0xA9:{
            //TODO - A9,XOR C,4,1,1
            break;
        }
        case 0xAA:{
            //TODO - AA,XOR D,4,1,1
            break;
        }
        case 0xAB:{
            //TODO - AB,XOR E,4,1,1
            break;
        }
        case 0xAC:{
            //TODO - AC,XOR H,4,1,1
            break;
        }
        case 0xAD:{
            //TODO - AD,XOR L,4,1,1
            break;
        }
        case 0xAE:{
            //TODO - AE,XOR (HL),7,2,1
            break;
        }
        case 0xAF:{
            //TODO - AF,XOR A,4,1,1
            break;
        }
        case 0xB0:{
            //TODO - B0,OR B,4,1,1
            break;
        }
        case 0xB1:{
            //TODO - B1,OR C,4,1,1
            break;
        }
        case 0xB2:{
            //TODO - B2,OR D,4,1,1
            break;
        }
        case 0xB3:{
            //TODO - B3,OR E,4,1,1
            break;
        }
        case 0xB4:{
            //TODO - B4,OR H,4,1,1
            break;
        }
        case 0xB5:{
            //TODO - B5,OR L,4,1,1
            break;
        }
        case 0xB6:{
            //TODO - B6,OR (HL),7,2,1
            break;
        }
        case 0xB7:{
            //TODO - B7,OR A,4,1,1
            break;
        }
        case 0xB8:{
            //TODO - B8,CP B,4,1,1
            break;
        }
        case 0xB9:{
            //TODO - B9,CP C,4,1,1
            break;
        }
        case 0xBA:{
            //TODO - BA,CP D,4,1,1
            break;
        }
        case 0xBB:{
            //TODO - BB,CP E,4,1,1
            break;
        }
        case 0xBC:{
            //TODO - BC,CP H,4,1,1
            break;
        }
        case 0xBD:{
            //TODO - BD,CP L,4,1,1
            break;
        }
        case 0xBE:{
            //TODO - BE,CP (HL),7,2,1
            break;
        }
        case 0xBF:{
            //TODO - BF,CP A,4,1,1
            break;
        }
        case 0xC0:{
            //TODO - C0,RET NZ,11/5,3/1,1/1,(met/not met)
            break;
        }
        case 0xC1:{
            //TODO - C1,POP BC,10,3,1
            break;
        }
        case 0xC2:{
            //TODO - C2 n n,JP NZ,(nn),10,3,1,(met or not)
            break;
        }
        case 0xC3:{
            //TODO - C3 n n,JP (nn),10,3,1
            break;
        }
        case 0xC4:{
            //TODO - C4 n n,CALL NZ,(nn),17/10,5/3,1/1,(met/not met)
            break;
        }
        case 0xC5:{
            //TODO - C5,PUSH BC,11,3,1
            break;
        }
        case 0xC6:{
            //TODO - C6 n,ADD A,n,7,2,1
            break;
        }
        case 0xC7:{
            //TODO - C7,RST 0H,11,3,1
            break;
        }
        case 0xC8:{
            //TODO - C8,RET Z,11/5,3/1,1/1,(met/not met)
            break;
        }
        case 0xC9:{
            //TODO - C9,RET,10,3,1
            break;
        }
        case 0xCA:{
            //TODO - CA n n,JP Z,(nn),10,3,1,(always same)
            break;
        }
        case 0xCC:{
            //TODO - CC n n,CALL Z,(nn),17/10,5/3,1/1,(met/not met)
            break;
        }
        case 0xCD:{
            //TODO - CD n n,CALL (nn),17,5,1
            break;
        }
        case 0xCE:{
            //TODO - CE n,ADC A,n,7,2,1
            break;
        }
        case 0xCF:{
            //TODO - CF,RST 8H,11,3,1
            break;
        }
        case 0xD0:{
            //TODO - D0,RET NC,5,1,1
            break;
        }
        case 0xD1:{
            //TODO - D1,POP DE,10,3,1
            break;
        }
        case 0xD2:{
            //TODO - D2 n n,JP NC,(nn),10,3,1,(met or not)
            break;
        }
        case 0xD3:{
            //TODO - D3 n,OUT (n),A,11,3,1
            break;
        }
        case 0xD4:{
            //TODO - D4 n n,CALL NC,(nn),17/10,5/3,1/1,(met/not met)
            break;
        }
        case 0xD5:{
            //TODO - D5,PUSH DE,11,3,1
            break;
        }
        case 0xD6:{
            //TODO - D6 n,SUB n,7,2,1
            break;
        }
        case 0xD7:{
            //TODO - D7,RST 10H,11,3,1
            break;
        }
        case 0xD8:{
            //TODO - D8,RET C,5,1,1
            break;
        }
        case 0xD9:{
            //TODO - D9,EXX,4,1,1
            break;
        }
        case 0xDA:{
            //TODO - DA n n,JP C,(nn),10,3,1,(met or not)
            break;
        }
        case 0xDB:{
            //TODO - DB n,IN A,(n),11,3,1
            break;
        }
        case 0xDC:{
            //TODO - DC n n,CALL C,(nn),17/10,5/3,1
            break;
        }
        case 0xDE:{
            //TODO - DE n,SBC A,n
            break;
        }
        case 0xDF:{
            //TODO - DF,RST 18H
            break;
        }
        case 0xE0:{
            //TODO - E0,RET PO
            break;
        }
        case 0xE1:{
            //TODO - E1,POP HL
            break;
        }
        case 0xE2:{
            //TODO - E2 n n,JP PO,(nn)
            break;
        }
        case 0xE3:{
            //TODO - E3,EX (SP),HL
            break;
        }
        case 0xE4:{
            //TODO - E4 n n,CALL PO,(nn)
            break;
        }
        case 0xE5:{
            //TODO - E5,PUSH HL
            break;
        }
        case 0xE6:{
            //TODO - E6 n,AND n
            break;
        }
        case 0xE7:{
            //TODO - E7,RST 20H
            break;
        }
        case 0xE8:{
            //TODO - E8,RET PE
            break;
        }
        case 0xE9:{
            //TODO - E9,JP (HL)
            break;
        }
        case 0xEA:{
            //TODO - EA n n,JP PE,(nn)
            break;
        }
        case 0xEB:{
            //TODO - EB,EX DE,HL
            break;
        }
        case 0xEC:{
            //TODO - EC n n,CALL PE,(nn)
            break;
        }
        case 0xEE:{
            //TODO - EE n,XOR n
            break;
        }
        case 0xEF:{
            //TODO - EF,RST 28H
            break;
        }
        case 0xF0:{
            //TODO - F0,RET P
            break;
        }
        case 0xF1:{
            //TODO - F1,POP AF
            break;
        }
        case 0xF2:{
            //TODO - F2 n n,JP P,(nn)
            break;
        }
        case 0xF3:{
            //TODO - F3,DI
            break;
        }
        case 0xF4:{
            //TODO - F4 n n,CALL P,(nn)
            break;
        }
        case 0xF5:{
            //TODO - F5,PUSH AF
            break;
        }
        case 0xF6:{
            //TODO - F6 n,OR n
            break;
        }
        case 0xF7:{
            //TODO - F7,RST 30H
            break;
        }
        case 0xF8:{
            //TODO - F8,RET M
            break;
        }
        case 0xF9:{
            //TODO - F9,LD SP,HL
            break;
        }
        case 0xFA:{
            //TODO - FA n n,JP M,(nn)
            break;
        }
        case 0xFB:{
            //TODO - FB,EI
            break;
        }
        case 0xFC:{
            //TODO - FC n n,CALL M,(nn)
            break;
        }
        case 0xFE:{
            //TODO - FE n,CP n
            break;
        }
        case 0xFF:{
            //TODO - FF,RST 38H
            break;
        }


        case 0xCB:
            extendedOpBITS();
            break;
        case 0xDD:
            extendedOpIX();
            break;
        case 0xED:
            extendedOpEXTD();
            break;
        case 0xFD:
            extendedOpIY();
            break;
    }
}

template<auto ... operation>
static inline auto createOpTable() {

}

