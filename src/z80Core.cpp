#include <functional>
#include <iostream>
#include <stdint.h>

/* Template bases */

using InvocableRead2 = uint8_t(*)();

using InvocableOperationNoParamNoRet2 = void(*)();

typedef void(*InvocableOperationSingleParamNoRet2)(uint8_t);

using InvocableRead3 = uint8_t(*)();

template<typename T>
concept IsInvocableRead = std::is_same<T, uint8_t(*)()>::value;

template<typename T>
concept IsInvocableWrite = std::is_same<T, void(*)(uint8_t)>::value;

template<typename T>
concept IsInvocableOperationNoParamNoRet = std::is_same<T, void(*)()>::value;

template<typename T>
concept IsInvocableOperationNoParamWithRet = std::is_same<T, uint8_t(*)()>::value;

template<typename T>
concept IsInvocableOperationSingleParamNoRet = std::is_same<T, void(*)(uint8_t)>::value;

template<typename T>
concept IsInvocableOperationSingleParamWithRet = std::is_same<T, uint8_t(*)(uint8_t)>::value;

template<typename T>
concept IsInvocableOperationDualParamNoRet = std::is_same<T, void(*)(uint8_t, uint8_t)>::value;

template<typename T>
concept IsInvocableOperationDualParamWithRet = std::is_same<T, uint8_t(*)(uint8_t, uint8_t)>::value;

template<auto operation, auto p2>
concept IsSingleParamNoWriteoutOp = IsInvocableOperationSingleParamNoRet<decltype(operation)> && IsInvocableRead<decltype(p2)>;

template<auto operation, auto p2>
concept IsNoParamWriteoutOp = IsInvocableOperationNoParamWithRet<decltype(operation)> && IsInvocableWrite<decltype(p2)>;

template<auto operation, auto p2, auto p3>
concept IsDualParamNoWriteoutOp = IsInvocableOperationDualParamNoRet<decltype(operation)> && IsInvocableRead<decltype(p2)> && IsInvocableRead<decltype(p3)>;

template<auto operation, auto p2, auto p3>
concept IsSingleParamWriteoutOp = IsInvocableOperationSingleParamWithRet<decltype(operation)> && IsInvocableRead<decltype(p2)> && IsInvocableWrite<decltype(p3)>;

template<auto operation, auto p2, auto p3, auto p4>
concept IsDualParamWriteoutOp = IsInvocableOperationDualParamWithRet<decltype(operation)> && IsInvocableRead<decltype(p2)> && IsInvocableRead<decltype(p3)> && IsInvocableWrite<decltype(p4)>;


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

template <auto operation, auto readO1, auto readO2_OrWrite>
    requires 
        ( IsDualParamNoWriteoutOp<operation, readO1, readO2_OrWrite> ) ||
        ( IsSingleParamWriteoutOp<operation, readO1, readO2_OrWrite> )
void OperationBase() {
    if constexpr( IsDualParamNoWriteoutOp<operation, readO1, readO2_OrWrite> ) {
        operation( readO1(), readO2_OrWrite() );
        std::cout << "Dual param, no ret\n";
    }
    else if constexpr( IsSingleParamWriteoutOp<operation, readO1, readO2_OrWrite> ) {
        readO2_OrWrite( operation( readO1() ) );
        std::cout << "Single param, with ret\n";
    }
    else {
        []<bool flag = false>() {
            static_assert( flag, "Invalid operation combination" );
        }();
    }
}

template <auto operation, auto readO1, auto readO2, auto write>
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

template<auto readOp>
void LDAOperationBase() {
    OperationBase<LDA_Op, readOp, LDA_Write>();
}

void z80Tick() {
    OperationBase<operateSomethingNoRetNoParam>();
    OperationBase<operateSomethingRetNoParam, writeSomething>();
    OperationBase<operateSomethingRetSingleParam, readSomething, writeSomething>();
    OperationBase<operateSomethingRetDualParam, readSomething, readSomething, writeSomething>();
    OperationBase<operateSomethingNoRetSingleParam, readSomething>();
    OperationBase<operateSomethingNoRetDualParam, readSomething, readSomething>();

    LDAOperationBase<readSomething>();
}