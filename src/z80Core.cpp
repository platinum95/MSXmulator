#include <functional>
#include <iostream>
#include <stdint.h>

/* Template bases */

using InvocableRead2 = uint8_t(*)();

using InvocableOperationNoParamNoRet2 = void(*)();

typedef void(*InvocableOperationSingleParamNoRet2)(uint8_t);

using InvocableRead3 = uint8_t(*)();

template<typename T>
concept IsInvocableRead = std::is_invocable_r_v<uint8_t, T> && std::is_same<T, uint8_t(*)()>::value;

template<typename T>
concept IsInvocableWrite = std::is_invocable_r_v<void, T, uint8_t>;

template<typename T>
concept InvocableOperationNoParamNoRet = std::is_invocable_r_v<void, T>;

template<typename T>
concept InvocableOperationNoParamWithRet = std::is_invocable_r_v<uint8_t, T>;

template<typename T>
concept InvocableOperationSingleParamNoRet = std::is_invocable_r_v<void, T, uint8_t>;

template<typename T>
concept InvocableOperationSingleParamWithRet = std::is_invocable_r_v<uint8_t, T, uint8_t>;

template<typename T>
concept InvocableOperationDualParamNoRet = std::is_invocable_r_v<void, T, uint8_t, uint8_t>;

template<typename T>
concept InvocableOperationDualParamWithRet = std::is_invocable_r_v<uint8_t, T, uint8_t, uint8_t>;

template<auto operation, auto p2>
concept IsSingleParamNoWriteoutOp = InvocableOperationSingleParamNoRet<decltype(operation)> && IsInvocableRead<decltype(p2)>;

template<auto operation, auto p2>
concept IsNoParamWriteoutOp = InvocableOperationNoParamWithRet<decltype(operation)> && IsInvocableWrite<decltype(p2)>;

template<auto operation, auto p2, auto p3>
concept IsDualParamNoWriteoutOp = InvocableOperationDualParamNoRet<decltype(operation)> && IsInvocableRead<decltype(p2)> && IsInvocableRead<decltype(p3)>;

template<auto operation, auto p2, auto p3>
concept IsSingleParamWriteoutOp = InvocableOperationSingleParamWithRet<decltype(operation)> && IsInvocableRead<decltype(p2)> && IsInvocableWrite<decltype(p3)>;

template<auto operation, auto p2, auto p3, auto p4>
concept IsDualParamWriteoutOp = InvocableOperationDualParamNoRet<decltype(operation)> && IsInvocableRead<decltype(p2)> && IsInvocableRead<decltype(p3)> && IsInvocableWrite<decltype(p4)>;


template <auto operation>
    requires InvocableOperationNoParamNoRet<decltype(operation)>
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

template <auto operation, auto readO1, auto readO2_OrWrite>
    requires 
        ( IsDualParamNoWriteoutOp<operation, readO1, readO2_OrWrite> ) ||
        ( IsSingleParamWriteoutOp<operation, readO1, readO2_OrWrite> )
void OperationBase() {
    if constexpr( IsDualParamNoWriteoutOp<operation, readO1, readO2_OrWrite> ) {
        operation( readO1(), readO2_OrWrite() );
    }
    else if constexpr( IsSingleParamWriteoutOp<operation, readO1, readO2_OrWrite> ) {
        readO2_OrWrite( operation( readO1() ) );
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
}

uint8_t readSomething() {
    std::cout << "We did it!" << std::endl;
    return 0;
}

uint16_t readSomething_invalid() {
    std::cout << "We did it!" << std::endl;
    return 0;
}

void operateSomethingNoRetNoParam() {
}

uint8_t operateSomethingRetNoParam() {
    return 0;
}

uint8_t operateSomethingRetSingleParam( uint8_t a ) {
    (void) a;
    return 0;
}
uint8_t operateSomethingRetDualParam( uint8_t a, uint8_t b ) {
    (void) a;
    (void) b;
    return 0;
}

void operateSomethingNoRetSingleParam( uint8_t a ) {
    (void) a;
}

void operateSomethingNoRetDualParam( uint8_t a, uint8_t b ) {
    (void) a;
    (void) b;
}

void writeSomething( uint8_t a ) {
    (void) a;
}

void writeSomething_invalid( uint16_t a ) {
    (void) a;
}

void z80Tick() {
    OperationBase<operateSomethingNoRetNoParam>();
    OperationBase<operateSomethingRetNoParam, writeSomething>();
    OperationBase<operateSomethingRetSingleParam, readSomething, writeSomething>();
    OperationBase<operateSomethingRetDualParam, readSomething, readSomething, writeSomething>();
    OperationBase<operateSomethingNoRetSingleParam, readSomething>();
    OperationBase<operateSomethingNoRetDualParam, readSomething, readSomething>();
    //OperationBase<operateSomething, readSomething_invalid>();
}