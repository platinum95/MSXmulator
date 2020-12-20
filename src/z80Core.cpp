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
concept InvocableOperationSingleParamNoRet = std::is_invocable_r_v<void, T, uint8_t>;

template<typename T>
concept InvocableOperationSingleParamWithRet = std::is_invocable_r_v<uint8_t, T, uint8_t>;

template<typename T>
concept InvocableOperationDualParamNoRet = std::is_invocable_r_v<void, T, uint8_t, uint8_t>;

template<typename T>
concept InvocableOperationDualParamWithRet = std::is_invocable_r_v<uint8_t, T, uint8_t, uint8_t>;


template <auto operation, auto readO1, auto readO2, auto write>
    requires
        ( InvocableOperationNoParamNoRet<decltype(operation)> && readO1 == nullptr && readO2 == nullptr && write == nullptr ) ||
        ( InvocableOperationSingleParamNoRet<decltype(operation)> && IsInvocableRead<decltype(readO1)> && readO2 == nullptr && write == nullptr ) ||
        ( InvocableOperationSingleParamWithRet<decltype(operation)> && IsInvocableRead<decltype(readO1)> && readO2 == nullptr && IsInvocableWrite<decltype(write)> ) ||
        ( InvocableOperationDualParamNoRet<decltype(operation)> && IsInvocableRead<decltype(readO1)> && IsInvocableRead<decltype(readO2)> && write == nullptr ) ||
        ( InvocableOperationDualParamWithRet<decltype(operation)> && IsInvocableRead<decltype(readO1)> && IsInvocableRead<decltype(readO2)> && IsInvocableWrite<decltype(write)> )
void OperationBase() {
    if constexpr( std::is_invocable_r_v<void, decltype(operation), uint8_t> ) {
        std::cout << "a\n";
        operation( readO1() );
    }
    else if constexpr( InvocableOperationSingleParamNoRet<decltype(operation)> ) {
        std::cout << "b\n";
    }
    else if constexpr( InvocableOperationSingleParamWithRet<decltype(operation)> ) {
        std::cout << "c\n";
    }
    else if constexpr( InvocableOperationDualParamNoRet<decltype(operation)> ) {
        std::cout << "d\n";
    }
    else if constexpr( InvocableOperationDualParamWithRet<decltype(operation)> ) {
        std::cout << "e\n";
    }
    else {
        []<bool flag = false>() {
            static_assert( flag, "Invalid operation combination" );
        }();
    }
}

template <auto operation>
    requires InvocableOperationNoParamNoRet<decltype(operation)>
void OperationBase() {
    operation();
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
    (void) a;
}

void operateSomething( uint8_t a ) {
    (void) a;
}

void writeSomething( uint8_t a ) {
    (void) a;
}

void z80Tick() {
    OperationBase<operateSomething, readSomething>();
    OperationBase<operateSomethingNoRetNoParam>();
    //OperationBase<operateSomething, readSomething_invalid>();
}