#include "AY_3_8910.h"

#include <array>
#include <assert.h>
#include <atomic>
#include <iostream>
#include <memory.h>
#include <portaudio.h>

constexpr uint8_t MainToneClockDivisor = 16;
constexpr uint8_t EnvelopeClockDivisor = 16;

static uint8_t RegisterSelectLatch;
static uint8_t portRegisters[ 16 ];

struct ChannelState {
    void operator=( const ChannelState &channelStateRhs ) {
        envelopEnable = channelStateRhs.envelopEnable;
        amplitude = channelStateRhs.amplitude;

        toneEnable = channelStateRhs.toneEnable;
        toneHalfPeriodDivisor = channelStateRhs.toneHalfPeriodDivisor;
        tonePeriodCounter = channelStateRhs.tonePeriodCounter;
        toneHigh = channelStateRhs.toneHigh;
        toneLevel.store( channelStateRhs.toneLevel.load() );

        noiseEnable = channelStateRhs.noiseEnable;
        noiseHalfPeriodDivisor = channelStateRhs.noiseHalfPeriodDivisor;
        noisePeriodCounter = channelStateRhs.noisePeriodCounter;
        noiseLevel = channelStateRhs.noiseLevel;
    }

    bool                    envelopEnable{ false };
    uint8_t                 amplitude{ 0u };

    bool                    toneEnable{ false };
    uint16_t                toneHalfPeriodDivisor{ 0u };
    uint16_t                tonePeriodCounter{ 0u };
    bool                    toneHigh{ false };
    std::atomic<uint8_t>    toneLevel{ 0 };

    bool                    noiseEnable{ false };
    uint16_t                noiseHalfPeriodDivisor{ 0u };
    uint16_t                noisePeriodCounter{ 0u };
    bool                    noiseLevel{ false };
};

struct State {
    uint8_t                     mainToneTickCounter{ 0 };
    uint8_t                     envelopeTickCounter{ 0 };

    uint16_t                    envelopePeriodCounter{ 0 };
    uint16_t                    envelopePeriod{ 0 };
    uint8_t                     envelopeValue{ 0 };
    std::array<ChannelState, 3> channelStates;
};

static State state;
static PaStream *portAudioStream;

static void envelopeTick() {
       
    uint8_t envelopeSelect = portRegisters[ 0x0D ] & 0x0F;

    switch( envelopeSelect ) {
        case 0x08: {
            if ( state.envelopePeriodCounter > state.envelopePeriod ) {
                state.envelopePeriodCounter = 0x00;
            }
            [[fallthrough]];
        }
        case 0x00:
        case 0x01:
        case 0x02:
        case 0x03: 
        case 0x09: {
            if ( state.envelopePeriodCounter > state.envelopePeriod ) {
                state.envelopeValue = static_cast<uint8_t>( 0x00 );
            }
            else {
                state.envelopeValue = static_cast<uint8_t>( 0x0F - ( ( 0x0F * state.envelopePeriodCounter ) / state.envelopePeriod ) );
            }
            break;
        }
        case 0x0C: {
            if ( state.envelopePeriodCounter > state.envelopePeriod ) {
                state.envelopePeriodCounter = 0x00;
            }
            [[fallthrough]];
        }
        case 0x04:
        case 0x05:
        case 0x06:
        case 0x07:
        case 0x0F: {
            if ( state.envelopePeriodCounter > state.envelopePeriod ) {
                state.envelopeValue = static_cast<uint8_t>( 0x00 );
            }
            else {
                state.envelopeValue = static_cast<uint8_t>( ( 0x0F * state.envelopePeriodCounter ) / state.envelopePeriod );
            }
            break;
        }
        case 0x0B:{
            if ( state.envelopePeriodCounter > state.envelopePeriod ) {
                state.envelopeValue = static_cast<uint8_t>( 0x0F );
            }
            else {
                state.envelopeValue = static_cast<uint8_t>( 0x0F - ( ( 0x0F * state.envelopePeriodCounter ) / state.envelopePeriod ) );
            }
            break;
        }
        case 0x0D: {
            if ( state.envelopePeriodCounter > state.envelopePeriod ) {
                state.envelopeValue = static_cast<uint8_t>( 0x0F );
            }
            else {
                state.envelopeValue = static_cast<uint8_t>( ( 0x0F * state.envelopePeriodCounter ) / state.envelopePeriod );
            }
            break;
        }
        case 0x0A: {
            if ( state.envelopePeriodCounter < state.envelopePeriod ) {
                state.envelopeValue = static_cast<uint8_t>( 0x0F - ( ( 0x0F * state.envelopePeriodCounter ) / state.envelopePeriod ) );
            }
            else if ( state.envelopePeriodCounter < state.envelopePeriod * 2 ) {
                state.envelopeValue = static_cast<uint8_t>( ( 0x0F * ( state.envelopePeriodCounter / 2 ) ) / state.envelopePeriod );
            }
            else {
                state.envelopePeriodCounter = 0;
            }
            state.envelopeValue = static_cast<uint8_t>( 0x0F );
            break;
        }
        case 0x0E: {
            if ( state.envelopePeriodCounter < state.envelopePeriod ) {
                state.envelopeValue = static_cast<uint8_t>( ( 0x0F * state.envelopePeriodCounter ) / state.envelopePeriod );
            }
            else if ( state.envelopePeriodCounter < state.envelopePeriod * 2 ) {
                state.envelopeValue = static_cast<uint8_t>( 0x0F - ( ( 0x0F * ( state.envelopePeriodCounter / 2 ) ) / state.envelopePeriod ) );
            }
            else {
                state.envelopePeriodCounter = 0;
            }
            state.envelopeValue = static_cast<uint8_t>( 0x0F );
            break;
        }
    }
    ++state.envelopePeriodCounter;
}

void channelTick() {
    state.envelopeTickCounter += 4;
    if ( state.envelopeTickCounter >= EnvelopeClockDivisor ) {
        // Envelope tick (every 256 clocks)
        state.envelopeTickCounter = 0;
        envelopeTick();
    }

    for( uint8_t i = 0; i < 3; ++i ) {
        ChannelState &channelState = state.channelStates[ i ];
        if( channelState.toneEnable ) {
            channelState.tonePeriodCounter += 2;
            if ( channelState.tonePeriodCounter >= channelState.toneHalfPeriodDivisor ) {
                channelState.tonePeriodCounter = 0;
                if ( channelState.toneHigh ) {
                    channelState.toneHigh = false;
                    channelState.toneLevel.store( 0x00 );
                }
                else {
                    channelState.toneHigh = true;
                    channelState.toneLevel.store( 0x0F );//channelState.envelopEnable ? state.envelopeValue : channelState.amplitude );
                }
                
            }
        }
        else {
            channelState.toneLevel = false;
        }
        // TODO - Noise
    }
}


int portAudioStreamCallback( const void *input, void *output, unsigned long frameCount,
                             const PaStreamCallbackTimeInfo* timeInfo,
                             PaStreamCallbackFlags statusFlags, void *userData ) {
    (void)input;
    (void)frameCount;
    (void)timeInfo;
    (void)statusFlags;
    (void)userData;

    channelTick();

    int16_t sampleSum = 0;
    for( uint8_t i = 0; i < 3; ++i ) {
        sampleSum += state.channelStates[ i ].toneLevel.load();
    }

    sampleSum *= ( INT16_MAX / ( 0x0F * 3 ) );
    
    *static_cast<int16_t*>( output ) = sampleSum;
    return paNoError;
}

void PSG::Reset() {
    memset( &portRegisters, 0x00, sizeof( portRegisters ) );
    state = State{};
    state.envelopePeriod = 1;

    if ( Pa_Initialize() != paNoError ) {
        std::cout << "Failed to initialise PortAudio\n";
    }

    if ( Pa_OpenDefaultStream( &portAudioStream, 0, 1, paInt16, 48000, 1, portAudioStreamCallback, NULL ) != paNoError ) {
        std::cout << "Failed to open PortAudio stream\n";
    }

    if ( Pa_StartStream( portAudioStream ) != paNoError ) {
        std::cout << "Failed to start PortAudio stream\n";
    }
}



void PSG::Tick() {
    // Assume ticking @ 2MHz
    if ( ++state.mainToneTickCounter == ( MainToneClockDivisor / 15 ) ) {
        state.mainToneTickCounter = 0;
        // Tick every 2MHz/16 = 125KHz

        // Tone-generator/noise tick
        
    }
    
}

static inline void RegisterWrite( uint8_t value ) {
    assert( RegisterSelectLatch < 16 );
    switch( RegisterSelectLatch ) {
        case 0: {
            // A - fine
            portRegisters[ 0 ] = value;
            uint16_t channelDivisor = ( static_cast<uint16_t>( portRegisters[ 1 ] & 0x0F ) << 8 ) | static_cast<uint16_t>( portRegisters[ 0 ] );
            ++channelDivisor;
            state.channelStates[ 0 ].toneHalfPeriodDivisor = channelDivisor / 2;
            break;
        }
        case 1: {
            // A - coarse
            portRegisters[ 1 ] = value;
            uint16_t channelDivisor = ( static_cast<uint16_t>( portRegisters[ 1 ] & 0x0F ) << 8 ) | static_cast<uint16_t>( portRegisters[ 0 ] );
            ++channelDivisor;
            state.channelStates[ 0 ].toneHalfPeriodDivisor = channelDivisor / 2;
            break;
        }
        case 2: {
            // B - fine
            portRegisters[ 2 ] = value;
            uint16_t channelDivisor = ( static_cast<uint16_t>( portRegisters[ 3 ] & 0x0F ) << 8 ) | static_cast<uint16_t>( portRegisters[ 2 ] );
            ++channelDivisor;
            state.channelStates[ 1 ].toneHalfPeriodDivisor = channelDivisor / 2;
            break;
        }
        case 3: {
            // B - coarse
            portRegisters[ 3 ] = value;
            uint16_t channelDivisor = ( static_cast<uint16_t>( portRegisters[ 3 ] & 0x0F ) << 8 ) | static_cast<uint16_t>( portRegisters[ 2 ] );
            ++channelDivisor;
            state.channelStates[ 1 ].toneHalfPeriodDivisor = channelDivisor / 2;
            break;
        }
        case 4: {
            // C - fine
            portRegisters[ 4 ] = value;
            uint16_t channelDivisor = ( static_cast<uint16_t>( portRegisters[ 5 ] & 0x0F ) << 8 ) | static_cast<uint16_t>( portRegisters[ 4 ] );
            ++channelDivisor;
            state.channelStates[ 2 ].toneHalfPeriodDivisor = channelDivisor / 2;
            break;
        }
        case 5: {
            // C - coarse
            portRegisters[ 5 ] = value;
            uint16_t channelDivisor = ( static_cast<uint16_t>( portRegisters[ 5 ] & 0x0F ) << 8 ) | static_cast<uint16_t>( portRegisters[ 4 ] );
            ++channelDivisor;
            state.channelStates[ 2 ].toneHalfPeriodDivisor = channelDivisor / 2;
            break;
        }
        case 6: {
            // TODO - noise control
            std::cout << "TODO - PSG noise control\n";
            break;
        }
        case 7: {
            // Mixer control
            portRegisters[ 7 ] = value;
            for( uint8_t i = 0; i < 3; ++i ) {
                state.channelStates[ i ].toneEnable = !static_cast<bool>( ( value >> i ) & 0x01 );
            }
            break;
        }
        case 8: {
            // Channel A amplitude
            state.channelStates[ 0 ].envelopEnable = static_cast<bool>( value & 0x10 );
            state.channelStates[ 0 ].amplitude = value & 0x0F;
            break;
        }
        case 9: {
            // Channel B amplitude
            state.channelStates[ 1 ].envelopEnable = static_cast<bool>( value & 0x10 );
            state.channelStates[ 1 ].amplitude = value & 0x0F;
            break;
        }
        case 0x0A: {
            // Channel C amplitude
            state.channelStates[ 2 ].envelopEnable = static_cast<bool>( value & 0x10 );
            state.channelStates[ 2 ].amplitude = value & 0x0F;
            break;
        }
        case 0x0B: {
            // Envelope period control - fine
            portRegisters[ 11 ] = value;
            uint16_t envelopeDivisor = ( static_cast<uint16_t>( portRegisters[ 12 ] ) << 8 ) | static_cast<uint16_t>( portRegisters[ 11 ] );
            ++envelopeDivisor;
            state.envelopePeriod = envelopeDivisor;
            state.envelopePeriodCounter = 0;
            state.envelopeTickCounter = 0;
            break;
        }
        case 0x0C: {
            // Envelope period control - coarse
            portRegisters[ 12 ] = value;
            uint16_t envelopeDivisor = ( static_cast<uint16_t>( portRegisters[ 12 ] ) << 8 ) | static_cast<uint16_t>( portRegisters[ 11 ] );
            ++envelopeDivisor;
            state.envelopePeriod = envelopeDivisor;
            state.envelopePeriodCounter = 0;
            state.envelopeTickCounter = 0;
            break;
        }
        case 0x0D: {
            // Envelope shape/cycle control
            portRegisters[ 13 ] = value;
            state.envelopePeriodCounter = 0;
            state.envelopeTickCounter = 0;
            break;
        }
        case 0x0E: {
            std::cout << "TODO - PSG IO port A\n";
            break;
        }
        case 0x0F: {
            std::cout << "TODO - PSG IO port B\n";
            break;
        }

    }
}

void PSG::PortAccess( uint8_t port, uint8_t &dataBus, bool writeLine ) {
    switch( port ) {
        case 0x00: {
            assert( writeLine );
            (void)writeLine;
            RegisterSelectLatch = dataBus;
            break;
        }
        case 0x01: {
            assert( writeLine );
            (void)writeLine;
            RegisterWrite( dataBus );
            break;
        }
        case 0x02: {
            assert( !writeLine );
            (void)writeLine;
            assert( RegisterSelectLatch < 16 );
            dataBus = *( (uint8_t*)&portRegisters + RegisterSelectLatch );
            break;
        }
    }
}
