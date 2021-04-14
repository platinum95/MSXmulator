#if defined USE_GL_INTERFACE
#include "GraphicalInterface.h"
#include "KeyboardInterface.h"

#define GLAD_GL_IMPLEMENTATION
#include "glad/gl.h"
#include <GLFW/glfw3.h>

#include <array>
#include <cassert>
#include <iostream>
#include <ranges>
#include <stdexcept>

namespace KeyboardInterface {
static inline void GLFWKeyEventCB( GLFWwindow* window, int key, int action, int, int );
}

namespace GraphicalInterface {

using PixelColour = std::tuple<uint8_t, uint8_t, uint8_t>;

inline std::uint8_t operator ""_u( unsigned long long value ) {
    return static_cast<std::uint8_t>( value );
}

namespace {

constexpr uint32_t RenderSurfaceWidth{ 256u };
constexpr uint32_t RenderSurfaceHeight{ 192u };
constexpr float    AspectRatio{ static_cast<float>( RenderSurfaceWidth ) / static_cast<float>( RenderSurfaceHeight ) };
constexpr uint32_t BytesPerPixel{ 3u };
constexpr uint32_t FramebufferSize{ RenderSurfaceHeight * RenderSurfaceWidth * BytesPerPixel };

constexpr std::array<float, 2 * 3 * 2> Vertices = {
    -1.0f, -1.0f,
    1.0f, -1.0f,
    1.0f, 1.0f,
    -1.0f, -1.0f,
    1.0f, 1.0f,
    -1.0f, 1.0f
};

constexpr std::array<float, 2 * 3 * 2> TextureCoords = {
    0.0f, 1.0f,
    1.0f, 1.0f,
    1.0f, 0.0f,
    0.0f, 1.0f,
    1.0f, 0.0f,
    0.0f, 0.0f
};

const char *VertexShaderSource = R"SHADER(
#version 330 core
layout ( location = 0 ) in vec2 vPos;
layout ( location = 1 ) in vec2 tPosVS;

out vec2 tPosFS;

void main() {
    tPosFS = tPosVS;
    gl_Position = vec4( vPos.x, vPos.y, 0.0, 1.0 );
}
)SHADER";

const char *FragmentShaderSource = R"SHADER(
#version 330 core
out vec4 FragColor;

in vec2 tPosFS;

uniform sampler2D frame;

void main() {
    FragColor = texture( frame, tPosFS );
} 
)SHADER";

struct GLInterfaceState {
    bool initialised{ false };

    GLFWwindow  *window{ nullptr };
    GLFWmonitor *monitor{ nullptr };
    GLFWwindow  *share{ nullptr };

    std::array<GLuint, 2>   VBOs;
    GLuint                  VAO;
    GLuint                  shader;
    GLuint                  texture;

    std::array<uint8_t, FramebufferSize> framebuffer{ 0u };

} state;

PixelColour GetColourValue( Colour colour ) {
    switch ( colour ) {
        case Colour::Transparent:   return { 0_u, 0_u, 0_u };
        case Colour::Black:         return { 1_u, 1_u, 1_u };
        case Colour::MediumGreen:   return { 62_u, 184_u, 73_u };
        case Colour::LightGreen:    return { 116_u, 208_u, 125_u };
        case Colour::DarkBlue:      return { 89_u, 85_u, 224_u };
        case Colour::LightBlue:     return { 128_u, 118_u, 241_u };
        case Colour::DarkRed:       return { 185_u, 94_u, 81_u };
        case Colour::Cyan:          return { 101_u, 219_u, 239_u };
        case Colour::MediumRed:     return { 219_u, 101_u, 89_u };
        case Colour::LightRed:      return { 255_u, 137_u, 125_u };
        case Colour::DarkYellow:    return { 204_u, 195_u, 94_u };
        case Colour::LightYellow:   return { 222_u, 208_u, 135_u };
        case Colour::DarkGreen:     return { 58_u, 162_u, 65_u };
        case Colour::Magenta:       return { 183_u, 102_u, 181_u };
        case Colour::Grey:          return { 204_u, 204_u, 204_u };
        case Colour::White:         return { 255_u, 255_u, 255_u };
        default: throw std::runtime_error( "Attempting to get invalid colour" );
    }
}

} // namespace

void Initialise( uint16_t width, uint16_t height ) {
    if ( !glfwInit() ) {
        throw std::runtime_error( "Failed to initialise GLFW" );
    }

    glfwWindowHint( GLFW_CONTEXT_VERSION_MAJOR, 3 );
    glfwWindowHint( GLFW_CONTEXT_VERSION_MINOR, 3 );
    glfwWindowHint( GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE );
    glfwWindowHint( GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE );

    state.window = glfwCreateWindow( width, height, "MSXMulator", state.monitor, state.share );

    if ( state.window == nullptr ) {
        throw std::runtime_error( "Failed to create GLFW window" );
    }

    glfwMakeContextCurrent( state.window );
    glfwSetWindowAspectRatio( state.window, RenderSurfaceWidth, RenderSurfaceHeight );
    glfwSetWindowSizeLimits( state.window, RenderSurfaceWidth, RenderSurfaceHeight, 0, 0 );

    glfwSetWindowSizeCallback( state.window, []( GLFWwindow*, int width, int height ) {
        glViewport( 0, 0, width, height );
    } );

    glfwSetKeyCallback( state.window, KeyboardInterface::GLFWKeyEventCB );

    auto version = gladLoadGL( glfwGetProcAddress );
    if ( version == 0 ) {
        throw std::runtime_error( "Failed to initialize OpenGL context\n" );
    }

    glGenVertexArrays( 1, &state.VAO );
    glBindVertexArray( state.VAO );

    glGenBuffers( static_cast<GLsizei>( state.VBOs.size() ), state.VBOs.data() );
    glBindBuffer( GL_ARRAY_BUFFER, state.VBOs[ 0 ] );
    glBufferData( GL_ARRAY_BUFFER, sizeof( Vertices ), Vertices.data(), GL_STATIC_DRAW );
    glVertexAttribPointer( 0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof( float ), nullptr );
    glEnableVertexAttribArray( 0 );

    glBindBuffer( GL_ARRAY_BUFFER, state.VBOs[ 1 ] );
    glBufferData( GL_ARRAY_BUFFER, sizeof( TextureCoords ), TextureCoords.data(), GL_STATIC_DRAW );
    glVertexAttribPointer( 1, 2, GL_FLOAT, GL_FALSE, 2 * sizeof( float ), nullptr );
    glEnableVertexAttribArray( 1 );

    glGenTextures( 1, &state.texture );
    glBindTexture( GL_TEXTURE_2D, state.texture );
    glTexImage2D( GL_TEXTURE_2D, 0, GL_RGB, RenderSurfaceWidth, RenderSurfaceHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, static_cast<void*>( state.framebuffer.data() ) );

    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST );
    glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST );

    auto CheckShaderCompilation = [] ( auto shaderId ) {
        int  success;
        char infoLog[ 512 ];
        glGetShaderiv( shaderId, GL_COMPILE_STATUS, &success );

        if ( !success ) {
            glGetShaderInfoLog( shaderId, 512, NULL, infoLog );
            throw std::runtime_error( "Shader compilation failed with error:\n" + std::string( infoLog ) );
        }
    };
    auto vertexShader = glCreateShader( GL_VERTEX_SHADER );
    auto fragmentShader = glCreateShader( GL_FRAGMENT_SHADER );

    glShaderSource( vertexShader, 1, &VertexShaderSource, nullptr );
    glCompileShader( vertexShader );
    CheckShaderCompilation( vertexShader );

    glShaderSource( fragmentShader, 1, &FragmentShaderSource, nullptr );
    glCompileShader( fragmentShader );
    CheckShaderCompilation( fragmentShader );

    state.shader = glCreateProgram();
    glAttachShader( state.shader, vertexShader );
    glAttachShader( state.shader, fragmentShader );
    glLinkProgram( state.shader );
    {
        int success;
        glGetProgramiv( state.shader, GL_LINK_STATUS, &success );

        if ( !success ) {
            char infoLog[ 512 ];
            glGetProgramInfoLog( state.shader, 512, NULL, infoLog );
            throw std::runtime_error( "Shader linking failed with error:\n" + std::string( infoLog ) );
        }
    }
    
    glDeleteShader( vertexShader );
    glDeleteShader( fragmentShader );
    
    state.initialised = true;
    std::cout << "OpenGL Interface Initialised" << std::endl;
}

void Cleanup() {
    if ( state.initialised ) {
        glDeleteBuffers( static_cast<GLsizei>( state.VBOs.size() ), state.VBOs.data() );
        glDeleteVertexArrays( 1, &state.VAO );
        glDeleteProgram( state.shader );
        glDeleteTextures( 1, &state.texture );

        glfwDestroyWindow( state.window );
        glfwTerminate();
    }
}

void SetPixel( uint32_t x, uint32_t y, Colour colour ) {
    const auto colourValue = GetColourValue( colour );
    const size_t pixelOffset = ( ( static_cast<size_t>( y ) * RenderSurfaceWidth ) + x ) * static_cast<size_t>( BytesPerPixel );

    // TODO - this could probably be done in a single block
    state.framebuffer[ pixelOffset + 0 ] = std::get<0>( colourValue );
    state.framebuffer[ pixelOffset + 1 ] = std::get<1>( colourValue );
    state.framebuffer[ pixelOffset + 2 ] = std::get<2>( colourValue );
}

void OutputFrame() {
    if ( !state.initialised ) {
        throw std::runtime_error( "Attempting to render frame before initialising OpenGL" );
    }
    glUseProgram( state.shader );
    glBindVertexArray( state.VAO );
    glBindTexture( GL_TEXTURE_2D, state.texture );
    glTexImage2D( GL_TEXTURE_2D, 0, GL_RGB, RenderSurfaceWidth, RenderSurfaceHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, static_cast<void*>( state.framebuffer.data() ) );
    glDrawArrays( GL_TRIANGLES, 0, 6 );

    glfwSwapBuffers( state.window );
    glfwPollEvents();
}

} // ns::GraphicalInterface


namespace KeyboardInterface {
namespace {

struct KeyboardInputState {
    std::vector<KeyEvent> events;
} state;

KeyEvent CreateKeyEvent( int keyCode, bool pressed ) {
    switch ( keyCode ) {
        case GLFW_KEY_ESCAPE: return { 7, 2, pressed }; // ESC
        case GLFW_KEY_1: return { 0, 1, pressed }; // 1
        case GLFW_KEY_2: return { 0, 2, pressed }; // 2
        case GLFW_KEY_3: return { 0, 3, pressed }; // 3
        case GLFW_KEY_4: return { 0, 4, pressed }; // 4
        case GLFW_KEY_5: return { 0, 5, pressed }; // 5
        case GLFW_KEY_6: return { 0, 6, pressed }; // 6
        case GLFW_KEY_7: return { 0, 7, pressed }; // 7
        case GLFW_KEY_8: return { 1, 0, pressed }; // 8
        case GLFW_KEY_9: return { 1, 1, pressed }; // 9
        case GLFW_KEY_0: return { 0, 0, pressed }; // 0
        case GLFW_KEY_MINUS: return { 1, 2, pressed }; // _-
        case GLFW_KEY_EQUAL: return { 1, 3, pressed }; // +=
        case GLFW_KEY_BACKSPACE: return { 7, 5, pressed }; // BS

        case GLFW_KEY_TAB: return { 7, 3, pressed }; // TAB
        case GLFW_KEY_Q: return { 4, 6, pressed }; // Q
        case GLFW_KEY_W: return { 5, 4, pressed }; // W
        case GLFW_KEY_E: return { 3, 2, pressed }; // E
        case GLFW_KEY_R: return { 4, 7, pressed }; // R
        case GLFW_KEY_T: return { 5, 1, pressed }; // T
        case GLFW_KEY_Y: return { 5, 6, pressed }; // Y
        case GLFW_KEY_U: return { 5, 2, pressed }; // U
        case GLFW_KEY_I: return { 3, 6, pressed }; // I
        case GLFW_KEY_O: return { 4, 4, pressed }; // O
        case GLFW_KEY_P: return { 4, 5, pressed }; // P
        case GLFW_KEY_LEFT_BRACKET: return { 1, 5, pressed }; // [{
        case GLFW_KEY_RIGHT_BRACKET: return { 1, 6, pressed }; // ]}

        case GLFW_KEY_ENTER: return { 7, 7, pressed }; // Return

        case GLFW_KEY_LEFT_CONTROL: return { 6, 2, pressed }; // L-CTRL
        case GLFW_KEY_A: return { 2, 6, pressed }; // A
        case GLFW_KEY_S: return { 5, 0, pressed }; // S
        case GLFW_KEY_D: return { 3, 1, pressed }; // D
        case GLFW_KEY_F: return { 3, 3, pressed }; // F
        case GLFW_KEY_G: return { 3, 4, pressed }; // G
        case GLFW_KEY_H: return { 3, 5, pressed }; // H
        case GLFW_KEY_J: return { 3, 7, pressed }; // J
        case GLFW_KEY_K: return { 4, 0, pressed }; // K
        case GLFW_KEY_L: return { 4, 1, pressed }; // L
        case GLFW_KEY_SEMICOLON: return { 1, 7, pressed }; // ;
        case GLFW_KEY_APOSTROPHE: return { 2, 0, pressed }; // '"
        case GLFW_KEY_GRAVE_ACCENT: return { 2, 1, pressed }; // ~`

        case GLFW_KEY_LEFT_SHIFT: return { 6, 0, pressed }; // L-SHIFT
        case GLFW_KEY_Z: return { 5, 7, pressed }; // Z
        case GLFW_KEY_X: return { 5, 5, pressed }; // X
        case GLFW_KEY_C: return { 3, 0, pressed }; // C
        case GLFW_KEY_V: return { 5, 3, pressed }; // V
        case GLFW_KEY_B: return { 2, 7, pressed }; // B
        case GLFW_KEY_N: return { 4, 3, pressed }; // N
        case GLFW_KEY_M: return { 4, 2, pressed }; // M
        case GLFW_KEY_COMMA: return { 2, 2, pressed }; // ,<
        case GLFW_KEY_PERIOD: return { 2, 3, pressed }; // .>
        case GLFW_KEY_SLASH: return { 2, 4, pressed }; // /?
        case GLFW_KEY_RIGHT_SHIFT: return { 6, 0, pressed }; // R-shift

        case GLFW_KEY_KP_MULTIPLY: return { 9, 0, pressed }; // NUM*
        case GLFW_KEY_LEFT_ALT: return { 6, 2, pressed }; // L-ALT -> GRAPH
        case GLFW_KEY_SPACE: return { 8, 0, pressed }; // Space
        case GLFW_KEY_CAPS_LOCK: return { 6, 3, pressed }; // CAPSLOCK

        case GLFW_KEY_F1: return { 6, 5, pressed }; // F1 - F1/6
        case GLFW_KEY_F2: return { 6, 6, pressed }; // F2 - F2/7
        case GLFW_KEY_F3: return { 6, 7, pressed }; // F3 - F3/8
        case GLFW_KEY_F4: return { 7, 0, pressed }; // F4 - F4/9
        case GLFW_KEY_F5: return { 7, 1, pressed }; // F5 - F5/10
        case GLFW_KEY_F6: return { 8, 1, pressed }; // F6 - HOME
        case GLFW_KEY_F7: return { 8, 2, pressed }; // F7 - INS
        case GLFW_KEY_F8: return { 8, 3, pressed }; // F8 - DEL
        case GLFW_KEY_F9: return { 7, 6, pressed }; // F9 - SELECT
        case GLFW_KEY_F10: return { 7, 4, pressed }; // F10 - STOP

        case GLFW_KEY_KP_7: return { 10, 2, pressed }; // NUM7
        case GLFW_KEY_KP_8: return { 10, 3, pressed }; // NUM8
        case GLFW_KEY_KP_9: return { 10, 4, pressed }; // NUM9
        case GLFW_KEY_KP_SUBTRACT: return { 10, 5, pressed }; // NUM-
        case GLFW_KEY_KP_4: return { 7, 7, pressed }; // NUM4
        case GLFW_KEY_KP_5: return { 10, 0, pressed }; // NUM5
        case GLFW_KEY_KP_6: return { 10, 1, pressed }; // NUM6
        case GLFW_KEY_KP_ADD: return { 9, 1, pressed }; // NUM+
        case GLFW_KEY_KP_1: return { 9, 4, pressed }; // NUM1
        case GLFW_KEY_KP_2: return { 9, 5, pressed }; // NUM2
        case GLFW_KEY_KP_3: return { 9, 6, pressed }; // NUM3
        case GLFW_KEY_KP_0: return { 9, 3, pressed }; // NUM0
        case GLFW_KEY_DELETE: return { 10, 7, pressed }; // NUM.
        case GLFW_KEY_KP_DIVIDE: return { 9, 2, pressed }; // NUM/

        case GLFW_KEY_RIGHT_ALT: return { 6, 4, pressed }; // R-ALT - CODE

        case GLFW_KEY_UP: return { 8, 5, pressed }; // UP
        case GLFW_KEY_LEFT: return { 8, 4, pressed }; // LEFT
        case GLFW_KEY_RIGHT: return { 8, 7, pressed }; // RIGHT
        case GLFW_KEY_DOWN: return { 8, 6, pressed }; // DOWN
        default: return { 2, 5, pressed };
    }
}
} // ns::

static inline void GLFWKeyEventCB( GLFWwindow*, int key, int, int action, int ) {
    state.events.push_back( CreateKeyEvent( key, action == GLFW_RELEASE ? false : true ) );
}

KeyEvent::KeyEvent( uint8_t _row, uint8_t bitpos, bool pressed )
    : row( _row )
    , bitmask( 1u << bitpos )
    , keyPressed( pressed ) {
    assert( bitpos < 8 );
}

std::vector<KeyEvent> GetKeyEvents() {
    if ( state.events.size() > 0 ) {
        return std::exchange( state.events, std::vector<KeyEvent>() );
    }
    return std::vector<KeyEvent>();
}

} // ns::KeyboardInterface

#endif //USE_GL_INTERFACE