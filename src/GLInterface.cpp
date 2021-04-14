#if defined USE_GL_INTERFACE
#include "GraphicalInterface.h"

#define GLAD_GL_IMPLEMENTATION
#include "glad/gl.h"
#include <GLFW/glfw3.h>

#include <array>
#include <cassert>
#include <iostream>
#include <stdexcept>

namespace GraphicalInterface {

using PixelColour = std::tuple<uint8_t, uint8_t, uint8_t>;

inline std::uint8_t operator ""_u( unsigned long long value ) {
    return static_cast<std::uint8_t>( value );
}

namespace {

constexpr uint32_t RenderSurfaceWidth{ 284u };
constexpr uint32_t RenderSurfaceHeight{ 243u };
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

    glfwPollEvents();
    glfwSwapBuffers( state.window );
}

}

#endif //USE_GL_INTERFACE