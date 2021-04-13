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

constexpr uint32_t RenderSurfaceWidth{ 284u * 2 };
constexpr uint32_t RenderSurfaceHeight{ 243u * 2};
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

const char *VertexShaderSource = R"SHADER(
#version 330 core
layout ( location = 0 ) in vec2 vPos;

void main() {
    gl_Position = vec4( vPos.x, vPos.y, 0.0, 1.0 );
}
)SHADER";

const char *FragmentShaderSource = R"SHADER(
#version 330 core
out vec4 FragColor;

void main() {
    FragColor = vec4( 1.0f, 0.5f, 0.2f, 1.0f );
} 
)SHADER";

struct GLInterfaceState {
    bool initialised{ false };
    GLFWwindow* window{ nullptr };
    GLFWmonitor *monitor{ nullptr };
    GLFWwindow *share{ nullptr };
    std::array<uint8_t, FramebufferSize> framebuffer{ 0u };

    uint32_t VBO;
    uint32_t VAO;
    uint32_t shader;
    

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

    glGenBuffers( 1, &state.VBO );
    glBindBuffer( GL_ARRAY_BUFFER, state.VBO );
    glBufferData( GL_ARRAY_BUFFER, sizeof( Vertices ), Vertices.data(), GL_STATIC_DRAW );
    glVertexAttribPointer( 0, 2, GL_FLOAT, GL_FALSE, 2 * sizeof( float ), nullptr );
    glEnableVertexAttribArray( 0 );

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
        glfwDestroyWindow( state.window );
        glDeleteBuffers( 1, &state.VBO );
        glDeleteVertexArrays( 1, &state.VAO );
        glDeleteProgram( state.shader );
    }
}

void SetPixel( uint32_t x, uint32_t y, Colour colour ) {
    const auto colourValue = GetColourValue( colour );
    const size_t pixelOffset = ( ( static_cast<size_t>( y ) * RenderSurfaceWidth ) + x ) * static_cast<size_t>( BytesPerPixel );

    // TODO - this could probably be done in a single block
    state.framebuffer[ pixelOffset ] = std::get<0>( colourValue );
    state.framebuffer[ pixelOffset + 1 ] = std::get<1>( colourValue );
    state.framebuffer[ pixelOffset + 1 ] = std::get<2>( colourValue );
}

void OutputFrame() {
    if ( !state.initialised ) {
        throw std::runtime_error( "Attempting to render frame before initialising OpenGL" );
    }
    glUseProgram( state.shader );
    glBindVertexArray( state.VAO );
    glBindBuffer( GL_ARRAY_BUFFER, state.VBO );
    glDrawArrays( GL_TRIANGLES, 0, 6 );

    glfwSwapBuffers( state.window );
}

}

#endif //USE_GL_INTERFACE