# CMakeList.txt : Top-level CMake project file, do global configuration
# and include sub-projects here.
#
cmake_minimum_required (VERSION 3.16)

project ("MSXmulator")

set( CMAKE_CXX_STANDARD 20 )
set( CMAKE_CXX_STANDARD_REQUIRED ON )

if(MSVC)
  add_compile_options( /W4 /WX )
  string(REGEX REPLACE "/W3" "/W4" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
else()
  add_compile_options( -Wno-unknown-pragmas -MMD -O0 -g -Wall -Werror -Wextra -Wformat=2 -Wshadow -pedantic -Werror=vla -march=native -Wno-unused-variable )
  add_link_options( -Wno-unknown-pragmas -MMD -O0 -g -Wall -Werror -Wextra -Wformat=2 -Wshadow -pedantic -Werror=vla -march=native -Wno-unused-variable )
  
  set(THREADS_PREFER_PTHREAD_FLAG ON)

  find_package(X11 REQUIRED)
  link_libraries(${X11_LIBRARIES} pthread)
  include_directories(${X11_INCLUDE_DIR})

endif()

add_definitions( -DUSE_GL_INTERFACE )
find_package(Threads REQUIRED)
find_package(portaudio REQUIRED)
find_package(glfw3 3.3 REQUIRED)

include_directories( "include" )

file( GLOB_RECURSE SRC "src/*.cpp" )
#list( APPEND SRC "src/gfx.c" )

add_executable ( msxmulator ${SRC} )
target_link_libraries( msxmulator portaudio_static Threads::Threads glfw )