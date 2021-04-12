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
endif()

include_directories( "include" )

file( GLOB_RECURSE SRC "src/*.cpp" )

add_executable ( msxmulator ${SRC} )
