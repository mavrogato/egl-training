
cmake_minimum_required(VERSION 3.22)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_COMPILER icx)
set(CMAKE_CXX_COMPILER icpx)
set(CMAKE_CXX_FLAGS "-std=c++2b -sycl-std=2020")

set(PROJECT "egl-training")
project(${PROJECT})
find_package(IntelDPCPP REQUIRED)

include_directories(
  /opt/intel/openapi/compiler/2022.2.0/linux/include/sycl)

add_executable(${PROJECT}
  main.cc)

target_compile_options(${PROJECT}
  PRIVATE
  -Wall
  -O3)

target_link_libraries(${PROJECT}
  PRIVATE
  EGL
  OpenGL)

add_custom_target(run
  DEPENDS ${PROJECT}
  COMMAND ./${PROJECT})
