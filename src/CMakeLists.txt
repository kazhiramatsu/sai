cmake_minimum_required(VERSION 2.8)

project(sai)

find_package(bison)

file(GLOB SAI_SRC_C "*.c")

bison_target(sai js.y "${CMAKE_CURRENT_SOURCE_DIR}/js.tab.c")

#include_directories(/Users/hiramatsu/miyabi/include /usr/local/include)
include_directories(../include)

#link_directories(/usr/local/lib)

add_definitions(-DDEBUG)

#SET(CMAKE_C_FLAGS_RELEASE "-Wall -O3")
SET(CMAKE_C_FLAGS_DEBUG "-g -Wall")
#SET(CMAKE_BUILD_TYPE Release)
set(CMAKE_C_FLAGS_PROFILE "-pg")

#cmake . -DCMAKE_BUILD_TYPE=Release
#set(CMAKE_C_FLAGS "-std=c99 -Wall")
#message("CMAKE_BUILD_TYPE is ${CMAKE_BUILD_TYPE}")

add_executable(sai ${SAI_SRC_C} ${BISON_sai_OUTPUTS})

target_link_libraries(sai)

