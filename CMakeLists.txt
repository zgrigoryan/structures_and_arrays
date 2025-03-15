cmake_minimum_required(VERSION 3.10)

project(StructuresAndArrays)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

set(SOURCES 
    src/main.cpp
)

add_executable(StructuresAndArrays ${SOURCES})
