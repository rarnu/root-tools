cmake_minimum_required(VERSION 3.3)
project(jni)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    Android.mk
    Application.mk
    common.cpp
    common.h
    termExec.cpp
    termExec.h)

add_executable(jni ${SOURCE_FILES})