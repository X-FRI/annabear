cmake_minimum_required(VERSION 3.14)
project(annabear)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include_directories(include)

option(BUILD_TEST "Whether or not to build the tests" ON)
if (${BUILD_TEST})
    message(STATUS "Building tests")
    enable_testing()
    
    include(FetchContent)
    FetchContent_Declare(
        googletest
        URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
    )

    set(gtest_force_shared_crt ON CACHE BOOL "" FORCE)
    FetchContent_MakeAvailable(googletest)
    
    aux_source_directory(tests TESTS_FILE)
    add_executable(test_${PROJECT_NAME} ${TESTS_FILE})
    target_link_libraries(test_${PROJECT_NAME} GTest::gtest_main)
    
    include(GoogleTest)
    gtest_discover_tests(test_${PROJECT_NAME})
endif()