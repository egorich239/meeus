cmake_minimum_required(VERSION 3.25)

message(Source dir: ${CMAKE_SOURCE_DIR})
message(Target dir: ${CMAKE_BINARY_DIR})

project(meeus)

enable_language(Fortran)
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(dialect "-ffree-form -std=f2008 -fimplicit-none")
    set(bounds "-fbounds-check")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
    set(dialect "-stand f08 -free -implicitnone")
    set(bounds "-check bounds")
endif()
if(CMAKE_Fortran_COMPILER_ID MATCHES "PGI")
    set(dialect "-Mfreeform -Mdclchk -Mstandard -Mallocatable=03")
    set(bounds "-C")
endif()

set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} ${bounds}")
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${dialect}")

add_library(objects OBJECT src/date.f90)

add_library(meeusStatic STATIC $<TARGET_OBJECTS:objects>)
set_target_properties(meeusStatic PROPERTIES OUTPUT_NAME meeus)
add_library(meeus SHARED $<TARGET_OBJECTS:objects>)

find_package(PythonExtensions QUIET)

if(SKBUILD)
    find_package(SWIG 4.0 COMPONENTS python)
    include(${SWIG_USE_FILE})
    file(COPY include/date.h DESTINATION pybindings)
    file(RENAME
        ${CMAKE_BINARY_DIR}/pybindings/date.h
        ${CMAKE_BINARY_DIR}/pybindings/date.i)

    find_package(PythonLibs)
    include_directories(${PYTHON_INCLUDE_PATH})
    include_directories(${CMAKE_SOURCE_DIR}/include)
    set(CMAKE_SWIG_FLAGS "")

    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
    swig_add_library(pybindings
        TYPE MODULE
        LANGUAGE python
        SOURCES ${CMAKE_BINARY_DIR}/pybindings/date.i)
    set_source_files_properties(${CMAKE_BINARY_DIR}/pybindings/date.i
        PROPERTIES SWIG_FLAGS "-includeall")
    set_property(SOURCE ${CMAKE_BINARY_DIR}/pybindings/date.i
        PROPERTY SWIG_MODULE_NAME pybindings)
    set_property(SOURCE ${CMAKE_BINARY_DIR}/pybindings/date.i
        PROPERTY USE_LIBRARY_INCLUDE_DIRECTORIES TRUE)
    set_property(TARGET meeusStatic
        PROPERTY SWIG_USE_TARGET_INCLUDE_DIRECTORIES TRUE)
    swig_link_libraries(pybindings meeusStatic)
    swig_link_libraries(pybindings ${PYTHON_LIBRARIES})
    install(TARGETS pybindings LIBRARY DESTINATION lib)
    add_custom_command(
            TARGET pybindings POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy
                    ${CMAKE_CURRENT_BINARY_DIR}/pybindings.py
                    ${CMAKE_SOURCE_DIR}/python/meeus/_bindings/pybindings.py
            COMMAND ${CMAKE_COMMAND} -E copy
                    ${CMAKE_CURRENT_BINARY_DIR}/lib/_pybindings.so
                    ${CMAKE_SOURCE_DIR}/python/meeus/_bindings/_pybindings.so)
endif()