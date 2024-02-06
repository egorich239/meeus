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

    find_package(PythonLibs)
    include_directories(${PYTHON_INCLUDE_PATH})
    include_directories(${CMAKE_SOURCE_DIR}/include)
    set(CMAKE_SWIG_FLAGS "")

    swig_add_library(meeus_c_impl
        TYPE MODULE
        LANGUAGE python
        SOURCES include/meeus.i)
    set_source_files_properties(include/meeus.i PROPERTIES SWIG_FLAGS "-includeall")
    set_property(SOURCE include/meeus.i PROPERTY USE_LIBRARY_INCLUDE_DIRECTORIES TRUE)
    set_property(TARGET meeusStatic PROPERTY SWIG_USE_TARGET_INCLUDE_DIRECTORIES TRUE)
    swig_link_libraries(meeus_c_impl meeusStatic)
    swig_link_libraries(meeus_c_impl ${PYTHON_LIBRARIES})
    install(TARGETS meeus_c_impl LIBRARY DESTINATION lib)
    add_custom_command(
            TARGET meeus_c_impl POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy
                    ${CMAKE_BINARY_DIR}/_c.py
                    ${CMAKE_SOURCE_DIR}/python/meeus/_c.py
            COMMAND ${CMAKE_COMMAND} -E copy
                    ${CMAKE_BINARY_DIR}/_meeus_c_impl.so
                    ${CMAKE_SOURCE_DIR}/python/meeus/_meeus_c_impl.so
                    )
endif()