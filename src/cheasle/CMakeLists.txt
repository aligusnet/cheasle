# generate files in CMAKE_CURRENT_BINARY_DIR
add_custom_command(
    OUTPUT lexer.h lexer.cpp
    DEPENDS Reflex ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l
    COMMAND Reflex ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l)

add_custom_command(
    OUTPUT parser.h parser.cpp
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/parser.y
    COMMAND bison ${CMAKE_CURRENT_SOURCE_DIR}/parser.y "-Wcounterexamples")

find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "LLVM targets: ${LLVM_TARGETS_TO_BUILD}")

add_library(ChealseParserLib STATIC
    lexer.cpp
    parser.cpp
    error.cpp
    value.cpp
)

add_library(CheasleLib STATIC
    ast.cpp
    ast_eval.cpp
    type_checker.cpp
    llvm_codegen.cpp
    function_push_up.cpp
)

add_executable(cheasle_test
    ast_test_util.cpp
    ast_eval_test.cpp
    integration_test.cpp
    parser_test.cpp
    type_checker_test.cpp
    llvm_codegen_test.cpp
    function_push_up_test.cpp
)

target_link_libraries(cheasle_test catch2 ReflexLibStatic ChealseParserLib CheasleLib)

list(APPEND INCLUDES ${CMAKE_SOURCE_DIR}/src)
list(APPEND INCLUDES ${CMAKE_CURRENT_BINARY_DIR})
list(APPEND INCLUDES ${CMAKE_SOURCE_DIR}/src/third_party)
list(APPEND INCLUDES ${CMAKE_SOURCE_DIR}/src/third_party/RE-flex/include)
list(APPEND INCLUDES ${LLVM_INCLUDE_DIRS})

target_include_directories(ChealseParserLib PUBLIC ${INCLUDES})
target_include_directories(CheasleLib PUBLIC ${INCLUDES})

separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
llvm_map_components_to_libnames(llvm_libs core orcjit native support)
target_link_libraries(cheasle_test ${llvm_libs})

set_property(TARGET ChealseParserLib PROPERTY CXX_STANDARD 20)
set_property(TARGET CheasleLib PROPERTY CXX_STANDARD 20)
set_property(TARGET cheasle_test PROPERTY CXX_STANDARD 20)
