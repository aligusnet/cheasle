# generate files in CMAKE_CURRENT_BINARY_DIR
add_custom_command(OUTPUT lexer.h lexer.cpp DEPENDS Reflex ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l COMMAND Reflex ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l)
add_custom_command(OUTPUT parser.h parser.cpp DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/parser.y COMMAND bison ${CMAKE_CURRENT_SOURCE_DIR}/parser.y "-Wcounterexamples")

add_library(ChealseParser STATIC lexer.cpp parser.cpp error.cpp)

add_executable(cheasle_test integration_test.cpp ast.cpp ast_eval.cpp symbol_table.cpp)
target_link_libraries(cheasle_test catch2 ReflexLibStatic ChealseParser)

list(APPEND INCLUDES ${CMAKE_SOURCE_DIR}/src)
list(APPEND INCLUDES ${CMAKE_CURRENT_BINARY_DIR})
list(APPEND INCLUDES ${CMAKE_SOURCE_DIR}/src/third_party)
list(APPEND INCLUDES ${CMAKE_SOURCE_DIR}/src/third_party/RE-flex/include)

target_include_directories(ChealseParser PUBLIC ${INCLUDES})

set_property(TARGET ChealseParser PROPERTY CXX_STANDARD 20)
set_property(TARGET cheasle_test PROPERTY CXX_STANDARD 20)
