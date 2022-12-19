# generate files in CMAKE_CURRENT_BINARY_DIR
add_custom_command(OUTPUT lexer.h lexer.cpp DEPENDS Reflex ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l COMMAND Reflex ${CMAKE_CURRENT_SOURCE_DIR}/lexer.l)
add_custom_command(OUTPUT parser.h parser.cpp DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/parser.y COMMAND bison ${CMAKE_CURRENT_SOURCE_DIR}/parser.y "-Wcounterexamples")

add_library(ChealseParserLib STATIC lexer.cpp parser.cpp error.cpp value.cpp)

add_library(CheasleLib STATIC ast.cpp ast_eval.cpp symbol_table.cpp)

add_executable(cheasle_test ast_test_util.cpp integration_test.cpp parser_test.cpp)
target_link_libraries(cheasle_test catch2 ReflexLibStatic ChealseParserLib CheasleLib)

list(APPEND INCLUDES ${CMAKE_SOURCE_DIR}/src)
list(APPEND INCLUDES ${CMAKE_CURRENT_BINARY_DIR})
list(APPEND INCLUDES ${CMAKE_SOURCE_DIR}/src/third_party)
list(APPEND INCLUDES ${CMAKE_SOURCE_DIR}/src/third_party/RE-flex/include)

target_include_directories(ChealseParserLib PUBLIC ${INCLUDES})
target_include_directories(CheasleLib PUBLIC ${INCLUDES})

set_property(TARGET ChealseParserLib PROPERTY CXX_STANDARD 20)
set_property(TARGET CheasleLib PROPERTY CXX_STANDARD 20)
set_property(TARGET cheasle_test PROPERTY CXX_STANDARD 20)
