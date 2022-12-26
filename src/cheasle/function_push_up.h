#pragma once

#include <cheasle/ast.h>
#include <cheasle/error.h>

namespace cheasle {
std::optional<AST> pushUpFunction(std::string mainName, ValueType mainType,
                                  AST ast, ErrorList &errors);
}
