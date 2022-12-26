#include <ratio>
#pragma once
#include <cheasle/ast.h>
#include <cheasle/error.h>

namespace cheasle {
std::optional<Value> compileAndRun(AST ast, ErrorList &errors);
} // namespace cheasle
