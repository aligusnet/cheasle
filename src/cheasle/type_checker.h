#pragma once

#include "Catch2/catch_amalgamated.hpp"
#include "cheasle/value.h"
#include <cheasle/ast.h>
#include <cheasle/error.h>

namespace cheasle {
ValueType checkTypes(AST &ast, ErrorList &errors);
} // namespace cheasle
