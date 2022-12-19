#pragma once

#include <cheasle/ast.h>
#include <cheasle/error.h>
#include <optional>

namespace cheasle {
std::optional<double> eval(const AST &node, ErrorList &errors);
} // namespace cheasle
