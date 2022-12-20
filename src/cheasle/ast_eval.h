#pragma once

#include <cheasle/ast.h>
#include <cheasle/error.h>
#include <iosfwd>
#include <optional>

namespace cheasle {
std::optional<Value> eval(const AST &node, ErrorList &errors, std::ostream &os);
} // namespace cheasle
