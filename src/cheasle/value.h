#pragma once
#include <iosfwd>
#include <variant>

namespace cheasle {
using Value = std::variant<bool, double>;

enum class ValueType { Any, Boolean, Double, Function };

std::ostream &operator<<(std::ostream &os, const Value &val);
std::ostream &operator<<(std::ostream &os, const ValueType &type);
} // namespace cheasle
