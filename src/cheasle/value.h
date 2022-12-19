#pragma once
#include <iosfwd>
#include <variant>

namespace cheasle {
using Value = std::variant<bool, double>;

enum class ValueType { Boolean, Double };

std::ostream &operator<<(std::ostream &os, const Value &val);
std::ostream &operator<<(std::ostream &os, const ValueType &type);
} // namespace cheasle
