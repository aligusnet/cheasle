#include "value.h"
#include <sstream>

namespace cheasle {

template <class... Ts> struct Visitor : Ts... {
  using Ts::operator()...;
};
template <class... Ts> Visitor(Ts...) -> Visitor<Ts...>;

std::ostream &operator<<(std::ostream &os, const Value &val) {
  std::visit(Visitor{[&os](const auto &v) { os << std::boolalpha << v; }}, val);
  return os;
}

std::ostream &operator<<(std::ostream &os, const ValueType &type) {
  switch (type) {
  case ValueType::Boolean:
    os << "bool";
    break;
  case ValueType::Double:
    os << "double";
    break;
  }
  return os;
}
} // namespace cheasle
