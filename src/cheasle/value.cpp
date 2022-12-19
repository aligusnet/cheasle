#include "value.h"
#include <sstream>

namespace cheasle {

template <class... Ts> struct Visitor : Ts... {
  using Ts::operator()...;
};
template <class... Ts> Visitor(Ts...) -> Visitor<Ts...>;

std::ostream &operator<<(std::ostream &os, const Value &val) {
  std::visit(Visitor{[&os](const auto &v) { os << v; }}, val);
  return os;
}
} // namespace cheasle
