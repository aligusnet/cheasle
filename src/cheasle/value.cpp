#include "value.h"
#include <sstream>

namespace cheasle {

namespace {
template <ValueType type> inline bool typeIs(ValueTypeMask mask) {
  return (mask & type) == type;
}

template <ValueType type> bool containsValueOf(const Value &value);

template <> inline bool containsValueOf<ValueType::Any>(const Value &value) {
  return false;
}

template <>
inline bool containsValueOf<ValueType::Boolean>(const Value &value) {
  return std::get_if<bool>(&value) != nullptr;
}

template <> inline bool containsValueOf<ValueType::Int>(const Value &value) {
  return std::get_if<int32_t>(&value) != nullptr;
}

template <> inline bool containsValueOf<ValueType::Double>(const Value &value) {
  return std::get_if<double>(&value) != nullptr;
}

template <> inline bool containsValueOf<ValueType::String>(const Value &value) {
  return std::get_if<std::string>(&value) != nullptr;
}

template <>
inline bool containsValueOf<ValueType::Function>(const Value &value) {
  return false;
}

template <ValueType type>
bool checkValueTypeFor(ValueTypeMask mask, const Value &value) {
  return typeIs<type>(mask) && containsValueOf<type>(value);
}
} // namespace

bool ValueTypeMask::check(const Value &value) const {
  return checkValueTypeFor<ValueType::Int>(*this, value) ||
         checkValueTypeFor<ValueType::Boolean>(*this, value) ||
         checkValueTypeFor<ValueType::Double>(*this, value) ||
         checkValueTypeFor<ValueType::String>(*this, value);
}

bool ValueTypeMask::check(const ValueType &type) const {
  return (*this & type) == type;
}

bool checkValueType(ValueType type, const Value &value) {
  switch (type) {
  case ValueType::Boolean:
    return std::get_if<bool>(&value) != nullptr;
  case ValueType::Int:
    return std::get_if<int32_t>(&value) != nullptr;
  case ValueType::Double:
    return std::get_if<double>(&value) != nullptr;
  case ValueType::String:
    return std::get_if<std::string>(&value) != nullptr;
  case ValueType::Any:
    return false;
  case ValueType::Function:
    return false;
  }
}

template <class... Ts> struct Visitor : Ts... {
  using Ts::operator()...;
};
template <class... Ts> Visitor(Ts...) -> Visitor<Ts...>;

std::ostream &operator<<(std::ostream &os, const Value &val) {
  std::visit(Visitor{[&os](const auto &v) { os << v; }}, val);
  return os;
}

std::ostream &operator<<(std::ostream &os, const ValueType &type) {
  switch (type) {
  case ValueType::Boolean:
    os << "bool";
    break;
  case ValueType::Int:
    os << "int";
    break;
  case ValueType::Double:
    os << "double";
    break;
  case ValueType::String:
    os << "string";
    break;
  case ValueType::Any:
    os << "any";
    break;
  case ValueType::Function:
    os << "function";
    break;
  }

  return os;
}

template <ValueType type>
void printIfTypeMatches(std::ostream &os, ValueTypeMask mask,
                        bool &firstMatched) {
  if (typeIs<type>(mask)) {
    if (firstMatched) {
      firstMatched = false;
    } else {
      os << ", ";
    }
    os << type;
  }
}

std::ostream &operator<<(std::ostream &os, const ValueTypeMask &mask) {
  bool firstMacthed = true;
  // don't print Any since it is no real type
  printIfTypeMatches<ValueType::Boolean>(os, mask, firstMacthed);
  printIfTypeMatches<ValueType::Int>(os, mask, firstMacthed);
  printIfTypeMatches<ValueType::Double>(os, mask, firstMacthed);
  printIfTypeMatches<ValueType::String>(os, mask, firstMacthed);
  printIfTypeMatches<ValueType::Function>(os, mask, firstMacthed);

  return os;
}
} // namespace cheasle
