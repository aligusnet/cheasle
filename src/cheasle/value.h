#pragma once
#include <iosfwd>
#include <string>
#include <variant>

namespace cheasle {
using Value = std::variant<bool, int32_t, double, std::string>;

enum class ValueType {
  Any = 0,
  Boolean = 1 << 0,
  Int = 1 << 1,
  Double = 1 << 2,
  String = 1 << 3,
  Function = 1 << 4
};

struct ValueTypeMask {
  using T = std::underlying_type_t<ValueType>;
  explicit ValueTypeMask(T mask = 0) : mask(mask) {}
  ValueTypeMask(ValueType type) : ValueTypeMask(static_cast<T>(type)) {}
  T mask;
  bool check(const Value &value) const;
  bool check(const ValueType &type) const;
  bool empty() const { return mask == 0; }
};

inline bool operator==(ValueTypeMask lhs, ValueTypeMask rhs) {
  return lhs.mask == rhs.mask;
}

inline bool operator==(ValueTypeMask lhs, ValueType rhs) {
  using T = std::underlying_type_t<ValueType>;
  return lhs.mask == static_cast<T>(rhs);
}

inline bool operator==(ValueType lhs, ValueTypeMask rhs) { return rhs == lhs; }

inline ValueTypeMask operator|(ValueTypeMask lhs, ValueType rhs) {
  using T = std::underlying_type_t<ValueType>;
  return ValueTypeMask{lhs.mask | static_cast<T>(rhs)};
}

inline ValueTypeMask operator|(ValueType lhs, ValueType rhs) {
  using T = std::underlying_type_t<ValueType>;
  return ValueTypeMask{static_cast<T>(lhs) | static_cast<T>(rhs)};
}

inline ValueTypeMask operator&(ValueTypeMask lhs, ValueType rhs) {
  using T = std::underlying_type_t<ValueType>;
  return ValueTypeMask{lhs.mask & static_cast<T>(rhs)};
}

inline ValueTypeMask operator&(ValueType lhs, ValueTypeMask rhs) {
  return rhs & lhs;
}

bool checkValueType(ValueType type, const Value &value);

std::ostream &operator<<(std::ostream &os, const Value &val);
std::ostream &operator<<(std::ostream &os, const ValueType &type);
std::ostream &operator<<(std::ostream &os, const ValueTypeMask &mask);
} // namespace cheasle
