#include "printf_utils.h"
#include "cheasle/value.h"
#include <ostream>
#include <sstream>
#include <string_view>
#include <unordered_map>
#include <variant>

namespace cheasle {
namespace {
std::unordered_map<char, PrintfModifier> makeTypeModifiersMap() {
  auto intBool = ValueType::Int | ValueType::Boolean;
  return std::unordered_map<char, PrintfModifier>{
      {'d', {'d', {}, intBool}},
      {'i', {'i', {}, intBool}},
      {'u', {'u', {}, intBool}},
      {'o', {'o', std::ios_base::oct, intBool}},
      {'x', {'x', std::ios_base::hex, intBool}},
      {'X', {'X', std::ios_base::hex | std::ios_base::uppercase, intBool}},
      {'f', {'f', std::ios_base::fixed, ValueType::Double}},
      {'F',
       {'F', std::ios_base::fixed | std::ios_base::uppercase,
        ValueType::Double}},
      {'e', {'e', std::ios_base::scientific, ValueType::Double}},
      {'E',
       {'E', std::ios_base::scientific | std::ios_base::uppercase,
        ValueType::Double}},
      {'g', {'g', {}, ValueType::Double}},
      {'G', {'G', std::ios_base::uppercase, ValueType::Double}},
      {'a', {'a', std::ios_base::floatfield, ValueType::Double}},
      {'A',
       {'A', std::ios_base::floatfield | std::ios_base::uppercase,
        ValueType::Double}},
      {'c', {'c', {}, ValueType::Int}},
      {'s', {'s', {}, ValueType::String}},
      {'p', {'p', {}, ValueTypeMask{}}},
      {'n', {'n', {}, ValueTypeMask{}}},
  };
}

const std::unordered_map<char, PrintfModifier> &getCachedTypeModifiersMap() {
  static const auto typeModifiersMap = makeTypeModifiersMap();
  return typeModifiersMap;
}

class PrintfIterator {
public:
  using ValueType = std::variant<std::string_view, PrintfModifier>;

  PrintfIterator(const char *pos = nullptr)
      : _pos(pos), _modifiersMap(getCachedTypeModifiersMap()) {}

  PrintfIterator &operator++() {
    _current = next();
    return *this;
  }

  ValueType &operator*() { return _current; }

  friend bool operator==(const PrintfIterator &a, const PrintfIterator &b) {
    return a._pos == b._pos;
  };

  friend bool operator!=(const PrintfIterator &a, const PrintfIterator &b) {
    return !(a == b);
  };

private:
  ValueType next() {
    if (_pos == nullptr || *_pos == '\0') {
      _pos = nullptr;
      return std::string_view("");
    }

    if (*_pos == '%') {
      ++_pos;
      // skip flags, width, precision, and length
      for (; !_modifiersMap.contains(*_pos) && *_pos != '%'; ++_pos)
        ;
      if (*_pos == '%') {
        // return single % symbol and move forward
        return std::string_view(_pos++, 1);
      }
      // we found that *_pos contains in the map a step easlier
      auto it = _modifiersMap.find(*_pos);
      // not required: assert (it != _modifiersMap.end());
      ++_pos; // move forward before returning, since we already examined the
              // current symbol
      return it->second;
    }

    auto start = _pos;
    for (; *_pos != '\0'; ++_pos) {
      if (*_pos == '%') {
        return std::string_view(start, _pos - start);
        // not moving forward, % will be processed on the next call
      }
    }

    // we exhasted format string
    return std::string_view(start, _pos - start);
  }

  const char *_pos;
  ValueType _current;
  const std::unordered_map<char, PrintfModifier> &_modifiersMap;
};
} // namespace

std::string PrintfModifier::errorMessage() const {
  std::ostringstream oss;
  oss << "Modifier '" << modifier << "' ";
  if (valueTypeMask.empty()) {
    oss << "is not supported";
  } else {
    oss << "expects " << valueTypeMask;
  }
  return oss.str();
}

std::pair<int, std::string> printfImpl(std::ostream &os,
                                       const std::string &format,
                                       const std::vector<Value> &arguments) {
  // remember default flags of the stream
  std::ios::fmtflags defaultFlags(os.flags());
  size_t argumentIndex = 0;
  PrintfIterator it(format.c_str());
  PrintfIterator end{};
  for (; it != end; ++it) {
    // restore the default flags on each iteration
    os.setf(defaultFlags);

    const auto &value = *it;
    if (std::get_if<std::string_view>(&value) != nullptr) {
      os << std::get<std::string_view>(value);
    } else {
      const auto &modifier = std::get<PrintfModifier>(value);
      if (argumentIndex >= arguments.size()) {
        return std::make_pair(-10, "not enough arguments");
      }

      const auto &arg = arguments[argumentIndex++];
      if (!modifier.valueTypeMask.check(arg)) {
        return std::make_pair(-11, modifier.errorMessage());
      }

      os.setf(modifier.formatFlags);
      // char case
      if (modifier.modifier == 'c' && std::get_if<int32_t>(&arg) != nullptr) {
        os << static_cast<char>(std::get<int32_t>(arg));
      } else {
        os << arg;
      }
    }
  }

  return std::make_pair(0, std::string());
}

std::string printfCheckTypes(const std::string &format,
                             const std::vector<ValueType> &argumentTypes) {
  PrintfIterator it(format.c_str());
  PrintfIterator end{};
  size_t argumentIndex = 0;
  for (; it != end; ++it) {
    const auto &value = *it;
    if (std::get_if<PrintfModifier>(&value) == nullptr) {
      continue;
    }
    const auto &modifier = std::get<PrintfModifier>(value);

    if (argumentIndex >= argumentTypes.size()) {
      return "not enough arguments";
    }

    auto argType = argumentTypes[argumentIndex++];

    if ((modifier.valueTypeMask & argType) != argType) {
      return modifier.errorMessage();
    }
  }

  if (argumentIndex != argumentTypes.size()) {
    return "too many arguments passed in";
  }

  return std::string();
}
}; // namespace cheasle
