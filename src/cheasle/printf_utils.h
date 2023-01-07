#pragma once

#include "cheasle/value.h"
#include <ios>
#include <string>

namespace cheasle {
struct PrintfModifier {
  char modifier;
  std::ios_base::fmtflags formatFlags;
  ValueTypeMask valueTypeMask;
  // User friendly error message in case if the types do not match.
  std::string errorMessage() const;
};

std::pair<int, std::string> printfImpl(std::ostream &os,
                                       const std::string &format,
                                       const std::vector<Value> &arguments);

// Return empty string if no errors, otherwise return error.
std::string printfCheckTypes(const std::string &format,
                             const std::vector<ValueType> &argumentType);

} // namespace cheasle
