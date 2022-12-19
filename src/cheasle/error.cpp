#include "error.h"
#include <sstream>

namespace cheasle {
std::ostream &operator<<(std::ostream &os, const Error &error) {
  os << '[' << error._source << "] " << error._location << ": "
     << error._message;
  return os;
}

std::ostream &operator<<(std::ostream &os, const ErrorList &errors) {
  os << errors._errors.size() << " errors." << std::endl;
  for (const auto &error : errors._errors) {
    os << error << std::endl;
  }
  return os;
}
} // namespace cheasle
