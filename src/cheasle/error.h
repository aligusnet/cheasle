#pragma once

#include <iosfwd>
#include <location.h>
#include <string>

namespace cheasle {

class Error {
public:
  Error(std::string source, std::string message, location location)
      : _source(std::move(source)), _message(std::move(message)),
        _location(std::move(location)) {}

private:
  friend std::ostream &operator<<(std::ostream &os, const Error &error);

  std::string _source;
  std::string _message;
  location _location;
};

class ErrorList {
public:
  void append(std::string source, std::string message, location location) {
    _errors.emplace_back(std::move(source), std::move(message),
                         std::move(location));
  }

  bool hasErrors() const { return !_errors.empty(); }

private:
  friend std::ostream &operator<<(std::ostream &os, const ErrorList &errors);

  std::vector<Error> _errors{};
};

std::ostream &operator<<(std::ostream &os, const Error &error);
std::ostream &operator<<(std::ostream &os, const ErrorList &errors);

} // namespace cheasle
