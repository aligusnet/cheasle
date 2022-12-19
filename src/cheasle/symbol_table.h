#pragma once

#include <cheasle/ast.h>
#include <cheasle/value.h>
#include <list>
#include <optional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

namespace cheasle {

struct ValueInfo {
  Value value;
  bool isConstant;
};

struct UserFunction {
  AST code;
  std::vector<std::string> arguments;
};

// value or user function
struct SymbolData {
  std::variant<ValueInfo, UserFunction> data;
};

class SymbolTable {
public:
  explicit SymbolTable(SymbolTable *parent = nullptr) : _parent(parent) {}

  void define(const std::string &name, UserFunction func) {
    _table[name].data = std::move(func);
  }

  void define(const std::string &name, ValueInfo val) {
    _table[name].data = val;
  }

  bool assign(const std::string &name, Value value);

  bool isDefined(const std::string &name) const {
    return _table.contains(name);
  }

  std::optional<Value> getValue(const std::string &name) const;

  std::optional<UserFunction> getFunction(const std::string &name) const;

private:
  using Table = std::unordered_map<std::string, SymbolData>;
  std::optional<Table::iterator> find(const std::string &name);
  std::optional<Table::const_iterator> find(const std::string &name) const;

  SymbolTable *_parent;
  Table _table;
};
} // namespace cheasle
