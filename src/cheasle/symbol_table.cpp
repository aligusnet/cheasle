#include "symbol_table.h"
#include <optional>

namespace cheasle {

bool SymbolTable::assign(const std::string &name, Value value) {
  auto itopt = find(name);
  if (itopt) {
    const auto &it = *itopt;
    if (auto *val = std::get_if<ValueSymbol>(&it->second.data)) {
      if (!val->isConstant) {
        val->value = value;
        return true;
      }
    }
  }

  return false;
}

std::optional<Value> SymbolTable::getValue(const std::string &name) const {
  auto itopt = find(name);
  if (itopt) {
    const auto &it = *itopt;
    if (const auto *val = std::get_if<ValueSymbol>(&it->second.data)) {
      return val->value;
    }
  }

  return std::nullopt;
}

std::optional<ValueSymbol>
SymbolTable::getVariable(const std::string &name) const {
  auto itopt = find(name);
  if (itopt) {
    const auto &it = *itopt;
    if (const auto *var = std::get_if<ValueSymbol>(&it->second.data)) {
      return *var;
    }
  }

  return std::nullopt;
}

std::optional<FunctionSymbol>
SymbolTable::getFunction(const std::string &name) const {
  auto itopt = find(name);
  if (itopt) {
    const auto &it = *itopt;
    if (const auto *func = std::get_if<FunctionSymbol>(&it->second.data)) {
      return *func;
    }
  }

  return std::nullopt;
}

std::optional<SymbolTable::Table::iterator>
SymbolTable::find(const std::string &name) {
  auto it = _table.find(name);
  if (it != _table.end()) {
    return it;
  }

  if (_parent != nullptr) {
    return _parent->find(name);
  }

  return std::nullopt;
}

std::optional<SymbolTable::Table::const_iterator>
SymbolTable::find(const std::string &name) const {
  auto it = _table.find(name);
  if (it != _table.end()) {
    return it;
  }

  if (_parent != nullptr) {
    return _parent->find(name);
  }

  return std::nullopt;
}
} // namespace cheasle
