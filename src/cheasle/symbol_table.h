#pragma once

#include <list>
#include <memory>
#include <string>
#include <unordered_map>

namespace cheasle {
template <typename T> class SymbolTable {
public:
  explicit SymbolTable(std::string scopeName, SymbolTable *parent = nullptr)
      : _scopeName(parent == nullptr ? scopeName
                                     : parent->_scopeName + "__" + scopeName),
        _parent(parent) {}

  std::optional<T> get(const std::string &name) const {
    auto it = _table.find(name);
    if (it != _table.end()) {
      return it->second;
    }
    if (_parent != nullptr) {
      return _parent->get(name);
    }
    return std::nullopt;
  }

  bool isLocallyDefined(const std::string &name) const {
    return _table.contains(name);
  }

  void define(const std::string &name, T val) { _table[name] = std::move(val); }

  bool assign(const std::string &name, T value) {
    auto it = _table.find(name);
    if (it != _table.end()) {
      _table[name] = std::move(value);
      return true;
    }

    if (_parent != nullptr) {
      return _parent->assign(name, std::move(value));
    }

    return false;
  }

  void clear() { _table.clear(); }

  const std::string &getScopeName() const { return _scopeName; }

private:
  const std::string _scopeName;
  SymbolTable<T> *_parent;
  std::unordered_map<std::string, T> _table;
};

template <typename T> class ScopedSymbolTable {
public:
  explicit ScopedSymbolTable(std::string globalScopeName) {
    auto table =
        std::make_unique<SymbolTable<T>>(std::move(globalScopeName), nullptr);
    _scopes.emplace_front(std::move(table));
  }

  std::optional<T> get(const std::string &name) const {
    return _scopes.front()->get(name);
  }

  bool isLocallyDefined(const std::string &name) const {
    return _scopes.front()->isLocallyDefined(name);
  }

  void define(const std::string &name, T value) {
    _scopes.front()->define(name, std::move(value));
  }

  bool assign(const std::string &name, T value) {
    return _scopes.front()->assign(name, std::move(value));
  }

  const std::string &getScopeName() const {
    return _scopes.front()->getScopeName();
  }

private:
  template <typename U> friend struct ScopeGuard;

  void enterScope(std::string scopeName) {
    auto table = std::make_unique<SymbolTable<T>>(std::move(scopeName),
                                                  _scopes.front().get());
    _scopes.emplace_front(std::move(table));
  }

  void leaveScope() { _scopes.pop_front(); }

  std::list<std::unique_ptr<SymbolTable<T>>> _scopes;
};

template <typename T> struct ScopeGuard {
  ScopeGuard(ScopedSymbolTable<T> &table, std::string scopeName)
      : _table(table) {
    _table.enterScope(std::move(scopeName));
  }

  ~ScopeGuard() { _table.leaveScope(); }

private:
  ScopedSymbolTable<T> &_table;
};
} // namespace cheasle
