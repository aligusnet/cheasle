#include "ast_eval.h"
#include "cheasle/ast.h"
#include "cheasle/symbol_table.h"
#include "location.h"
#include <optional>
#include <sstream>

namespace cheasle {
namespace {

std::optional<bool> getBool(const std::optional<Value> &val) {
  if (val.has_value() && std::get_if<bool>(&val.value()) != nullptr) {
    return std::get<bool>(*val);
  }

  return std::nullopt;
}

std::optional<double> getDouble(const std::optional<Value> &val) {
  if (val.has_value() && std::get_if<double>(&val.value()) != nullptr) {
    return std::get<double>(*val);
  }

  return std::nullopt;
}

struct EvalWalker {
  EvalWalker(SymbolTable symbolTable, ErrorList &errors, std::ostream &os)
      : _symbolTable(std::move(symbolTable)), _errors(errors), _os(os) {}

  std::optional<Value> operator()(const AST &, const BinaryExpression &node) {
    auto lhs = getDouble(node.lhs.visit(*this));
    auto rhs = getDouble(node.rhs.visit(*this));
    if (!lhs || !rhs) {
      error("Binary expression expects both arguments of type double",
            node.location);
      return std::nullopt;
    }

    switch (node.op) {
    case BinaryOperator::Add:
      return *lhs + *rhs;
    case BinaryOperator::Subtract:
      return *lhs - *rhs;
    case BinaryOperator::Divide:
      return *lhs / *rhs;
    case BinaryOperator::Multiply:
      return *lhs * *rhs;
    }

    error("Unkwnown operator", node.location);
    return std::nullopt;
  }

  std::optional<Value> operator()(const AST &ast,
                                  const ComparisonExpression &node) {
    auto lhs = getDouble(node.lhs.visit(*this));
    auto rhs = getDouble(node.rhs.visit(*this));
    if (!lhs || !rhs) {
      error("Binary logical expression expects both arguments of type double",
            node.location);
      return std::nullopt;
    }

    switch (node.op) {
    case BinaryLogicalOperator::EQ:
      return *lhs == *rhs;
    case BinaryLogicalOperator::NE:
      return *lhs != *rhs;
    case BinaryLogicalOperator::GT:
      return *lhs > *rhs;
    case BinaryLogicalOperator::GE:
      return *lhs >= *rhs;
    case BinaryLogicalOperator::LT:
      return *lhs < *rhs;
    case BinaryLogicalOperator::LE:
      return *lhs <= *rhs;
    }

    error("Unkwnown boolean operator", node.location);
    return std::nullopt;
  }

  std::optional<Value> operator()(const AST &, const UnaryExpression &node) {
    auto value = getDouble(node.child.visit(*this));
    if (!value) {
      error("Unary operator expects one argument of type double.",
            node.location);
      return std::nullopt;
    }

    switch (node.op) {
    case UnaryOperator::Abs:
      return fabs(*value);
    case UnaryOperator::Minus:
      return -*value;
    }

    error("Unkwnown unary operator", node.location);
    return std::nullopt;
  }

  std::optional<Value> operator()(const AST &, const ConstantValue &node) {
    return node.value;
  }

  std::optional<Value> operator()(const AST &, const Block &node) {
    std::optional<Value> value = 0.0;
    for (const auto &child : node.children) {
      value = child.visit(*this);
      if (!value) {
        return std::nullopt;
      }
    }

    return value;
  }

  std::optional<Value> operator()(const AST &, const IfExpression &node) {
    auto value = getBool(node.condition.visit(*this));
    if (!value) {
      error("<If> expects a predicate to be a boolean.", node.location);
      return std::nullopt;
    }

    if (*value) {
      return node.thenBranch.visit(*this);
    } else {
      return node.elseBranch.visit(*this);
    }
  }

  std::optional<Value> operator()(const AST &, const WhileExpression &node) {
    const auto &pred = node.condition;
    const auto &body = node.body;
    std::optional<Value> value = 0.0;
    std::optional<Value> predValue;

    while (getBool(pred.visit(*this)).value_or(false)) {
      value = body.visit(*this);
      if (!value) {
        return std::nullopt;
      }
    }
    return value;
  }

  std::optional<Value> operator()(const AST &, const BuiltInFunction &node) {
    switch (node.id) {
    case BuiltInFunctionId::Exp:
      return callExp(node.arguments, node.location);
    case BuiltInFunctionId::Log:
      return callLog(node.arguments, node.location);
    case BuiltInFunctionId::Print:
      return callPrint(node.arguments, node.location);
    case BuiltInFunctionId::Sqrt:
      return callSqrt(node.arguments, node.location);
    }

    error("Unknown builtin function", node.location);
    return std::nullopt;
  }

  std::optional<Value> operator()(const AST &, const FunctionDefinition &node) {
    _symbolTable.define(
        node.name, FunctionSymbol{node.returnType, node.arguments, node.code});
    return 0.0;
  }

  std::optional<Value> operator()(const AST &, const FunctionCall &node) {
    auto funcOpt = _symbolTable.getFunction(node.name);
    if (!funcOpt) {
      error("Unknown function " + node.name, node.location);
      return std::nullopt;
    }

    if (node.arguments.size() != funcOpt->arguments.size()) {
      error("Wrong number of arguments passed to function " + node.name,
            node.location);
      return std::nullopt;
    }

    std::vector<ValueSymbol> values;
    values.reserve(node.arguments.size());

    SymbolTable childTable{&_symbolTable};
    for (size_t i = 0; i < funcOpt->arguments.size(); ++i) {
      auto value = node.arguments[i].visit(*this);
      if (!value) {
        return std::nullopt;
      }

      values.emplace_back();
      childTable.define(funcOpt->arguments[i].name,
                        ValueSymbol{funcOpt->arguments[i].type, *value, false});
    }

    EvalWalker eval{childTable, _errors, _os};
    return funcOpt->code.visit(eval);
  }

  std::optional<Value> operator()(const AST &, const VariableDefinition &node) {
    if (_symbolTable.isDefined(node.name)) {
      error("Variable " + node.name +
                " is already defined in the current scope.",
            node.location);
      return std::nullopt;
    }

    auto value = node.expr.visit(*this);
    if (!value) {
      return std::nullopt;
    }
    _symbolTable.define(node.name,
                        ValueSymbol{node.type, *value, node.isConstant});
    return value;
  }

  std::optional<Value> operator()(const AST &,
                                  const AssignmentExpression &node) {
    auto value = node.expr.visit(*this);
    if (!value) {
      return std::nullopt;
    }

    if (!_symbolTable.assign(node.name, *value)) {
      std::ostringstream oss;
      oss << "Failed to assign variable " << node.name << " to " << *value;
      error(oss.str(), node.location);
      return std::nullopt;
    }

    return value;
  }

  std::optional<Value> operator()(const AST &, const NameReference &node) {
    auto value = _symbolTable.getValue(node.name);
    if (value) {
      return *value;
    } else {
      error("Unknown name: " + node.name, node.location);
      return std::nullopt;
    }
  }

  std::optional<Value> callExp(const std::vector<AST> arguments,
                               const location &location) {
    if (arguments.size() == 1) {
      auto value = getDouble(arguments[0].visit(*this));
      if (!value) {
        return std::nullopt;
        error("Bultin function <exp> expects 1 double argument", location);
      }

      return exp(*value);
    }

    error("wrong number of arguments for exp", location);
    return std::nullopt;
  }

  std::optional<Value> callLog(const std::vector<AST> arguments,
                               const location &location) {
    if (arguments.size() == 1) {
      auto value = getDouble(arguments[0].visit(*this));
      if (!value) {
        error("Bultin function <log> expects 1 double argument", location);
        return std::nullopt;
      }
      return log(*value);
    }

    error("wrong number of arguments for log", location);
    return std::nullopt;
  }

  std::optional<Value> callPrint(const std::vector<AST> arguments,
                                 const location &location) {
    std::optional<Value> value = 0.0;
    _os << "out:";
    for (const auto &arg : arguments) {
      value = arg.visit(*this);
      if (!value) {
        return std::nullopt;
      }
      _os << ' ' << *value;
    }

    _os << std::endl;

    return value;
  }

  std::optional<Value> callSqrt(const std::vector<AST> arguments,
                                const location &location) {
    if (arguments.size() == 1) {
      auto value = getDouble(arguments[0].visit(*this));
      if (!value) {
        error("Bultin function <sqrt> expects 1 double argument", location);
        return std::nullopt;
      }

      return sqrt(*value);
    }

    error("wrong number of arguments for sqrt", location);
    return std::nullopt;
  }

  void error(std::string message, location location) {
    _errors.append("ast-eval", std::move(message), std::move(location));
  }

  SymbolTable _symbolTable;
  ErrorList &_errors;
  std::ostream &_os;
};
} // namespace

std::optional<Value> eval(const AST &node, ErrorList &errors,
                          std::ostream &os) {
  SymbolTable symbolTable{};
  EvalWalker ew{std::move(symbolTable), errors, os};
  return node.visit(ew);
}

} // namespace cheasle
