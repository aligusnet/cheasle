#include "ast_eval.h"
#include "cheasle/ast.h"
#include "cheasle/printf_utils.h"
#include "cheasle/symbol.h"
#include "cheasle/symbol_table.h"
#include "cheasle/value.h"
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

class EvalWalker {
private:
  EvalWalker(SymbolTable<VariableSymbol> variables,
             SymbolTable<FunctionSymbol> functions, ErrorList &errors,
             std::ostream &os)
      : _variables(std::move(variables)), _functions(std::move(functions)),
        _errors(errors), _os(os) {}

public:
  EvalWalker(ErrorList &errors, std::ostream &os)
      : _variables("global"), _functions("global"), _errors(errors), _os(os) {}

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
                                  const EqualityExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    if (!lhs || !rhs || lhs->index() != rhs->index()) {
      error("Equality expression expects both arguments of same type",
            node.location);
      return std::nullopt;
    }

    switch (node.op) {
    case EqualityOperator::EQ:
      return *lhs == *rhs;
    case EqualityOperator::NE:
      return *lhs != *rhs;
    }

    error("Unkwnown equality operator", node.location);
    return std::nullopt;
  }

  std::optional<Value> operator()(const AST &ast,
                                  const ComparisonExpression &node) {
    auto lhs = getDouble(node.lhs.visit(*this));
    auto rhs = getDouble(node.rhs.visit(*this));
    if (!lhs || !rhs) {
      error("Comparison expression expects both arguments of type double",
            node.location);
      return std::nullopt;
    }

    switch (node.op) {
    case ComparisonOperator::GT:
      return *lhs > *rhs;
    case ComparisonOperator::GE:
      return *lhs >= *rhs;
    case ComparisonOperator::LT:
      return *lhs < *rhs;
    case ComparisonOperator::LE:
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

  std::optional<Value> operator()(const AST &,
                                  const BinaryLogicalExpression &node) {
    auto lhs = getBool(node.lhs.visit(*this));
    if (!lhs) {
      error("Binary logical expression expects first argument of type bool",
            node.location);
      return std::nullopt;
    }

    switch (node.op) {
    case BinaryLogicalOperator::And:
      if (!(*lhs)) {
        return false;
      }

      break;
    case BinaryLogicalOperator::Or:
      if (*lhs) {
        return true;
      }

      break;
    }

    auto rhs = getBool(node.rhs.visit(*this));
    if (!rhs) {
      error("Binary logical expression expects second argument of type bool",
            node.location);
      return std::nullopt;
    }

    switch (node.op) {
    case BinaryLogicalOperator::And:
      // lhs is expected to be true here.
      return *rhs;
    case BinaryLogicalOperator::Or:
      // lhs is expected to be false here.
      return *rhs;
    }

    return std::nullopt;
  }

  std::optional<Value> operator()(const AST &, const NotExpression &node) {
    auto child = getBool(node.child.visit(*this));
    if (!child) {
      error("Not expression expects its argument of type bool", node.location);
      return std::nullopt;
    }

    return !*child;
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
    case BuiltInFunctionId::Sqrt:
      return callSqrt(node.arguments, node.location);
    case BuiltInFunctionId::Printf:
      return callPrintf(node.arguments, node.location);
    }

    error("Unknown builtin function", node.location);
    return std::nullopt;
  }

  std::optional<Value> operator()(const AST &, const FunctionDefinition &node) {
    _functions.define(
        node.name, FunctionSymbol{node.returnType, node.arguments, node.code});
    return 0.0;
  }

  std::optional<Value> operator()(const AST &, const FunctionCall &node) {
    auto funcOpt = _functions.get(node.name);
    if (!funcOpt) {
      error("Unknown function " + node.name, node.location);
      return std::nullopt;
    }

    if (node.arguments.size() != funcOpt->arguments.size()) {
      error("Wrong number of arguments passed to function " + node.name,
            node.location);
      return std::nullopt;
    }

    std::vector<VariableSymbol> values;
    values.reserve(node.arguments.size());

    SymbolTable<VariableSymbol> variables{node.name, &_variables};
    SymbolTable<FunctionSymbol> functions{node.name, &_functions};
    for (size_t i = 0; i < funcOpt->arguments.size(); ++i) {
      auto value = node.arguments[i].visit(*this);
      if (!value) {
        return std::nullopt;
      }

      values.emplace_back();
      variables.define(
          funcOpt->arguments[i].name,
          VariableSymbol{funcOpt->arguments[i].type, *value, false});
    }

    EvalWalker eval{std::move(variables), std::move(functions), _errors, _os};
    return funcOpt->code.visit(eval);
  }

  std::optional<Value> operator()(const AST &, const VariableDefinition &node) {
    if (_variables.isLocallyDefined(node.name)) {
      error("Variable " + node.name +
                " is already defined in the current scope.",
            node.location);
      return std::nullopt;
    }

    auto value = node.expr.visit(*this);
    if (!value) {
      return std::nullopt;
    }
    _variables.define(node.name,
                      VariableSymbol{node.type, *value, node.isConstant});
    return value;
  }

  std::optional<Value> operator()(const AST &,
                                  const AssignmentExpression &node) {
    auto value = node.expr.visit(*this);
    if (!value) {
      return std::nullopt;
    }

    auto valueSymbol = _variables.get(node.name);
    if (!valueSymbol) {
      std::ostringstream oss;
      oss << "Failed to assign variable " << node.name << " to " << *value;
      error(oss.str(), node.location);
      return std::nullopt;
    }

    valueSymbol->value = std::move(*value);
    if (!_variables.assign(node.name, std::move(*valueSymbol))) {
      std::ostringstream oss;
      oss << "Failed to assign variable " << node.name << " to " << *value;
      error(oss.str(), node.location);
      return std::nullopt;
    }

    return value;
  }

  std::optional<Value> operator()(const AST &, const NameReference &node) {
    auto valueSymbol = _variables.get(node.name);
    if (valueSymbol) {
      return valueSymbol->value;
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

  std::optional<Value> callPrintf(const std::vector<AST> arguments,
                                  const location &location) {
    // Naive implementation of printf.
    if (arguments.size() == 0) {
      error("Printf expects at least one argument of type string", location);
      return -1;
    }

    auto formatVar = arguments.front().visit(*this);
    if (!formatVar) {
      return -1;
    }

    if (std::get_if<std::string>(&*formatVar) == nullptr) {
      error("First argument of printf is expected to be string", location);
      return -1;
    }

    std::stringstream os;
    std::string format = std::get<std::string>(*formatVar);
    std::vector<Value> argValues;
    argValues.reserve(arguments.size() -
                      1); // first argument is always format string
    for (size_t i = 1; i < arguments.size(); ++i) {
      auto argValue = arguments[i].visit(*this);
      if (!argValue) {
        return -1;
      }
      argValues.emplace_back(std::move(*argValue));
    }

    auto result = printfImpl(os, format, argValues);
    if (result.first != 0) {
      error(result.second, location);
      return result.first;
    }

    std::string outputString = os.str();
    _os << outputString;

    return static_cast<int32_t>(outputString.size());
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

  SymbolTable<VariableSymbol> _variables;
  SymbolTable<FunctionSymbol> _functions;
  ErrorList &_errors;
  std::ostream &_os;
};
} // namespace

std::optional<Value> eval(const AST &node, ErrorList &errors,
                          std::ostream &os) {
  EvalWalker ew{errors, os};
  return node.visit(ew);
}

} // namespace cheasle
