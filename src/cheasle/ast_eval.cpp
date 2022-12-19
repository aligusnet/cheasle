#include "ast_eval.h"
#include "cheasle/ast.h"
#include "cheasle/symbol_table.h"
#include "location.h"
#include <iostream>
#include <sstream>

namespace cheasle {

struct EvalWalker {

  EvalWalker(SymbolTable symbolTable, ErrorList &errors)
      : _symbolTable(std::move(symbolTable)), _errors(errors) {}

  std::optional<double> operator()(const AST &, const BinaryExpression &node) {
    auto lhs = node.get<0>().visit(*this);
    auto rhs = node.get<1>().visit(*this);
    if (!lhs || !rhs) {
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

  std::optional<double> operator()(const AST &ast,
                                   const BinaryLogicalExpression &node) {
    auto lhs = node.get<0>().visit(*this);
    auto rhs = node.get<1>().visit(*this);
    if (!lhs || !rhs) {
      return std::nullopt;
    }

    switch (node.op) {
    case BinaryLogicalOperator::EQ:
      return *lhs == *rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::NE:
      return *lhs != *rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::GT:
      return *lhs > *rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::GE:
      return *lhs >= *rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::LT:
      return *lhs < *rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::LE:
      return *lhs <= *rhs ? 1.0 : -1.0;
    }

    error("Unkwnown boolean operator", node.location);
    return std::nullopt;
  }

  std::optional<double> operator()(const AST &, const UnaryExpression &node) {
    auto value = node.get<0>().visit(*this);
    if (!value) {
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

  std::optional<double> operator()(const AST &, const Number &node) {
    return node.value;
  }

  std::optional<double> operator()(const AST &, const NoOp &) { return 0.0; }

  std::optional<double> operator()(const AST &, const Block &node) {
    std::optional<double> value = 0;
    for (const auto &child : node.nodes()) {
      value = child.visit(*this);
      if (!value) {
        return std::nullopt;
      }
    }

    return value;
  }

  std::optional<double> operator()(const AST &, const IfExpression &node) {
    auto value = node.get<0>().visit(*this);
    if (!value) {
      return std::nullopt;
    }

    if (*value > 0) {
      return node.get<1>().visit(*this);
    } else {
      return node.get<2>().visit(*this);
    }
  }

  std::optional<double> operator()(const AST &, const WhileExpression &node) {
    const auto &pred = node.get<0>();
    const auto &body = node.get<1>();
    std::optional<double> value = 0;
    std::optional<double> predValue;
    while ((predValue = pred.visit(*this)), predValue && *predValue > 0) {
      value = body.visit(*this);
      if (!value) {
        return std::nullopt;
      }
    }
    return value;
  }

  std::optional<double> operator()(const AST &, const BuiltInFunction &node) {
    switch (node.id) {
    case BuiltInFunctionId::Exp:
      return callExp(node.nodes(), node.location);
    case BuiltInFunctionId::Log:
      return callLog(node.nodes(), node.location);
    case BuiltInFunctionId::Print:
      return callPrint(node.nodes(), node.location);
    case BuiltInFunctionId::Sqrt:
      return callSqrt(node.nodes(), node.location);
    }

    error("Unknown builtin function", node.location);
    return std::nullopt;
  }

  std::optional<double> operator()(const AST &,
                                   const FunctionDefinition &node) {
    _symbolTable.define(node.name, UserFunction{node.get<0>(), node.arguments});
    return 0;
  }

  std::optional<double> operator()(const AST &, const FunctionCall &node) {
    auto funcOpt = _symbolTable.getFunction(node.name);
    if (!funcOpt) {
      error("Unknown function " + node.name, node.location);
      return std::nullopt;
    }

    if (node.nodes().size() != funcOpt->arguments.size()) {
      error("Wrong number of arguments passed to function " + node.name,
            node.location);
      return std::nullopt;
    }

    std::vector<ValueInfo> values;
    values.reserve(node.nodes().size());

    for (const auto &arg : node.nodes()) {
      auto value = arg.visit(*this);
      if (!value) {
        return std::nullopt;
      }
      values.emplace_back(ValueInfo{*value, false});
    }

    SymbolTable childTable{&_symbolTable};
    for (size_t i = 0; i < funcOpt->arguments.size(); ++i) {
      childTable.define(funcOpt->arguments[i], values[i]);
    }

    EvalWalker eval{childTable, _errors};
    return funcOpt->code.visit(eval);
  }

  std::optional<double> operator()(const AST &,
                                   const VariableDefinition &node) {
    if (_symbolTable.isDefined(node.name)) {
      error("Variable " + node.name +
                " is already defined in the current scope.",
            node.location);
      return std::nullopt;
    }

    auto value = node.get<0>().visit(*this);
    if (!value) {
      return std::nullopt;
    }
    _symbolTable.define(node.name, ValueInfo{*value, node.isConstant});
    return value;
  }

  std::optional<double> operator()(const AST &,
                                   const AssignmentExpression &node) {
    auto value = node.get<0>().visit(*this);
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

  std::optional<double> operator()(const AST &, const NameReference &node) {
    auto value = _symbolTable.getValue(node.name);
    if (value) {
      return *value;
    } else {
      error("Unknown name: " + node.name, node.location);
      return std::nullopt;
    }
  }

  std::optional<double> callExp(const std::vector<AST> arguments,
                                const location &location) {
    if (arguments.size() == 1) {
      auto value = arguments[0].visit(*this);
      if (!value) {
        return std::nullopt;
      }
      return exp(*value);
    }

    error("wrong number of arguments for exp", location);
    return std::nullopt;
  }

  std::optional<double> callLog(const std::vector<AST> arguments,
                                const location &location) {
    if (arguments.size() == 1) {
      auto value = arguments[0].visit(*this);
      if (!value) {
        return std::nullopt;
      }
      return log(*value);
    }

    error("wrong number of arguments for log", location);
    return std::nullopt;
  }

  std::optional<double> callPrint(const std::vector<AST> arguments,
                                  const location &location) {
    std::optional<double> value = 0;
    std::cout << "out:";
    for (const auto &arg : arguments) {
      value = arg.visit(*this);
      if (!value) {
        return std::nullopt;
      }
      std::cout << ' ' << *value;
    }

    std::cout << std::endl;

    return value;
  }

  std::optional<double> callSqrt(const std::vector<AST> arguments,
                                 const location &location) {
    if (arguments.size() == 1) {
      auto value = arguments[0].visit(*this);
      if (!value) {
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
};

std::optional<double> eval(const AST &node, ErrorList &errors) {
  SymbolTable symbolTable{};
  EvalWalker ew{std::move(symbolTable), errors};
  return node.visit(ew);
}

} // namespace cheasle
