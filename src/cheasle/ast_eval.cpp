#include "ast_eval.h"
#include "cheasle/ast.h"
#include "cheasle/symbol_table.h"
#include <iostream>

namespace cheasle {

struct EvalWalker {

  EvalWalker(SymbolTable symbolTable) : _symbolTable(std::move(symbolTable)) {}

  double operator()(const AST &, const BinaryExpression &node) {
    double lhs = node.get<0>().visit(*this);
    double rhs = node.get<1>().visit(*this);
    switch (node.op) {
    case BinaryOperator::Add:
      return lhs + rhs;
    case BinaryOperator::Subtract:
      return lhs - rhs;
    case BinaryOperator::Divide:
      return lhs / rhs;
    case BinaryOperator::Multiply:
      return lhs * rhs;
    }

    return 0;
  }

  double operator()(const AST &ast, const BinaryLogicalExpression &node) {
    double lhs = node.get<0>().visit(*this);
    double rhs = node.get<1>().visit(*this);

    switch (node.op) {
    case BinaryLogicalOperator::EQ:
      return lhs == rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::NE:
      return lhs != rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::GT:
      return lhs > rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::GE:
      return lhs >= rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::LT:
      return lhs < rhs ? 1.0 : -1.0;
    case BinaryLogicalOperator::LE:
      return lhs <= rhs ? 1.0 : -1.0;
    }

    return 0;
  }

  double operator()(const AST &, const UnaryExpression &node) {
    double value = node.get<0>().visit(*this);
    switch (node.op) {
    case UnaryOperator::Abs:
      value = fabs(value);
      break;
    case UnaryOperator::Minus:
      value = -value;
      break;
    }
    return value;
  }

  double operator()(const AST &, const Number &node) { return node.value; }

  double operator()(const AST &, const NoOp &) { return 0.0; }

  double operator()(const AST &, const Block &node) {
    double value = 0;
    for (const auto &child : node.nodes()) {
      value = child.visit(*this);
    }

    return value;
  }

  double operator()(const AST &, const IfExpression &node) {
    double value = node.get<0>().visit(*this);
    if (value > 0) {
      return node.get<1>().visit(*this);
    } else {
      return node.get<2>().visit(*this);
    }
  }

  double operator()(const AST &, const WhileExpression &node) {
    const auto &pred = node.get<0>();
    const auto &body = node.get<1>();
    double value = 0;
    while (pred.visit(*this) > 0) {
      value = body.visit(*this);
    }
    return value;
  }

  double operator()(const AST &, const BuiltInFunction &node) {
    double value = 0;
    switch (node.id) {
    case BuiltInFunctionId::Exp:
      value = callExp(node.nodes());
      break;
    case BuiltInFunctionId::Log:
      value = callLog(node.nodes());
      break;
    case BuiltInFunctionId::Print:
      value = callPrint(node.nodes());
      break;
    case BuiltInFunctionId::Sqrt:
      value = callSqrt(node.nodes());
      break;
    }
    return value;
  }

  double operator()(const AST &, const FunctionDefinition &node) {
    _symbolTable.define(node.name, UserFunction{node.get<0>(), node.arguments});
    return 0;
  }

  double operator()(const AST &, const FunctionCall &node) {
    auto funcOpt = _symbolTable.getFunction(node.name);
    if (!funcOpt) {
      std::cerr << "Unknown function " << node.name << std::endl;
      return 0;
    }

    if (node.nodes().size() != funcOpt->arguments.size()) {
      std::cerr << "Wrong number of arguments passed to function " << node.name
                << std::endl;
      return 0;
    }

    std::vector<ValueInfo> values;
    values.reserve(node.nodes().size());

    for (const auto &arg : node.nodes()) {
      values.emplace_back(ValueInfo{arg.visit(*this), false});
    }

    SymbolTable childTable{&_symbolTable};
    for (size_t i = 0; i < funcOpt->arguments.size(); ++i) {
      childTable.define(funcOpt->arguments[i], values[i]);
    }

    EvalWalker eval{childTable};
    return funcOpt->code.visit(eval);
  }

  double operator()(const AST &, const VariableDefinition &node) {
    if (_symbolTable.isDefined(node.name)) {
      std::cerr << "Variable " << node.name
                << " is already defined in the current scope." << std::endl;
    }

    double value = node.get<0>().visit(*this);
    _symbolTable.define(node.name, ValueInfo{value, node.isConstant});
    return value;
  }

  double operator()(const AST &, const AssignmentExpression &node) {
    double value = node.get<0>().visit(*this);
    if (!_symbolTable.assign(node.name, value)) {
      std::cerr << "Failed to assign variable " << node.name << " to " << value
                << std::endl;
    }

    return value;
  }

  double operator()(const AST &, const NameReference &node) {
    auto value = _symbolTable.getValue(node.name);
    if (value) {
      return *value;
    } else {
      std::cerr << "Unknown name: " << node.name << std::endl;
      return 0;
    }
  }

  double callExp(const std::vector<AST> arguments) {
    if (arguments.size() == 1) {
      return exp(arguments[0].visit(*this));
    }

    std::cerr << "wrong number of arguments for exp" << std::endl;

    return 0;
  }

  double callLog(const std::vector<AST> arguments) {
    if (arguments.size() == 1) {
      return log(arguments[0].visit(*this));
    }

    std::cerr << "wrong number of arguments for log" << std::endl;

    return 0;
  }

  double callPrint(const std::vector<AST> arguments) {
    double value = 0;
    std::cout << "out:";
    for (const auto &arg : arguments) {
      value = arg.visit(*this);
      std::cerr << ' ' << value;
    }

    std::cout << std::endl;

    return value;
  }

  double callSqrt(const std::vector<AST> arguments) {
    if (arguments.size() == 1) {
      return sqrt(arguments[0].visit(*this));
    }

    std::cerr << "wrong number of arguments for sqrt" << std::endl;

    return 0;
  }

  SymbolTable _symbolTable;
};

double eval(const AST &node) {
  SymbolTable symbolTable{};
  EvalWalker ew{std::move(symbolTable)};
  return node.visit(ew);
}

} // namespace cheasle
