#include "ast.h"
#include <ostream>
#include <sstream>

namespace cheasle {

struct ASTPrinter {
  explicit ASTPrinter(std::ostream &os) : _os{os}, _indent(0) {}

  void operator()(const AST &, const BinaryExpression &node) {
    _os << '(';
    node.get<0>().visit(*this);
    _os << ' ' << node.op << ' ';
    node.get<1>().visit(*this);
    _os << ')';
  }

  void operator()(const AST &, const BinaryLogicalExpression &node) {
    _os << '(';
    node.get<0>().visit(*this);
    _os << ' ' << node.op << ' ';
    node.get<1>().visit(*this);
    _os << ')';
  }

  void operator()(const AST &, const UnaryExpression &node) {
    _os << '(';
    _os << ' ' << node.op;
    node.get<0>().visit(*this);
    _os << ')';
  }

  void operator()(const AST &, const Number &node) { _os << node.value; }

  void operator()(const AST &, const Block &node) {
    _indent += 2;
    for (const auto &child : node.nodes()) {
      indent();
      child.visit(*this);
      _os << std::endl;
    }
    _indent -= 2;
  }

  void operator()(const AST &, const NoOp &node) {}

  void operator()(const AST &, const IfExpression &node) {
    _os << "if ";
    node.get<0>().visit(*this);
    _os << std::endl;
    indent();
    _os << "then" << std::endl;
    node.get<1>().visit(*this);
    indent();
    _os << "else" << std::endl;
    node.get<2>().visit(*this);
  }

  void operator()(const AST &, const WhileExpression &node) {
    _os << "while ";
    node.get<0>().visit(*this);
    indent();
    _os << "do" << std::endl;
    node.get<1>().visit(*this);
  }

  void operator()(const AST &, const BuiltInFunction &node) {
    _os << node.id << '(';
    for (const auto &arg : node.nodes()) {
      if (&arg != &node.nodes().front()) {
        _os << ", ";
      }
      arg.visit(*this);
    }
    _os << ')';
  }

  void operator()(const AST &, const FunctionDefinition &node) {
    _os << node.name << '(';
    for (const auto &arg : node.arguments) {
      if (&arg != &node.arguments.front()) {
        _os << ", ";
      }
      _os << arg;
    }
    _os << ')' << std::endl;
    node.get<0>().visit(*this);
  }

  void operator()(const AST &, const FunctionCall &node) {
    _os << node.name << '(';
    for (const auto &arg : node.nodes()) {
      if (&arg != &node.nodes().front()) {
        _os << ", ";
      }
      arg.visit(*this);
    }
    _os << ')';
  }

  void operator()(const AST &, const VariableDefinition &node) {
    _os << (node.isConstant ? "const " : "let ") << node.name << " = ";
    node.get<0>().visit(*this);
  }

  void operator()(const AST &, const AssignmentExpression &node) {
    _os << node.name << " = ";
    node.get<0>().visit(*this);
  }

  void operator()(const AST &, const NameReference &node) { _os << node.name; }

private:
  void indent() { _os << std::string(_indent, ' '); }

  std::ostream &_os;
  unsigned _indent;
};

std::ostream &operator<<(std::ostream &os, BinaryOperator op) {
  switch (op) {
  case BinaryOperator::Add:
    os << '+';
    break;
  case BinaryOperator::Subtract:
    os << '-';
    break;
  case BinaryOperator::Multiply:
    os << '*';
    break;
  case BinaryOperator::Divide:
    os << '/';
    break;
  }
  return os;
}

std::ostream &operator<<(std::ostream &os, UnaryOperator op) {
  switch (op) {
  case UnaryOperator::Minus:
    os << '-';
    break;
  case UnaryOperator::Abs:
    os << '|';
    break;
  }

  return os;
}

std::ostream &operator<<(std::ostream &os, BinaryLogicalOperator op) {
  switch (op) {
  case BinaryLogicalOperator::EQ:
    os << "==";
    break;
  case BinaryLogicalOperator::NE:
    os << "!=";
    break;
  case BinaryLogicalOperator::GT:
    os << ">";
    break;
  case BinaryLogicalOperator::GE:
    os << ">=";
    break;
  case BinaryLogicalOperator::LT:
    os << "<";
    break;
  case BinaryLogicalOperator::LE:
    os << "<=";
    break;
  }

  return os;
}

std::ostream &operator<<(std::ostream &os, BuiltInFunctionId id) {
  switch (id) {
  case BuiltInFunctionId::Exp:
    os << "exp";
    break;
  case BuiltInFunctionId::Log:
    os << "log";
    break;
  case BuiltInFunctionId::Print:
    os << "print";
    break;
  case BuiltInFunctionId::Sqrt:
    os << "sqrt";
    break;
  }
  return os;
}

std::ostream &operator<<(std::ostream &os, const AST &ast) {
  ASTPrinter printer(os);
  ast.visit(printer);
  return os;
}
} // namespace cheasle
