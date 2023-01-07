#include "ast.h"
#include <ostream>
#include <sstream>

namespace cheasle {

struct ASTPrinter {
  explicit ASTPrinter(std::ostream &os) : _os{os}, _indent(0) {}

  void operator()(const AST &, const BinaryExpression &node) {
    _os << '(';
    node.lhs.visit(*this);
    _os << ' ' << node.op << ' ';
    node.rhs.visit(*this);
    _os << ')';
  }

  void operator()(const AST &, const EqualityExpression &node) {
    _os << '(';
    node.lhs.visit(*this);
    _os << ' ' << node.op << ' ';
    node.rhs.visit(*this);
    _os << ')';
  }

  void operator()(const AST &, const ComparisonExpression &node) {
    _os << '(';
    node.lhs.visit(*this);
    _os << ' ' << node.op << ' ';
    node.rhs.visit(*this);
    _os << ')';
  }

  void operator()(const AST &, const UnaryExpression &node) {
    _os << '(';
    _os << ' ' << node.op;
    node.child.visit(*this);
    _os << ')';
  }

  void operator()(const AST &, const BinaryLogicalExpression &node) {
    _os << '(';
    node.lhs.visit(*this);
    _os << ' ' << node.op << ' ';
    node.rhs.visit(*this);
    _os << ')';
  }

  void operator()(const AST &, const NotExpression &node) {
    _os << "(not ";
    node.child.visit(*this);
    _os << ')';
  }

  void operator()(const AST &, const ConstantValue &node) { _os << node.value; }

  void operator()(const AST &, const Block &node) {
    _indent += 2;
    for (const auto &child : node.children) {
      indent();
      child.visit(*this);
      _os << std::endl;
    }
    _indent -= 2;
  }

  void operator()(const AST &, const IfExpression &node) {
    _os << "if ";
    node.condition.visit(*this);
    _os << std::endl;
    indent();
    _os << "then" << std::endl;
    node.thenBranch.visit(*this);
    indent();
    _os << "else" << std::endl;
    node.elseBranch.visit(*this);
  }

  void operator()(const AST &, const WhileExpression &node) {
    _os << "while ";
    node.condition.visit(*this);
    indent();
    _os << std::endl;
    node.body.visit(*this);
  }

  void operator()(const AST &, const BuiltInFunction &node) {
    _os << node.id << '(';
    for (const auto &arg : node.arguments) {
      if (&arg != &node.arguments.front()) {
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
      _os << arg.name << ": " << arg.type;
    }
    _os << "): " << node.returnType << std::endl;
    node.code.visit(*this);
  }

  void operator()(const AST &, const FunctionCall &node) {
    _os << node.name << '(';
    for (const auto &arg : node.arguments) {
      if (&arg != &node.arguments.front()) {
        _os << ", ";
      }
      arg.visit(*this);
    }
    _os << ')';
  }

  void operator()(const AST &, const VariableDefinition &node) {
    _os << (node.isConstant ? "const " : "let ") << node.name << ": "
        << node.type << " = ";
    node.expr.visit(*this);
  }

  void operator()(const AST &, const AssignmentExpression &node) {
    _os << node.name << " = ";
    node.expr.visit(*this);
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

std::ostream &operator<<(std::ostream &os, EqualityOperator op) {
  switch (op) {
  case EqualityOperator::EQ:
    os << "==";
    break;
  case EqualityOperator::NE:
    os << "!=";
    break;
  }

  return os;
}

std::ostream &operator<<(std::ostream &os, ComparisonOperator op) {
  switch (op) {
  case ComparisonOperator::GT:
    os << ">";
    break;
  case ComparisonOperator::GE:
    os << ">=";
    break;
  case ComparisonOperator::LT:
    os << "<";
    break;
  case ComparisonOperator::LE:
    os << "<=";
    break;
  }

  return os;
}

std::ostream &operator<<(std::ostream &os, BinaryLogicalOperator op) {
  switch (op) {
  case BinaryLogicalOperator::And:
    os << "and";
    break;
  case BinaryLogicalOperator::Or:
    os << "or";
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
  case BuiltInFunctionId::Printf:
    os << "printf";
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
