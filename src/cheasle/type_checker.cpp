#include "type_checker.h"
#include <cheasle/symbol_table.h>
#include <sstream>

namespace cheasle {

class TypeChecker {
public:
  TypeChecker(SymbolTable &symbolTable, ErrorList &errorList)
      : _symbolTable(symbolTable), _errors(errorList) {}

  ValueType operator()(const AST &, const BinaryExpression &node) {
    auto lhs = node.get<0>().visit(*this);
    auto rhs = node.get<1>().visit(*this);

    if (lhs != ValueType::Double || rhs != ValueType::Double) {
      error(
          "Binary expression expects both operands having the same double type",
          node.location);
    }

    return ValueType::Double;
  }

  ValueType operator()(const AST &, const UnaryExpression &node) {
    auto child = node.get<0>().visit(*this);

    if (child != ValueType::Double) {
      error("Unary expression expects an operand of double type",
            node.location);
    }

    return ValueType::Double;
  }

  ValueType operator()(const AST &, const BinaryLogicalExpression &node) {
    auto lhs = node.get<0>().visit(*this);
    auto rhs = node.get<1>().visit(*this);

    if (lhs != ValueType::Double || rhs != ValueType::Double) {
      error("Binary logical expression expects both operands having the same "
            "double type",
            node.location);
    }

    return ValueType::Boolean;
  }

  ValueType operator()(const AST &, const ConstantValue &node) {
    if (std::get_if<double>(&node.value) != nullptr) {
      return ValueType::Double;
    } else if (std::get_if<bool>(&node.value) != nullptr) {
      return ValueType::Boolean;
    }

    error("Unable to obtain constant value type", node.location);
    return ValueType::Double;
  }

  ValueType operator()(const AST &, const Block &node) {
    if (node.nodes().empty()) {
      error("Block cannot by empty", node.location);
      return ValueType::Double;
    }

    ValueType type = ValueType::Double;
    for (const auto &child : node.nodes()) {
      type = child.visit(*this);
    }

    return type;
  }

  ValueType operator()(const AST &, const IfExpression &node) {
    auto predType = node.get<0>().visit(*this);
    if (predType != ValueType::Boolean) {
      error("ValueType of condition expression in <if> is expected to be bool.",
            node.location);
    }

    auto thenType = node.get<1>().visit(*this);
    auto elseType = node.get<2>().visit(*this);

    if (thenType != elseType) {
      error(
          "Types os else and then branches must be the same in if expression.",
          node.location);
    }

    return thenType;
  }

  ValueType operator()(const AST &, const WhileExpression &node) {
    auto predType = node.get<0>().visit(*this);
    if (predType != ValueType::Boolean) {
      error("ValueType of condition expression in <while..do> is expected to "
            "be bool.",
            node.location);
    }

    return node.get<1>().visit(*this);
  }

  ValueType operator()(const AST &, const BuiltInFunction &node) {
    switch (node.id) {
    case BuiltInFunctionId::Exp:
      if (node.nodes().size() != 1 ||
          node.nodes().front().visit(*this) != ValueType::Double) {
        error("Bultin function <exp> expects 1 double argument", node.location);
      }
      return ValueType::Double;
    case BuiltInFunctionId::Log:
      if (node.nodes().size() != 1 ||
          node.nodes().front().visit(*this) != ValueType::Double) {
        error("Bultin function <log> expects 1 double argument", node.location);
      }
      return ValueType::Double;
    case BuiltInFunctionId::Sqrt:
      if (node.nodes().size() != 1 ||
          node.nodes().front().visit(*this) != ValueType::Double) {
        error("Bultin function <sqrt> expects 1 double argument",
              node.location);
      }
      return ValueType::Double;
    case BuiltInFunctionId::Print:
      if (node.nodes().size() == 0) {
        error("Bultin function <print> expects at least 1 argument",
              node.location);
        return ValueType::Double;
      }
      return node.nodes().back().visit(*this);
    }

    error("Unknown bultin function", node.location);
    return ValueType::Double;
  }

  ValueType operator()(const AST &, const FunctionCall &node) {
    auto func = _symbolTable.getFunction(node.name);
    if (!func) {
      error("Function <" + node.name + "> is undefined", node.location);
      return ValueType::Double;
    }

    if (node.nodes().size() != func->arguments.size()) {
      std::ostringstream oss;
      oss << "Function <" << node.name << "> expects " << func->arguments.size()
          << " arguments, but " << node.nodes().size() << " were passed in";
      error(oss.str(), node.location);
    }

    const size_t nargs = std::min(node.nodes().size(), func->arguments.size());
    for (size_t i = 0; i < nargs; ++i) {
      auto type = node.nodes()[i].visit(*this);
      const auto &arg = func->arguments[i];
      if (type != arg.type) {
        std::ostringstream oss;
        oss << "Function <" << node.name << "> expects " << arg.type
            << " value for <" << arg.name << "> argument";
        error(oss.str(), node.location);
      }
    }

    return func->returnType;
  }

  ValueType operator()(const AST &, const FunctionDefinition &node) {
    _symbolTable.define(
        node.name,
        FunctionSymbol{node.returnType, node.arguments, node.get<0>()});

    SymbolTable funcSymbolTable{&_symbolTable};
    for (const auto &arg : node.arguments) {
      funcSymbolTable.define(arg.name, ValueSymbol{arg.type, Value{}, false});
    }

    TypeChecker funcTypeChecker{funcSymbolTable, _errors};
    auto returnType = node.get<0>().visit(funcTypeChecker);
    if (node.returnType != returnType) {
      std::ostringstream oss;
      oss << "Function <" << node.name << "> expects " << node.returnType
          << " return type, but evaluates to " << returnType;
      error(oss.str(), node.location);
    }

    return node.returnType;
  }

  ValueType operator()(const AST &, const VariableDefinition &node) {
    auto type = node.get<0>().visit(*this);
    if (type != node.type) {
      std::ostringstream oss;
      oss << "Variable <" << node.name << "> declared as " << node.type
          << " but is assigned to " << type << " value";
      error(oss.str(), node.location);
    }

    _symbolTable.define(node.name,
                        ValueSymbol{node.type, Value{}, node.isConstant});
    return node.type;
  }

  ValueType operator()(const AST &, const AssignmentExpression &node) {
    auto type = node.get<0>().visit(*this);
    auto varInfo = _symbolTable.getVariable(node.name);
    if (!varInfo) {
      error("Unknown variable " + node.name, node.location);
      return type;
    }

    if (varInfo->isConstant) {
      error("Assignment to constant variable " + node.name, node.location);
    }

    if (type != varInfo->type) {
      std::ostringstream oss;
      oss << "Variable <" << node.name << "> declared as " << varInfo->type
          << " but is assigned to " << type << " value";
      error(oss.str(), node.location);
    }

    return varInfo->type;
  }

  ValueType operator()(const AST &, const NameReference &node) {
    auto varInfo = _symbolTable.getVariable(node.name);
    if (!varInfo) {
      error("Unknown variable " + node.name, node.location);
      return ValueType::Double;
    }

    return varInfo->type;
  }

private:
  void error(std::string message, location location) {
    _errors.append("type-checkerÍ", std::move(message), std::move(location));
  }

  SymbolTable &_symbolTable;
  ErrorList &_errors;
};

ValueType checkTypes(const AST &ast, ErrorList &errors) {
  SymbolTable symbolTable{};
  TypeChecker typeChecker{symbolTable, errors};
  return ast.visit(typeChecker);
}
} // namespace cheasle