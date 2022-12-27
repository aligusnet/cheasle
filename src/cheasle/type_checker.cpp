#include "type_checker.h"
#include "cheasle/ast.h"
#include "cheasle/value.h"
#include <cheasle/symbol.h>
#include <cheasle/symbol_table.h>
#include <sstream>

namespace cheasle {

class TypeChecker {
public:
  TypeChecker(SymbolTable<VariableSymbol> &variables,
              SymbolTable<FunctionSymbol> &functions, ErrorList &errorList)
      : _variables(variables), _functions(functions), _errors(errorList) {}

  ValueType operator()(const AST &, const BinaryExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);

    if (lhs != ValueType::Double || rhs != ValueType::Double) {
      error(
          "Binary expression expects both operands having the same double type",
          node.location);
    }

    return ValueType::Double;
  }

  ValueType operator()(const AST &, const UnaryExpression &node) {
    auto child = node.child.visit(*this);

    if (child != ValueType::Double) {
      error("Unary expression expects an operand of double type",
            node.location);
    }

    return ValueType::Double;
  }

  ValueType operator()(const AST &, const EqualityExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);

    if (lhs != rhs) {
      error("Equality expression expects both operands having the same "
            "double type",
            node.location);
    }

    return ValueType::Boolean;
  }

  ValueType operator()(const AST &, const ComparisonExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);

    if (lhs != ValueType::Double || rhs != ValueType::Double) {
      error("Comparison expression expects both operands having the same "
            "double type",
            node.location);
    }

    return ValueType::Boolean;
  }

  ValueType operator()(const AST &, const NotExpression &node) {
    auto child = node.child.visit(*this);

    if (child != ValueType::Boolean) {
      error("Not expression expects an operand of double type", node.location);
    }

    return ValueType::Boolean;
  }

  ValueType operator()(const AST &, const BinaryLogicalExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);

    if (lhs != ValueType::Boolean || rhs != ValueType::Boolean) {
      error("Binary logical expression expects both operands having the same "
            "boolean type",
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
    if (node.children.empty()) {
      error("Block cannot by empty", node.location);
      return ValueType::Double;
    }

    ValueType type = ValueType::Double;
    for (const auto &child : node.children) {
      type = child.visit(*this);
    }

    return type;
  }

  ValueType operator()(const AST &, const IfExpression &node) {
    auto predType = node.condition.visit(*this);
    if (predType != ValueType::Boolean) {
      error("ValueType of condition expression in <if> is expected to be bool.",
            node.location);
    }

    auto thenType = node.thenBranch.visit(*this);
    auto elseType = node.elseBranch.visit(*this);

    if (thenType != elseType) {
      error(
          "Types os else and then branches must be the same in if expression.",
          node.location);
    }

    return thenType;
  }

  ValueType operator()(const AST &, const WhileExpression &node) {
    auto predType = node.condition.visit(*this);
    if (predType != ValueType::Boolean) {
      error("ValueType of condition expression in <while..do> is expected to "
            "be bool.",
            node.location);
    }

    return node.body.visit(*this);
  }

  ValueType operator()(const AST &, const BuiltInFunction &node) {
    switch (node.id) {
    case BuiltInFunctionId::Exp:
      if (node.arguments.size() != 1 ||
          node.arguments.front().visit(*this) != ValueType::Double) {
        error("Bultin function <exp> expects 1 double argument", node.location);
      }
      return ValueType::Double;
    case BuiltInFunctionId::Log:
      if (node.arguments.size() != 1 ||
          node.arguments.front().visit(*this) != ValueType::Double) {
        error("Bultin function <log> expects 1 double argument", node.location);
      }
      return ValueType::Double;
    case BuiltInFunctionId::Sqrt:
      if (node.arguments.size() != 1 ||
          node.arguments.front().visit(*this) != ValueType::Double) {
        error("Bultin function <sqrt> expects 1 double argument",
              node.location);
      }
      return ValueType::Double;
    case BuiltInFunctionId::Printd:
      if (node.arguments.size() == 0) {
        error("Bultin function <printd> expects at least 1 argument",
              node.location);
        return ValueType::Double;
      }
      for (const auto &arg : node.arguments) {
        if (arg.visit(*this) != ValueType::Double) {
          error("Bultin function <printd> expects arguments of type double",
                node.location);
        }
      }
      return ValueType::Double;
    case BuiltInFunctionId::Printb:
      if (node.arguments.size() == 0) {
        error("Bultin function <printb> expects at least 1 argument",
              node.location);
        return ValueType::Boolean;
      }
      for (const auto &arg : node.arguments) {
        if (arg.visit(*this) != ValueType::Boolean) {
          error("Bultin function <printd> expects arguments of type bool",
                node.location);
        }
      }
      return ValueType::Boolean;
    }

    error("Unknown bultin function", node.location);
    return ValueType::Double;
  }

  ValueType operator()(const AST &, const FunctionCall &node) {
    auto func = _functions.get(node.name);
    if (!func) {
      error("Function <" + node.name + "> is undefined", node.location);
      return ValueType::Double;
    }

    if (node.arguments.size() != func->arguments.size()) {
      std::ostringstream oss;
      oss << "Function <" << node.name << "> expects " << func->arguments.size()
          << " arguments, but " << node.arguments.size() << " were passed in";
      error(oss.str(), node.location);
    }

    const size_t nargs =
        std::min(node.arguments.size(), func->arguments.size());
    for (size_t i = 0; i < nargs; ++i) {
      auto type = node.arguments[i].visit(*this);
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
    _functions.define(
        node.name, FunctionSymbol{node.returnType, node.arguments, node.code});

    SymbolTable<VariableSymbol> variables{node.name, &_variables};
    SymbolTable<FunctionSymbol> functions{node.name, &_functions};
    for (const auto &arg : node.arguments) {
      variables.define(arg.name, VariableSymbol{arg.type, Value{}, false});
    }

    TypeChecker funcTypeChecker{variables, functions, _errors};
    auto returnType = node.code.visit(funcTypeChecker);
    if (node.returnType != returnType) {
      std::ostringstream oss;
      oss << "Function <" << node.name << "> expects " << node.returnType
          << " return type, but evaluates to " << returnType;
      error(oss.str(), node.location);
    }

    return node.returnType;
  }

  ValueType operator()(const AST &, const VariableDefinition &node) {
    auto type = node.expr.visit(*this);
    if (type != node.type) {
      std::ostringstream oss;
      oss << "Variable <" << node.name << "> declared as " << node.type
          << " but is assigned to " << type << " value";
      error(oss.str(), node.location);
    }

    _variables.define(node.name,
                      VariableSymbol{node.type, Value{}, node.isConstant});
    return node.type;
  }

  ValueType operator()(const AST &, const AssignmentExpression &node) {
    auto type = node.expr.visit(*this);
    auto varInfo = _variables.get(node.name);
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
    auto varInfo = _variables.get(node.name);
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

  SymbolTable<VariableSymbol> &_variables;
  SymbolTable<FunctionSymbol> &_functions;
  ErrorList &_errors;
};

ValueType checkTypes(const AST &ast, ErrorList &errors) {
  SymbolTable<VariableSymbol> variables{"global"};
  SymbolTable<FunctionSymbol> functions{"global"};
  TypeChecker typeChecker{variables, functions, errors};
  return ast.visit(typeChecker);
}
} // namespace cheasle
