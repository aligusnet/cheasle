#include "type_checker.h"
#include "cheasle/ast.h"
#include "cheasle/printf_utils.h"
#include "cheasle/value.h"
#include "location.h"
#include <cheasle/symbol.h>
#include <cheasle/symbol_table.h>
#include <optional>
#include <sstream>

namespace cheasle {

class TypeChecker {
public:
  TypeChecker(SymbolTable<VariableSymbol> &variables,
              SymbolTable<FunctionSymbol> &functions, ErrorList &errorList)
      : _variables(variables), _functions(functions), _errors(errorList) {}

  ValueType operator()(const AST &, BinaryExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    node.type = lhs; // default value, can be change because of conversions.

    const ValueTypeMask typeMask = ValueType::Double | ValueType::Int;
    if (!typeMask.check(lhs) || !typeMask.check(rhs)) {
      error("Binary expression expects both operands must be of double or int "
            "type",
            node.location);
      return node.type;
    }

    if (lhs != rhs) {
      auto commonType = insertConversion(node);
      if (commonType) {
        node.type = *commonType;
      } else {
        conversionError(lhs, rhs, "binary expression", node.location);
      }
    }

    return node.type;
  }

  ValueType operator()(const AST &, UnaryExpression &node) {
    auto child = node.child.visit(*this);

    const ValueTypeMask typeMask = ValueType::Double | ValueType::Int;
    if (!typeMask.check(child)) {
      error("Unary expression expects an operand of double, int type",
            node.location);
    }

    node.type = child;
    return node.type;
  }

  ValueType operator()(const AST &, EqualityExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);

    node.type = ValueType::Boolean;

    const ValueTypeMask typeMask =
        ValueType::Boolean | ValueType::Int | ValueType::Double;
    if (!typeMask.check(lhs) || !typeMask.check(rhs)) {
      error("Equality expression expects both operands having bool, "
            "int, double type",
            node.location);
      return node.type;
    }

    if (lhs != rhs) {
      auto commonType = insertConversion(node);
      if (!commonType) {
        conversionError(lhs, rhs, "equality expression", node.location);
      }
    }

    return node.type;
  }

  ValueType operator()(const AST &, ComparisonExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);

    node.type = ValueType::Boolean;

    const ValueTypeMask typeMask = ValueType::Int | ValueType::Double;
    if (!typeMask.check(lhs) || !typeMask.check(rhs)) {
      error("Comparison expression expects both operands having the same "
            "int, double type",
            node.location);
      return node.type;
    }

    if (lhs != rhs) {
      auto commonType = insertConversion(node);
      if (!commonType) {
        conversionError(lhs, rhs, "comparison expression", node.location);
      }
    }

    return node.type;
  }

  ValueType operator()(const AST &, NotExpression &node) {
    auto child = node.child.visit(*this);

    if (child != ValueType::Boolean) {
      error("Not expression expects an operand of boolean type", node.location);
    }

    node.type = ValueType::Boolean;
    return node.type;
  }

  ValueType operator()(const AST &, BinaryLogicalExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);

    if (lhs != ValueType::Boolean || rhs != ValueType::Boolean) {
      error("Binary logical expression expects both operands having the same "
            "boolean type",
            node.location);
    }

    node.type = ValueType::Boolean;
    return node.type;
  }

  ValueType operator()(const AST &, ConstantValue &node) {
    if (std::get_if<double>(&node.value) != nullptr) {
      node.type = ValueType::Double;
      return node.type;
    } else if (std::get_if<bool>(&node.value) != nullptr) {
      node.type = ValueType::Boolean;
      return node.type;
    } else if (std::get_if<std::string>(&node.value) != nullptr) {
      node.type = ValueType::String;
      return node.type;
    } else if (std::get_if<int32_t>(&node.value) != nullptr) {
      node.type = ValueType::Int;
      return node.type;
    }

    error("Unable to obtain constant value type", node.location);
    return ValueType::Any;
  }

  ValueType operator()(const AST &, Block &node) {
    if (node.children.empty()) {
      error("Block cannot by empty", node.location);
      return ValueType::Double;
    }

    ValueType type = ValueType::Double;
    for (auto &child : node.children) {
      type = child.visit(*this);
    }

    node.type = type;
    return node.type;
  }

  ValueType operator()(const AST &, IfExpression &node) {
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

    node.type = thenType;
    return node.type;
  }

  ValueType operator()(const AST &, WhileExpression &node) {
    auto predType = node.condition.visit(*this);
    if (predType != ValueType::Boolean) {
      error("ValueType of condition expression in <while..do> is expected to "
            "be bool.",
            node.location);
    }

    node.type = node.body.visit(*this);
    return node.type;
  }

  ValueType operator()(const AST &, BuiltInFunction &node) {
    switch (node.id) {
    case BuiltInFunctionId::Exp:
      if (node.arguments.size() != 1 ||
          node.arguments.front().visit(*this) != ValueType::Double) {
        error("Bultin function <exp> expects 1 double argument", node.location);
      }
      node.type = ValueType::Double;
      return node.type;
    case BuiltInFunctionId::Log:
      if (node.arguments.size() != 1 ||
          node.arguments.front().visit(*this) != ValueType::Double) {
        error("Bultin function <log> expects 1 double argument", node.location);
      }
      node.type = ValueType::Double;
      return node.type;
    case BuiltInFunctionId::Sqrt:
      if (node.arguments.size() != 1 ||
          node.arguments.front().visit(*this) != ValueType::Double) {
        error("Bultin function <sqrt> expects 1 double argument",
              node.location);
      }
      node.type = ValueType::Double;
      return node.type;
    case BuiltInFunctionId::Printf:
      if (node.arguments.size() == 0) {
        error("Bultin function <printf> expects at least 1 argument",
              node.location);
      } else {
        auto formatType = node.arguments.front().visit(*this);
        if (formatType != ValueType::String) {
          error("Bultin function <printf> expects first argument to be string",
                node.location);
        }

        std::vector<ValueType> argumentTypes;
        // First argument is format string.
        argumentTypes.reserve(node.arguments.size() - 1);
        for (size_t i = 1; i < node.arguments.size(); ++i) {
          argumentTypes.push_back(node.arguments[i].visit(*this));
        }

        // if the first argument is a constant string, we can check other
        // argument types
        if (node.arguments[0].is<ConstantValue>()) {
          auto formatStringNode = node.arguments[0].cast<ConstantValue>();
          if (formatStringNode->type == ValueType::String) {
            auto errorMessage = printfCheckTypes(
                std::get<std::string>(formatStringNode->value), argumentTypes);
            if (!errorMessage.empty()) {
              error("printf: " + errorMessage, node.location);
            }
          }
        }
      }
      node.type = ValueType::Int;
      return node.type;
    }

    error("Unknown bultin function", node.location);
    return ValueType::Any;
  }

  ValueType operator()(const AST &, FunctionCall &node) {
    auto func = _functions.get(node.name);
    if (!func) {
      error("Function <" + node.name + "> is undefined", node.location);
      return ValueType::Any;
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

    node.type = func->returnType;
    return node.type;
  }

  ValueType operator()(const AST &, FunctionDefinition &node) {
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

    node.type = ValueType::Function;
    return node.type;
  }

  ValueType operator()(const AST &, VariableDefinition &node) {
    auto type = node.expr.visit(*this);
    auto commonType = assignmentConversion(node, node.type);
    if (!commonType) {
      std::ostringstream oss;
      oss << "Variable <" << node.name << "> declared as " << node.type
          << " but is assigned to " << type << " value";
      error(oss.str(), node.location);
    }

    _variables.define(node.name,
                      VariableSymbol{node.type, Value{}, node.isConstant});
    return node.type;
  }

  ValueType operator()(const AST &, AssignmentExpression &node) {
    auto type = node.expr.visit(*this);
    auto varInfo = _variables.get(node.name);
    if (!varInfo) {
      error("Unknown variable " + node.name, node.location);
      return ValueType::Any;
    }
    node.type = varInfo->type;

    if (varInfo->isConstant) {
      error("Assignment to constant variable " + node.name, node.location);
    }

    auto commonType = assignmentConversion(node, varInfo->type);
    if (!commonType) {
      std::ostringstream oss;
      oss << "Variable <" << node.name << "> declared as " << varInfo->type
          << " but is assigned to " << type << " value";
      error(oss.str(), node.location);
    } else {
      node.type = *commonType;
    }

    return node.type;
  }

  ValueType operator()(const AST &, NameReference &node) {
    auto varInfo = _variables.get(node.name);
    if (!varInfo) {
      error("Unknown variable " + node.name, node.location);
      return ValueType::Any;
    }

    node.type = varInfo->type;
    return node.type;
  }

  ValueType operator()(const AST &, TypeConversion &node) {
    auto child = node.child.visit(*this);
    switch (node.id) {
    case TypeConversionId::ftoi:
      node.type = ValueType::Int;
      if (child != ValueType::Double) {
        error("ftoi conversion expects a hild expression of double type",
              node.location);
      }
      break;
    case TypeConversionId::itof:
      node.type = ValueType::Double;
      if (child != ValueType::Int) {
        error("itof conversion expects a hild expression of int type",
              node.location);
      }
      break;
    }
    return node.type;
  }

private:
  // returns Common type if conversion found
  template <typename BinaryNode>
  std::optional<ValueType> insertConversion(BinaryNode &node) {
    ValueType lhs = getType(node.lhs);
    ValueType rhs = getType(node.rhs);
    const auto &[type, lhsConv, rhsConv] = findNumberConversion(lhs, rhs);
    if (!type) {
      return type;
    }

    if (lhsConv) {
      auto location = getLocation(node.lhs);
      node.lhs = AST::make<TypeConversion>(std::move(node.lhs), *lhsConv, *type,
                                           std::move(location));
    }

    if (rhsConv) {
      auto location = getLocation(node.rhs);
      node.rhs = AST::make<TypeConversion>(std::move(node.rhs), *rhsConv, *type,
                                           std::move(location));
    }

    return type;
  }

  template <typename Node>
  std::optional<ValueType> assignmentConversion(Node &node,
                                                ValueType expectedType) {
    auto actualType = node.expr.visit(*this);
    if (actualType == expectedType) {
      return expectedType;
    }

    // the only allowed automatic conversion: double = int
    if (actualType == ValueType::Int && expectedType == ValueType::Double) {
      auto location = getLocation(node.expr);
      node.expr = AST::make<TypeConversion>(std::move(node.expr),
                                            TypeConversionId::itof,
                                            expectedType, std::move(location));
      return expectedType;
    }

    return std::nullopt;
  }

  std::tuple<std::optional<ValueType>, std::optional<TypeConversionId>,
             std::optional<TypeConversionId>>
  findNumberConversion(ValueType lhsType, ValueType rhsType) {
    if (lhsType == ValueType::Int && rhsType == ValueType::Double) {
      return {ValueType::Double, TypeConversionId::itof, std::nullopt};
    }

    if (lhsType == ValueType::Double && rhsType == ValueType::Int) {
      return {ValueType::Double, std::nullopt, TypeConversionId::itof};
    }

    return {std::nullopt, std::nullopt, std::nullopt};
  }

  void conversionError(ValueType lhs, ValueType rhs, std::string nodeType,
                       location location) {
    std::ostringstream oss;
    oss << "No suitable conversion between " << lhs << " and " << rhs
        << " found for " << nodeType;
    error(oss.str(), std::move(location));
  }

  void error(std::string message, location location) {
    _errors.append("type-checker", std::move(message), std::move(location));
  }

  SymbolTable<VariableSymbol> &_variables;
  SymbolTable<FunctionSymbol> &_functions;
  ErrorList &_errors;
};

ValueType checkTypes(AST &ast, ErrorList &errors) {
  SymbolTable<VariableSymbol> variables{"global"};
  SymbolTable<FunctionSymbol> functions{"global"};
  TypeChecker typeChecker{variables, functions, errors};
  return ast.visit(typeChecker);
}
} // namespace cheasle
