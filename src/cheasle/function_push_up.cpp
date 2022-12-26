#include "function_push_up.h"
#include "cheasle/symbol_table.h"
#include "cheasle/type_checker.h"
#include <memory>
#include <optional>

namespace cheasle {
namespace {

template <typename T> struct UniquePtrSwapper {
  UniquePtrSwapper(std::unique_ptr<T> &lhs, std::unique_ptr<T> &rhs)
      : _lhs(lhs), _rhs(rhs) {
    _lhs.swap(_rhs);
  }

  ~UniquePtrSwapper() { _lhs.swap(_rhs); }

private:
  std::unique_ptr<T> &_lhs;
  std::unique_ptr<T> &_rhs;
};

class FunctionPushUpVisitor {
public:
  FunctionPushUpVisitor(AST ast, ErrorList &errors)
      : _errors(errors), _ast(std::move(ast)), _symbolTable("") {}

  std::optional<AST> build(std::string mainName, ValueType mainType) {
    auto ast = _ast.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!ast) {
      error("main expression is exptected to be non-empty", location());
      return std::nullopt;
    }

    _functions.emplace_back(AST::make<FunctionDefinition>(
        std::move(mainName), mainType, std::move(*ast),
        std::vector<FunctionArgument>{}, location()));
    return AST::make<Block>(std::move(_functions), location());
  }

  std::optional<AST> operator()(const AST &, BinaryExpression node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!lhs) {
      error("left side of BinaryExpression is expected to have at least one "
            "expression",
            node.location);
    }
    if (!rhs) {
      error("right side of BinaryExpression is expected to have at least one "
            "expression",
            node.location);
    }
    if (hasErrors()) {
      return std::nullopt;
    }

    node.lhs = std::move(*lhs);
    node.rhs = std::move(*rhs);
    return AST::make<BinaryExpression>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, UnaryExpression node) {
    auto expr = node.child.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!expr) {
      error("Unary expressions is expected to have non-empty child expressions",
            node.location);
    }
    if (hasErrors()) {
      return std::nullopt;
    }

    node.child = std::move(*expr);
    return AST::make<UnaryExpression>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, EqualityExpression node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!lhs) {
      error("left side of EqualityExpression is expected to have at least one "
            "expression",
            node.location);
    }
    if (!rhs) {
      error("right side of EqualityExpression is expected to have at least one "
            "expression",
            node.location);
    }
    if (hasErrors()) {
      return std::nullopt;
    }

    node.lhs = std::move(*lhs);
    node.rhs = std::move(*rhs);
    return AST::make<EqualityExpression>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, ComparisonExpression node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!lhs) {
      error("left side of ComparisonExpression is expected to have at least "
            "one expression",
            node.location);
    }
    if (!rhs) {
      error("right side of ComparisonExpression is expected to have at least "
            "one expression",
            node.location);
    }
    if (hasErrors()) {
      return std::nullopt;
    }

    node.lhs = std::move(*lhs);
    node.rhs = std::move(*rhs);
    return AST::make<ComparisonExpression>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, BinaryLogicalExpression node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!lhs) {
      error("left side of BinaryLogicalExpression is expected to have at least "
            "one expression",
            node.location);
    }
    if (!rhs) {
      error("right side of BinaryLogicalExpression is expected to have at "
            "least one expression",
            node.location);
    }
    if (hasErrors()) {
      return std::nullopt;
    }

    node.lhs = std::move(*lhs);
    node.rhs = std::move(*rhs);
    return AST::make<BinaryLogicalExpression>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, NotExpression node) {
    auto expr = node.child.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!expr) {
      error("Not expressions is expected to have non-empty child expressions",
            node.location);
    }
    if (hasErrors()) {
      return std::nullopt;
    }

    node.child = std::move(*expr);
    return AST::make<NotExpression>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, ConstantValue node) {
    return AST::make<ConstantValue>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, Block node) {
    std::vector<AST> statements{};
    statements.reserve(node.children.size());
    for (auto &&statement : node.children) {
      auto expr = statement.visit(*this);
      if (expr) {
        statements.emplace_back(std::move(*expr));
      }
    }

    if (hasErrors()) {
      return std::nullopt;
    }

    node.children = std::move(statements);
    return AST::make<Block>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, IfExpression node) {
    auto condition = node.condition.visit(*this);
    auto thenBranch = node.thenBranch.visit(*this);
    auto elseBranch = node.elseBranch.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!condition) {
      error("Condition of if expression is expected to be non-empty",
            node.location);
    }
    if (!thenBranch) {
      error("Then branch of if expression is expected to be non-empty",
            node.location);
    }
    if (!elseBranch) {
      error("Else branch of if expression is expected to be non-empty",
            node.location);
    }
    if (hasErrors()) {
      return std::nullopt;
    }

    node.condition = std::move(*condition);
    node.thenBranch = std::move(*thenBranch);
    node.elseBranch = std::move(*elseBranch);
    return AST::make<IfExpression>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, WhileExpression node) {
    auto condition = node.condition.visit(*this);
    auto body = node.body.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!condition) {
      error("Condition of while expression is expected to be non-empty",
            node.location);
    }
    if (!body) {
      error("Body of while expression is expected to be non-empty",
            node.location);
    }
    if (hasErrors()) {
      return std::nullopt;
    }

    node.condition = std::move(*condition);
    node.body = std::move(*body);
    return AST::make<WhileExpression>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, BuiltInFunction node) {
    std::vector<AST> arguments{};
    arguments.reserve(node.arguments.size());

    for (auto &&arg : node.arguments) {
      auto optArg = arg.visit(*this);
      if (!optArg) {
        error("All arguments of a function call is expected to be non-empty",
              node.location);
        return std::nullopt;
      }
      arguments.emplace_back(*optArg);
    }

    if (hasErrors()) {
      return std::nullopt;
    }

    node.arguments = std::move(arguments);
    return AST::make<BuiltInFunction>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, FunctionCall node) {
    auto functionName = _symbolTable.get(node.name);
    if (!functionName) {
      error("Function <" + node.name + "> is not defined", node.location);
      return std::nullopt;
    }

    node.name = std::move(*functionName);

    std::vector<AST> arguments{};
    arguments.reserve(node.arguments.size());

    for (auto &&arg : node.arguments) {
      auto optArg = arg.visit(*this);
      if (!optArg) {
        error("All arguments of a function call is expected to be non-empty",
              node.location);
        return std::nullopt;
      }
      arguments.emplace_back(*optArg);
    }

    node.arguments = std::move(arguments);
    return AST::make<FunctionCall>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, FunctionDefinition node) {
    std::string functionName = _symbolTable.getScopeName() + "%" + node.name;
    _symbolTable.define(node.name, functionName);

    ScopeGuard<std::string> guard{_symbolTable, node.name};
    node.name = std::move(functionName);

    auto body = node.code.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!body) {
      error(
          "Body of function definition is expected to be non-empty expression",
          node.location);
      return std::nullopt;
    }

    node.code = std::move(*body);
    _functions.emplace_back(AST::make<FunctionDefinition>(std::move(node)));
    return std::nullopt;
  }

  std::optional<AST> operator()(const AST &, VariableDefinition node) {
    auto expr = node.expr.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!expr) {
      error("Child expression of assignment is exptected to be non-empty",
            node.location);
      return std::nullopt;
    }

    node.expr = std::move(*expr);
    return AST::make<VariableDefinition>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, AssignmentExpression node) {
    auto expr = node.expr.visit(*this);
    if (hasErrors()) {
      return std::nullopt;
    }

    if (!expr) {
      error("Child expression of assignment is exptected to be non-empty",
            node.location);
      return std::nullopt;
    }

    node.expr = std::move(*expr);
    return AST::make<AssignmentExpression>(std::move(node));
  }

  std::optional<AST> operator()(const AST &, NameReference node) {
    return AST::make<NameReference>(std::move(node));
  }

private:
  void error(std::string message, location location) {
    _errors.append("functions-pus-up", std::move(message), std::move(location));
  }

  bool hasErrors() const { return _errors.hasErrors(); }

  ErrorList &_errors;
  AST _ast;
  std::vector<AST> _functions;
  ScopedSymbolTable<std::string> _symbolTable;
};
} // namespace

std::optional<AST> pushUpFunction(std::string mainName, ValueType mainType,
                                  AST ast, ErrorList &errors) {
  auto type = checkTypes(ast, errors);
  if (errors.hasErrors()) {
    return std::move(ast);
  }

  FunctionPushUpVisitor visitor(std::move(ast), errors);
  return visitor.build(std::move(mainName), mainType);
}

} // namespace cheasle
