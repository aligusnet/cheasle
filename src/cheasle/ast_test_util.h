#pragma once
#include <Catch2/catch_amalgamated.hpp>
#include <cheasle/ast.h>

namespace cheasle {
class TAST {
public:
  static inline AST number(double value) {
    return AST::make<ConstantValue>(value, loc);
  }

  static inline AST add(AST lhs, AST rhs) {
    return AST::make<BinaryExpression>(std::move(lhs), std::move(rhs),
                                       BinaryOperator::Add, loc);
  }

  static inline AST sub(AST lhs, AST rhs) {
    return AST::make<BinaryExpression>(std::move(lhs), std::move(rhs),
                                       BinaryOperator::Subtract, loc);
  }

  static inline AST mul(AST lhs, AST rhs) {
    return AST::make<BinaryExpression>(std::move(lhs), std::move(rhs),
                                       BinaryOperator::Multiply, loc);
  }

  static inline AST div(AST lhs, AST rhs) {
    return AST::make<BinaryExpression>(std::move(lhs), std::move(rhs),
                                       BinaryOperator::Divide, loc);
  }

  static inline AST ref(std::string name) {
    return AST::make<NameReference>(std::move(name), loc);
  }

  static inline AST minus(AST child) {
    return AST::make<UnaryExpression>(std::move(child), UnaryOperator::Minus,
                                      loc);
  }

  static inline AST abs(AST child) {
    return AST::make<UnaryExpression>(std::move(child), UnaryOperator::Abs,
                                      loc);
  }

  static inline AST ifexp(AST pred, AST thenb, AST elseb) {
    return AST::make<IfExpression>(std::move(pred), std::move(thenb),
                                   std::move(elseb), loc);
  }

  static inline AST whileexp(AST pred, AST code) {
    return AST::make<WhileExpression>(std::move(pred), std::move(code), loc);
  }

  static inline AST lt(AST lhs, AST rhs) {
    return AST::make<BinaryLogicalExpression>(std::move(lhs), std::move(rhs),
                                              BinaryLogicalOperator::LT, loc);
  }

  static inline AST gt(AST lhs, AST rhs) {
    return AST::make<BinaryLogicalExpression>(std::move(lhs), std::move(rhs),
                                              BinaryLogicalOperator::GT, loc);
  }

  static inline AST b(AST child) {
    return AST::make<Block>(std::vector<AST>{child}, loc);
  }

  static inline AST def(std::string name, ValueType returnType,
                        std::vector<FunctionArgument> args, AST code) {
    return AST::make<FunctionDefinition>(std::move(name), returnType,
                                         std::move(code), std::move(args), loc);
  }

  static inline AST def(std::string name, std::vector<std::string> args,
                        AST code) {
    std::vector<FunctionArgument> arguments(args.size());
    std::transform(
        args.begin(), args.end(), arguments.begin(), [](std::string name) {
          return FunctionArgument{std::move(name), ValueType::Double};
        });
    return def(std::move(name), ValueType::Double, std::move(arguments),
               std::move(code));
  }

  static inline AST ufcall(std::string name, std::vector<AST> args) {
    return AST::make<FunctionCall>(std::move(name), std::move(args), loc);
  }

  static inline AST constexp(std::string name, ValueType type, AST expr) {
    return AST::make<VariableDefinition>(std::move(name), type, true,
                                         std::move(expr), loc);
  }

  static inline AST constexp(std::string name, AST expr) {
    return constexp(std::move(name), ValueType::Double, std::move(expr));
  }

  static inline AST let(std::string name, ValueType type, AST expr) {
    return AST::make<VariableDefinition>(std::move(name), type, false,
                                         std::move(expr), loc);
  }

  static inline AST let(std::string name, AST expr) {
    return let(std::move(name), ValueType::Double, std::move(expr));
  }

  static inline AST assig(std::string name, AST expr) {
    return AST::make<AssignmentExpression>(std::move(name), std::move(expr),
                                           loc);
  }

  static inline AST builtin(BuiltInFunctionId id, std::vector<AST> args) {
    return AST::make<BuiltInFunction>(id, std::move(args), loc);
  }

private:
  static const location loc;
};

void requireAst(const AST &expected, const AST &actual);

#define REQUIRE_AST(expected, actual) requireAst((expected), (actual))
} // namespace cheasle
