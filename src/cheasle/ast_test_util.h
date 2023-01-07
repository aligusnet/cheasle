#pragma once
#include <Catch2/catch_amalgamated.hpp>
#include <cheasle/ast.h>

#include <variant>

namespace cheasle {
class TAST {
public:
  static inline AST constant(double value) {
    return AST::make<ConstantValue>(value, loc);
  }

  static inline AST constant(bool value) {
    return AST::make<ConstantValue>(value, loc);
  }

  static inline AST constant(int value) {
    return AST::make<ConstantValue>(static_cast<int32_t>(value), loc);
  }

  static inline AST constant(const char *value) {
    return AST::make<ConstantValue>(std::string(value), loc);
  }

  static inline AST constant(std::string value) {
    return AST::make<ConstantValue>(std::move(value), loc);
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

  static inline AST add(std::string lhs, std::string rhs) {
    return AST::make<BinaryExpression>(TAST::ref(std::move(lhs)),
                                       TAST::ref(std::move(rhs)),
                                       BinaryOperator::Add, loc);
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
    return AST::make<ComparisonExpression>(std::move(lhs), std::move(rhs),
                                           ComparisonOperator::LT, loc);
  }

  static inline AST le(AST lhs, AST rhs) {
    return AST::make<ComparisonExpression>(std::move(lhs), std::move(rhs),
                                           ComparisonOperator::LE, loc);
  }

  static inline AST gt(AST lhs, AST rhs) {
    return AST::make<ComparisonExpression>(std::move(lhs), std::move(rhs),
                                           ComparisonOperator::GT, loc);
  }

  static inline AST ge(AST lhs, AST rhs) {
    return AST::make<ComparisonExpression>(std::move(lhs), std::move(rhs),
                                           ComparisonOperator::GE, loc);
  }

  static inline AST lt(double lhs, double rhs) {
    return AST::make<ComparisonExpression>(
        TAST::constant(lhs), TAST::constant(rhs), ComparisonOperator::LT, loc);
  }

  static inline AST le(double lhs, double rhs) {
    return AST::make<ComparisonExpression>(
        TAST::constant(lhs), TAST::constant(rhs), ComparisonOperator::LE, loc);
  }

  static inline AST gt(double lhs, double rhs) {
    return AST::make<ComparisonExpression>(
        TAST::constant(lhs), TAST::constant(rhs), ComparisonOperator::GT, loc);
  }

  static inline AST ge(double lhs, double rhs) {
    return AST::make<ComparisonExpression>(
        TAST::constant(lhs), TAST::constant(rhs), ComparisonOperator::GE, loc);
  }

  static inline AST eq(AST lhs, AST rhs) {
    return AST::make<EqualityExpression>(std::move(lhs), std::move(rhs),
                                         EqualityOperator::EQ, loc);
  }

  static inline AST ne(AST lhs, AST rhs) {
    return AST::make<EqualityExpression>(std::move(lhs), std::move(rhs),
                                         EqualityOperator::NE, loc);
  }

  static inline AST eq(double lhs, double rhs) {
    return AST::make<EqualityExpression>(
        TAST::constant(lhs), TAST::constant(rhs), EqualityOperator::EQ, loc);
  }

  static inline AST ne(double lhs, double rhs) {
    return AST::make<EqualityExpression>(
        TAST::constant(lhs), TAST::constant(rhs), EqualityOperator::NE, loc);
  }

  static inline AST eq(bool lhs, bool rhs) {
    return AST::make<EqualityExpression>(
        TAST::constant(lhs), TAST::constant(rhs), EqualityOperator::EQ, loc);
  }

  static inline AST ne(bool lhs, bool rhs) {
    return AST::make<EqualityExpression>(
        TAST::constant(lhs), TAST::constant(rhs), EqualityOperator::NE, loc);
  }

  static inline AST andexp(AST lhs, AST rhs) {
    return AST::make<BinaryLogicalExpression>(std::move(lhs), std::move(rhs),
                                              BinaryLogicalOperator::And, loc);
  }

  static inline AST orexp(AST lhs, AST rhs) {
    return AST::make<BinaryLogicalExpression>(std::move(lhs), std::move(rhs),
                                              BinaryLogicalOperator::Or, loc);
  }

  static inline AST notexp(AST child) {
    return AST::make<NotExpression>(std::move(child), loc);
  }

  static inline AST b(AST child) {
    return AST::make<Block>(std::vector<AST>{std::move(child)}, loc);
  }

  static inline AST b(std::vector<AST> children) {
    return AST::make<Block>(std::move(children), loc);
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

  static inline AST sqrt(AST arg) {
    return AST::make<BuiltInFunction>(BuiltInFunctionId::Sqrt,
                                      std::vector<AST>{std::move(arg)}, loc);
  }

  static inline AST exp(AST arg) {
    return AST::make<BuiltInFunction>(BuiltInFunctionId::Exp,
                                      std::vector<AST>{std::move(arg)}, loc);
  }

  static inline AST log(AST arg) {
    return AST::make<BuiltInFunction>(BuiltInFunctionId::Log,
                                      std::vector<AST>{std::move(arg)}, loc);
  }

  static inline AST printf(std::vector<AST> args) {
    return AST::make<BuiltInFunction>(BuiltInFunctionId::Printf,
                                      std::move(args), loc);
  }

  static inline AST printf(std::string format, std::vector<AST> args) {
    args.insert(args.begin(), constant(format));
    return AST::make<BuiltInFunction>(BuiltInFunctionId::Printf,
                                      std::move(args), loc);
  }

private:
  static const location loc;
};

void requireAst(const AST &expected, const AST &actual);

#define REQUIRE_AST(expected, actual) requireAst((expected), (actual))

template <typename T> void requireType(const std::optional<Value> &val) {
  REQUIRE(val);
  if (std::get_if<T>(&*val) == nullptr) {
    std::cout << "Unexpected variant value: " << *val << std::endl;
  }
  REQUIRE(std::get_if<T>(&*val) != nullptr);
}

#define REQUIRE_DOUBLE(val) requireType<double>((val))

#define REQUIRE_BOOL(val) requireType<bool>((val))

#define REQUIRE_STRING(val) requireType<std::string>((val))

#define REQUIRE_INT(val) requireType<int32_t>((val))
} // namespace cheasle
