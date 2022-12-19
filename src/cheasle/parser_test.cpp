#include "mongodb/concepts.h"
#include <Catch2/catch_amalgamated.hpp>
#include <cheasle/ast.h>
#include <cheasle/driver.h>
#include <lexer.h>
#include <location.h>
#include <parser.h>
#include <sstream>

namespace cheasle {
namespace {
namespace TAST {
location loc{};
AST number(double value) { return AST::make<Number>(value, loc); }
AST add(AST lhs, AST rhs) {
  return AST::make<BinaryExpression>(std::move(lhs), std::move(rhs),
                                     BinaryOperator::Add, loc);
}

AST sub(AST lhs, AST rhs) {
  return AST::make<BinaryExpression>(std::move(lhs), std::move(rhs),
                                     BinaryOperator::Subtract, loc);
}

AST mul(AST lhs, AST rhs) {
  return AST::make<BinaryExpression>(std::move(lhs), std::move(rhs),
                                     BinaryOperator::Multiply, loc);
}

AST div(AST lhs, AST rhs) {
  return AST::make<BinaryExpression>(std::move(lhs), std::move(rhs),
                                     BinaryOperator::Divide, loc);
}

AST ref(std::string name) {
  return AST::make<NameReference>(std::move(name), loc);
}

AST minus(AST child) {
  return AST::make<UnaryExpression>(std::move(child), UnaryOperator::Minus,
                                    loc);
}

AST abs(AST child) {
  return AST::make<UnaryExpression>(std::move(child), UnaryOperator::Abs, loc);
}

AST ifexp(AST pred, AST thenb, AST elseb) {
  return AST::make<IfExpression>(std::move(pred), std::move(thenb),
                                 std::move(elseb), loc);
}

AST whileexp(AST pred, AST code) {
  return AST::make<WhileExpression>(std::move(pred), std::move(code), loc);
}

AST lt(AST lhs, AST rhs) {
  return AST::make<BinaryLogicalExpression>(std::move(lhs), std::move(rhs),
                                            BinaryLogicalOperator::LT, loc);
}

AST gt(AST lhs, AST rhs) {
  return AST::make<BinaryLogicalExpression>(std::move(lhs), std::move(rhs),
                                            BinaryLogicalOperator::GT, loc);
}

AST b(AST child) { return AST::make<Block>(std::vector<AST>{child}, loc); }

AST def(std::string name, std::vector<std::string> args, AST code) {
  return AST::make<FunctionDefinition>(std::move(name), std::move(code),
                                       std::move(args), loc);
}

AST ufcall(std::string name, std::vector<AST> args) {
  return AST::make<FunctionCall>(std::move(name), std::move(args), loc);
}

AST constexp(std::string name, AST expr) {
  return AST::make<VariableDefinition>(std::move(name), true, std::move(expr),
                                       loc);
}

AST let(std::string name, AST expr) {
  return AST::make<VariableDefinition>(std::move(name), false, std::move(expr),
                                       loc);
}

AST assig(std::string name, AST expr) {
  return AST::make<AssignmentExpression>(std::move(name), std::move(expr), loc);
}

AST builtin(BuiltInFunctionId id, std::vector<AST> args) {
  return AST::make<BuiltInFunction>(id, std::move(args), loc);
}
} // namespace TAST

AST parse(const std::string &code) {
  Lexer lexer(code);
  Driver driver{};
  Parser parser{lexer, &driver};
  auto result = parser.parse();

  if (result != 0) {
    std::cout << driver.getErrors();
  }

  REQUIRE(result == 0);
  REQUIRE(!driver.getErrors().hasErrors());
  return std::move(driver.getAST());
}

void requireAst(const AST &expected, const AST &actual) {
  std::stringstream expectedS;
  expectedS << TAST::b(expected);

  std::stringstream actualS;
  actualS << actual;

  REQUIRE(expectedS.str() == actualS.str());
}

} // namespace

TEST_CASE("addition expression", "[parser]") {
  std::string code = "5 + 10;";
  auto ast = parse(code);

  auto expected = TAST::add(TAST::number(5), TAST::number(10));

  requireAst(expected, ast);
}

TEST_CASE("arithmetic expression", "[parser]") {
  std::string code = "5 - 10 * -a / |c|;";
  auto ast = parse(code);

  auto mult = TAST::mul(TAST::number(10), TAST::minus(TAST::ref("a")));
  auto div = TAST::div(std::move(mult), TAST::abs(TAST::ref("c")));
  auto sub = TAST::sub(TAST::number(5), std::move(div));

  requireAst(sub, ast);
}

TEST_CASE("if expression", "[parser]") {
  std::string code = "if a < b then c; else d; end";
  auto ast = parse(code);

  auto expected = TAST::ifexp(TAST::lt(TAST::ref("a"), TAST::ref("b")),
                              TAST::b(TAST::ref("c")), TAST::b(TAST::ref("d")));
  requireAst(expected, ast);
}

TEST_CASE("while expression", "[parser]") {
  std::string code = "while a > b do c; end";
  auto ast = parse(code);

  auto expected = TAST::whileexp(TAST::gt(TAST::ref("a"), TAST::ref("b")),
                                 TAST::b(TAST::ref("c")));
  requireAst(expected, ast);
}

TEST_CASE("user function definition", "[parser]") {
  std::string code = "def average(a, b) = (a + b) * 0.5; end";
  auto ast = parse(code);

  auto body =
      TAST::mul(TAST::add(TAST::ref("a"), TAST::ref("b")), TAST::number(0.5));
  auto expected = TAST::def("average", {"a", "b"}, TAST::b(std::move(body)));
  requireAst(expected, ast);
}

TEST_CASE("user function cal", "[parser]") {
  std::string code = "average(a, b);";
  auto ast = parse(code);

  auto expected = TAST::ufcall("average", {TAST::ref("a"), TAST::ref("b")});
  requireAst(expected, ast);
}

TEST_CASE("const declaration", "[parser]") {
  std::string code = "const a = b;";
  auto ast = parse(code);

  auto expected = TAST::constexp("a", TAST::ref("b"));
  requireAst(expected, ast);
}

TEST_CASE("variable declaration", "[parser]") {
  std::string code = "let a = b;";
  auto ast = parse(code);

  auto expected = TAST::let("a", TAST::ref("b"));
  requireAst(expected, ast);
}

TEST_CASE("assignment", "[parser]") {
  std::string code = "a = b;";
  auto ast = parse(code);

  auto expected = TAST::assig("a", TAST::ref("b"));
  requireAst(expected, ast);
}

TEST_CASE("variable reference", "[parser]") {
  std::string code = "a;";
  auto ast = parse(code);

  auto expected = TAST::ref("a");
  requireAst(expected, ast);
}

TEST_CASE("bultin function call", "[parser]") {
  SECTION("sqrt") {
    std::string code = "sqrt(a);";
    auto ast = parse(code);

    auto expected = TAST::builtin(BuiltInFunctionId::Sqrt, {TAST::ref("a")});
    requireAst(expected, ast);
  }

  SECTION("exp") {
    std::string code = "exp(a);";
    auto ast = parse(code);

    auto expected = TAST::builtin(BuiltInFunctionId::Exp, {TAST::ref("a")});
    requireAst(expected, ast);
  }

  SECTION("log") {
    std::string code = "log(a);";
    auto ast = parse(code);

    auto expected = TAST::builtin(BuiltInFunctionId::Log, {TAST::ref("a")});
    requireAst(expected, ast);
  }

  SECTION("print") {
    std::string code = "print(a, 10, b);";
    auto ast = parse(code);

    auto expected =
        TAST::builtin(BuiltInFunctionId::Print,
                      {TAST::ref("a"), TAST::number(10), TAST::ref("b")});
    requireAst(expected, ast);
  }
}
} // namespace cheasle
