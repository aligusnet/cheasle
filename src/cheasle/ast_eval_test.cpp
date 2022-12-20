#include "Catch2/catch_amalgamated.hpp"
#include "ast_test_util.h"
#include "cheasle/error.h"
#include <cheasle/ast_eval.h>
#include <optional>
#include <sstream>

namespace cheasle {
using Catch::Matchers::ContainsSubstring;
using Catch::Matchers::WithinRel;

template <typename T> void requireType(const std::optional<Value> &val) {
  REQUIRE(val);
  REQUIRE(std::get_if<T>(&*val) != nullptr);
}

#define REQUIRE_DOUBLE(val) requireType<double>((val));

#define REQUIRE_BOOL(val) requireType<bool>((val));

std::optional<Value> eval(const AST &node) {
  std::ostringstream oss;
  ErrorList errors;
  return eval(node, errors, oss);
  REQUIRE_FALSE(errors.hasErrors());
}

TEST_CASE("let and reference", "[ast-eval]") {
  auto letAst = TAST::let("a", TAST::constant(10.0));
  auto refAst = TAST::ref("a");
  auto ast = TAST::b({letAst, refAst});

  auto result = eval(ast);

  REQUIRE_DOUBLE(result);
  REQUIRE_THAT(std::get<double>(*result), WithinRel(10.0, 1e-5));
}

TEST_CASE("const and reference", "[ast-eval]") {
  auto letAst = TAST::constexp("a", TAST::constant(10.0));
  auto refAst = TAST::ref("a");
  auto ast = TAST::b({letAst, refAst});

  auto result = eval(ast);

  REQUIRE_DOUBLE(result);
  REQUIRE_THAT(std::get<double>(*result), WithinRel(10.0, 1e-5));
}

TEST_CASE("binary expression", "[ast-eval]") {
  SECTION("add") {
    auto ast = TAST::add(TAST::constant(10.0), TAST::constant(17.0));
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(27.0, 1e-5));
  }

  SECTION("substract") {
    auto ast = TAST::sub(TAST::constant(10.0), TAST::constant(17.0));
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(-7.0, 1e-5));
  }

  SECTION("multiply") {
    auto ast = TAST::mul(TAST::constant(10.0), TAST::constant(17.0));
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(170.0, 1e-5));
  }

  SECTION("divide") {
    auto ast = TAST::div(TAST::constant(10.0), TAST::constant(17.0));
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(0.588235, 1e-5));
  }
}

TEST_CASE("unary expression", "[ast-eval]") {
  SECTION("minus") {
    auto ast = TAST::minus(TAST::constant(-7.1));
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(7.1, 1e-5));
  }

  SECTION("minus") {
    auto ast = TAST::abs(TAST::constant(-7.1));
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(7.1, 1e-5));
  }
}

void testLogicalExpression(double lhs, double rhs) {
  {
    auto ast = TAST::gt(TAST::constant(lhs), TAST::constant(rhs));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == (lhs > rhs));
  }

  {
    auto ast = TAST::ge(TAST::constant(lhs), TAST::constant(rhs));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == (lhs >= rhs));
  }

  {
    auto ast = TAST::lt(TAST::constant(lhs), TAST::constant(rhs));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == (lhs < rhs));
  }

  {
    auto ast = TAST::le(TAST::constant(lhs), TAST::constant(rhs));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == (lhs <= rhs));
  }

  {
    auto ast = TAST::eq(TAST::constant(lhs), TAST::constant(rhs));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == (lhs == rhs));
  }

  {
    auto ast = TAST::ne(TAST::constant(lhs), TAST::constant(rhs));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == (lhs != rhs));
  }
}

TEST_CASE("binary logical expression", "[ast-eval]") {
  SECTION("1.0 <=> 5.0") { testLogicalExpression(1.0, 5.0); }
  SECTION("-1.0 <=> -5.0") { testLogicalExpression(-1.0, -5.0); }
  SECTION("1.0 <=> -5.0") { testLogicalExpression(1.0, -5.0); }
  SECTION("-1.0 <=> 5.0") { testLogicalExpression(-1.0, 5.0); }

  SECTION("11.0 <=> 11.0") { testLogicalExpression(11.0, 11.0); }
  SECTION("-11.0 <=> -11.0") { testLogicalExpression(-11.0, -11.0); }
  SECTION("11.0 <=> -11.0") { testLogicalExpression(11.0, -11.0); }
  SECTION("-11.0 <=> 11.0") { testLogicalExpression(-11.0, 11.0); }
}

TEST_CASE("if expression", "[ast-eval]") {
  SECTION("then-branch") {
    auto ast = TAST::ifexp(TAST::constant(true), TAST::constant(true),
                           TAST::constant(false));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == true);
  }

  SECTION("else-branch") {
    auto ast = TAST::ifexp(TAST::constant(false), TAST::constant(true),
                           TAST::constant(false));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == false);
  }
}

TEST_CASE("while..do expression", "[ast-eval]") {
  SECTION("no loops") {
    /*
    while false do:
      11.0;
    end
    */
    auto ast = TAST::whileexp(TAST::constant(false), TAST::constant(11.0));
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result),
                 WithinRel(0.0, 1e-10)); // default value 0.0
  }

  SECTION("some loops") {
    /*
    let a: double = 0.0;
    while a < 10.0 do
      a = a + 1;
    end;
    */
    auto let = TAST::let("a", TAST::constant(0.0));
    auto body = TAST::b(
        {TAST::assig("a", TAST::add(TAST::ref("a"), TAST::constant(1.0)))});
    auto loop = TAST::whileexp(TAST::lt(TAST::ref("a"), TAST::constant(10.0)),
                               std::move(body));
    auto ast = TAST::b({let, loop});
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(10.0, 1e-10));
  }
}

TEST_CASE("builtin functions") {
  SECTION("sqrt") {
    auto ast = TAST::builtin(BuiltInFunctionId::Sqrt, {TAST::constant(81.0)});
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(9.0, 1e-10));
  }

  SECTION("log") {
    auto ast = TAST::builtin(BuiltInFunctionId::Log, {TAST::constant(81.0)});
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(4.3944491547, 1e-10));
  }

  SECTION("exp") {
    auto ast = TAST::builtin(BuiltInFunctionId::Exp, {TAST::constant(2.0)});
    auto result = eval(ast);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(7.3890560989, 1e-10));
  }

  SECTION("print") {
    auto ast = TAST::builtin(BuiltInFunctionId::Print,
                             std::vector<AST>{TAST::constant(10.2),
                                              TAST::constant(false),
                                              TAST::constant(11.9)});

    std::ostringstream oss;
    ErrorList errors{};
    auto result = eval(ast, errors, oss);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result), WithinRel(11.9, 1e-10));

    REQUIRE_FALSE(errors.hasErrors());

    std::string output = oss.str();
    REQUIRE_THAT(output, ContainsSubstring("10.2") &&
                             ContainsSubstring("false") &&
                             ContainsSubstring("11.9"));
  }
}
} // namespace cheasle
