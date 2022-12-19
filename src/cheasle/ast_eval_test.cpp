#include "Catch2/catch_amalgamated.hpp"
#include "ast_test_util.h"
#include "cheasle/error.h"
#include <cheasle/ast_eval.h>
#include <optional>

namespace cheasle {
using Catch::Matchers::WithinRel;

template <typename T> void requireType(const std::optional<Value> &val) {
  REQUIRE(val);
  REQUIRE(std::get_if<T>(&*val) != nullptr);
}

#define REQUIRE_DOUBLE(val) requireType<double>((val));

#define REQUIRE_BOOL(val) requireType<bool>((val));

TEST_CASE("let and reference", "[ast-eval]") {
  auto letAst = TAST::let("a", TAST::constant(10.0));
  auto refAst = TAST::ref("a");
  auto ast = TAST::b({letAst, refAst});

  ErrorList errors{};
  auto result = eval(ast, errors);

  REQUIRE_DOUBLE(result);
  REQUIRE(!errors.hasErrors());

  REQUIRE_THAT(std::get<double>(*result), WithinRel(10.0, 1e-5));
}

TEST_CASE("const and reference", "[ast-eval]") {
  auto letAst = TAST::constexp("a", TAST::constant(10.0));
  auto refAst = TAST::ref("a");
  auto ast = TAST::b({letAst, refAst});

  ErrorList errors{};
  auto result = eval(ast, errors);

  REQUIRE_DOUBLE(result);
  REQUIRE(!errors.hasErrors());

  REQUIRE_THAT(std::get<double>(*result), WithinRel(10.0, 1e-5));
}

TEST_CASE("binary expression", "[ast-eval]") {
  SECTION("add") {
    auto ast = TAST::add(TAST::constant(10.0), TAST::constant(17.0));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_DOUBLE(result);
    REQUIRE(!errors.hasErrors());

    REQUIRE_THAT(std::get<double>(*result), WithinRel(27.0, 1e-5));
  }

  SECTION("substract") {
    auto ast = TAST::sub(TAST::constant(10.0), TAST::constant(17.0));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_DOUBLE(result);
    REQUIRE(!errors.hasErrors());

    REQUIRE_THAT(std::get<double>(*result), WithinRel(-7.0, 1e-5));
  }

  SECTION("multiply") {
    auto ast = TAST::mul(TAST::constant(10.0), TAST::constant(17.0));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_DOUBLE(result);
    REQUIRE(!errors.hasErrors());

    REQUIRE_THAT(std::get<double>(*result), WithinRel(170.0, 1e-5));
  }

  SECTION("divide") {
    auto ast = TAST::div(TAST::constant(10.0), TAST::constant(17.0));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_DOUBLE(result);
    REQUIRE(!errors.hasErrors());

    REQUIRE_THAT(std::get<double>(*result), WithinRel(0.588235, 1e-5));
  }
}

TEST_CASE("unary expression", "[ast-eval]") {
  SECTION("minus") {
    auto ast = TAST::minus(TAST::constant(-7.1));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_DOUBLE(result);
    REQUIRE(!errors.hasErrors());

    REQUIRE_THAT(std::get<double>(*result), WithinRel(7.1, 1e-5));
  }

  SECTION("minus") {
    auto ast = TAST::abs(TAST::constant(-7.1));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_DOUBLE(result);
    REQUIRE(!errors.hasErrors());

    REQUIRE_THAT(std::get<double>(*result), WithinRel(7.1, 1e-5));
  }
}

void testLogicalExpression(double lhs, double rhs) {
  {
    auto ast = TAST::gt(TAST::constant(lhs), TAST::constant(rhs));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_BOOL(result);
    REQUIRE(!errors.hasErrors());
    REQUIRE(std::get<bool>(*result) == (lhs > rhs));
  }

  {
    auto ast = TAST::ge(TAST::constant(lhs), TAST::constant(rhs));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_BOOL(result);
    REQUIRE(!errors.hasErrors());
    REQUIRE(std::get<bool>(*result) == (lhs >= rhs));
  }

  {
    auto ast = TAST::lt(TAST::constant(lhs), TAST::constant(rhs));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_BOOL(result);
    REQUIRE(!errors.hasErrors());
    REQUIRE(std::get<bool>(*result) == (lhs < rhs));
  }

  {
    auto ast = TAST::le(TAST::constant(lhs), TAST::constant(rhs));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_BOOL(result);
    REQUIRE(!errors.hasErrors());
    REQUIRE(std::get<bool>(*result) == (lhs <= rhs));
  }

  {
    auto ast = TAST::eq(TAST::constant(lhs), TAST::constant(rhs));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_BOOL(result);
    REQUIRE(!errors.hasErrors());
    REQUIRE(std::get<bool>(*result) == (lhs == rhs));
  }

  {
    auto ast = TAST::ne(TAST::constant(lhs), TAST::constant(rhs));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_BOOL(result);
    REQUIRE(!errors.hasErrors());
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
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == true);
  }

  SECTION("else-branch") {
    auto ast = TAST::ifexp(TAST::constant(false), TAST::constant(true),
                           TAST::constant(false));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == false);
  }
}

TEST_CASE("while..do expression", "[ast-eval]") {
  SECTION("no loops") {
    auto ast = TAST::whileexp(TAST::constant(false), TAST::constant(11.0));
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result),
                 WithinRel(0.0, 1e-10)); // default value 0.0
  }

  SECTION("some loops") {
    auto let = TAST::let("a", TAST::constant(0.0));
    auto body = TAST::b(
        {TAST::assig("a", TAST::add(TAST::ref("a"), TAST::constant(1.0)))});
    auto loop = TAST::whileexp(TAST::lt(TAST::ref("a"), TAST::constant(10.0)),
                               std::move(body));
    auto ast = TAST::b({let, loop});
    ErrorList errors{};
    auto result = eval(ast, errors);

    REQUIRE_DOUBLE(result);
    REQUIRE_THAT(std::get<double>(*result),
                 WithinRel(10.0, 1e-10)); // default value 0.0
  }
}
} // namespace cheasle
