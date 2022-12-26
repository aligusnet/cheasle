#include "Catch2/catch_amalgamated.hpp"
#include "cheasle/error.h"
#include <cheasle/ast_test_util.h>
#include <cheasle/llvm_codegen.h>

namespace cheasle {
using Catch::Matchers::WithinRel;

template <typename T> T compileAndRun(AST ast) {
  ErrorList errors{};
  auto result = compileAndRun(std::move(ast), errors);
  if (errors.hasErrors()) {
    std::cout << errors;
  }

  REQUIRE_FALSE(errors.hasErrors());
  REQUIRE(result);

  requireType<T>(result);
  return std::get<T>(*result);
}

TEST_CASE("constant expression", "[llvm jit]") {
  SECTION("double") {
    auto ast = TAST::constant(100.0);
    auto result = compileAndRun<double>(std::move(ast));

    REQUIRE_THAT(result, WithinRel(100.0, 1e-8));
  }

  SECTION("false") {
    auto ast = TAST::constant(false);
    auto result = compileAndRun<bool>(std::move(ast));
    REQUIRE(result == false);
  }

  SECTION("true") {
    auto ast = TAST::constant(true);
    auto result = compileAndRun<bool>(std::move(ast));
    REQUIRE(result == true);
  }
}

TEST_CASE("binary expression", "[llvm jit]") {
  SECTION("double + double") {
    auto ast = TAST::add(TAST::constant(11.0), TAST::constant(100.0));
    auto result = compileAndRun<double>(std::move(ast));
    REQUIRE_THAT(result, WithinRel(111.0, 1e-8));
  }

  SECTION("double - double") {
    auto ast = TAST::sub(TAST::constant(11.0), TAST::constant(100.0));
    auto result = compileAndRun<double>(std::move(ast));
    REQUIRE_THAT(result, WithinRel(-89.0, 1e-8));
  }

  SECTION("double * double") {
    auto ast = TAST::mul(TAST::constant(11.0), TAST::constant(100.0));
    auto result = compileAndRun<double>(std::move(ast));
    REQUIRE_THAT(result, WithinRel(1100.0, 1e-8));
  }

  SECTION("double / double") {
    auto ast = TAST::div(TAST::constant(11.0), TAST::constant(100.0));
    auto result = compileAndRun<double>(std::move(ast));
    REQUIRE_THAT(result, WithinRel(0.11, 1e-8));
  }
}

TEST_CASE("user functions", "[llvm jit]") {
  auto body = TAST::add(TAST::ref("a"), TAST::ref("b"));
  auto function = TAST::def("add", {"a", "b"}, std::move(body));
  auto call = TAST::ufcall(
      "add", std::vector<AST>{TAST::constant(8.0), TAST::constant(11.0)});
  auto ast = TAST::b(std::vector<AST>{std::move(function), std::move(call)});
  auto result = compileAndRun<double>(std::move(ast));
  REQUIRE_THAT(result, WithinRel(19.0, 1e-8));
}

TEST_CASE("nested user functions", "[llvm jit]") {
  auto add2 = TAST::def("add2", {"a", "b"}, TAST::add("a", "b"));

  auto add3Body = TAST::b(
      {add2,
       TAST::add(TAST::ref("a"),
                 TAST::ufcall("add2", {TAST::ref("b"), TAST::ref("c")}))});
  auto add3 = TAST::def("add3", {"a", "b", "c"}, std::move(add3Body));

  auto add4Body = TAST::b(
      {add2,
       TAST::add(TAST::ufcall("add2", {TAST::ref("a"), TAST::ref("b")}),
                 TAST::ufcall("add2", {TAST::ref("c"), TAST::ref("d")}))});
  auto add4 = TAST::def("add4", {"a", "b", "c", "d"}, std::move(add4Body));

  auto ast = TAST::b(
      {std::move(add2), std::move(add3), std::move(add4),
       TAST::ufcall(
           "add2",
           {TAST::ufcall("add3", {TAST::constant(1.0), TAST::constant(2.0),
                                  TAST::constant(3.0)}),
            TAST::ufcall("add4",
                         {TAST::constant(4.0), TAST::constant(5.0),
                          TAST::constant(6.0), TAST::constant(7.0)})})});

  ErrorList errors{};
  auto result = compileAndRun(std::move(ast), errors);
  if (errors.hasErrors()) {
    std::cout << errors;
  }
  REQUIRE_FALSE(errors.hasErrors());
  REQUIRE(result);
  REQUIRE_THAT(std::get<double>(*result), WithinRel(28.0, 1e-8));
}

TEST_CASE("builtin function", "[llvm jit]") {
  SECTION("sqrt") {
    auto ast = TAST::sqrt(TAST::constant(10.0));
    auto result = compileAndRun<double>(std::move(ast));
    REQUIRE_THAT(result, WithinRel(sqrt(10.0), 1e-8));
  }

  SECTION("exp") {
    auto ast = TAST::exp(TAST::constant(10.0));
    auto result = compileAndRun<double>(std::move(ast));
    REQUIRE_THAT(result, WithinRel(exp(10.0), 1e-8));
  }

  SECTION("log") {
    auto ast = TAST::log(TAST::constant(10.0));
    auto result = compileAndRun<double>(std::move(ast));
    REQUIRE_THAT(result, WithinRel(log(10.0), 1e-8));
  }
}
} // namespace cheasle
