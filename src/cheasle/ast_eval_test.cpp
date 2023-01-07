#include "Catch2/catch_amalgamated.hpp"
#include "ast_test_util.h"
#include "cheasle/ast.h"
#include "cheasle/error.h"
#include "cheasle/value.h"
#include <cheasle/ast_eval.h>
#include <cheasle/type_checker.h>
#include <optional>
#include <sstream>

namespace cheasle {
using Catch::Matchers::WithinRel;

std::optional<Value> eval(AST &node) {
  ErrorList errors;

  checkTypes(node, errors);
  if (errors.hasErrors()) {
    std::cerr << errors;
  }
  REQUIRE_FALSE(errors.hasErrors());

  std::ostringstream oss;
  auto result = eval(node, errors, oss);
  if (errors.hasErrors()) {
    std::cerr << errors;
  }

  REQUIRE_FALSE(errors.hasErrors());
  return result;
}

TEST_CASE("let and reference [double]", "[ast-eval]") {
  auto letAst = TAST::let("a", TAST::constant(10.0));
  auto refAst = TAST::ref("a");
  auto ast = TAST::b({letAst, refAst});

  auto result = eval(ast);

  REQUIRE_DOUBLE(result);
  REQUIRE_THAT(std::get<double>(*result), WithinRel(10.0, 1e-5));
}

TEST_CASE("let and reference [int]", "[ast-eval]") {
  auto letAst = TAST::let("a", ValueType::Int, TAST::constant(10));
  auto refAst = TAST::ref("a");
  auto ast = TAST::b({letAst, refAst});

  auto result = eval(ast);

  REQUIRE_INT(result);
  REQUIRE(std::get<int32_t>(*result) == 10);
}

TEST_CASE("const and reference", "[ast-eval]") {
  auto letAst = TAST::constexp("a", TAST::constant(10.0));
  auto refAst = TAST::ref("a");
  auto ast = TAST::b({letAst, refAst});

  auto result = eval(ast);

  REQUIRE_DOUBLE(result);
  REQUIRE_THAT(std::get<double>(*result), WithinRel(10.0, 1e-5));
}

TEST_CASE("binary expression - double", "[ast-eval]") {
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

TEST_CASE("binary expression - int", "[ast-eval]") {
  SECTION("add") {
    auto ast = TAST::add(TAST::constant(10), TAST::constant(17));
    auto result = eval(ast);

    REQUIRE_INT(result);
    REQUIRE(std::get<int>(*result) == 27);
  }

  SECTION("substract") {
    auto ast = TAST::sub(TAST::constant(10), TAST::constant(17));
    auto result = eval(ast);

    REQUIRE_INT(result);
    REQUIRE(std::get<int>(*result) == -7);
  }

  SECTION("multiply") {
    auto ast = TAST::mul(TAST::constant(10), TAST::constant(17));
    auto result = eval(ast);

    REQUIRE_INT(result);
    REQUIRE(std::get<int>(*result) == 170);
  }

  SECTION("divide") {
    auto ast = TAST::div(TAST::constant(10), TAST::constant(17));
    auto result = eval(ast);

    REQUIRE_INT(result);
    REQUIRE(std::get<int>(*result) == 0);
  }
}

TEST_CASE("unary expression - double", "[ast-eval]") {
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

TEST_CASE("unary expression - int", "[ast-eval]") {
  SECTION("minus") {
    auto ast = TAST::minus(TAST::constant(-7));
    auto result = eval(ast);

    REQUIRE_INT(result);
    REQUIRE(std::get<int32_t>(*result) == 7);
  }

  SECTION("minus") {
    auto ast = TAST::abs(TAST::constant(-7));
    auto result = eval(ast);

    REQUIRE_INT(result);
    REQUIRE(std::get<int32_t>(*result) == 7);
  }
}

template <typename T> void testEqualityAndComparisonExpressions(T lhs, T rhs) {
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

TEST_CASE("equality and comparison expression - double", "[ast-eval]") {
  SECTION("1.0 <=> 5.0") { testEqualityAndComparisonExpressions(1.0, 5.0); }
  SECTION("-1.0 <=> -5.0") { testEqualityAndComparisonExpressions(-1.0, -5.0); }
  SECTION("1.0 <=> -5.0") { testEqualityAndComparisonExpressions(1.0, -5.0); }
  SECTION("-1.0 <=> 5.0") { testEqualityAndComparisonExpressions(-1.0, 5.0); }

  SECTION("11.0 <=> 11.0") { testEqualityAndComparisonExpressions(11.0, 11.0); }
  SECTION("-11.0 <=> -11.0") {
    testEqualityAndComparisonExpressions(-11.0, -11.0);
  }
  SECTION("11.0 <=> -11.0") {
    testEqualityAndComparisonExpressions(11.0, -11.0);
  }
  SECTION("-11.0 <=> 11.0") {
    testEqualityAndComparisonExpressions(-11.0, 11.0);
  }
}

TEST_CASE("equality and comparison expression - int", "[ast-eval]") {
  SECTION("1 <=> 5") { testEqualityAndComparisonExpressions(1, 5); }
  SECTION("-1 <=> -5") { testEqualityAndComparisonExpressions(-1, -5); }
  SECTION("1 <=> -5") { testEqualityAndComparisonExpressions(1, -5); }
  SECTION("-1 <=> 5") { testEqualityAndComparisonExpressions(-1, 5); }

  SECTION("11 <=> 11") { testEqualityAndComparisonExpressions(11, 11); }
  SECTION("-11 <=> -11") { testEqualityAndComparisonExpressions(-11, -11); }
  SECTION("11 <=> -11") { testEqualityAndComparisonExpressions(11, -11); }
  SECTION("-11 <=> 11") { testEqualityAndComparisonExpressions(-11, 11); }
}

TEST_CASE("binary logical expression", "[ast-eval]") {
  SECTION("true and true") {
    auto ast = TAST::andexp(TAST::constant(true), TAST::constant(true));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == true);
  }

  SECTION("true and false") {
    auto ast = TAST::andexp(TAST::constant(true), TAST::constant(false));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == false);
  }

  SECTION("false and true") {
    auto ast = TAST::andexp(TAST::constant(false), TAST::constant(true));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == false);
  }

  SECTION("true or true") {
    auto ast = TAST::orexp(TAST::constant(true), TAST::constant(true));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == true);
  }

  SECTION("false or true") {
    auto ast = TAST::orexp(TAST::constant(false), TAST::constant(true));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == true);
  }

  SECTION("true or false") {
    auto ast = TAST::orexp(TAST::constant(true), TAST::constant(false));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == true);
  }

  SECTION("false or false") {
    auto ast = TAST::orexp(TAST::constant(false), TAST::constant(false));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == false);
  }
}

TEST_CASE("not expression", "[ast-eval]") {
  SECTION("not true") {
    auto ast = TAST::notexp(TAST::constant(true));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == false);
  }

  SECTION("not false") {
    auto ast = TAST::notexp(TAST::constant(false));
    auto result = eval(ast);

    REQUIRE_BOOL(result);
    REQUIRE(std::get<bool>(*result) == true);
  }
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

  SECTION("printf") {
    auto ast = TAST::printf(
        "Hello: %f, %d, %A, %x",
        std::vector<AST>{TAST::constant(10.1), TAST::constant(true),
                         TAST::constant(931.799), TAST::constant(572)});

    std::ostringstream oss;
    ErrorList errors{};
    auto result = eval(ast, errors, oss);

    std::string expectedString =
        "Hello: 10.100000, 1, 0X1.D1E645A1CAC08P+9, 572";
    REQUIRE_INT(result);
    REQUIRE(std::get<int32_t>(*result) == expectedString.size());

    REQUIRE_FALSE(errors.hasErrors());

    std::string output = oss.str();
    REQUIRE(output == expectedString);
  }
}

TEST_CASE("user function", "[ast-eval]") {
  /*
  def less(a: double, b: double): bool =
    a < b;
  end
  less(10.0, 10.1);
  */
  auto func = TAST::def("less", ValueType::Boolean,
                        {FunctionArgument{"a", ValueType::Double},
                         FunctionArgument{"b", ValueType::Double}},
                        TAST::lt(TAST::ref("a"), TAST::ref("b")));
  auto call =
      TAST::ufcall("less", {TAST::constant(10.0), TAST::constant(10.1)});
  auto ast = TAST::b({std::move(func), std::move(call)});
  auto result = eval(ast);

  REQUIRE_BOOL(result);
  REQUIRE(std::get<bool>(*result) == true);
}
} // namespace cheasle
