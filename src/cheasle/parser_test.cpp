#include "ast_test_util.h"
#include "cheasle/value.h"
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
} // namespace

TEST_CASE("addition expression", "[parser]") {
  std::string code = "5 + 10;";
  auto ast = parse(code);

  auto expected = TAST::add(TAST::constant(5.0), TAST::constant(10.0));

  REQUIRE_AST(expected, ast);
}

TEST_CASE("comparison expression", "[parser]") {
  SECTION("gt") {
    std::string code = "5 > 10;";
    auto ast = parse(code);

    auto expected = TAST::gt(TAST::constant(5.0), TAST::constant(10.0));

    REQUIRE_AST(expected, ast);
  }
  SECTION("ne") {
    std::string code = "5 != 10;";
    auto ast = parse(code);

    auto expected = TAST::ne(TAST::constant(5.0), TAST::constant(10.0));

    REQUIRE_AST(expected, ast);
  }
}

TEST_CASE("binary logical expression", "[parser]") {
  SECTION("and") {
    std::string code = "a and b;";

    auto ast = parse(code);

    auto expected = TAST::andexp(TAST::ref("a"), TAST::ref("b"));

    REQUIRE_AST(expected, ast);
  }

  SECTION("or") {
    std::string code = "a or b;";

    auto ast = parse(code);

    auto expected = TAST::orexp(TAST::ref("a"), TAST::ref("b"));

    REQUIRE_AST(expected, ast);
  }
}

TEST_CASE("not expression", "[parser]") {
  std::string code = "not a;";

  auto ast = parse(code);

  auto expected = TAST::notexp(TAST::ref("a"));

  REQUIRE_AST(expected, ast);
}

TEST_CASE("arithmetic expression", "[parser]") {
  std::string code = "5 - 10 * -a / |c|;";
  auto ast = parse(code);

  auto mult = TAST::mul(TAST::constant(10.0), TAST::minus(TAST::ref("a")));
  auto div = TAST::div(std::move(mult), TAST::abs(TAST::ref("c")));
  auto sub = TAST::sub(TAST::constant(5.0), std::move(div));

  REQUIRE_AST(sub, ast);
}

TEST_CASE("if expression", "[parser]") {
  std::string code = "if a < b { c; } else { d; }";
  auto ast = parse(code);

  auto expected = TAST::ifexp(TAST::lt(TAST::ref("a"), TAST::ref("b")),
                              TAST::b(TAST::ref("c")), TAST::b(TAST::ref("d")));
  REQUIRE_AST(expected, ast);
}

TEST_CASE("while expression", "[parser]") {
  std::string code = "while a > b { c; }";
  auto ast = parse(code);

  auto expected = TAST::whileexp(TAST::gt(TAST::ref("a"), TAST::ref("b")),
                                 TAST::b(TAST::ref("c")));
  REQUIRE_AST(expected, ast);
}

TEST_CASE("user function definition", "[parser]") {
  std::string code =
      "def average(a: double, b: double) : double { (a + b) * 0.5; }";
  auto ast = parse(code);

  auto body =
      TAST::mul(TAST::add(TAST::ref("a"), TAST::ref("b")), TAST::constant(0.5));
  auto expected = TAST::def("average", {"a", "b"}, TAST::b(std::move(body)));
  REQUIRE_AST(expected, ast);
}

TEST_CASE("user function cal", "[parser]") {
  std::string code = "average(a, b);";
  auto ast = parse(code);

  auto expected = TAST::ufcall("average", {TAST::ref("a"), TAST::ref("b")});
  REQUIRE_AST(expected, ast);
}

TEST_CASE("const declaration", "[parser]") {
  std::string code = "const a: double = b;";
  auto ast = parse(code);

  auto expected = TAST::constexp("a", TAST::ref("b"));
  REQUIRE_AST(expected, ast);
}

TEST_CASE("variable declaration", "[parser]") {
  std::string code = "let a: bool = b;";
  auto ast = parse(code);

  auto expected = TAST::let("a", ValueType::Boolean, TAST::ref("b"));
  REQUIRE_AST(expected, ast);
}

TEST_CASE("assignment", "[parser]") {
  std::string code = "a = b;";
  auto ast = parse(code);

  auto expected = TAST::assig("a", TAST::ref("b"));
  REQUIRE_AST(expected, ast);
}

TEST_CASE("variable reference", "[parser]") {
  std::string code = "a;";
  auto ast = parse(code);

  auto expected = TAST::ref("a");
  REQUIRE_AST(expected, ast);
}

TEST_CASE("bultin function call", "[parser]") {
  SECTION("sqrt") {
    std::string code = "sqrt(a);";
    auto ast = parse(code);

    auto expected = TAST::builtin(BuiltInFunctionId::Sqrt, {TAST::ref("a")});
    REQUIRE_AST(expected, ast);
  }

  SECTION("exp") {
    std::string code = "exp(a);";
    auto ast = parse(code);

    auto expected = TAST::builtin(BuiltInFunctionId::Exp, {TAST::ref("a")});
    REQUIRE_AST(expected, ast);
  }

  SECTION("log") {
    std::string code = "log(a);";
    auto ast = parse(code);

    auto expected = TAST::builtin(BuiltInFunctionId::Log, {TAST::ref("a")});
    REQUIRE_AST(expected, ast);
  }

  SECTION("print") {
    std::string code = "print(a, 10, b);";
    auto ast = parse(code);

    auto expected =
        TAST::builtin(BuiltInFunctionId::Print,
                      {TAST::ref("a"), TAST::constant(10.0), TAST::ref("b")});
    REQUIRE_AST(expected, ast);
  }
}
} // namespace cheasle
