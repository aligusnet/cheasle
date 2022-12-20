#include "cheasle/error.h"
#include <Catch2/catch_amalgamated.hpp>
#include <cheasle/ast_eval.h>
#include <cheasle/driver.h>
#include <cheasle/type_checker.h>
#include <lexer.h>
#include <parser.h>

using Catch::Matchers::WithinRel;
namespace cheasle {

Value execute(const std::string &code, const std::string &testName,
              bool trace = false) {
  if (trace) {
    std::cout << "********************************" << std::endl;
    std::cout << "Building and executing program: " << std::endl;
    std::cout << "--------------------------------" << std::endl;
    std::cout << code;
    std::cout << std::endl;
  }

  Lexer lexer(code);
  lexer.filename = testName + ".che";
  Driver driver;
  Parser parser{lexer, &driver};
  auto parseResult = parser.parse();

  if (trace && parseResult != 0) {
    std::cout << "Parse failed: " << std::endl;
    std::cout << driver.getErrors();
  }

  REQUIRE(parseResult == 0);
  REQUIRE(!driver.getErrors().hasErrors());

  const auto &ast = driver.getAST();

  if (trace) {
    std::cout << "--------------------------------" << std::endl;
    std::cout << "AST:" << std::endl;
    std::cout << ast << std::endl;
  }

  cheasle::ErrorList typeCheckerErrors{};
  auto type = cheasle::checkTypes(ast, typeCheckerErrors);

  if (trace && typeCheckerErrors.hasErrors()) {
    std::cout << "--------------------------------" << std::endl;
    std::cout << "Type checking:" << std::endl;
    std::cout << typeCheckerErrors;
  }

  REQUIRE_FALSE(typeCheckerErrors.hasErrors());
  REQUIRE(type == ValueType::Double);

  cheasle::ErrorList evalErrors{};
  auto result = eval(ast, evalErrors, std::cout);

  if (trace && evalErrors.hasErrors()) {
    std::cout << "--------------------------------" << std::endl;
    std::cout << "Execution:" << std::endl;
    std::cout << evalErrors;
  }

  REQUIRE_FALSE(evalErrors.hasErrors());
  REQUIRE(result);

  if (trace) {
    std::cout << "--------------------------------" << std::endl;
    std::cout << "Execution result: " << *result << std::endl << std::endl;
  }

  return *result;
}

TEST_CASE("Fibonacci sequnce", "[integration]") {
  std::string code = "def fibonacci(n: double): double {\n"
                     "  if n == 0 {\n"
                     "    0;\n"
                     "  } else {\n"
                     "    if n == 1 {\n"
                     "      1;\n"
                     "    } else {\n"
                     "      fibonacci(n - 1) + fibonacci(n - 2);\n"
                     "    }\n"
                     "  }\n"
                     "}\n"
                     "fibonacci(21);\n";
  auto result = execute(code, "fibonacci", true);
  REQUIRE_THAT(std::get<double>(result), WithinRel(10946.0, 1e-8));
}

TEST_CASE("Sqrt", "[integration]") {
  std::string code =
      "def mySqrt(n: double) : double {\n"
      "  def average(a: double, b: double) : double { (a+b)/2; }\n"
      "  const eps: double = 0.0001;\n"
      "  let e: double = 1;\n"
      "  let t: double = n;\n"
      "  while |t - e| > eps {\n"
      "    t = n / e;\n"
      "    e = average(e, t);\n"
      "  }\n"
      "}\n"
      "const arg: double = 171;\n"
      "mySqrt(arg);\n";
  auto result = execute(code, "sqrt", true);
  REQUIRE_THAT(std::get<double>(result), WithinRel(sqrt(171.0), 1e-4));
}

}; // namespace cheasle
