#include "cheasle/error.h"
#include <Catch2/catch_amalgamated.hpp>
#include <cheasle/ast_eval.h>
#include <cheasle/ast_test_util.h>
#include <cheasle/driver.h>
#include <cheasle/llvm_codegen.h>
#include <cheasle/type_checker.h>
#include <lexer.h>
#include <parser.h>

using Catch::Matchers::WithinRel;
namespace cheasle {

AST generateAST(Lexer &lexer, bool trace) {
  Driver driver;
  Parser parser{lexer, &driver};
  auto parseResult = parser.parse();

  if (trace && parseResult != 0) {
    std::cout << "Parse failed: " << std::endl;
    std::cout << driver.getErrors();
  }

  REQUIRE(parseResult == 0);
  REQUIRE(!driver.getErrors().hasErrors());

  if (trace) {
    std::cout << "--------------------------------" << std::endl;
    std::cout << "AST:" << std::endl;
    std::cout << driver.getAST() << std::endl;
  }

  return driver.getAST();
}

template <typename T> T interpret(AST &ast, bool trace = false) {
  cheasle::ErrorList typeCheckerErrors{};
  auto type = cheasle::checkTypes(ast, typeCheckerErrors);

  if (trace && typeCheckerErrors.hasErrors()) {
    std::cout << "--------------------------------" << std::endl;
    std::cout << "Type checking:" << std::endl;
    std::cout << typeCheckerErrors;
  }

  REQUIRE_FALSE(typeCheckerErrors.hasErrors());

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

  requireType<T>(result);
  return std::get<T>(*result);
}

template <typename T> T compileAndRun(AST ast, bool trace) {
  ErrorList errors{};
  auto result = compileAndRun(ast, errors);
  if (errors.hasErrors()) {
    std::cout << errors;
  }

  REQUIRE_FALSE(errors.hasErrors());
  REQUIRE(result);

  requireType<T>(result);
  return std::get<T>(*result);
}

template <typename T> void validateResult(T expected, T actual) {
  REQUIRE(expected == actual);
}

template <> void validateResult<double>(double expected, double actual) {
  REQUIRE_THAT(expected, WithinRel(actual, 1e-4));
}

template <typename T>
void runIntegrationTest(const std::string &code, const std::string &testName,
                        bool trace, T expected) {
  if (trace) {
    std::cout << "********************************" << std::endl;
    std::cout << "Building and executing program: " << std::endl;
    std::cout << "--------------------------------" << std::endl;
    std::cout << code;
    std::cout << std::endl;
  }

  Lexer lexer(code);
  lexer.filename = testName + ".che";

  auto ast = generateAST(lexer, trace);
  {
    auto result = interpret<T>(ast, trace);
    validateResult(expected, result);
  }
  {
    auto result = compileAndRun<T>(std::move(ast), trace);
    validateResult(expected, result);
  }
}

TEST_CASE("Fibonacci sequnce", "[integration]") {
  std::string code = "def fibonacci(n: double): double {\n"
                     "  if n == 0.0 {\n"
                     "    0.0;\n"
                     "  } else {\n"
                     "    if n == 1.0 {\n"
                     "      1.0;\n"
                     "    } else {\n"
                     "      fibonacci(n - 1.0) + fibonacci(n - 2.0);\n"
                     "    }\n"
                     "  }\n"
                     "}\n"
                     "fibonacci(21.0);\n";
  runIntegrationTest(code, "fibonacci", false, 10946.0);
}

TEST_CASE("Sqrt", "[integration]") {
  std::string code =
      "def mySqrt(n: double) : double {\n"
      "  def average(a: double, b: double) : double { (a+b)/2.0; }\n"
      "  const eps: double = 0.0001;\n"
      "  let e: double = 1.0;\n"
      "  let t: double = n;\n"
      "  while |t - e| > eps {\n"
      "    t = n / e;\n"
      "    e = average(e, t);\n"
      "  }\n"
      "}\n"
      "const arg: double = 171.0;\n"
      "mySqrt(arg);\n";
  runIntegrationTest(code, "sqrt", false, sqrt(171.0));
}

TEST_CASE("Within", "[integration]") {
  std::string code =
      "def within(begin: double, end: double, value: double): bool {\n"
      "  value >= begin and value < end;\n"
      "}\n"
      "def within2(begin: double, end: double, value: double): bool {\n"
      "  not (value < begin or value >= end);\n"
      "}\n"
      "within(5.0, 10.0, 7.0) and within2(5.0, 10.0, 7.0);\n";
  runIntegrationTest(code, "within", false, true);
}

}; // namespace cheasle
