#include <Catch2/catch_amalgamated.hpp>
#include <cheasle/ast_eval.h>
#include <cheasle/driver.h>
#include <lexer.h>
#include <parser.h>

using Catch::Matchers::WithinRel;
namespace cheasle {

Value execute(const std::string &code, bool trace = false) {
  if (trace) {
    std::cout << "********************************" << std::endl;
    std::cout << "Building and executing program: " << std::endl;
    std::cout << "--------------------------------" << std::endl;
    std::cout << code;
    std::cout << std::endl;
  }

  Lexer lexer(code);
  lexer.filename = "input-string";
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

    std::cout << "--------------------------------" << std::endl;
    std::cout << "Execution:" << std::endl;
  }

  cheasle::ErrorList evalErrors{};
  auto result = eval(ast, evalErrors);

  if (trace && evalErrors.hasErrors()) {
    std::cout << evalErrors;
  }

  REQUIRE(!evalErrors.hasErrors());
  REQUIRE(result);

  if (trace) {
    std::cout << "--------------------------------" << std::endl;
    std::cout << "Execution result: " << *result << std::endl << std::endl;
  }

  return *result;
}

TEST_CASE("Fibinacci sequnce", "[integration]") {
  std::string code = "def fibonacci(n) = \n"
                     "  if n == 0 then\n"
                     "    0;\n"
                     "  else\n"
                     "    if n == 1 then\n"
                     "      1;\n"
                     "    else\n"
                     "      fibonacci(n - 1) + fibonacci(n - 2);\n"
                     "    end\n"
                     "  end\n"
                     "end\n"
                     "fibonacci(21);\n";
  auto result = execute(code, false);
  REQUIRE_THAT(std::get<double>(result), WithinRel(10946.0, 1e-8));
}

TEST_CASE("Sqrt", "[integration]") {
  std::string code = "def mySqrt(n) = \n"
                     "  def average(a,b) = (a+b)/2; end\n"
                     "  const eps = 0.0001;\n"
                     "  let e=1;\n"
                     "  let t=n;\n"
                     "  while |t - e| > eps do\n"
                     "    t = n / e;\n"
                     "    e = average(e, t);\n"
                     "  end\n"
                     "end\n"
                     "const arg = 171;\n"
                     "mySqrt(arg);\n";
  auto result = execute(code, false);
  REQUIRE_THAT(std::get<double>(result), WithinRel(sqrt(171.0), 1e-4));
}

}; // namespace cheasle
