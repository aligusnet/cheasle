#include "cheasle/error.h"
#include <cheasle/ast_eval.h>
#include <iostream>
#include <lexer.h>
#include <parser.h>
#include <string>

void executeProgram(const std::string &program) {
  std::cout << "********************************" << std::endl;
  std::cout << "Building and executing program: " << std::endl;
  std::cout << "--------------------------------" << std::endl;
  std::cout << program;
  std::cout << std::endl;
  std::cout << "--------------------------------" << std::endl;

  cheasle::Lexer lexer(program);
  lexer.filename = "input-string";

  cheasle::Driver driver;
  cheasle::Parser parser{lexer, &driver};
  const auto &ast = driver.getAST();
  if (parser.parse() != 0) {
    std::cerr << "Failed to parse\n";
    std::cerr << driver.getErrors();
  } else if (!ast.empty()) {
    cheasle::ErrorList evalErrors{};
    std::cout << "AST:" << std::endl;
    std::cout << ast << std::endl;
    std::cout << "--------------------------------" << std::endl;
    std::cout << "Execution:" << std::endl;
    auto result = eval(ast, evalErrors);
    if (evalErrors.hasErrors()) {
      std::cerr << evalErrors;
    }

    if (result) {
      std::cout << "--------------------------------" << std::endl;
      std::cout << "Execution result: " << *result << std::endl << std::endl;
    }
  }
}

int main(int argc, char **argv) {
  std::string testProgram =
      "const a = |-12.1|; const b = 9.9;\n"
      "def add(a, b, c) = a + b + -c; end\n"
      "const result = if 2.1 < 4.5\n"
      "  then 10.1 - 11.1; add(print(11.0, a), sqrt(9), b);\n"
      "  else print(4.0);\n"
      "end\n"
      "print(result);\n"
      "print(|-10.01 - 121/11|);";

  std::string sqrtProgram = "def mySqrt(n) = \n"
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
                            "print(mySqrt(arg));\n"
                            "print(sqrt(arg));\n"
                            "print(mySqrt(arg) - sqrt(arg));";

  std::string fibonacciProgram = "def fibonacci(n) = \n"
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
                                 "print( fibonacci(10) );\n"
                                 "print( fibonacci(21) );";

  executeProgram(testProgram);
  executeProgram(sqrtProgram);
  executeProgram(fibonacciProgram);
}
