#include "Catch2/catch_amalgamated.hpp"
#include "ast_test_util.h"
#include "cheasle/error.h"
#include "cheasle/value.h"
#include <cheasle/type_checker.h>

namespace cheasle {

ValueType checkTypes(const AST &ast) {
  ErrorList errors{};
  auto type = checkTypes(ast, errors);
  if (errors.hasErrors()) {
    std::cerr << errors;
  }
  REQUIRE_FALSE(errors.hasErrors());
  return type;
}

ValueType checkTypesWithErrors(const AST &ast) {
  ErrorList errors{};
  auto type = checkTypes(ast, errors);
  REQUIRE(errors.hasErrors());
  return type;
}

TEST_CASE("let and reference", "[type-checker]") {
  SECTION("double") {
    auto let = TAST::let("a", ValueType::Double, TAST::constant(10.0));
    auto ref = TAST::ref("a");
    auto ast = TAST::b({std::move(let), std::move(ref)});
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("bool") {
    auto let = TAST::let("a", ValueType::Boolean, TAST::constant(false));
    auto ref = TAST::ref("a");
    auto ast = TAST::b({std::move(let), std::move(ref)});
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("double = bool") {
    auto let = TAST::let("a", ValueType::Double, TAST::constant(false));
    auto ref = TAST::ref("a");
    auto ast = TAST::b({std::move(let), std::move(ref)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("bool = double") {
    auto let = TAST::let("a", ValueType::Boolean, TAST::constant(10.0));
    auto ref = TAST::ref("a");
    auto ast = TAST::b({std::move(let), std::move(ref)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }
}

TEST_CASE("let and assignment", "[type-checker]") {
  SECTION("double") {
    auto let = TAST::let("a", ValueType::Double, TAST::constant(10.0));
    auto assign = TAST::assig("a", TAST::constant(11.0));
    auto ast = TAST::b({std::move(let), std::move(assign)});
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("bool") {
    auto let = TAST::let("a", ValueType::Boolean, TAST::constant(false));
    auto assign = TAST::assig("a", TAST::constant(true));
    auto ast = TAST::b({std::move(let), std::move(assign)});
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("double = bool") {
    auto let = TAST::let("a", ValueType::Double, TAST::constant(10.0));
    auto assign = TAST::assig("a", TAST::constant(true));
    auto ast = TAST::b({std::move(let), std::move(assign)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }
}

TEST_CASE("const and assignment", "[type-checker]") {
  auto let = TAST::constexp("a", ValueType::Double, TAST::constant(10.0));
  auto assign = TAST::assig("a", TAST::constant(11.0));
  auto ast = TAST::b({std::move(let), std::move(assign)});
  auto type = checkTypesWithErrors(ast);
  REQUIRE(type == ValueType::Double);
}

TEST_CASE("binary expressions", "[type-checker]") {
  SECTION("subtract of doubles") {
    auto ast = TAST::sub(TAST::constant(10.0), TAST::constant(11.0));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("addition of boolean and double") {
    auto ast = TAST::add(TAST::constant(10.0), TAST::constant(true));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }
}

TEST_CASE("unary expressions", "[type-checker]") {
  SECTION("abs(double)") {
    auto ast = TAST::abs(TAST::constant(-11.0));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("-bool") {
    auto ast = TAST::minus(TAST::constant(false));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }
}

TEST_CASE("equality expressions", "[type-checker]") {
  SECTION("bool == double") {
    auto ast = TAST::eq(TAST::constant(false), TAST::constant(11.0));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("bool != bool") {
    auto ast = TAST::ne(TAST::constant(false), TAST::constant(true));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }
}

TEST_CASE("comparison expressions", "[type-checker]") {
  SECTION("double < double") {
    auto ast = TAST::lt(TAST::constant(10.0), TAST::constant(11.0));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("bool >= double") {
    auto ast = TAST::ge(TAST::constant(false), TAST::constant(11.0));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("bool < bool") {
    auto ast = TAST::lt(TAST::constant(false), TAST::constant(true));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }
}

TEST_CASE("binary logical expressions", "[type-checker]") {
  SECTION("bool and bool") {
    auto ast = TAST::andexp(TAST::constant(true), TAST::constant(false));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("double and bool") {
    auto ast = TAST::andexp(TAST::constant(10.0), TAST::constant(false));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("bool and double") {
    auto ast = TAST::andexp(TAST::constant(true), TAST::constant(10.0));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("bool or bool") {
    auto ast = TAST::orexp(TAST::constant(true), TAST::constant(false));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("double or bool") {
    auto ast = TAST::orexp(TAST::constant(10.0), TAST::constant(false));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("bool or double") {
    auto ast = TAST::orexp(TAST::constant(true), TAST::constant(10.0));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }
}

TEST_CASE("not expressions", "[type-checker]") {
  SECTION("not bool") {
    auto ast = TAST::notexp(TAST::constant(false));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }
  SECTION("not double") {
    auto ast = TAST::notexp(TAST::constant(11.0));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }
}

TEST_CASE("block", "[type-checker]") {
  auto ast = TAST::b(
      {TAST::constant(10.0), TAST::constant(11.0), TAST::constant(false)});
  auto type = checkTypes(ast);
  REQUIRE(type == ValueType::Boolean);
}

TEST_CASE("if expression", "[type-checker]") {
  SECTION("if bool then bool else bool") {
    auto ast = TAST::ifexp(TAST::constant(true), TAST::constant(true),
                           TAST::constant(false));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("if bool then double else double") {
    auto ast = TAST::ifexp(TAST::constant(true), TAST::constant(12.0),
                           TAST::constant(11.0));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("if double then double else double") {
    auto ast = TAST::ifexp(TAST::constant(10.0), TAST::constant(12.0),
                           TAST::constant(11.0));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("if bool then double else bool") {
    auto ast = TAST::ifexp(TAST::constant(true), TAST::constant(12.0),
                           TAST::constant(false));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("if bool then bool else double") {
    auto ast = TAST::ifexp(TAST::constant(true), TAST::constant(true),
                           TAST::constant(11.0));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }
}

TEST_CASE("while..do expression", "[type-checker]") {
  SECTION("while bool do double") {
    auto ast = TAST::whileexp(TAST::constant(true), TAST::constant(10.0));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("while bool do bool") {
    auto ast = TAST::whileexp(TAST::constant(true), TAST::constant(false));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("while double do double") {
    auto ast = TAST::whileexp(TAST::constant(5.0), TAST::constant(10.0));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("while double do bool") {
    auto ast = TAST::whileexp(TAST::constant(5.0), TAST::constant(false));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }
}

TEST_CASE("buuiltin functions", "[type-checker]") {
  SECTION("print(double, bool)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Print,
                             {TAST::constant(10.0), TAST::constant(true)});
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("print()") {
    auto ast = TAST::builtin(BuiltInFunctionId::Print, {});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("sqrt(double)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Sqrt, {TAST::constant(10.0)});
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("sqrt(double, double)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Sqrt,
                             {TAST::constant(10.0), TAST::constant(11.0)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("sqrt(bool)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Sqrt, {TAST::constant(false)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("sqrt()") {
    auto ast = TAST::builtin(BuiltInFunctionId::Sqrt, {});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("log(double)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Log, {TAST::constant(10.0)});
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("log(double, double)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Log,
                             {TAST::constant(10.0), TAST::constant(11.0)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("log(bool)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Log, {TAST::constant(false)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("exp(double)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Exp, {TAST::constant(10.0)});
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("exp(double, double)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Exp,
                             {TAST::constant(10.0), TAST::constant(11.0)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("exp(bool)") {
    auto ast = TAST::builtin(BuiltInFunctionId::Exp, {TAST::constant(false)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }
}

TEST_CASE("user function definition", "[type-checker]") {
  /*
  def less(a: double, b: double): bool =
    a < b;
  end
  */
  SECTION("good case") {
    auto ast = TAST::def("less", ValueType::Boolean,
                         {FunctionArgument{"a", ValueType::Double},
                          FunctionArgument{"b", ValueType::Double}},
                         TAST::lt(TAST::ref("a"), TAST::ref("b")));
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("wrong return type") {
    auto ast = TAST::def("less", ValueType::Double,
                         {FunctionArgument{"a", ValueType::Double},
                          FunctionArgument{"b", ValueType::Double}},
                         TAST::lt(TAST::ref("a"), TAST::ref("b")));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Double);
  }

  SECTION("wrong argument type") {
    auto ast = TAST::def("less", ValueType::Boolean,
                         {FunctionArgument{"a", ValueType::Boolean},
                          FunctionArgument{"b", ValueType::Double}},
                         TAST::lt(TAST::ref("a"), TAST::ref("b")));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("missing argument") {
    auto ast = TAST::def("less", ValueType::Boolean,
                         {FunctionArgument{"a", ValueType::Double}},
                         TAST::lt(TAST::ref("a"), TAST::ref("b")));
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }
}

TEST_CASE("user function call", "[type-checker]") {
  /*
  def less(a: double, b: double): bool =
    a < b;
  end
  less(10.0, 10.1);
  */

  SECTION("good case") {
    auto func = TAST::def("less", ValueType::Boolean,
                          {FunctionArgument{"a", ValueType::Double},
                           FunctionArgument{"b", ValueType::Double}},
                          TAST::lt(TAST::ref("a"), TAST::ref("b")));
    auto call =
        TAST::ufcall("less", {TAST::constant(10.0), TAST::constant(10.1)});
    auto ast = TAST::b({std::move(func), std::move(call)});
    auto type = checkTypes(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("wrong type of passed argumentd") {
    auto func = TAST::def("less", ValueType::Boolean,
                          {FunctionArgument{"a", ValueType::Double},
                           FunctionArgument{"b", ValueType::Double}},
                          TAST::lt(TAST::ref("a"), TAST::ref("b")));
    auto call =
        TAST::ufcall("less", {TAST::constant(false), TAST::constant(10.1)});
    auto ast = TAST::b({std::move(func), std::move(call)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }

  SECTION("wrong number of arguments") {
    auto func = TAST::def("less", ValueType::Boolean,
                          {FunctionArgument{"a", ValueType::Double},
                           FunctionArgument{"b", ValueType::Double}},
                          TAST::lt(TAST::ref("a"), TAST::ref("b")));
    auto call = TAST::ufcall("less", {TAST::constant(10.1)});
    auto ast = TAST::b({std::move(func), std::move(call)});
    auto type = checkTypesWithErrors(ast);
    REQUIRE(type == ValueType::Boolean);
  }
}

TEST_CASE("Name reference", "[type-checker]") {
  auto ast = TAST::ref("b");
  auto type = checkTypesWithErrors(ast);
  REQUIRE(type == ValueType::Double);
}

} // namespace cheasle
