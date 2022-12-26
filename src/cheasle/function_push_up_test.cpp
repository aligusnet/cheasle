#include "Catch2/catch_amalgamated.hpp"
#include "cheasle/ast.h"
#include "cheasle/ast_test_util.h"
#include "cheasle/error.h"
#include "cheasle/function_push_up.h"
#include "cheasle/type_checker.h"
#include <vector>

namespace cheasle {
namespace {

const std::string MainName = "main";

ValueType getMainType(const AST &ast) {
  ErrorList errors{};
  auto type = checkTypes(ast, errors);
  if (errors.hasErrors()) {
    std::cout << errors;
  }
  REQUIRE_FALSE(errors.hasErrors());
  return type;
}

AST pushUpFunction(AST ast) {
  ErrorList errors{};
  auto newAst =
      pushUpFunction(MainName, getMainType(ast), std::move(ast), errors);
  if (errors.hasErrors()) {
    std::cout << errors;
  }

  REQUIRE_FALSE(errors.hasErrors());
  REQUIRE(newAst);

  return *newAst;
}

/* Simple test function suitable for AST's without function definitions. */
void pushUpFunctionTest(AST ast) {
  auto expectedAst = TAST::b(TAST::def(MainName, getMainType(ast),
                                       std::vector<FunctionArgument>{}, ast));
  auto actualAst = pushUpFunction(ast);
  REQUIRE_AST(expectedAst, actualAst);
}

} // namespace

TEST_CASE("double + double", "[function-push-up]") {
  auto ast = TAST::add(TAST::constant(10.0), TAST::constant(11.0));
  pushUpFunctionTest(std::move(ast));
}

TEST_CASE("let, const, and binary expressions", "[function-push-up]") {
  auto ast = TAST::b({TAST::let("a", TAST::constant(10.0)),
                      TAST::constexp("b", TAST::constant(11.0)),
                      TAST::div(TAST::ref("b"), TAST::ref("a"))});
  pushUpFunctionTest(std::move(ast));
}

TEST_CASE("complext logical expression", "[function-push-up]") {
  auto lhs = TAST::orexp(TAST::gt(10.0, 11.0), TAST::eq(true, false));
  auto rhs = TAST::orexp(TAST::le(10.0, 11.0), TAST::ne(11.0, 11.0));
  auto ast = TAST::andexp(std::move(lhs), std::move(rhs));
  pushUpFunctionTest(std::move(ast));
}

TEST_CASE("builtin function call", "[function-push-up]") {
  auto arg1 = TAST::orexp(TAST::gt(10.0, 11.0), TAST::eq(true, false));
  auto arg2 = TAST::orexp(TAST::le(10.0, 11.0), TAST::ne(11.0, 11.0));
  auto ast = TAST::builtin(BuiltInFunctionId::Print,
                           std::vector<AST>{std::move(arg1), std::move(arg2)});
}

TEST_CASE("nested user functions", "[function-push-up]") {
  auto add2 = TAST::def("add2", {"a", "b"}, TAST::add("a", "b"));

  auto add3Body = TAST::b(
      {add2,
       TAST::add(TAST::ref("a"),
                 TAST::ufcall("add2", {TAST::ref("b"), TAST::ref("c")}))});
  auto add3 = TAST::def("add3", {"a", "b", "c"}, std::move(add3Body));

  auto ast =
      TAST::b({std::move(add3),
               TAST::ufcall("add3", {TAST::constant(1.0), TAST::constant(2.0),
                                     TAST::constant(3.0)})});

  std::string migledAdd2 = "__add3\%add2";
  std::string mingledAdd3 = "\%add3";
  auto add2Expected = TAST::def(migledAdd2, {"a", "b"}, TAST::add("a", "b"));
  auto add3BodyExpected = TAST::b(
      {TAST::add(TAST::ref("a"),
                 TAST::ufcall(migledAdd2, {TAST::ref("b"), TAST::ref("c")}))});
  auto add3Expected =
      TAST::def(mingledAdd3, {"a", "b", "c"}, std::move(add3BodyExpected));
  auto mainExpected =
      TAST::def(MainName, {},
                TAST::b(TAST::ufcall(mingledAdd3,
                                     {TAST::constant(1.0), TAST::constant(2.0),
                                      TAST::constant(3.0)})));
  auto expected = TAST::b({std::move(add2Expected), std::move(add3Expected),
                           std::move(mainExpected)});

  ErrorList errors{};
  auto actual =
      pushUpFunction(MainName, ValueType::Double, std::move(ast), errors);
  if (errors.hasErrors()) {
    std::cout << errors;
  }

  REQUIRE_FALSE(errors.hasErrors());
  REQUIRE(actual);

  REQUIRE_AST(expected, *actual);
}
} // namespace cheasle
