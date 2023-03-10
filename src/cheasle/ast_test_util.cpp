#include "ast_test_util.h"
#include "cheasle/ast.h"

namespace cheasle {
const location TAST::loc{};

void requireAst(const AST &expected, const AST &actual) {
  std::stringstream expectedS;
  if (expected.is<Block>()) {
    expectedS << expected;
  } else {
    expectedS << TAST::b(expected);
  }

  std::stringstream actualS;
  actualS << actual;

  REQUIRE(expectedS.str() == actualS.str());
}
} // namespace cheasle
