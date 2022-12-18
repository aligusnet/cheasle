#pragma once

#include <cheasle/ast.h>

namespace cheasle {

class Driver {
public:
  void setAST(AST ast) { _ast = std::move(ast); }

  const AST &getAST() const { return _ast; }

private:
  AST _ast;
};
} // namespace cheasle
