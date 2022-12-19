#pragma once

#include <cheasle/ast.h>
#include <cheasle/error.h>

namespace cheasle {

class Driver {
public:
  void setAST(AST ast) { _ast = std::move(ast); }

  const AST &getAST() const { return _ast; }

  ErrorList &getErrors() { return _errors; }

private:
  AST _ast;
  ErrorList _errors;
};
} // namespace cheasle
