#pragma once

#include <cheasle/ast.h>
#include <cheasle/value.h>
#include <string>
#include <vector>

namespace cheasle {

struct VariableSymbol {
  ValueType type;
  Value value;
  bool isConstant;
};

struct FunctionSymbol {
  ValueType returnType;
  std::vector<FunctionArgument> arguments;
  AST code;
};
} // namespace cheasle
