#pragma once

#include <cheasle/value.h>
#include <iosfwd>
#include <location.h>
#include <mongodb/operator.h>
#include <mongodb/polyvalue.h>
#include <string>
#include <vector>

namespace cheasle {

class BinaryExpression;
class UnaryExpression;
class BinaryLogicalExpression;
class ConstantValue;
class Block;
class IfExpression;
class WhileExpression;
class BuiltInFunction;
class FunctionCall;
class FunctionDefinition;
class VariableDefinition;
class AssignmentExpression;
class NameReference;

using AST = mongodb::PolyValue<
    BinaryExpression, BinaryLogicalExpression, UnaryExpression, ConstantValue,
    Block, IfExpression, WhileExpression, BuiltInFunction, FunctionDefinition,
    FunctionCall, VariableDefinition, AssignmentExpression, NameReference>;

enum class BinaryOperator { Add, Subtract, Multiply, Divide };

class BinaryExpression : public mongodb::OpSpecificArity<AST, 2> {
  using Base = mongodb::OpSpecificArity<AST, 2>;

public:
  BinaryExpression(AST lhs, AST rhs, BinaryOperator op, location location)
      : Base(std::move(lhs), std::move(rhs)), op(op),
        location(std::move(location)) {}

  BinaryOperator op;
  location location;
};

enum class UnaryOperator { Minus, Abs };

class UnaryExpression : public mongodb::OpSpecificArity<AST, 1> {
  using Base = mongodb::OpSpecificArity<AST, 1>;

public:
  UnaryExpression(AST child, UnaryOperator op, location location)
      : Base(std::move(child)), op(op), location(std::move(location)) {}

  UnaryOperator op;
  location location;
};

enum class BinaryLogicalOperator { EQ, NE, GT, GE, LT, LE };

class BinaryLogicalExpression : public mongodb::OpSpecificArity<AST, 2> {
  using Base = mongodb::OpSpecificArity<AST, 2>;

public:
  BinaryLogicalExpression(AST lhs, AST rhs, BinaryLogicalOperator op,
                          location location)
      : Base(std::move(lhs), std::move(rhs)), op(op),
        location(std::move(location)) {}

  BinaryLogicalOperator op;
  location location;
};

class ConstantValue : public mongodb::OpSpecificArity<AST, 0> {
  using Base = mongodb::OpSpecificArity<AST, 0>;

public:
  explicit ConstantValue(Value value, location location)
      : value(std::move(value)), location(std::move(location)) {}

  Value value;
  location location;
};

class Block : public mongodb::OpSpecificDynamicArity<AST, 0> {
  using Base = mongodb::OpSpecificDynamicArity<AST, 0>;

public:
  Block(std::vector<AST> nodes, location location)
      : Base(std::move(nodes)), location(std::move(location)) {}

  location location;
};

class IfExpression : public mongodb::OpSpecificArity<AST, 3> {
  using Base = mongodb::OpSpecificArity<AST, 3>;

public:
  IfExpression(AST condition, AST thenBlock, AST elseBlock, location location)
      : Base(std::move(condition), std::move(thenBlock), std::move(elseBlock)),
        location(std::move(location)) {}

  location location;
};

class WhileExpression : public mongodb::OpSpecificArity<AST, 2> {
  using Base = mongodb::OpSpecificArity<AST, 2>;

public:
  WhileExpression(AST condition, AST block, location location)
      : Base(std::move(condition), std::move(block)),
        location(std::move(location)) {}

  location location;
};

enum class BuiltInFunctionId { Sqrt, Exp, Log, Print };

class BuiltInFunction : public mongodb::OpSpecificDynamicArity<AST, 0> {
  using Base = mongodb::OpSpecificDynamicArity<AST, 0>;

public:
  BuiltInFunction(BuiltInFunctionId id, std::vector<AST> arguments,
                  location location)
      : Base(std::move(arguments)), id(id), location(std::move(location)) {}

  location location;

  BuiltInFunctionId id;
};

struct FunctionArgument {
  std::string name;
  ValueType type;
};

class FunctionDefinition : public mongodb::OpSpecificArity<AST, 1> {
  using Base = mongodb::OpSpecificArity<AST, 1>;

public:
  FunctionDefinition(std::string name, ValueType returnType, AST code,
                     std::vector<FunctionArgument> arguments, location location)
      : Base(std::move(code)), name(std::move(name)), returnType(returnType),
        arguments(std::move(arguments)), location(std::move(location)) {}

  std::string name;
  ValueType returnType;
  std::vector<FunctionArgument> arguments;
  location location;
};

class FunctionCall : public mongodb::OpSpecificDynamicArity<AST, 0> {
  using Base = mongodb::OpSpecificDynamicArity<AST, 0>;

public:
  FunctionCall(std::string name, std::vector<AST> arguments, location location)
      : Base(std::move(arguments)), name(std::move(name)),
        location(std::move(location)) {}

  std::string name;
  location location;
};

class VariableDefinition : public mongodb::OpSpecificArity<AST, 1> {
  using Base = mongodb::OpSpecificArity<AST, 1>;

public:
  VariableDefinition(std::string name, ValueType type, bool isConstant,
                     AST expr, location location)
      : Base(std::move(expr)), name(std::move(name)), type(type),
        isConstant(isConstant), location(std::move(location)) {}

  std::string name;
  ValueType type;
  bool isConstant;
  location location;
};

class AssignmentExpression : public mongodb::OpSpecificArity<AST, 1> {
  using Base = mongodb::OpSpecificArity<AST, 1>;

public:
  AssignmentExpression(std::string name, AST expr, location location)
      : Base(std::move(expr)), name(std::move(name)),
        location(std::move(location)) {}

  std::string name;
  location location;
};

class NameReference : public mongodb::OpSpecificArity<AST, 0> {
public:
  NameReference(std::string name, location location)
      : name(std::move(name)), location(std::move(location)) {}

  std::string name;
  location location;
};

std::ostream &operator<<(std::ostream &os, BinaryOperator op);
std::ostream &operator<<(std::ostream &os, UnaryOperator op);
std::ostream &operator<<(std::ostream &os, BinaryLogicalOperator op);
std::ostream &operator<<(std::ostream &os, BuiltInFunctionId id);
std::ostream &operator<<(std::ostream &os, const AST &ast);

} // namespace cheasle
