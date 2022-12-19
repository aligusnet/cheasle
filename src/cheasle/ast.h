#pragma once

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
class Number;
class Block;
class NoOp;
class IfExpression;
class WhileExpression;
class BuiltInFunction;
class FunctionCall;
class FunctionDefinition;
class VariableDefinition;
class AssignmentExpression;
class NameReference;

using AST =
    mongodb::PolyValue<BinaryExpression, BinaryLogicalExpression,
                       UnaryExpression, Number, Block, IfExpression,
                       WhileExpression, BuiltInFunction, FunctionDefinition,
                       FunctionCall, VariableDefinition, AssignmentExpression,
                       NameReference, NoOp>;

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

class Number : public mongodb::OpSpecificArity<AST, 0> {
  using Base = mongodb::OpSpecificArity<AST, 0>;

public:
  explicit Number(double value, location location)
      : value(value), location(std::move(location)) {}

  double value;
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

class FunctionDefinition : public mongodb::OpSpecificArity<AST, 1> {
  using Base = mongodb::OpSpecificArity<AST, 1>;

public:
  FunctionDefinition(const std::string &name, AST code,
                     std::vector<std::string> arguments, location location)
      : Base(std::move(code)), name(name), arguments(std::move(arguments)),
        location(std::move(location)) {}

  std::string name;
  std::vector<std::string> arguments;
  location location;
};

class FunctionCall : public mongodb::OpSpecificDynamicArity<AST, 0> {
  using Base = mongodb::OpSpecificDynamicArity<AST, 0>;

public:
  FunctionCall(const std::string &name, std::vector<AST> arguments,
               location location)
      : Base(std::move(arguments)), name(name), location(std::move(location)) {}

  std::string name;
  location location;
};

class VariableDefinition : public mongodb::OpSpecificArity<AST, 1> {
  using Base = mongodb::OpSpecificArity<AST, 1>;

public:
  VariableDefinition(std::string name, bool isConstant, AST expr,
                     location location)
      : Base(std::move(expr)), name(name), isConstant(isConstant),
        location(std::move(location)) {}

  std::string name;
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
  NameReference(const std::string &name, location location)
      : name(name), location(std::move(location)) {}

  std::string name;
  location location;
};

class NoOp : public mongodb::OpSpecificArity<AST, 0> {
public:
  NoOp() {}
};

std::ostream &operator<<(std::ostream &os, BinaryOperator op);
std::ostream &operator<<(std::ostream &os, UnaryOperator op);
std::ostream &operator<<(std::ostream &os, BinaryLogicalOperator op);
std::ostream &operator<<(std::ostream &os, BuiltInFunctionId id);
std::ostream &operator<<(std::ostream &os, const AST &ast);

} // namespace cheasle
