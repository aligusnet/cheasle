#pragma once

#include <cheasle/value.h>
#include <iosfwd>
#include <location.h>
#include <mongodb/polyvalue.h>
#include <string>
#include <vector>

namespace cheasle {

struct BinaryExpression;
struct UnaryExpression;
struct EqualityExpression;
struct ComparisonExpression;
struct BinaryLogicalExpression;
struct NotExpression;
struct ConstantValue;
struct Block;
struct IfExpression;
struct WhileExpression;
struct BuiltInFunction;
struct FunctionCall;
struct FunctionDefinition;
struct VariableDefinition;
struct AssignmentExpression;
struct NameReference;

using AST = mongodb::PolyValue<
    BinaryExpression, UnaryExpression, EqualityExpression, ComparisonExpression,
    BinaryLogicalExpression, NotExpression, ConstantValue, Block, IfExpression,
    WhileExpression, BuiltInFunction, FunctionDefinition, FunctionCall,
    VariableDefinition, AssignmentExpression, NameReference>;

enum class BinaryOperator { Add, Subtract, Multiply, Divide };

struct BinaryExpression {
  BinaryExpression(AST lhs, AST rhs, BinaryOperator op, location location)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(op), type(ValueType::Any),
        location(std::move(location)) {}

  AST lhs;
  AST rhs;
  BinaryOperator op;
  ValueType type;
  location location;
};

enum class UnaryOperator { Minus, Abs };

struct UnaryExpression {
  UnaryExpression(AST child, UnaryOperator op, location location)
      : child(std::move(child)), op(op), type(ValueType::Any),
        location(std::move(location)) {}

  AST child;
  UnaryOperator op;
  ValueType type;
  location location;
};

enum class EqualityOperator { EQ, NE };

struct EqualityExpression {
  EqualityExpression(AST lhs, AST rhs, EqualityOperator op, location location)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(op), type(ValueType::Any),
        location(std::move(location)) {}

  AST lhs;
  AST rhs;
  EqualityOperator op;
  ValueType type;
  location location;
};

enum class ComparisonOperator { GT, GE, LT, LE };

struct ComparisonExpression {
  ComparisonExpression(AST lhs, AST rhs, ComparisonOperator op,
                       location location)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(op), type(ValueType::Any),
        location(std::move(location)) {}

  AST lhs;
  AST rhs;
  ComparisonOperator op;
  ValueType type;
  location location;
};

enum class BinaryLogicalOperator { And, Or };

struct BinaryLogicalExpression {
  BinaryLogicalExpression(AST lhs, AST rhs, BinaryLogicalOperator op,
                          location location)
      : lhs(std::move(lhs)), rhs(std::move(rhs)), op(op), type(ValueType::Any),
        location(std::move(location)) {}

  AST lhs;
  AST rhs;
  BinaryLogicalOperator op;
  ValueType type;
  location location;
};

struct NotExpression {
  NotExpression(AST child, location location)
      : child(std::move(child)), type(ValueType::Any),
        location(std::move(location)) {}

  AST child;
  ValueType type;
  location location;
};

struct ConstantValue {
  ConstantValue(Value value, location location)
      : value(std::move(value)), type(ValueType::Any),
        location(std::move(location)) {}

  Value value;
  ValueType type;
  location location;
};

struct Block {
  Block(std::vector<AST> children, location location)
      : children(std::move(children)), location(std::move(location)) {}

  std::vector<AST> children;
  ValueType type;
  location location;
};

struct IfExpression {
  IfExpression(AST condition, AST thenBranch, AST elseBranch, location location)
      : condition(std::move(condition)), thenBranch(std::move(thenBranch)),
        elseBranch(std::move(elseBranch)), type(ValueType::Any),
        location(std::move(location)) {}

  AST condition;
  AST thenBranch;
  AST elseBranch;
  ValueType type;
  location location;
};

struct WhileExpression {
  WhileExpression(AST condition, AST body, location location)
      : condition(std::move(condition)), body(std::move(body)),
        type(ValueType::Any), location(std::move(location)) {}

  AST condition;
  AST body;
  ValueType type;
  location location;
};

enum class BuiltInFunctionId { Sqrt, Exp, Log, Printf };

struct BuiltInFunction {
  BuiltInFunction(BuiltInFunctionId id, std::vector<AST> arguments,
                  location location)
      : arguments(std::move(arguments)), id(id), type(ValueType::Any),
        location(std::move(location)) {}

  std::vector<AST> arguments;
  BuiltInFunctionId id;
  ValueType type;
  location location;
};

struct FunctionArgument {
  std::string name;
  ValueType type;
};

struct FunctionDefinition {
  FunctionDefinition(std::string name, ValueType returnType, AST code,
                     std::vector<FunctionArgument> arguments, location location)
      : code(std::move(code)), name(std::move(name)), returnType(returnType),
        arguments(std::move(arguments)), type(ValueType::Function),
        location(std::move(location)) {}

  AST code;
  std::string name;
  ValueType returnType;
  std::vector<FunctionArgument> arguments;
  ValueType type;
  location location;
};

struct FunctionCall {
  FunctionCall(std::string name, std::vector<AST> arguments, location location)
      : arguments(std::move(arguments)), name(std::move(name)),
        type(ValueType::Any), location(std::move(location)) {}

  std::vector<AST> arguments;
  std::string name;
  ValueType type;
  location location;
};

struct VariableDefinition {
  VariableDefinition(std::string name, ValueType type, bool isConstant,
                     AST expr, location location)
      : expr(std::move(expr)), name(std::move(name)), type(type),
        isConstant(isConstant), location(std::move(location)) {}

  AST expr;
  std::string name;
  ValueType type;
  bool isConstant;
  location location;
};

struct AssignmentExpression {
  AssignmentExpression(std::string name, AST expr, location location)
      : expr(std::move(expr)), name(std::move(name)), type(ValueType::Any),
        location(std::move(location)) {}

  AST expr;
  std::string name;
  ValueType type;
  location location;
};

struct NameReference {
  NameReference(std::string name, location location)
      : name(std::move(name)), type(ValueType::Any),
        location(std::move(location)) {}

  std::string name;
  ValueType type;
  location location;
};

ValueType getType(const AST &ast);

std::ostream &operator<<(std::ostream &os, BinaryOperator op);
std::ostream &operator<<(std::ostream &os, UnaryOperator op);
std::ostream &operator<<(std::ostream &os, EqualityOperator op);
std::ostream &operator<<(std::ostream &os, ComparisonOperator op);
std::ostream &operator<<(std::ostream &os, BinaryLogicalOperator op);
std::ostream &operator<<(std::ostream &os, BuiltInFunctionId id);
std::ostream &operator<<(std::ostream &os, const AST &ast);
} // namespace cheasle
