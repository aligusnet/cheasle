#include "llvm_codegen.h"
#include "cheasle/ast.h"
#include "cheasle/cheasle_jit.h"
#include "cheasle/error.h"
#include "cheasle/function_push_up.h"
#include "cheasle/symbol_table.h"
#include "cheasle/type_checker.h"
#include "cheasle/value.h"
#include "location.h"

#include <llvm/ExecutionEngine/Orc/ThreadSafeModule.h>
#include <llvm/IR/Constant.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/Error.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Target/TargetMachine.h>
#include <memory>
#include <optional>
#include <sstream>

namespace cheasle {
class CodeGenerator {
public:
  CodeGenerator(ErrorList &errors, llvm::LLVMContext &context,
                llvm::Module &module, llvm::IRBuilder<> &builder)
      : _errors(errors), _context(context), _module(module), _builder(builder),
        _namedValues("global"), _functions("global") {
    // Set BuiltIn functions.
    setBuiltinMathFunction("sqrt");
    setBuiltinMathFunction("log");
    setBuiltinMathFunction("exp");
    setBuiltinMathFunction("fabs");
    setBuiltinPrintfFunction();
  }

  llvm::Value *operator()(const AST &, const BinaryExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    if (lhs == nullptr || rhs == nullptr) {
      return nullptr;
    }
    switch (node.op) {
    case BinaryOperator::Add:
      return _builder.CreateFAdd(lhs, rhs, "add");
    case BinaryOperator::Subtract:
      return _builder.CreateFSub(lhs, rhs, "sub");
    case BinaryOperator::Multiply:
      return _builder.CreateFMul(lhs, rhs, "mul");
    case BinaryOperator::Divide:
      return _builder.CreateFDiv(lhs, rhs, "div");
    }
  }

  llvm::Value *operator()(const AST &, const UnaryExpression &node) {
    switch (node.op) {
    case UnaryOperator::Abs:
      return callFunction("fabs", std::vector<AST>{node.child}, node.location);
    case UnaryOperator::Minus: {
      auto val = node.child.visit(*this);
      if (val == nullptr) {
        return nullptr;
      }
      auto minusOne = llvm::ConstantFP::get(_context, llvm::APFloat(-1.0));
      return _builder.CreateFMul(minusOne, val, "minus");
    }
    }
    error("Unknown unary operator", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const EqualityExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    if (lhs == nullptr || rhs == nullptr) {
      return nullptr;
    }

    auto lhsType = lhs->getType();
    auto rhsType = rhs->getType();
    if (lhsType != rhsType) {
      error("Both operands of equality expression are exptected to be the same "
            "type.",
            node.location);
      return nullptr;
    }

    if (lhsType->isFloatingPointTy()) {
      switch (node.op) {
      case EqualityOperator::EQ:
        return _builder.CreateFCmpOEQ(lhs, rhs);
      case EqualityOperator::NE:
        return _builder.CreateFCmpONE(lhs, rhs);
      }
    } else if (lhsType->isIntegerTy()) {
      switch (node.op) {
      case EqualityOperator::EQ:
        return _builder.CreateICmpEQ(lhs, rhs);
      case EqualityOperator::NE:
        return _builder.CreateICmpNE(lhs, rhs);
      }
    }

    error("EqualityExpression for these operands are not supported",
          node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const ComparisonExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    if (lhs == nullptr || rhs == nullptr) {
      return nullptr;
    }

    auto lhsType = lhs->getType();
    auto rhsType = rhs->getType();
    if (lhsType != rhsType || !lhsType->isFloatingPointTy()) {
      error(
          "Both operands of comparison expression are exptected to be the same "
          "type of double.",
          node.location);
      return nullptr;
    }

    switch (node.op) {
    case ComparisonOperator::GE:
      return _builder.CreateFCmpOGE(lhs, rhs);
    case ComparisonOperator::GT:
      return _builder.CreateFCmpOGT(lhs, rhs);
    case ComparisonOperator::LE:
      return _builder.CreateFCmpOLE(lhs, rhs);
    case ComparisonOperator::LT:
      return _builder.CreateFCmpOLT(lhs, rhs);
    }

    error("Unknown comparison expression operator", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const BinaryLogicalExpression &node) {
    auto lhs = node.lhs.visit(*this);
    auto rhs = node.rhs.visit(*this);
    if (lhs == nullptr || rhs == nullptr) {
      return nullptr;
    }

    if (!isBoolean(lhs) || !isBoolean(rhs)) {
      error(
          "Both operands of comparison expression are exptected to be the same "
          "type of bool.",
          node.location);
      return nullptr;
    }

    switch (node.op) {
    case BinaryLogicalOperator::And:
      return _builder.CreateLogicalAnd(lhs, rhs);
    case BinaryLogicalOperator::Or:
      return _builder.CreateLogicalOr(lhs, rhs);
    }

    error("Unknown binary logical operator", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const NotExpression &node) {
    auto child = node.child.visit(*this);
    if (child == nullptr) {
      return nullptr;
    }

    if (!isBoolean(child)) {
      error("Not operator expects a boolean expression", node.location);
    }

    return _builder.CreateNot(child);
  }

  llvm::Value *operator()(const AST &, const ConstantValue &node) {
    switch (node.type) {
    case ValueType::Boolean:
      return llvm::ConstantInt::getBool(_context, std::get<bool>(node.value));
    case ValueType::Double:
      return llvm::ConstantFP::get(_context,
                                   llvm::APFloat(std::get<double>(node.value)));
    case ValueType::String:
      return _builder.CreateGlobalString(std::get<std::string>(node.value), "",
                                         0, &_module);
    case ValueType::Int:
      return llvm::Constant::getIntegerValue(
          getLlvmType(node.type), llvm::APSInt(std::get<int32_t>(node.value)));
    case ValueType::Function:
      error("Cannot create a value of type function", node.location);
      return nullptr;
    case ValueType::Any:
      error("Cannot create a value of type Any", node.location);
      return nullptr;
    }
  }

  llvm::Value *operator()(const AST &, const Block &node) {
    if (node.children.empty()) {
      error("Block of statements cannot be empty", node.location);
      return nullptr;
    }

    llvm::Value *value = nullptr;
    for (const auto &statement : node.children) {
      value = statement.visit(*this);
    }
    return value;
  }

  llvm::Value *operator()(const AST &, const IfExpression &node) {
    auto condition = node.condition.visit(*this);
    if (condition == nullptr) {
      return nullptr;
    }

    if (!isBoolean(condition)) {
      error("Condition expresssion in if should be bool", node.location);
      return nullptr;
    }

    auto function = _builder.GetInsertBlock()->getParent();
    // Else block is inserted into the enf of the function.
    auto thenBlock = llvm::BasicBlock::Create(_context, "then", function);
    // Else and merge blocks are not yet inserted.
    auto elseBlock = llvm::BasicBlock::Create(_context, "else");
    auto mergeBlock = llvm::BasicBlock::Create(_context, "merge");

    _builder.CreateCondBr(condition, thenBlock, elseBlock);

    // Emit then block;
    _builder.SetInsertPoint(thenBlock);
    auto thenValue = node.thenBranch.visit(*this);
    if (thenValue == nullptr) {
      return nullptr;
    }

    _builder.CreateBr(mergeBlock);
    // Update then block pointer in case if codegen chaged the current
    // block.
    thenBlock = _builder.GetInsertBlock();

    // Emit else block.
    function->getBasicBlockList().push_back(elseBlock);
    _builder.SetInsertPoint(elseBlock);
    auto elseValue = node.elseBranch.visit(*this);
    if (elseValue == nullptr) {
      return nullptr;
    }

    _builder.CreateBr(mergeBlock);
    // Update else block pointer in case if codegen chaged the current
    // block.
    elseBlock = _builder.GetInsertBlock();

    if (thenValue->getType() != elseValue->getType()) {
      error("Else and then branches of if expression must be the same type.",
            node.location);
      return nullptr;
    }

    // Emit merge block.
    function->getBasicBlockList().push_back(mergeBlock);
    _builder.SetInsertPoint(mergeBlock);
    auto phiNode = _builder.CreatePHI(thenValue->getType(), 2, "if");
    phiNode->addIncoming(thenValue, thenBlock);
    phiNode->addIncoming(elseValue, elseBlock);
    return phiNode;
  }

  llvm::Value *operator()(const AST &, const WhileExpression &node) {
    auto function = _builder.GetInsertBlock()->getParent();
    auto headerBlock = _builder.GetInsertBlock();

    auto whileBlock = llvm::BasicBlock::Create(_context, "while");
    auto endBlock = llvm::BasicBlock::Create(_context, "end");

    auto condition = node.condition.visit(*this);
    if (condition == nullptr) {
      return nullptr;
    }
    if (!isBoolean(condition)) {
      error("Condition expresssion in while should be bool", node.location);
      return nullptr;
    }
    _builder.CreateCondBr(condition, whileBlock, endBlock);

    // Emit while block.
    function->getBasicBlockList().push_back(whileBlock);
    _builder.SetInsertPoint(whileBlock);
    auto body = node.body.visit(*this);
    if (body == nullptr) {
      return nullptr;
    }
    condition = node.condition.visit(*this);
    _builder.CreateCondBr(condition, whileBlock, endBlock);
    whileBlock = _builder.GetInsertBlock();

    // Emit end block
    function->getBasicBlockList().push_back(endBlock);
    _builder.SetInsertPoint(endBlock);
    auto phiNode = _builder.CreatePHI(body->getType(), 2);
    phiNode->addIncoming(body, whileBlock);
    phiNode->addIncoming(llvm::Constant::getNullValue(body->getType()),
                         headerBlock);
    return phiNode;
  }

  llvm::Value *operator()(const AST &, const BuiltInFunction &node) {
    switch (node.id) {
    case BuiltInFunctionId::Sqrt:
      return callFunction("sqrt", node.arguments, node.location);
    case BuiltInFunctionId::Exp:
      return callFunction("exp", node.arguments, node.location);
    case BuiltInFunctionId::Log:
      return callFunction("log", node.arguments, node.location);
    case BuiltInFunctionId::Printf:
      return callPrintf(node.arguments, node.location);
    }

    error("BuiltInFunction is not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const FunctionCall &node) {
    return callFunction(node.name, node.arguments, node.location);
  }

  llvm::Value *operator()(const AST &, const FunctionDefinition &node) {
    auto *function =
        generateFunctionPrototype(node.name, node.returnType, node.arguments);
    if (function == nullptr) {
      return nullptr;
    }
    _functions.define(node.name, function);

    llvm::BasicBlock *bb =
        llvm::BasicBlock::Create(_context, "function", function);
    _builder.SetInsertPoint(bb);

    CodeGenerator funcCodeGen(*this, node.name);
    for (auto &arg : function->args()) {
      auto alloca = createEntryBlockAlloca(function, std::string(arg.getName()),
                                           arg.getType());
      _builder.CreateStore(&arg, alloca);
      funcCodeGen._namedValues.define(std::string(arg.getName()), alloca);
    }
    if (llvm::Value *returnValue = node.code.visit(funcCodeGen)) {
      _builder.CreateRet(returnValue);
      std::string validationMessage;
      llvm::raw_string_ostream oss{validationMessage};
      if (llvm::verifyFunction(*function, &oss)) {
        error("Function <" + node.name +
                  "> failed validation: " + validationMessage,
              node.location);
        std::cerr << getInstructions(function) << std::endl;
        return nullptr;
      }
    } else {
      function->eraseFromParent();
    }

    return nullptr;
  }

  llvm::Value *operator()(const AST &, const VariableDefinition &node) {
    if (_namedValues.isLocallyDefined(node.name)) {
      error("Variable " + node.name +
                " is already defined in the current scope.",
            node.location);
      return nullptr;
    }

    auto value = node.expr.visit(*this);
    if (value == nullptr) {
      return nullptr;
    }

    auto function = _builder.GetInsertBlock()->getParent();
    auto alloca = createEntryBlockAlloca(function, node.name, value->getType());
    _builder.CreateStore(value, alloca);

    _namedValues.define(node.name, alloca);
    return value;
  }

  llvm::Value *operator()(const AST &, const AssignmentExpression &node) {
    auto value = node.expr.visit(*this);
    if (value == nullptr) {
      return nullptr;
    }

    auto variable = _namedValues.get(node.name);
    if (!variable) {
      error("Variable <" + node.name + "> is unknown", node.location);
      return nullptr;
    }

    _builder.CreateStore(value, *variable);
    return value;
  }

  llvm::Value *operator()(const AST &, const NameReference &node) {
    auto optValue = _namedValues.get(node.name);
    if (!optValue || *optValue == nullptr) {
      error("Variable <" + node.name + "> is unknown", node.location);
      return nullptr;
    }
    return _builder.CreateLoad((*optValue)->getAllocatedType(), *optValue,
                               node.name);
  }

private:
  CodeGenerator(const CodeGenerator &parent) = delete;
  CodeGenerator(CodeGenerator &&parent) = delete;

  CodeGenerator(CodeGenerator &parent, const std::string &scopeName)
      : _errors(parent._errors), _context(parent._context),
        _module(parent._module), _builder(parent._builder),
        _namedValues(scopeName, &parent._namedValues),
        _functions(scopeName, &parent._functions) {}

  llvm::Type *getLlvmType(ValueType type) {
    switch (type) {
    case ValueType::Boolean:
      return llvm::Type::getInt1Ty(_context);
    case ValueType::Double:
      return llvm::Type::getDoubleTy(_context);
    case ValueType::Any:
      error("Unsupported type Any", location());
      return nullptr;
    case ValueType::Function:
      error("Unsupported type Function", location());
      return nullptr;
    case ValueType::String:
      return llvm::Type::getInt8PtrTy(_context);
    case ValueType::Int:
      return llvm::Type::getInt32Ty(_context);
    }
  }

  llvm::Function *
  generateFunctionPrototype(const std::string &name, ValueType returnType,
                            const std::vector<FunctionArgument> &args) {
    std::vector<llvm::Type *> argTypes{};
    argTypes.reserve(args.size());
    for (const auto &arg : args) {
      argTypes.push_back(getLlvmType(arg.type));
    }

    auto *functionType = llvm::FunctionType::get(getLlvmType(returnType),
                                                 argTypes, /*isVarArg*/ false);
    auto *function = llvm::Function::Create(
        functionType, llvm::Function::ExternalLinkage, name, _module);

    size_t argIndex = 0;
    for (auto &arg : function->args()) {
      arg.setName(args[argIndex++].name);
    }

    return function;
  }

  void setBuiltinMathFunction(const std::string &name) {
    auto optFunc = _functions.get(name);
    if (!optFunc) {
      auto func = generateFunctionPrototype(
          name, ValueType::Double, {FunctionArgument{"x", ValueType::Double}});
      _functions.define(name, func);
    }
  }

  void setBuiltinPrintfFunction() {
    std::string name = "printf";
    auto optFunc = _functions.get(name);
    if (!optFunc) {
      std::vector<llvm::Type *> argTypes{llvm::Type::getInt8PtrTy(_context)};
      auto *functionType = llvm::FunctionType::get(
          llvm::Type::getInt32Ty(_context), argTypes, /*isVarArg*/ true);
      auto *function = llvm::Function::Create(
          functionType, llvm::Function::ExternalLinkage, name, _module);
      _functions.define(name, function);
      for (auto &arg : function->args()) {
        arg.setName("format");
      }
    }
  }

  llvm::Value *callPrintf(const std::vector<AST> &arguments,
                          location location) {
    std::string name = "printf";
    auto optFunction = _functions.get(name);
    if (!optFunction || *optFunction == nullptr) {
      error("Function <" + name + "> is unknown", location);
      return nullptr;
    }

    auto function = *optFunction;
    std::vector<llvm::Value *> argValues{};
    argValues.reserve(arguments.size());
    for (const auto &arg : arguments) {
      argValues.push_back(arg.visit(*this));
      if (argValues.back() == nullptr) {
        return nullptr;
      }
    }

    return _builder.CreateCall(function, std::move(argValues), "call");
  }

  llvm::Value *callFunction(const std::string &name,
                            const std::vector<AST> &arguments,
                            const location &location) {
    auto optFunction = _functions.get(name);
    if (!optFunction || *optFunction == nullptr) {
      error("Function <" + name + "> is unknown", location);
      return nullptr;
    }

    auto function = *optFunction;

    if (function->arg_size() != arguments.size()) {
      error("Incorrect number of arguments passed to function <" + name + ">",
            location);
      return nullptr;
    }

    std::vector<llvm::Value *> argValues{};
    argValues.reserve(arguments.size());
    for (const auto &arg : arguments) {
      argValues.push_back(arg.visit(*this));
      if (argValues.back() == nullptr) {
        return nullptr;
      }
    }
    return _builder.CreateCall(function, std::move(argValues), "call");
  }

  bool isBoolean(const llvm::Value *value) const {
    return value != nullptr && value->getType()->isIntegerTy(1);
  }

  static llvm::AllocaInst *createEntryBlockAlloca(llvm::Function *function,
                                                  const std::string &varName,
                                                  llvm::Type *varType) {
    llvm::IRBuilder<> builder(&function->getEntryBlock(),
                              function->getEntryBlock().begin());
    return builder.CreateAlloca(varType, 0, varName.c_str());
  }

  void error(const std::string &message, location location) {
    _errors.append("[llvm-codegen]", message, location);
  }

  template <typename T> std::string getInstructions(const T *llvmEntity) {
    std::string instructions;
    llvm::raw_string_ostream oss{instructions};
    oss << *llvmEntity;
    return instructions;
  }

  ErrorList &_errors;
  llvm::Module &_module;
  llvm::LLVMContext &_context;
  llvm::IRBuilder<> &_builder;
  SymbolTable<llvm::AllocaInst *> _namedValues;
  SymbolTable<llvm::Function *> _functions;
};

bool llvmError(ErrorList &errors, llvm::Error &&error, std::string message) {
  if (error) {
    llvm::raw_string_ostream oss{message};
    oss << ": ";
    oss << error;
    errors.append("jit llvm", message, location());
    return true;
  }

  return false;
}

template <typename T>
bool llvmError(ErrorList &errors, llvm::Expected<T> &expected,
               std::string message) {
  return llvmError(errors, expected.takeError(), message);
}

template <typename ReturnType>
ReturnType callEntryPoint(llvm::JITTargetAddress address) {
  ReturnType (*entry)() = (ReturnType(*)())(intptr_t)address;
  return entry();
}

Value callEntryPoint(llvm::JITTargetAddress address, ValueType returnType) {
  switch (returnType) {
  case ValueType::Boolean:
    return callEntryPoint<bool>(address);
  case ValueType::Double:
    return callEntryPoint<double>(address);
  case ValueType::Int:
    return callEntryPoint<int32_t>(address);
  case ValueType::String:
    return callEntryPoint<char *>(address);
  case ValueType::Any:
    break;
  case ValueType::Function:
    break;
  }
  return Value();
}

std::optional<Value> compileAndRun(AST ast, ErrorList &errors) {
  const std::string mainFunctionName = "__cheasle_main";
  auto type = checkTypes(ast, errors);
  if (errors.hasErrors()) {
    return std::nullopt;
  }

  auto entry = pushUpFunction(mainFunctionName, type, std::move(ast), errors);
  if (!entry || errors.hasErrors()) {
    return std::nullopt;
  }

  llvm::InitializeNativeTarget();
  llvm::InitializeNativeTargetAsmPrinter();
  llvm::InitializeNativeTargetAsmParser();

  auto jitExpected = CheasleJIT::Create();
  if (llvmError(errors, jitExpected, "Failed to initialize JIT")) {
    return std::nullopt;
  }
  auto jit = std::move(*jitExpected);

  auto context = std::make_unique<llvm::LLVMContext>();
  auto module = std::make_unique<llvm::Module>("Cheasle JIT", *context);
  auto builder = std::make_unique<llvm::IRBuilder<>>(*context);

  module->setDataLayout(jit->getDataLayout());

  CodeGenerator codegen{errors, *context, *module, *builder};
  auto value = entry->visit(codegen);
  if (errors.hasErrors()) {
    return std::nullopt;
  }

  auto resourceTracker = jit->getMainJITDylib().createResourceTracker();

  auto threadSafeModule =
      llvm::orc::ThreadSafeModule(std::move(module), std::move(context));
  auto addModuleError =
      jit->addModule(std::move(threadSafeModule), resourceTracker);
  if (llvmError(errors, std::move(addModuleError),
                "failed to add a module to the JIT")) {
    return std::nullopt;
  }

  auto exprSymbolExptected = jit->lookup(mainFunctionName);
  if (llvmError(errors, exprSymbolExptected,
                "failed to locate the __cheasle_main symbol.")) {
    return std::nullopt;
  }

  auto exprSymbol = std::move(*exprSymbolExptected);
  auto result = callEntryPoint(exprSymbol.getAddress(), type);

  auto rtRemove = resourceTracker->remove();
  llvmError(errors, std::move(rtRemove),
            "failed to delete the anonymous expression module from the JIT");

  return result;
}
} // namespace cheasle
