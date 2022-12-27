#include "llvm_codegen.h"
#include "cheasle/ast.h"
#include "cheasle/cheasle_jit.h"
#include "cheasle/error.h"
#include "cheasle/function_push_up.h"
#include "cheasle/symbol_table.h"
#include "cheasle/type_checker.h"
#include "cheasle/value.h"
#include "location.h"

#include <ios>
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
#include <stdarg.h>

extern "C" double printd(int count, ...) {
  double val;
  va_list args;
  va_start(args, count);
  for (int i = 0; i < count; ++i) {
    val = va_arg(args, double);
    if (i != 0) {
      std::cout << ' ';
    }
    std::cout << val;
  }
  va_end(args);

  std::cout << std::endl;

  return val;
}

extern "C" bool printb(int count, ...) {
  int val;
  va_list args;
  va_start(args, count);
  for (int i = 0; i < count; ++i) {
    val = va_arg(args, int);
    if (i != 0) {
      std::cout << ' ';
    }
    std::cout << std::boolalpha << (val > 0);
  }
  va_end(args);

  std::cout << std::endl;

  return val > 0;
}

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
    setBuiltinPrintFunction("printd", ValueType::Double);
    setBuiltinPrintFunction("printb", ValueType::Boolean);
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
    error("UnaryExpression not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const EqualityExpression &node) {
    error("EqualityExpression not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const ComparisonExpression &node) {
    error("ComparisonExpression not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const BinaryLogicalExpression &node) {
    error("BinaryLogicalExpression not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const NotExpression &node) {
    error("NotExpression not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const ConstantValue &node) {
    if (std::get_if<bool>(&node.value) != nullptr) {
      return llvm::ConstantInt::getBool(_context, std::get<bool>(node.value));
    } else if (std::get_if<double>(&node.value) != nullptr) {
      return llvm::ConstantFP::get(_context,
                                   llvm::APFloat(std::get<double>(node.value)));
    } else {
      error("Unknown type of ConstantValue", node.location);
    }

    return nullptr;
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
    error("IfExpression not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const WhileExpression &node) {
    error("WhileExpression not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const BuiltInFunction &node) {
    switch (node.id) {
    case BuiltInFunctionId::Sqrt:
      return callFunction("sqrt", node.arguments, node.location);
    case BuiltInFunctionId::Exp:
      return callFunction("exp", node.arguments, node.location);
    case BuiltInFunctionId::Log:
      return callFunction("log", node.arguments, node.location);
    case BuiltInFunctionId::Printd:
      return callPrint("printd", node.arguments, node.location);
    case BuiltInFunctionId::Printb:
      return callPrint("printb", node.arguments, node.location);
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
      funcCodeGen._namedValues.define(std::string(arg.getName()), &arg);
    }
    if (llvm::Value *returnValue = node.code.visit(funcCodeGen)) {
      _builder.CreateRet(returnValue);
      std::string validationMessage;
      llvm::raw_string_ostream oss{validationMessage};
      if (llvm::verifyFunction(*function, &oss)) {
        error("Function <" + node.name +
                  "> failed validation: " + validationMessage,
              node.location);
        return nullptr;
      }
    } else {
      function->eraseFromParent();
    }

    return nullptr;
  }

  llvm::Value *operator()(const AST &, const VariableDefinition &node) {
    error("VariableDefinition not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const AssignmentExpression &node) {
    error("AssignmentExpression not implemented", node.location);
    return nullptr;
  }

  llvm::Value *operator()(const AST &, const NameReference &node) {
    auto optValue = _namedValues.get(node.name);
    if (!optValue || *optValue == nullptr) {
      error("Variable <" + node.name + "> is unknown", node.location);
      return nullptr;
    }
    return *optValue;
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

  void setBuiltinPrintFunction(const std::string &name, ValueType valueType) {
    auto optFunc = _functions.get(name);
    if (!optFunc) {
      std::vector<llvm::Type *> argTypes{llvm::Type::getInt32Ty(_context)};
      auto *functionType = llvm::FunctionType::get(getLlvmType(valueType),
                                                   argTypes, /*isVarArg*/ true);
      auto *function = llvm::Function::Create(
          functionType, llvm::Function::ExternalLinkage, name, _module);
      _functions.define(name, function);
      for (auto &arg : function->args()) {
        arg.setName("count");
      }
    }
  }

  llvm::Value *callPrint(const std::string &name,
                         const std::vector<AST> &arguments, location location) {
    auto optFunction = _functions.get(name);
    if (!optFunction || *optFunction == nullptr) {
      error("Function <" + name + "> is unknown", location);
      return nullptr;
    }

    auto function = *optFunction;
    std::vector<llvm::Value *> argValues{};
    argValues.reserve(arguments.size() + 1);
    argValues.emplace_back(llvm::ConstantInt::getSigned(
        llvm::Type::getInt32Ty(_context), arguments.size()));
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
  SymbolTable<llvm::Value *> _namedValues;
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
