# Cheasle

Stronly-typed mini language built using RE-flex, bison, and llvm.

## Examples

### Fibonacci Sequence

```python
def fibonacci(n: int): int {
  if n == 0 {
    0;
  } else {
    if n == 1 {
      1;
    } else {
      fibonacci(n - 1) + fibonacci(n - 2);
    }
  }
}
printf("%d\n", fibonacci(21) );
```

### Square root

```python
def mySqrt(n: double) : double {
  def average(a: double, b: double) : double { (a+b)/2; }
  const eps: double = 0.0001;
  let e: double = 1;
  let t: double = n;
  while |t - e| > eps {
    t = n / e;
    e = average(e, t);
  }
  e;
}
const arg: double = 171;
printf("%f\n", mySqrt(arg) - sqrt(arg) );
```

### Logical expressions

```python
def within1(begin: double, end: double, value: double): bool {
  value >= begin and value < end;
}

def within2(begin: double, end: double, value: double): bool {
  not (value < begin or value >= end);
}

const begin: double = 5.0;
const end: double = 10.0;
const val: double = 7.0;

printf("%d\n", within1(begin, end, val) and within2(begin, end, val) );
```

## Short language description

### Data Types

* `bool`: supported in equality, and logical expressions
* `int`: supported in binary, unary, equality, and comparison expressions. Implicitly covertible into double.
* `double`: supported in binary, unary, equality, and comparison expressions.
* `string`: supported in functions only.

### Expressions
* binary: `+`, `-`, `*`, `/`
* unary: `|n|` - abs value, `-` - unary minus
* equality: `==`, `!=`
* comparison: `>`, `<`, `>=`, `<=`
* logical: `and`, `or`, `not`


### Variable declarations

Variable can be declared as mutable using keyword `let`, e.g.:

```python
lex x: int = 10;
```

or constant, using keyword `const`:

```python
const x: int = 10;
```

### Flow control

#### if expression
```python
if boolean-expression {
  expressions;
} else {
  expressions;
}
```
* `if` is an expression, not a statement, it returns value of executed branch, therefore, both branches must return values of the same type.

#### while expression
```python
while boolean-expression {
  expressions;
}
```

* `while` is an expression, not a statement, it returns value of latest executed expression in its body, if its body was never executed it returns a default value of type type of its body.

### Block of expressions

```python
{
  expression1;
  expression2;
  expression3;
}
```

A block of statements is defined using `{` and `}`, expressions in a block are separated by `;`. The return value of a block is its latest executed expression. The latest statement defines a type of a block as well.


### Bultin Functions
* sqrt
* log
* exp
* printf

### User Defined Functions

```python
def functionName(arg1: type, arg2: type, ...) : type {
  function body;
}
```

* no `return` keyword, functions return latest executed instruction,
* function declration is also a statemnt, its type is `function`, however, this type is not allowed to be returned from any other statemnts, like `if`, `while`, etc., therefore, functions are allowed at the beginning and in the middle of an expression block, but are not allowed as the last expression in the block.

## Pre requirements

### macOS

```
brew install bison
brew install llvm
export PATH="$(brew --prefix bison)/bin:$PATH"
```
