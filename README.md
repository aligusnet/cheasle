# Cheasle

## Examples

```rust
def fibonacci(n: double): double {
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
printd( fibonacci(21) );
```

```rust
def mySqrt(n: double) : double {
  def average(a: double, b: double) : double { (a+b)/2; }
  const eps: double = 0.0001;
  let e: double = 1;
  let t: double = n;
  while |t - e| > eps {
    t = n / e;
    e = average(e, t);
  }
}
const arg: double = 171;
printd( mySqrt(arg) - sqrt(arg) );
```

```rust
def within1(begin: double, end: double, value: double): bool {
  value >= begin and value < end;
}

def within2(begin: double, end: double, value: double): bool {
  not (value < begin or value >= end);
}

const begin: double = 5.0;
const end: double = 10.0;
const val: double = 7.0;

printb( within1(begin, end, val) and within2(begin, end, val) );
```

## Pre requirements

### macOS

```
brew install bison
brew install llvm
export PATH="$(brew --prefix bison)/bin:$PATH"
```
