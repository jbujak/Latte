# Compiler of Latte to x86_64
A project written in Haskell for Compiler Construction university course.
Latte is a subset of Java designed as a training language for CS students.

# Building
Compiler can be built by calling `make`. This creates compiler executable, `latc_x86_64`.

# Project structure
```
/
|--src/      compiler source code
|  |
|  |--bnfc/  Latte grammar in BNFC format
|
|--lib/      rutime library and its source code
```

# Features
* Arithmetic and boolean expressions
* Loops
* Conditionals,
* Functions
* Classes with inheritance and method overriding
