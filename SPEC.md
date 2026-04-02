# Crisp Language Specification

Crisp is a minimalist, expression-oriented Lisp subset designed for educational exploration of functional programming and interpreter architecture.

## Syntax

```ebnf
(* The Top-Level Structure *)
program    = { expression } ;
expression = atom | list | quoted ;

(* The Core Containers *)
list       = "(" , { expression } , ")" ;
quoted     = "'" , expression ;

(* The Building Blocks (Atoms) *)
atom       = number | boolean | string | symbol ;

(* Atomic Definitions *)
number     = [ "-" ] , digit , { digit } , [ "." , digit , { digit } ] ;
boolean    = "#t" | "#f" ;
string     = '"' , { character - '"' } , '"' ;
symbol     = symbol_char , { symbol_char | digit } ;

(* Character Sets *)
digit       = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
symbol_char = "a"..."z" | "A"..."Z" | "+" | "-" | "*" | "/" | ">" | "<" | "=" | "!" | "?" | "_" ;
whitespace  = " " | "\n" | "\r" | "\t" ;
comment     = ";" , { character - "\n" } , "\n" ;
```

---

## Evaluation Model

Crisp follows Eager Evaluation (Call-by-value). Every expression is reduced to a value before being passed to a function.

### The Evaluation Rules:

1. Atoms: Evaluate to themselves (e.g., 42 -> 42, #t -> #t).
2. Symbols: Evaluated by looking up their value in the current Environment. If the symbol is not defined, an error is returned.
3. Lists:
    * If the list is empty (), it evaluates to an empty list.
    * If the list is non-empty, the first element is treated as a **Procedure**. The remaining elements are evaluated as arguments and passed to that procedure.
4. Quoted Expressions: Any expression preceded by ' is returned literally without evaluation (e.g., '(+ 1 2) evaluates to the list (+ 1 2), not the number 3).

---

## Core Primitives (The "Special Forms")

To make Crisp functional, the interpreter must implement these built-in symbols:

| Symbol   | Syntax                           | Description                                                          |
| -------- | -------------------------------- | -------------------------------------------------------------------- |
| `define` | `(define <sym> <expr>)`          | Binds the value of `expr` to `sym` in the global environment.        |
| `if`     | `(if <test> <conseq> <alt>)`     | Evaluates `conseq` if `test` is not `#f`, otherwise evaluates `alt`. |
| `lambda` | `(lambda (<args>) <body>)`       | Creates an anonymous function (closure) with its own lexical scope.  |
| `let`    | `(let ((<s1> <e1>) ...) <body>)` | Binds symbols locally within the body.                               |
| `quote`  | `(quote <expr>)`                 | Equivalent to `'<expr>`. Prevents evaluation.                        |


---

## Arithmetic & Comparison

Crisp supports standard prefix notation for math:

* **Math:** `+`, `-`, `*`, `/`
* **Logic:** `=`, `>`, `<`, `>=`, `<=`
* **Example:** `(+ 1 (* 2 3))` -> `7`
