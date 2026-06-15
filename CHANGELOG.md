# Changelog for `crisp`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy (PVP)](https://pvp.haskell.org/).

## [0.1.3.1] - 2026-06-15

### Added

- Add multiple golden test

### Changed

- Move error and result rendering to its own module
- Move `evalSExprs` and `evalExpr` from `Main` module to `Eval` module
- Use custom `consumeResults` instead of `evalSExprs`
- Improve README
- Write the language specification
- Clarify binding values and print values in language specification

### Fixed

- Setup golden tests and add basic primitive test
- Fix REPL prompt in README.md

## [0.1.3.0] - 2026-06-09

### Added

- Add support for multi-line expressions in repl

### Changed

- Keep track automatically of symbol number for unit tests

## [0.1.2.0] - 2026-06-09

### Added

- Add `equal?` primitive

## [0.1.1.0] - 2026-06-09

### Added

- Allow quoted strings

## [0.1.0.0] - 2026-06-09

### Added

- Define EBNF grammar and evaluation semantics in SPEC.md
- Add basic README.md
- Add core token types and basic tokenization logic
- Add mtl dependency
- Implement parentheses and number parsers
- Parse string
- Parse boolean
- Parse symbols
- Parse reserved kewords and quotes
- Implement recursive tokenize loop and runTokenizer entry point
- Include lists error handling
- Add property tokens' position must always advance
- Add a valid expression generator for testing
- Add property lexer can resume tokenization from any valid atom token's starting position
- Add comprehensive unit tests for lexer's error handling
- Define parser and ast structure
- Add helper functions
- Parse atoms
- Parse multi-token expressions
- Add function to generate AST/S-Expression list from tokens
- Add AST to crisp.cabal
- Add additional unit tests
- Implement initial expression evaluator
- Add primitive support, special forms, and ReaderT monad stack
- Add pretty-printing for special symbols for tests
- Add arithmetic and comparison operations for `Number` type
- Add primitives for arithmetic operations
- Add comparison operations
- Add `not` primitive
- Add quote special form
- Add cons primitive
- Add car primitive
- Add cdr primitive
- Add list primitive
- Add null primitive
- Add display primitive
- Implement interactive REPL and pretty-printing
- Add file interpretation capabilities and support multiple expression evaluation in repl

### Changed

- Initial commit
- Migrate to monad transformer stack
- Introduce Parser newtype and Alternative instance
- Return no match instead of invalid number when number is `-`
- Include position tracking in error type
- Derive Ord in Position
- Export error types
- Differentiate floats and inteters during tokenization
- Use `gets` instead of pattern matching to grab token stream in popToken
- Write more concise error message on AST gen fail
- Change name `ProgramGen` to `ProgramHelpers` and add AST pretty printer helper
- Extract location tracking to dedicated module and simplify token type
- Move error handling into its own file
- Move structural validation from Lexer to AST parser
- Switch `containers` to `unordered-containers`
- Lift Located wrapping out of parseAtom branches
- Use default hash with salt for symbol id
- Rename special form related stuff from "primitive" to "special form"
- Rename `Val` to much clearer `EvalResult`
- Make `runTokenizer` use Data.Text and update tests accordingly
- Change how special symbols are treated
- Make `let` BEHAVE more like `let*` from Racket rather than `let`
- Allow arithmetic operations to accept arbitrarily many arguments (fold-like behavior)
- Use case pattern-matching instead of function pattern-matching for built-ins' implementation
- Use case statement for expression pattern-matching in `eval`
- Replace generic evaluation errors with detailed context types
- Thread ASTParserState explicitly through runAST
- Implement EvalCtx for explicit local and global scoping

### Fixed

- Consume closing " on strings
- Catch token position before consuming bool
- Get token position before consume opening `"` while parsing string
- Advance position when parsing quote
- Get position before consuming symbol
- Consume `#` before peeking bool value
- Include digits in symbols after first char
- Setup tests and add test for empty program logic
- Include line advance logic in "property: tokens' position always strictly advance"
- Generate multi-line programs for test cases
- Limit float representation to decimal in generator
- "empty program is just eof" is not a property but a unit test
- Property valid syntax never fails
- Use multi-line programs instead of lists in "property: tokens' position always strictly advance"
- Correct token boundary validation and error reporting
- Assert a minus sign with no digit is a symbol
- Assert minus sign at the end or in between digits of a number is invalid
- Ensure minus sign can be only placed at the beginning of a number
- Avoid including digits at the first two chars in program generator
- Clarity improvement in multi-line program generator
- Separate test and generator into multiple files
- Test round-trip property
- Unit test repeated symbol share the same id
- Make sure repeated symbols share the same id
- Fix type checking error for built-ins' arguments
- Fix division and subtraction logic
- Fix inverted allowZero check
- Fix primitive `cons` logic
- Fix some details

### Removed

- Remove SpecialSymbols
- Remove redundant arg number checking for closure calling

[0.1.3.1]: https://github.com/P1X3R/crisp/compare/0.1.3.0..0.1.3.1
[0.1.3.0]: https://github.com/P1X3R/crisp/compare/0.1.2.0..0.1.3.0
[0.1.2.0]: https://github.com/P1X3R/crisp/compare/0.1.1.0..0.1.2.0
[0.1.1.0]: https://github.com/P1X3R/crisp/compare/0.1.0.0..0.1.1.0
[0.1.0.0]: https://github.com/P1X3R/crisp/tree/0.1.0.0

