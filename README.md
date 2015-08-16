Lua 5.3 parser and pretty-printer. Parsed nodes include source location
and token information to allow all types of types of analysis, including style-checking.

Similar to https://hackage.haskell.org/package/language-lua, but:

- Supports full Lua 5.3 syntax, including bitwise operators
- Simpler code (I hope) with a more declarative parser
- AST annotated with token lists for style-checking analysis

GitHub-hosted haddocks: http://mitchellwrosen.github.io/language-lua/