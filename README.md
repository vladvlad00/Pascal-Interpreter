# Pascal-Interpreter
An AST-based interpreter for a subset of Pascal. The grammar of the language can be found in [grammar.txt](https://github.com/vladvlad00/Pascal-Interpreter/blob/master/grammar.txt)

## How it works
The program's text file is broken down into tokens by the lexer. Then the Parser processes the tokens and builds the AST. Finally, the interpreter traverses the AST, analyzes the code and executes it.
