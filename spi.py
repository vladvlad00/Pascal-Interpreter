# Token types
#
# EOF - end-of-file
INTEGER       = 'INTEGER'
REAL          = 'REAL'
INTEGER_CONST = 'INTEGER_CONST'
REAL_CONST    = 'REAL_CONST'
PLUS          = 'PLUS'
MINUS         = 'MINUS'
MUL           = 'MUL'
INTEGER_DIV   = 'INTEGER_DIV'
FLOAT_DIV     = 'FLOAT_DIV'
LPAR          = 'LPAR'
RPAR          = 'RPAR'
ID            = 'ID'
ASSIGN        = 'ASSIGN'
BEGIN         = 'BEGIN'
END           = 'END'
SEMI          = 'SEMI'
DOT           = 'DOT'
PROGRAM       = 'PROGRAM'
VAR           = 'VAR'
COLON         = 'COLON'
COMMA         = 'COMMA'
EOF           = 'EOF'


class Token:
    def __init__(self, type, value):
        # token type: INTEGER, PLUS, MINUS, MUL, DIV, EOF
        self.type = type
        # token value: 0..9, '+', '-', '*', '/', '(', ')' or None
        self.value = value

    def __str__(self):
        """ string representation of class instance

         Example:  Token(INTEGER, 3)
                   Token(PLUS, '+')
        """
        return 'Token({type}, {value})'.format(
            type=self.type,
            value=repr(self.value)
        )

    def __repr__(self):
        return self.__str__()


RESERVED_KEYWORDS = {
    'PROGRAM': Token('PROGRAM', 'PROGRAM'),
    'VAR': Token('VAR', 'VAR'),
    'DIV': Token('INTEGER_DIV', 'DIV'),
    'INTEGER': Token('INTEGER', 'INTEGER'),
    'REAL': Token('REAL', 'REAL'),
    'BEGIN': Token('BEGIN', 'BEGIN'),
    'END': Token('END', 'END'),
}


class Lexer:
    def __init__(self, text):
        # string to lex
        self.text = text
        # index of the character in text
        self.pos = 0
        self.current_character = self.text[self.pos]

    def error(self):
        raise Exception('Error parsing input')

    def advance(self):
        # advance pos and set current_character
        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_character = None  # EOF
        else:
            self.current_character = self.text[self.pos]

    def peek(self):
        # returns the next character in the text without advancing
        peek_pos = self.pos + 1
        if peek_pos > len(self.text) - 1:
            return None
        else:
            return self.text[peek_pos]

    def skip_whitespace(self):
        while self.current_character is not None and self.current_character.isspace():
            self.advance()

    def skip_comment(self):
        while self.current_character is not None and self.current_character != '}':
            self.advance()
        if self.current_character is None:
            self.error()
        self.advance()

    def number(self):
        # returns an integer_const or real_const from the input
        result = ''
        while self.current_character is not None and self.current_character.isdigit():
            result += self.current_character
            self.advance()

        if self.current_character == '.':
            result += self.current_character
            self.advance()

            while self.current_character is not None and self.current_character.isdigit():
                result += self.current_character
                self.advance()

            token = Token(REAL_CONST, float(result))
        else:
            token = Token(INTEGER_CONST, int(result))

        return token

    def _id(self):
        # handles identifiers and reserved keywords
        result = ''
        while self.current_character is not None and self.current_character.isalnum():
            result += self.current_character
            self.advance()

        token = RESERVED_KEYWORDS.get(result, Token(ID, result))
        return token

    def get_next_token(self):
        """
        This metod is responsible for breaking a sentence
        into tokens (one at a time).
        """
        while self.current_character is not None:

            if self.current_character.isspace():
                self.skip_whitespace()
                continue

            if self.current_character == '{':
                self.skip_comment()
                continue

            if self.current_character.isdigit():
                return self.number()

            if self.current_character == '+':
                self.advance()
                return Token(PLUS, '+')

            if self.current_character == '-':
                self.advance()
                return Token(MINUS, '-')

            if self.current_character == '*':
                self.advance()
                return Token(MUL, '*')

            if self.current_character == '/':
                self.advance()
                return Token(FLOAT_DIV, '/')

            if self.current_character == '(':
                self.advance()
                return Token(LPAR, '(')

            if self.current_character == ')':
                self.advance()
                return Token(RPAR, ')')

            if self.current_character.isalpha():
                return self._id()

            if self.current_character == ':' and self.peek() == '=':
                self.advance()
                self.advance()
                return Token(ASSIGN, ':=')

            if self.current_character == ';':
                self.advance()
                return Token(SEMI, ';')

            if self.current_character == '.':
                self.advance()
                return Token(DOT, '.')

            if self.current_character == ':':
                self.advance()
                return Token(COLON, ':')

            if self.current_character == ',':
                self.advance()
                return Token(COMMA, ',')

            self.error()

        return Token(EOF, None)


class AST:
    pass


class UnaryOp(AST):
    def __init__(self, op, expr):
        self.token = self.op = op
        self.expr = expr


class BinOp(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.token = self.op = op
        self.right = right


class Num(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Compound(AST):
    # Represents a 'BEGIN ... END' block
    def __init__(self):
        self.children = []


class Assign(AST):
    def __init__(self, left, op, right):
        self.left = left
        self.op = op
        self.right = right


class Var(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class NoOp(AST):
    # an empty statement
    pass


class Program(AST):
    def __init__(self, name, block):
        self.name = name
        self.block = block


class Block(AST):
    def __init__(self, declarations, compound_statement):
        self.declarations = declarations
        self.compound_statement = compound_statement


class VarDecl(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class Type(AST):
    def __init__(self, token):
        self.token = token
        self.value = token.value


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self):
        raise Exception('Invalid syntax')

    def validate(self, token_type):
        # compare current token type with passed token type
        # if they match, go to the next token
        # else raise an exception
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.lexer.error()

    def factor(self):
        # factor: PLUS factor | MINUS factor | INTEGER_CONST | REAL_CONST | LPAR expr RPAR | variable
        token = self.current_token
        if token.type == PLUS:
            self.validate(PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == MINUS:
            self.validate(MINUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == INTEGER_CONST:
            self.validate(INTEGER_CONST)
            return Num(token)
        elif token.type == REAL_CONST:
            self.validate(REAL_CONST)
            return Num(token)
        elif token.type == LPAR:
            self.validate(LPAR)
            node = self.expr()
            self.validate(RPAR)
            return node
        else:
            node = self.variable()
            return node

    def term(self):
        # term : factor ((MULT | INTEGER_DIV | FLOAR_DIV) factor)*
        node = self.factor()

        while self.current_token.type in (MUL, INTEGER_DIV, FLOAT_DIV):
            token = self.current_token
            if token.type == MUL:
                self.validate(MUL)
            elif token.type == INTEGER_DIV:
                self.validate(INTEGER_DIV)
            elif token.type == FLOAT_DIV:
                self.validate(FLOAT_DIV)

            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def expr(self):
        # Parses an arithmetic expression and returns the result
        # expr : term ((PLUS | MINUS) term)*
        # term : factor ((MULT | DIV) factor)*
        # factor : INTEGER
        node = self.term()

        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token
            if token.type == PLUS:
                self.validate(PLUS)
            elif token.type == MINUS:
                self.validate(MINUS)

            node = BinOp(left=node, op=token, right=self.term())

        return node

    def program(self):
        # program: PROGRAM variable SEMI block DOT
        self.validate(PROGRAM)
        var_node = self.variable()
        program_name = var_node.value
        self.validate(SEMI)
        block_node = self.block()
        self.validate(DOT)
        node = Program(program_name, block_node)
        return node

    def compound_statement(self):
        # compound_statement: BEGIN statement_list END
        self.validate(BEGIN)
        nodes = self.statement_list()
        self.validate(END)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self):
        # statement_list: statement | statement SEMI statement_list
        node = self.statement()

        results = [node]

        while self.current_token.type == SEMI:
            self.validate(SEMI)
            results.append(self.statement())

        if self.current_token.type == ID:
            self.error()

        return results

    def statement(self):
        # statement: compound_statement | assignment_statement | empty
        if self.current_token.type == BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == ID:
            node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def assignment_statement(self):
        # assignment_statement: variable ASSIGN expr
        left = self.variable()
        token = self.current_token
        self.validate(ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        return node

    def variable(self):
        # variable: ID
        node = Var(self.current_token)
        self.validate(ID)
        return node

    def empty(self):
        return NoOp()

    def block(self):
        # block: declarations compound_statement
        declaration_nodes = self.declarations()
        compound_statement_node = self.compound_statement()
        node = Block(declaration_nodes, compound_statement_node)
        return node

    def declarations(self):
        # declarations: VAR (variable_declaration SEMI)+ | empty
        declarations = []
        if self.current_token.type == VAR:
            self.validate(VAR)
            while self.current_token.type == ID:
                var_decl = self.variable_declaration()
                declarations.extend(var_decl)
                self.validate(SEMI)

        return declarations

    def variable_declaration(self):
        # variable_declaration: ID (COMMA ID)* COLON type_spec
        var_nodes = [Var(self.current_token)]
        self.validate(ID)

        while self.current_token.type == COMMA:
            self.validate(COMMA)
            var_nodes.append(Var(self.current_token))
            self.validate(ID)

        self.validate(COLON)

        type_node = self.type_spec()
        var_declarations = [VarDecl(var_node, type_node) for var_node in var_nodes]
        return var_declarations

    def type_spec(self):
        # type_spec: INTEGER | REAL
        token = self.current_token
        if self.current_token.type == INTEGER:
            self.validate(INTEGER)
        else:
            self.validate(REAL)
        node = Type(token)
        return node

    def parse(self):
        node = self.program()
        if self.current_token.type != EOF:
            self.error()
        return node


class NodeVisitor:
    def visit(self, node):
        # this method calls the visit function that matches the node's type
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


class Interpreter(NodeVisitor):

    GLOBAL_SCOPE = {}

    def __init__(self, parser):
        self.parser = parser

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == PLUS:
            return self.visit(node.expr)
        if op == MINUS:
            return -self.visit(node.expr)

    def visit_BinOp(self, node):
        if node.op.type == PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == INTEGER_DIV:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == FLOAT_DIV:
            return float(self.visit(node.left)) / float(self.visit(node.right))

    def visit_Num(self, node):
        return node.value

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node):
        pass

    def visit_Assign(self, node):
        var_name = node.left.value
        self.GLOBAL_SCOPE[var_name] = self.visit(node.right)

    def visit_Var(self, node):
        var_name = node.value
        val = self.GLOBAL_SCOPE.get(var_name)
        if val is None:
            raise NameError(repr(var_name))
        else:
            return val

    def visit_Program(self, node):
        self.visit(node.block)

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        pass

    def visit_Type(self, node):
        pass

    def interpret(self):
        tree = self.parser.parse()
        return self.visit(tree)


def main():
    fin = open("program.txt","r")
    text = fin.read()
    lexer = Lexer(text)
    parser = Parser(lexer)
    interpreter = Interpreter(parser)
    result = interpreter.interpret()
    for variable in interpreter.GLOBAL_SCOPE:
        print('{var} = {value}'.format(var=variable, value=interpreter.GLOBAL_SCOPE[variable]))


if __name__ == '__main__':
    main()
