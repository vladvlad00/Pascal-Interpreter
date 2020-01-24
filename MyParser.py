from MyLexer import *

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