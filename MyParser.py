from MyLexer import *
from MyErrors import *


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


class ProcedureDecl(AST):
    def __init__(self, proc_name, params, block_node):
        self.proc_name = proc_name
        self.params = params
        self.block_node = block_node


class Param(AST):
    def __init__(self, var_node, type_node):
        self.var_node = var_node
        self.type_node = type_node


class ProcedureCall(AST):
    def __init__(self, proc_name, actual_params, token):
        self.proc_name = proc_name
        self.actual_params = actual_params
        self.token = token


class Parser:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def error(self, error_code, token):
        raise ParserError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def validate(self, token_type):
        # compare current token type with passed token type
        # if they match, go to the next token
        # else raise an exception
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.error(error_code=ErrorCode.UNEXPECTED_TOKEN, token=self.current_token)

    def factor(self):
        # factor: PLUS factor | MINUS factor | INTEGER_CONST | REAL_CONST | LPAR expr RPAR | variable
        token = self.current_token
        if token.type == TokenType.PLUS:
            self.validate(TokenType.PLUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.MINUS:
            self.validate(TokenType.MINUS)
            node = UnaryOp(token, self.factor())
            return node
        elif token.type == TokenType.INTEGER_CONST:
            self.validate(TokenType.INTEGER_CONST)
            return Num(token)
        elif token.type == TokenType.REAL_CONST:
            self.validate(TokenType.REAL_CONST)
            return Num(token)
        elif token.type == TokenType.LPAR:
            self.validate(TokenType.LPAR)
            node = self.expr()
            self.validate(TokenType.RPAR)
            return node
        else:
            node = self.variable()
            return node

    def term(self):
        # term : factor ((MULT | INTEGER_DIV | FLOAR_DIV) factor)*
        node = self.factor()

        while self.current_token.type in (TokenType.MUL, TokenType.INTEGER_DIV, TokenType.FLOAT_DIV):
            token = self.current_token
            if token.type == TokenType.MUL:
                self.validate(TokenType.MUL)
            elif token.type == TokenType.INTEGER_DIV:
                self.validate(TokenType.INTEGER_DIV)
            elif token.type == TokenType.FLOAT_DIV:
                self.validate(TokenType.FLOAT_DIV)

            node = BinOp(left=node, op=token, right=self.factor())

        return node

    def expr(self):
        # Parses an arithmetic expression and returns the result
        # expr : term ((PLUS | MINUS) term)*
        # term : factor ((MULT | DIV) factor)*
        # factor : INTEGER
        node = self.term()

        while self.current_token.type in (TokenType.PLUS, TokenType.MINUS):
            token = self.current_token
            if token.type == TokenType.PLUS:
                self.validate(TokenType.PLUS)
            elif token.type == TokenType.MINUS:
                self.validate(TokenType.MINUS)

            node = BinOp(left=node, op=token, right=self.term())

        return node

    def program(self):
        # program: PROGRAM variable SEMI block DOT
        self.validate(TokenType.PROGRAM)
        var_node = self.variable()
        program_name = var_node.value
        self.validate(TokenType.SEMI)
        block_node = self.block()
        self.validate(TokenType.DOT)
        node = Program(program_name, block_node)
        return node

    def compound_statement(self):
        # compound_statement: BEGIN statement_list END
        self.validate(TokenType.BEGIN)
        nodes = self.statement_list()
        self.validate(TokenType.END)

        root = Compound()
        for node in nodes:
            root.children.append(node)

        return root

    def statement_list(self):
        # statement_list: statement | statement SEMI statement_list
        node = self.statement()

        results = [node]

        while self.current_token.type == TokenType.SEMI:
            self.validate(TokenType.SEMI)
            results.append(self.statement())

        if self.current_token.type == TokenType.ID:
            self.error(error_code=ErrorCode.UNEXPECTED_TOKEN, token=self.current_token)

        return results

    def statement(self):
        # statement: compound_statement | assignment_statement | proccall_statement | empty
        if self.current_token.type == TokenType.BEGIN:
            node = self.compound_statement()
        elif self.current_token.type == TokenType.ID:
            if self.lexer.current_character == '(':
                node = self.proccall_statement()
            else:
                node = self.assignment_statement()
        else:
            node = self.empty()
        return node

    def assignment_statement(self):
        # assignment_statement: variable ASSIGN expr
        left = self.variable()
        token = self.current_token
        self.validate(TokenType.ASSIGN)
        right = self.expr()
        node = Assign(left, token, right)
        return node

    def proccall_statement(self):
        # proccall_statement : ID LPAR (expr (COMMA expr)*)? RPAR"
        token = self.current_token

        proc_name = token.value
        self.validate(TokenType.ID)
        self.validate(TokenType.LPAR)

        actual_params = []
        if self.current_token.type != TokenType.RPAR:
            node = self.expr()
            actual_params.append(node)
        while self.current_token.type == TokenType.COMMA:
            self.validate(TokenType.COMMA)
            node = self.expr()
            actual_params.append(node)
        self.validate(TokenType.RPAR)

        node = ProcedureCall(proc_name,actual_params,token)
        return node

    def variable(self):
        # variable: ID
        node = Var(self.current_token)
        self.validate(TokenType.ID)
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
        # declarations : (VAR (variable_declaration SEMI)+)? procedure_declaration
        declarations = []

        while True:
            if self.current_token.type == TokenType.VAR:
                self.validate(TokenType.VAR)
                while self.current_token.type == TokenType.ID:
                    var_decl = self.variable_declaration()
                    declarations.extend(var_decl)
                    self.validate(TokenType.SEMI)

            elif self.current_token.type == TokenType.PROCEDURE:
                proc_decl = self.procedure_declaration()
                declarations.append(proc_decl)
            else:
                break

        return declarations

    def procedure_declaration(self):
        # (PROCEDURE ID (LPAR formal_parameter_list RPAR)? SEMI block SEMI)*
        self.validate(TokenType.PROCEDURE)
        proc_name = self.current_token.value
        self.validate(TokenType.ID)
        params = []

        if self.current_token.type == TokenType.LPAR:
            self.validate(TokenType.LPAR)
            params = self.formal_parameter_list()
            self.validate(TokenType.RPAR)
        self.validate(TokenType.SEMI)
        block_node = self.block()
        proc_decl = ProcedureDecl(proc_name, params, block_node)
        self.validate(TokenType.SEMI)
        return proc_decl

    def variable_declaration(self):
        # variable_declaration: ID (COMMA ID)* COLON type_spec
        var_nodes = [Var(self.current_token)]
        self.validate(TokenType.ID)

        while self.current_token.type == TokenType.COMMA:
            self.validate(TokenType.COMMA)
            var_nodes.append(Var(self.current_token))
            self.validate(TokenType.ID)

        self.validate(TokenType.COLON)

        type_node = self.type_spec()
        var_declarations = [VarDecl(var_node, type_node) for var_node in var_nodes]
        return var_declarations

    def type_spec(self):
        # type_spec: INTEGER | REAL
        token = self.current_token
        if self.current_token.type == TokenType.INTEGER:
            self.validate(TokenType.INTEGER)
        else:
            self.validate(TokenType.REAL)
        node = Type(token)
        return node

    def formal_parameter_list(self):
        # formal_parameter_list : formal_parameters | formal_parameters SEMI formal_parameter_list
        if not self.current_token.type == TokenType.ID:
            return []
        param_nodes = self.formal_parameters()
        while self.current_token.type == TokenType.SEMI:
            self.validate(TokenType.SEMI)
            param_nodes.extend(self.formal_parameters())
        return param_nodes

    def formal_parameters(self):
        # formal_parameters : ID (COMMA ID)* COLON type_spec
        param_nodes = []

        param_tokens = [self.current_token]
        self.validate(TokenType.ID)
        while self.current_token.type == TokenType.COMMA:
            self.validate(TokenType.COMMA)
            param_tokens.append(self.current_token)
            self.validate(TokenType.ID)

        self.validate(TokenType.COLON)
        type_node = self.type_spec()

        for param_token in param_tokens:
            param_node = Param(Var(param_token), type_node)
            param_nodes.append(param_node)

        return param_nodes

    def parse(self):
        node = self.program()
        if self.current_token.type != TokenType.EOF:
            self.error(error_code=ErrorCode.UNEXPECTED_TOKEN, token=self.current_token)
        return node
