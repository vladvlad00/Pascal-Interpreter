from MyParser import *
from MySymbolTable import *
from MyErrors import *
from MyMemory import *


class NodeVisitor:
    def visit(self, node):
        # this method calls the visit function that matches the node's type
        method_name = 'visit_' + type(node).__name__
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node):
        raise Exception('No visit_{} method'.format(type(node).__name__))


class SemanticAnalyzer(NodeVisitor):
    def __init__(self):
        self.current_scope = None

    def error(self, error_code, token):
        raise SemanticError(
            error_code=error_code,
            token=token,
            message=f'{error_code.value} -> {token}',
        )

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_Program(self, node):
        # print("BEGIN SCOPE GLOBAL")
        global_scope = ScopedSymbolTable(scope_name='global',scope_level=1)
        self.current_scope = global_scope
        self.visit(node.block)
        # print(self.current_scope)
        # print("END SCOPE GLOBAL")
        self.current_scope = self.current_scope.enclosing_scope

    def visit_Compound(self, node):
        for child in node.children:
            self.visit(child)

    def visit_NoOp(self, node):
        pass

    def visit_BinOp(self, node):
        self.visit(node.left)
        self.visit(node.right)

    def visit_VarDecl(self, node):
        type_name = node.type_node.value
        type_symbol = self.current_scope.lookup(type_name)

        var_name = node.var_node.value
        var_symbol = VarSymbol(var_name, type_symbol)

        if self.current_scope.lookup(var_name,current_scope_only=True):
            self.error(error_code=ErrorCode.DUPLICATE_ID, token=node.var_node.token)
        self.current_scope.insert(var_symbol)

    def visit_Assign(self, node):
        self.visit(node.right)
        self.visit(node.left)

    def visit_Var(self, node):
        var_name = node.value
        var_symbol = self.current_scope.lookup(var_name)
        if var_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)

    def visit_ProcedureDecl(self, node):
        proc_name = node.proc_name
        proc_symbol = ProcedureSymbol(proc_name)
        self.current_scope.insert(proc_symbol)

        # print("BEGIN SCOPE '%s'" %proc_name)

        procedure_scope = ScopedSymbolTable(
            scope_name=proc_name,
            scope_level=self.current_scope.scope_level+1,
            enclosing_scope=self.current_scope
        )
        self.current_scope = procedure_scope

        for param in node.params:
            param_type = self.current_scope.lookup(param.type_node.value)
            param_name = param.var_node.value
            var_symbol = VarSymbol(param_name, param_type)
            self.current_scope.insert(var_symbol)
            proc_symbol.params.append(var_symbol)

        self.visit(node.block_node)

        # print(self.current_scope)
        # print("END SCOPE '%s'" %proc_name)
        self.current_scope=self.current_scope.enclosing_scope

    def visit_ProcedureCall(self, node):
        proc_name = node.proc_name
        proc_symbol = self.current_scope.lookup(proc_name)
        if proc_symbol is None:
            self.error(error_code=ErrorCode.ID_NOT_FOUND, token=node.token)
        else:
            num_actual_params = len(node.actual_params)
            num_formal_params = len(proc_symbol.params)
            if num_actual_params != num_formal_params:
                self.error(error_code=ErrorCode.INCORRECT_PARAMETERS, token=node.token)

        for param_node in node.actual_params:
            self.visit(param_node)

    def visit_Num(self, node):
        pass

    def visit_UnaryOp(self, node):
        self.visit(node.expr)


class Interpreter(NodeVisitor):
    def __init__(self, tree):
        self.tree = tree
        self.call_stack = CallStack()

    def visit_UnaryOp(self, node):
        op = node.op.type
        if op == TokenType.PLUS:
            return self.visit(node.expr)
        if op == TokenType.MINUS:
            return -self.visit(node.expr)

    def visit_BinOp(self, node):
        if node.op.type == TokenType.PLUS:
            return self.visit(node.left) + self.visit(node.right)
        elif node.op.type == TokenType.MINUS:
            return self.visit(node.left) - self.visit(node.right)
        elif node.op.type == TokenType.MUL:
            return self.visit(node.left) * self.visit(node.right)
        elif node.op.type == TokenType.INTEGER_DIV:
            return self.visit(node.left) // self.visit(node.right)
        elif node.op.type == TokenType.FLOAT_DIV:
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
        var_value = self.visit(node.right)

        self.call_stack.set_ID(var_name, var_value)

    def visit_Var(self, node):
        var_name = node.value
        val = self.call_stack.get_ID(var_name)

        return val

    def visit_Program(self, node):
        program_name = node.name

        ar = ActivationRecord(
            name=program_name,
            type=ARType.PROGRAM,
            nesting_level=1
        )
        self.call_stack.push(ar)
        self.visit(node.block)
        print(self.call_stack)
        self.call_stack.pop()

    def visit_Block(self, node):
        for declaration in node.declarations:
            self.visit(declaration)
        self.visit(node.compound_statement)

    def visit_VarDecl(self, node):
        self.call_stack.set_ID(node.var_node.value,0)

    def visit_Type(self, node):
        pass

    def visit_ProcedureDecl(self, node):
        self.call_stack.set_ID(node.proc_name, node)

    def visit_ProcedureCall(self, node):
        ar = ActivationRecord(
            name=node.proc_name,
            type=ARType.PROCEDURE,
            nesting_level=self.call_stack.top().nesting_level
        )
        proc_decl=self.call_stack.get_ID(node.proc_name)
        formal_params=proc_decl.params
        for i in range(len(node.actual_params)):
            ar[formal_params[i].var_node.value] = self.visit(node.actual_params[i])
        self.call_stack.push(ar)
        self.visit(proc_decl.block_node)
        self.call_stack.pop()

    def interpret(self):
        tree = self.tree
        if tree is None:
            return ''
        return self.visit(tree)


def main():
    fin = open("program.txt","r")
    text = fin.read()

    try:
        lexer = Lexer(text)
        parser = Parser(lexer)
        tree = parser.parse()

        semantic_analyzer = SemanticAnalyzer()
        semantic_analyzer.visit(tree)

        interpreter = Interpreter(tree)
        result = interpreter.interpret()
    except Error as e:
        print(e.message)


if __name__ == '__main__':
    main()
