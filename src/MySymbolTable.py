class Symbol:
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class BuiltInTypesSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    def __repr__(self):
        return "<{class_name}(name='{name}')>".format(class_name=self.__class__.__name__,name=self.name)


class VarSymbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return "<{class_name}(name='{name}', type='{type}')>".format(class_name=self.__class__.__name__,name=self.name,type=self.type)

    __repr__ = __str__


class ProcedureSymbol(Symbol):
    def __init__(self, name, params=None):
        super().__init__(name)
        self.params = params if params is not None else []

    def __str__(self):
        return '<{class_name}(name={name}, parameters={params})>'.format(
            class_name=self.__class__.__name__,
            name=self.name,
            params=self.params,
        )

    __repr__ = __str__


class ScopedSymbolTable:
    def __init__(self, scope_name, scope_level, enclosing_scope=None):
        self._symbols = {}
        self.scope_name = scope_name
        self.scope_level = scope_level
        self.enclosing_scope = enclosing_scope
        self._init_builtins()

    def _init_builtins(self):
        self.insert(BuiltInTypesSymbol('INTEGER'))
        self.insert(BuiltInTypesSymbol('REAL'))

    def __str__(self):
        h1 = 'SCOPE (SCOPED SYMBOL TABLE)'
        lines = ['\n', h1, '=' * len(h1)]
        for header_name, header_value in (
            ('Scope name', self.scope_name),
            ('Scope level', self.scope_level),
            ('Enclosing scope',
             self.enclosing_scope.scope_name if self.enclosing_scope else None
            )
        ):
            lines.append('%-15s: %s' % (header_name, header_value))
        h2 = 'Scope (Scoped symbol table) contents'
        lines.extend([h2, '-' * len(h2)])
        lines.extend(
            ('%7s: %r' % (key, value))
            for key, value in self._symbols.items()
        )
        lines.append('\n')
        s = '\n'.join(lines)
        return s

    __repr__ = __str__

    def insert(self, symbol):
        # adds a symbol to the symbol table
        self._symbols[symbol.name] = symbol

    def lookup(self, name, current_scope_only=False):
        # returns the symbol with the passed name, or None if it doesn't exist
        symbol = self._symbols.get(name)
        if symbol is not None:
            return symbol
        if not current_scope_only and self.enclosing_scope is not None:
            return self.enclosing_scope.lookup(name)
        return None
