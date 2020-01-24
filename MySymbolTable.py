class Symbol:
    def __init__(self, name, type=None):
        self.name = name
        self.type = type


class BuiltInTypesSymbol(Symbol):
    def __init__(self, name):
        super().__init__(name)

    def __str__(self):
        return self.name

    __repr__ = __str__


class VarSymbol(Symbol):
    def __init__(self, name, type):
        super().__init__(name, type)

    def __str__(self):
        return '<{name}:{type}'.format(name=self.name, type=self.type)

    __repr__ = __str__


class SymbolTable:
    def __init__(self):
        self._symbols = {}
        self._init_builtins()

    def _init_builtins(self):
        self.define(BuiltInTypesSymbol('INTEGER'))
        self.define(BuiltInTypesSymbol('REAL'))

    def __str__(self):
        s = 'Symbols: {symbols}'.format(symbols=[value for value in self._symbols.values()])
        return s

    __repr__ = __str__

    def define(self, symbol):
        # adds a symbol to the symbol table
        self._symbols[symbol.name] = symbol

    def lookup(self, name):
        # returns the symbol with the passed name, or None if it doesn't exist
        symbol = self._symbols.get(name)
        return symbol
