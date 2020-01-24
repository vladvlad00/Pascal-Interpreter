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
