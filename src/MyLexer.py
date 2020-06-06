from enum import  Enum

class TokenType(Enum):
    # single-character token types
    PLUS = '+'
    MINUS = '-'
    MUL = '*'
    FLOAT_DIV = '/'
    LPAR = '('
    RPAR = ')'
    SEMI = ';'
    DOT = '.'
    COLON = ':'
    COMMA = ','
    EQUAL = '='
    LESS = '<'
    GREATER = '>'
    # block of reserved words
    PROGRAM = 'PROGRAM'  # marks the beginning of the block
    INTEGER = 'INTEGER'
    REAL = 'REAL'
    INTEGER_DIV = 'DIV'
    VAR = 'VAR'
    PROCEDURE = 'PROCEDURE'
    AND = 'AND'
    OR = 'OR'
    IF = 'IF'
    THEN = 'THEN'
    ELSE = 'ELSE'
    WHILE = 'WHILE'
    DO = 'DO'
    FOR = 'FOR'
    TO = 'TO'
    DOWNTO = 'DOWNTO'
    BEGIN = 'BEGIN'
    END = 'END'  # marks the end of the block
    # misc
    ID = 'ID'
    INTEGER_CONST = 'INTEGER_CONST'
    REAL_CONST = 'REAL_CONST'
    ASSIGN = ':='
    NOT_EQUAL = '<>'
    LESS_EQUAL = '<='
    GREATER_EQUAL = '>='
    EOF = 'EOF'


class Token:
    def __init__(self, type, value, line=None, column=None):
        self.type = type
        self.value = value
        # token position
        self.line = line
        self.column = column

    def __str__(self):
        """ string representation of class instance

         Example:  Token(INTEGER, 3)
                   Token(PLUS, '+')
        """
        return 'Token({type}, {value}, position={line}:{column})'.format(
            type=self.type,
            value=repr(self.value),
            line=self.line,
            column=self.column
        )

    def __repr__(self):
        return self.__str__()


def _build_reserved_keywords():
    """Build a dictionary of reserved keywords.

    The function relies on the fact that in the TokenType
    enumeration the beginning of the block of reserved keywords is
    marked with PROGRAM and the end of the block is marked with
    the END keyword.
    """
    # enumerations support iteration, in definition order
    tt_list = list(TokenType)
    start_index = tt_list.index(TokenType.PROGRAM)
    end_index = tt_list.index(TokenType.END)
    reserved_keywords = {
        token_type.value: token_type
        for token_type in tt_list[start_index:end_index + 1]
    }
    return reserved_keywords


RESERVED_KEYWORDS = _build_reserved_keywords()


class Lexer:
    def __init__(self, text):
        # string to lex
        self.text = text
        # index of the character in text
        self.pos = 0
        self.current_character = self.text[self.pos]
        # token line and column number
        self.line = 1
        self.column = 1

    def error(self):
        from MyErrors import LexerError
        s = "Lexer error on '{char}' line: '{line}' column: '{column}'".format(
            char=self.current_character,
            line=self.line,
            column=self.column
        )
        raise LexerError(message=s)

    def advance(self):
        # advance pos and set current_character and update line and column position
        if self.current_character == '\n':
            self.line += 1
            self.column = 0

        self.pos += 1
        if self.pos > len(self.text) - 1:
            self.current_character = None  # EOF
        else:
            self.current_character = self.text[self.pos]
            self.column += 1

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

        token = Token(type=None, value=None, line=self.line, column=self.column)

        if self.current_character == '.':
            result += self.current_character
            self.advance()

            while self.current_character is not None and self.current_character.isdigit():
                result += self.current_character
                self.advance()

            token.type = TokenType.REAL_CONST
            token.value = float(result)
        else:

            token.type = TokenType.INTEGER_CONST
            token.value = int(result)

        return token

    def _id(self):
        # handles identifiers and reserved keywords
        result = ''
        while self.current_character is not None and self.current_character.isalnum():
            result += self.current_character
            self.advance()

        token = Token(type=None, value=None, line=self.line, column=self.column)
        token_type = RESERVED_KEYWORDS.get(result.upper())
        if token_type is None:
            token.type = TokenType.ID
            token.value = result
        else:
            token.type = token_type
            token.value = result.upper()
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
                self.advance()
                self.skip_comment()
                continue

            if self.current_character.isalpha():
                return self._id()

            if self.current_character.isdigit():
                return self.number()

            if self.current_character == ':' and self.peek() == '=':
                token = Token(
                    type=TokenType.ASSIGN,
                    value=TokenType.ASSIGN.value,  # ':='
                    line=self.line,
                    column=self.column,
                )
                self.advance()
                self.advance()
                return token

            if self.current_character == '<' and self.peek() == '>':
                token = Token(
                    type=TokenType.NOT_EQUAL,
                    value=TokenType.NOT_EQUAL.value,
                    line=self.line,
                    column=self.column
                )
                self.advance()
                self.advance()
                return token

            if self.current_character == '<' and self.peek() == '=':
                token = Token(
                    type=TokenType.LESS_EQUAL,
                    value=TokenType.LESS_EQUAL.value,
                    line=self.line,
                    column=self.column
                )
                self.advance()
                self.advance()
                return token

            if self.current_character == '>' and self.peek() == '=':
                token = Token(
                    type=TokenType.GREATER_EQUAL,
                    value=TokenType.GREATER_EQUAL.value,
                    line=self.line,
                    column=self.column
                )
                self.advance()
                self.advance()
                return token

            # single-character token
            try:
                # get enum member by value, e.g.
                # TokenType(';') --> TokenType.SEMI
                token_type = TokenType(self.current_character)
            except ValueError:
                # no enum member with value equal to self.current_char
                self.error()
            else:
                # create a token with a single-character lexeme as its value
                token = Token(
                    type=token_type,
                    value=token_type.value,  # e.g. ';', '.', etc
                    line=self.line,
                    column=self.column,
                )
                self.advance()
                return token

        # EOF (end-of-file) token indicates that there is no more
        # input left for lexical analysis
        return Token(type=TokenType.EOF, value=None)
