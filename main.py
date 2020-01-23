# Token types
#
# EOF - end-of-file
INTEGER, PLUS, MINUS, MUL, DIV, LPAR, RPAR, EOF = 'INTEGER', 'PLUS', 'MINUS', 'MUL', 'DIV', 'LPAR', 'RPAR', "EOF"


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

    def skip_whitespace(self):
        while self.current_character is not None and self.current_character.isspace():
            self.advance()

    def integer(self):
        # returns an integer from the input
        result = ''
        while self.current_character is not None and self.current_character.isdigit():
            result += self.current_character
            self.advance()
        return int(result)

    def get_next_token(self):
        """
        This metod is responsible for breaking a sentence
        into tokens (one at a time).
        """
        while self.current_character is not None:

            if self.current_character.isspace():
                self.skip_whitespace()
                continue

            if self.current_character.isdigit():
                return Token(INTEGER, self.integer())

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
                return Token(DIV, '/')

            if self.current_character == '(':
                self.advance()
                return Token(LPAR,'(')

            if self.current_character == ')':
                self.advance()
                return Token(RPAR,')')

            self.error()

        return Token(EOF, None)


class Interpreter:
    def __init__(self, lexer):
        self.lexer = lexer
        self.current_token = self.lexer.get_next_token()

    def validate(self, token_type):
        # compare current token type with passed token type
        # if they match, go to the next token
        # else raise an exception
        if self.current_token.type == token_type:
            self.current_token = self.lexer.get_next_token()
        else:
            self.lexer.error()

    def factor(self):
        # factor : INTEGER | LPAR expr RPAR
        token = self.current_token
        if token.type == INTEGER:
            self.validate(INTEGER)
            return token.value
        elif token.type == LPAR:
            self.validate(LPAR)
            result = self.expr()
            self.validate(RPAR)
            return result

    def term(self):
        # term : factor ((MULT | DIV) factor)*
        result = self.factor()

        while self.current_token.type in (MUL, DIV):
            token = self.current_token
            if token.type == MUL:
                self.validate(MUL)
                result *= self.factor()
            elif token.type == DIV:
                self.validate(DIV)
                result //= self.factor()

        return result

    def expr(self):
        # Parses an arithmetic expression and returns the result
        # expr : term ((PLUS | MINUS) term)*
        # term : factor ((MULT | DIV) factor)*
        # factor : INTEGER
        result = self.term()

        while self.current_token.type in (PLUS, MINUS):
            token = self.current_token
            if token.type == PLUS:
                self.validate(PLUS)
                result += self.term()
            elif token.type == MINUS:
                self.validate(MINUS)
                result -= self.term()

        return result


def main():
    while True:
        try:
            text = input('calc> ')
        except EOFError:
            break
        if not text:
            continue
        lexer = Lexer(text)
        interpreter = Interpreter(lexer)
        result = interpreter.expr()
        print(result)


if __name__ == '__main__':
    main()
