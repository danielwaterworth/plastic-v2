class LexError(Exception):
    pass

class Token:
    def __init__(self, tag, **kwargs):
        self.tag = tag
        self.attributes = kwargs
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __repr__(self):
        return "Token(%s, %s)" % (repr(self.tag), repr(self.attributes))

symbol_chars = '<>+-='

keywords = {
    'as',
    'break',
    'define',
    'else',
    'enum',
    'extern',
    'if',
    'let',
    'loop',
    'return',
    'struct',
    'yield',
    'constant',
}

special_symbols = {
    '(': 'open_paren',
    ')': 'close_paren',
    '{': 'open_brace',
    '}': 'close_brace',
    '[': 'open_square',
    ']': 'close_square',
    ',': 'comma',
    '.': 'dot',
    '@': 'at',
    ':': 'colon',
    ';': 'semicolon',
}

class Lexer:
    def __init__(self, text):
        self.remaining = text
        self.line = 1
        self.tokens = []

    def eof(self):
        return len(self.remaining) == 0

    @property
    def next(self):
        return self.remaining[0]

    def advance(self, n):
        self.line += self.remaining[:n].count('\n')
        self.remaining = self.remaining[n:]

    def expect(self, s):
        if not self.remaining.startswith(s):
            raise ParseError()
        else:
            self.advance(len(s))

    def skip_ws(self):
        while True:
            while not self.eof() and self.next in ' \n\t\r':
                self.advance(1)
            if not self.eof() and self.next == '#':
                while not self.eof() and self.next != '\n':
                    self.advance(1)
            else:
                break

    def next_is_valid_identifier_char(self):
        next = self.next
        return next.isalpha() or next.isdigit() or next in '_'

    def lex_identifier(self):
        output = ""
        if not self.next.isalpha():
            raise ParseError()
        output += self.next
        self.advance(1)
        while not self.eof() and self.next_is_valid_identifier_char():
            output += self.next
            self.advance(1)
        if output in keywords:
            return Token('keyword', keyword = output)
        return Token('identifier', name = output)

    def lex_number(self):
        s = ""
        while not self.eof() and self.next.isdigit():
            s += self.next
            self.advance(1)
        return Token('number', n = int(s))

    def lex_symbol(self):
        s = ""
        while not self.eof() and self.next in symbol_chars:
            s += self.next
            self.advance(1)
        return Token('symbol', symbol = s)

    def lex_string(self):
        assert self.next == '"'
        self.advance(1)
        st = ""
        while not self.eof() and self.next != '"':
            if self.next == '\\':
                self.advance(1)
                hex_digits = '0123456789abcdefABCDEF'
                if self.next == '\\':
                    st += '\\'
                    self.advance(1)
                elif self.next == '"':
                    st += '"'
                    self.advance(1)
                elif self.next == 'n':
                    st += '\n'
                    self.advance(1)
                elif self.next == 't':
                    st += '\t'
                    self.advance(1)
                elif self.next == 'r':
                    st += '\r'
                    self.advance(1)
                else:
                    if not self.next in hex_digits:
                        raise ParseError()
                    d = self.next
                    self.advance(1)
                    if not self.next in hex_digits:
                        raise ParseError()
                    d += self.next
                    self.advance(1)
                    st += chr(int(d, 16))
            else:
                st += self.next
                self.advance(1)
        if self.eof():
            raise LexError()
        self.advance(1)
        return Token('string', string = st)

    def lex_char(self):
        self.expect('\'')
        c = self.next
        self.advance(1)
        self.expect('\'')
        return Token('character', c = c)

    def lex(self):
        self.skip_ws()
        while not self.eof():
            if self.next in special_symbols:
                self.tokens.append(Token(special_symbols[self.next]))
                self.advance(1)
            elif self.next == '"':
                self.tokens.append(self.lex_string())
            elif self.next == '\'':
                self.tokens.append(self.lex_char())
            elif self.next in symbol_chars:
                self.tokens.append(self.lex_symbol())
            elif self.next.isdigit():
                self.tokens.append(self.lex_number())
            elif self.next.isalpha():
                self.tokens.append(self.lex_identifier())
            else:
                raise NotImplementedError()
            self.skip_ws()
        return self.tokens
