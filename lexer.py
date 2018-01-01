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

symbol_chars = '!<>+-=/|'

keywords = {
    'as',
    'break',
    'constant',
    'define',
    'else',
    'enum',
    'extern',
    'if',
    'import',
    'let',
    'loop',
    'match',
    'return',
    'struct',
    'yield',
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
    '&': 'ampersand',
    '*': 'asterisk',
}

class Lexer:
    def __init__(self, text):
        self.remaining = text
        self.line = 1
        self.column = 1
        self.tokens = []

    @property
    def pos(self):
        return (self.line, self.column)

    def eof(self):
        return len(self.remaining) == 0

    @property
    def next(self):
        return self.remaining[0]

    def advance(self, n):
        skipping_over = self.remaining[:n]
        for c in skipping_over:
            if c == '\n':
                self.column = 1
                self.line += 1
            else:
                self.column += 1
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
        pos = self.pos
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
        return Token('identifier', pos = pos, name = output)

    def lex_number(self):
        pos = self.pos
        s = ""
        while not self.eof() and self.next.isdigit():
            s += self.next
            self.advance(1)
        return Token('number', pos = pos, n = int(s))

    def lex_symbol(self):
        pos = self.pos
        s = ""
        while not self.eof() and self.next in symbol_chars:
            s += self.next
            self.advance(1)
        return Token('symbol', pos = pos, symbol = s)

    def lex_string(self):
        pos = self.pos
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
        return Token('string', pos = pos, string = st)

    def lex_char(self):
        pos = self.pos
        self.expect('\'')
        c = self.next
        self.advance(1)
        self.expect('\'')
        return Token('character', pos = pos, c = c)

    def lex(self):
        self.skip_ws()
        while not self.eof():
            pos = self.pos
            if self.next in special_symbols:
                self.tokens.append(Token(special_symbols[self.next], pos = pos))
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
                print(self.next)
                raise NotImplementedError()
            self.skip_ws()
        return self.tokens
