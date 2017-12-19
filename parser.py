import sys

class ParseError(Exception):
    pass

class ASTNode:
    def __init__(self, tag, **kwargs):
        self.tag = tag
        self.attributes = kwargs
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __repr__(self):
        return "ASTNode(%s, %s)" % (repr(self.tag), repr(self.attributes))

class Parser:
    def __init__(self, remaining):
        self.remaining = remaining
        self.line = 1
        self.stack = []

    @property
    def next(self):
        return self.remaining[0]

    def save(self):
        self.stack.append((self.remaining, self.line))

    def restore(self):
        (self.remaining, self.line) = self.stack.pop()

    def discard(self):
        self.stack.pop()

    def eof(self):
        return len(self.remaining) == 0

    def expect(self, s):
        if not self.remaining.startswith(s):
            raise ParseError()
        else:
            self.advance(len(s))

    def advance(self, n):
        self.line += self.remaining[:n].count('\n')
        self.remaining = self.remaining[n:]

    def skip_ws(self):
        while not self.eof() and self.next in ' \n\t\r':
            self.advance(1)

    def next_is_valid_identifier_char(self):
        next = self.next
        return next.isalpha() or next.isdigit() or next in '_'

    def parse_identifier(self):
        output = ""
        if not self.next.isalpha():
            raise ParseError()
        output += self.next
        self.advance(1)
        while not self.eof() and self.next_is_valid_identifier_char():
            output += self.next
            self.advance(1)
        return output

    def parse_symbol(self):
        output = ""
        while not self.eof() and self.next in "-=><+*":
            output += self.next
            self.advance(1)
        if len(output) == 0:
            raise ParseError()
        return output

    def parse_number(self):
        output = ""
        while not self.eof() and self.next.isdigit():
            output += self.next
            self.advance(1)
        if len(output) == 0:
            raise ParseError()
        return int(output)

    def parse_type_arg_list(self):
        things = []
        while True:
            if self.next == ')':
                self.advance(1)
                return things
            else:
                things.append(self.parse_type())
                self.skip_ws()
                if self.next == ')':
                    self.advance(1)
                    return things
                self.expect(',')
                self.skip_ws()

    def parse_type(self):
        name = self.parse_identifier()
        self.skip_ws()
        output = ASTNode('named_type', name = name)
        if self.next == '(':
            self.advance(1)
            self.skip_ws()
            return \
                ASTNode(
                    'type_application',
                    function = output,
                    args = self.parse_type_arg_list(),
                )
        else:
            return output

    def parse_enum(self):
        name = self.parse_identifier()
        self.skip_ws()
        constructors = []

        while True:
            self.save()
            try:
                constructor_name = self.parse_identifier()
                self.skip_ws()
                self.expect('(')
                self.skip_ws()
                values = self.parse_type_arg_list()
                self.skip_ws()
                self.expect(',')
                self.skip_ws()
            except ParseError:
                self.restore()
                break
            else:
                constructors.append((constructor_name, values))
                self.discard()

        return \
            ASTNode(
                'enum',
                name = name,
                constructors = constructors,
            )

    def parse_struct(self):
        name = self.parse_identifier()
        self.skip_ws()
        fields = []

        while True:
            self.save()
            try:
                field_name = self.parse_identifier()
                self.skip_ws()
                self.expect(':')
                self.skip_ws()
                value = self.parse_type()
                self.skip_ws()
                self.expect(',')
                self.skip_ws()
            except ParseError:
                self.restore()
                break
            else:
                fields.append((field_name, value))
                self.discard()

        return \
            ASTNode(
                'struct',
                name = name,
                fields = fields,
            )

    def parse_term_arg_list(self):
        things = []
        while True:
            if self.next == ')':
                self.advance(1)
                return things
            else:
                things.append(self.parse_expression())
                self.skip_ws()
                if self.next == ')':
                    self.advance(1)
                    return things
                self.expect(',')
                self.skip_ws()

    def parse_bracketed_expression(self):
        self.expect('(')
        self.skip_ws()
        expr = self.parse_expression()
        self.skip_ws()
        self.expect(')')
        return expr

    def parse_expression_0(self):
        if self.next == '(':
            return self.parse_bracketed_expression()
        elif self.next == '\'':
            self.advance(1)
            c = self.next
            self.advance(1)
            self.expect('\'')
            return ASTNode('character_literal', character = c)
        elif self.next == '"':
            self.advance(1)
            st = ""
            while self.next != '"':
                st += self.next
                self.advance(1)
            self.advance(1)
            return ASTNode('string_literal', string = st)
        else:
            self.save()
            try:
                name = self.parse_identifier()
            except ParseError:
                self.restore()
            else:
                self.discard()
                self.skip_ws()
                if name == 'yield':
                    expr = self.parse_expression()
                    return ASTNode('yield_expression', expr = expr)
                else:
                    return ASTNode('variable', name = name)

            n = self.parse_number()
            self.skip_ws()
            return ASTNode('number_literal', n = n)

    def parse_expression_1(self):
        expr = self.parse_expression_0()
        self.skip_ws()
        while not self.eof():
            if self.next == '(':
                self.advance(1)
                args = self.parse_term_arg_list()
                self.skip_ws()
                expr = ASTNode('application', function = expr, args = args)
            elif self.next == '.':
                self.advance(1)
                self.skip_ws()
                field = self.parse_identifier()
                self.skip_ws()
                expr = ASTNode('field_access', x = expr, field = field)
            else:
                break
        return expr

    def parse_expression_2(self):
        expr = self.parse_expression_1()
        self.skip_ws()
        while not self.eof():
            self.save()
            try:
                symbol = self.parse_identifier()
                if symbol == 'as':
                    self.discard()
                    self.skip_ws()
                    type = self.parse_type()
                    expr = ASTNode('cast', expr = expr, type = type)
                else:
                    self.restore()
                    break
            except ParseError:
                self.restore()
                break
        return expr

    def parse_expression_3(self):
        expr = self.parse_expression_2()
        self.skip_ws()
        while not self.eof():
            self.save()
            try:
                symbol = self.parse_symbol()
                if symbol == '-':
                    self.discard()
                    self.skip_ws()
                    other = self.parse_expression_2()
                    expr = ASTNode('-', a = expr, b = other)
                else:
                    self.restore()
                    break
            except ParseError:
                self.restore()
                break
        return expr

    def parse_expression_4(self):
        expr = self.parse_expression_3()
        self.skip_ws()
        while not self.eof():
            self.save()
            try:
                symbol = self.parse_symbol()
                self.skip_ws()
                if symbol == '==':
                    self.discard()
                    other = self.parse_expression_3()
                    self.skip_ws()
                    expr = ASTNode('==', a = expr, b = other)
                else:
                    self.restore()
                    break
            except ParseError:
                self.restore()
                break
        return expr

    def parse_expression(self):
        return self.parse_expression_4()

    def parse_let_statement(self):
        name = self.parse_identifier()
        self.skip_ws()
        if name != 'let':
            raise ParseError()
        name = self.parse_identifier()
        self.skip_ws()
        self.expect('=')
        self.skip_ws()
        expr = self.parse_expression()
        self.expect(';')
        return ASTNode('let_statement', name = name, expr = expr)

    def parse_loop_statement(self):
        name = self.parse_identifier()
        self.skip_ws()
        if name != 'loop':
            raise ParseError()
        self.expect('{')
        self.skip_ws()
        body = self.parse_body()
        return ASTNode('loop_statement', body = body)

    def parse_if_statement(self):
        name = self.parse_identifier()
        self.skip_ws()
        if name != 'if':
            raise ParseError()
        condition = self.parse_expression()
        self.skip_ws()
        self.expect('{')
        self.skip_ws()
        true_side = self.parse_body()
        self.skip_ws()
        name = self.parse_identifier()
        if name != 'else':
            raise ParseError()
        self.skip_ws()
        self.expect('{')
        self.skip_ws()
        false_side = self.parse_body()

        return \
            ASTNode(
                'if_statement',
                condition = condition,
                true_side = true_side,
                false_side = false_side,
            )

    def parse_assignment(self):
        name = self.parse_identifier()
        self.skip_ws()
        symbol = self.parse_symbol()
        if symbol != '=':
            raise ParseError()
        self.skip_ws()
        expr = self.parse_expression()
        self.skip_ws()
        self.expect(';')
        self.skip_ws()
        return ASTNode('assignment', name = name, expr = expr)

    def parse_break(self):
        name = self.parse_identifier()
        if name != 'break':
            raise ParseError()
        self.skip_ws()
        self.expect(';')
        self.skip_ws()
        return ASTNode('break')

    def parse_statement(self):
        self.save()
        try:
            statement = self.parse_let_statement()
        except ParseError:
            self.restore()
        else:
            self.discard()
            return statement

        self.save()
        try:
            statement = self.parse_loop_statement()
        except ParseError:
            self.restore()
        else:
            self.discard()
            return statement

        self.save()
        try:
            statement = self.parse_if_statement()
        except ParseError:
            self.restore()
        else:
            self.discard()
            return statement

        self.save()
        try:
            statement = self.parse_assignment()
        except ParseError:
            self.restore()
        else:
            self.discard()
            return statement

        self.save()
        try:
            statement = self.parse_break()
        except ParseError:
            self.restore()
        else:
            self.discard()
            return statement

        expr = self.parse_expression()
        self.skip_ws()
        self.expect(';')
        return ASTNode('expr_statement', expr = expr)

    def parse_body(self):
        statements = []

        while True:
            self.save()
            try:
                statement = self.parse_statement()
                self.skip_ws()
            except ParseError:
                self.restore()
                break
            else:
                self.discard()
                statements.append(statement)

        self.expect('}')

        return statements

    def parse_function(self):
        name = self.parse_identifier()
        self.skip_ws()
        args = []
        product_type = None
        consume_type = None
        return_type = ASTNode('named_type', name = 'void')
        body = None

        self.save()
        symbol = self.parse_symbol()

        self.skip_ws()

        if symbol == "<-":
            self.discard()
            while True:
                self.save()
                try:
                    arg_name = self.parse_identifier()
                    self.skip_ws()
                    self.expect(':')
                    self.skip_ws()
                    arg_type = self.parse_type()
                    self.skip_ws()
                    self.expect(',')
                    self.skip_ws()
                except ParseError:
                    self.restore()
                    break
                else:
                    args.append((arg_name, arg_type))
                    self.discard()
            self.save()
            try:
                symbol = self.parse_symbol()
            except ParseError:
                symbol = None
            self.skip_ws()

        if symbol == '<=':
            self.discard()
            consume_type = self.parse_type()

            self.save()
            try:
                symbol = self.parse_symbol()
            except ParseError:
                symbol = None
            self.skip_ws()

        if symbol == '=>':
            self.discard()
            product_type = self.parse_type()

            self.save()
            try:
                symbol = self.parse_symbol()
            except ParseError:
                symbol = None
            self.skip_ws()

        if symbol == "->":
            self.discard()
            return_type = self.parse_type()

            self.save()
            try:
                symbol = self.parse_symbol()
            except ParseError:
                symbol = None
            self.skip_ws()

        if symbol != '=':
            raise ParseError('function with no body')

        self.discard()
        self.skip_ws()
        self.expect('{')
        self.skip_ws()

        body = self.parse_body()

        return \
            ASTNode(
                'function',
                name = name,
                args = args,
                product_type = product_type,
                consume_type = consume_type,
                return_type = return_type,
                body = body,
            )

    def parse_extern(self):
        name = self.parse_identifier()
        self.skip_ws()

        arg_types = []
        return_type = ASTNode('void')

        self.save()
        try:
            symbol = self.parse_symbol()
        except ParseError:
            symbol = None

        self.skip_ws()

        if symbol == "<-":
            self.discard()
            while True:
                self.save()
                try:
                    arg_type = self.parse_type()
                    self.skip_ws()
                    self.expect(',')
                    self.skip_ws()
                except ParseError:
                    self.restore()
                    break
                else:
                    arg_types.append(arg_type)
                    self.discard()
            self.save()
            try:
                symbol = self.parse_symbol()
            except ParseError:
                symbol = None
            self.skip_ws()

        if symbol == "->":
            self.discard()
            return_type = self.parse_type()
        else:
            self.restore()

        return \
            ASTNode(
                'extern',
                name = name,
                arg_types = arg_types,
                return_type = return_type,
            )

    def parse_top_level_decl(self):
        type = self.parse_identifier()
        self.skip_ws()
        if type == 'enum':
            return self.parse_enum()
        elif type == 'struct':
            return self.parse_struct()
        elif type == 'define':
            return self.parse_function()
        elif type == 'extern':
            return self.parse_extern()
        else:
            raise ParseError('unknown top-level entity: %s' % type)

    def parse_file(self):
        self.skip_ws()
        decls = []
        while not self.eof():
            decls.append(self.parse_top_level_decl())
            self.skip_ws()
        return decls
