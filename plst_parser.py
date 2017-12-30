from constants import *

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
    def __init__(self, tokens):
        self.tokens = tokens
        self.index = 0
        self.stack = []

    @property
    def next(self):
        return self.tokens[self.index]

    def advance(self):
        self.index += 1
        assert self.index <= len(self.tokens)

    def save(self):
        self.stack.append(self.index)

    def restore(self):
        self.index = self.stack.pop()

    def discard(self):
        self.stack.pop()

    def eof(self):
        return self.index == len(self.tokens)

    def expect(self, tag):
        token = self.next
        if self.next.tag != tag:
            raise ParseError()
        self.advance()
        return token

    def expect_symbol(self, symbol):
        if self.expect('symbol').symbol != symbol:
            raise ParseError()

    def expect_keyword(self, keyword):
        if self.expect('keyword').keyword != keyword:
            raise ParseError()

    def parse_type_arg_list(self):
        things = []
        while True:
            if self.next.tag == 'close_paren':
                self.advance()
                return things
            else:
                things.append(self.parse_type())
                if self.next.tag == 'close_paren':
                    self.advance()
                    return things
                self.expect('comma')

    def parse_keyword(self):
        return self.expect('keyword').keyword

    def parse_identifier(self):
        return self.expect('identifier').name

    def parse_number(self):
        return self.expect('number').n

    def parse_symbol(self):
        return self.expect('symbol').symbol

    def parse_type(self):
        if self.next.tag == 'identifier':
            name = self.parse_identifier()
            output = ASTNode('named_type', name = name)
            if self.next.tag == 'open_paren':
                self.advance()
                return \
                    ASTNode(
                        'type_application',
                        function = output,
                        args = self.parse_type_arg_list(),
                    )
            else:
                return output
        elif self.next.tag == 'number':
            n = self.parse_number()
            return \
                ASTNode(
                    'type_number',
                    n = n,
                )
        else:
            raise ParseError()

    def parse_enum(self):
        name = self.parse_identifier()
        constructors = []

        while True:
            self.save()
            try:
                constructor_name = self.parse_identifier()
                self.expect('open_paren')
                values = self.parse_type_arg_list()
                self.expect('comma')
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
        fields = []

        while True:
            self.save()
            try:
                field_name = self.parse_identifier()
                self.expect('colon')
                value = self.parse_type()
                self.expect('comma')
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
            if self.next.tag == 'close_paren':
                self.advance()
                return things
            else:
                things.append(self.parse_expression())
                if self.next.tag == 'close_paren':
                    self.advance()
                    return things
                self.expect('comma')

    def parse_bracketed_expression(self):
        self.expect('open_paren')
        expr = self.parse_expression()
        self.expect('close_paren')
        return expr

    def parse_expression_0(self):
        if self.next.tag == 'open_paren':
            return self.parse_bracketed_expression()
        elif self.next.tag == 'character':
            c = self.next.c
            self.advance()
            return ASTNode('character_literal', character = c)
        elif self.next.tag == 'string':
            st = self.next.string
            self.advance()
            return ASTNode('string_literal', string = st)
        elif self.next.tag == 'identifier':
            name = self.next.name
            self.advance()
            return ASTNode('variable', name = name)
        else:
            n = self.parse_number()
            return ASTNode('number_literal', n = n)

    def parse_expression_1(self):
        expr = self.parse_expression_0()
        while not self.eof():
            if self.next.tag == 'open_paren':
                self.advance()
                args = self.parse_term_arg_list()
                expr = ASTNode('application', function = expr, args = args)
            elif self.next.tag == 'dot':
                self.advance()
                field = self.parse_identifier()
                expr = ASTNode('field_access', x = expr, field = field)
            elif self.next.tag == 'at':
                self.advance()
                self.expect('open_paren')
                args = self.parse_type_arg_list()
                expr = ASTNode('apply_type_args', function = expr, args = args)
            else:
                break
        return expr

    def parse_expression_2(self):
        if self.next.tag == 'ampersand':
            self.advance()
            expr = self.parse_l_expr()
            return \
                ASTNode(
                    'address_of',
                    expr = expr,
                )
        elif self.next.tag == 'symbol' and self.next.symbol == '-':
            self.advance()
            expr = self.parse_expression_2()
            return \
                ASTNode(
                    'uminus',
                    expr = expr,
                )
        else:
            return self.parse_expression_1()

    def parse_expression_3(self):
        if self.next.tag == 'asterisk':
            self.advance()
            expr = self.parse_expression_3()
            return \
                ASTNode(
                    'deref',
                    expr = expr,
                )
        else:
            return self.parse_expression_2()

    def parse_expression_4(self):
        expr = self.parse_expression_3()
        while not self.eof():
            if self.next.tag == 'keyword' and self.next.keyword == 'as':
                self.advance()
                type = self.parse_type()
                expr = ASTNode('cast', expr = expr, type = type)
            else:
                break
        return expr

    def parse_expression_5(self):
        expr = self.parse_expression_4()
        while not self.eof():
            if self.next.tag == 'asterisk':
                self.advance()
                other = self.parse_expression_4()
                expr = ASTNode('*', a = expr, b = other)
            elif self.next.tag == 'symbol' and self.next.tag.symbol == '/':
                self.advance()
                other = self.parse_expression_4()
                expr = ASTNode('/', a = expr, b = other)
            else:
                break
        return expr
    def parse_expression_6(self):
        expr = self.parse_expression_5()
        while not self.eof():
            operators = ['-', '+']
            if self.next.tag == 'symbol':
                symbol = self.next.symbol
                if self.next.symbol in operators:
                    self.advance()
                    other = self.parse_expression_5()
                    expr = ASTNode(symbol, a = expr, b = other)
                else:
                    break
            else:
                break
        return expr

    def parse_expression_7(self):
        expr = self.parse_expression_6()
        while not self.eof():
            if self.next.tag == 'symbol':
                symbol = self.next.symbol
                if symbol in comparison_operators:
                    self.advance()
                    other = self.parse_expression_6()
                    expr = ASTNode(symbol, a = expr, b = other)
                else:
                    break
            else:
                break
        return expr

    def parse_expression(self):
        return self.parse_expression_7()

    def parse_let_statement(self):
        self.expect_keyword('let')
        name = self.parse_identifier()
        if self.next.tag == 'colon':
            self.advance()
            ty = self.parse_type()
        else:
            ty = None
        self.expect_symbol('=')
        expr = self.parse_expression()
        self.expect('semicolon')
        return ASTNode('let_statement', name = name, expr = expr, ty = ty)

    def parse_loop_statement(self):
        self.expect_keyword('loop')
        self.expect('open_brace')
        body = self.parse_body()
        return ASTNode('loop_statement', body = body)

    def parse_else(self):
        self.expect_keyword('else')
        self.expect('open_brace')
        false_side = self.parse_body()

    def parse_if_statement(self):
        self.expect_keyword('if')
        condition = self.parse_expression()
        self.expect('open_brace')
        true_side = self.parse_body()
        false_side = self.try_(self.parse_else) or []

        return \
            ASTNode(
                'if_statement',
                condition = condition,
                true_side = true_side,
                false_side = false_side,
            )

    def parse_assignment(self):
        l_expr = self.parse_l_expr()
        self.expect_symbol('=')
        expr = self.parse_expression()
        self.expect('close_brace')
        return ASTNode('assignment', l_expr = l_expr, expr = expr)

    def parse_l_expr(self):
        return self.parse_l_expr_2()

    def parse_l_expr_2(self):
        if self.next.tag == 'asterisk':
            self.advance()
            expr = self.parse_expression()
            return \
                ASTNode(
                    'deref',
                    expr = expr,
                )
        else:
            return self.parse_l_expr_1()

    def parse_l_expr_1(self):
        l_expr = self.parse_l_expr_0()
        while True:
            if self.next.tag == 'dot':
                self.advance()
                field = self.parse_identifier()
                l_expr = \
                    ASTNode(
                        'field_access',
                        l_expr = l_expr,
                        field = field,
                    )
            elif self.next.tag == 'open_square':
                self.advance()
                expr = self.parse_expression()
                if self.next.tag != 'close_square':
                    raise ParseError()
                self.advance()
                l_expr = \
                    ASTNode(
                        'array_access',
                        l_expr = l_expr,
                        index = expr,
                    )
            else:
                return l_expr

    def parse_l_expr_0(self):
        if self.next.tag == 'open_paren':
            return self.parse_bracketed_l_expr()
        else:
            name = self.parse_identifier()
            return \
                ASTNode(
                    'variable',
                    name = name,
                )

    def parse_bracketed_l_expr(self):
        self.expect('open_paren')
        l_expr = self.parse_l_expr()
        self.expect('close_paren')
        return l_expr

    def parse_break(self):
        self.expect_keyword('break')
        self.expect('semicolon')
        return ASTNode('break')

    def parse_return(self):
        self.expect_keyword('return')
        expr = self.parse_expression()
        self.expect('semicolon')
        return ASTNode('return', expr = expr)

    def try_(self, func):
        self.save()
        try:
            output = func()
        except ParseError:
            self.restore()
        else:
            self.discard()
            return output

    def parse_statement(self):
        if self.next.tag == 'keyword':
            if self.next.keyword == 'let':
                return self.parse_let_statement()
            elif self.next.keyword == 'loop':
                return self.parse_loop_statement()
            elif self.next.keyword == 'if':
                return self.parse_if_statement()
            elif self.next.keyword == 'break':
                return self.parse_break()
            elif self.next.keyword == 'return':
                return self.parse_return()

        statement = self.try_(self.parse_assignment)
        if statement:
            return statement

        expr = self.parse_expression()
        self.expect('semicolon')
        return ASTNode('expr_statement', expr = expr)

    def parse_body(self):
        statements = []

        while self.next.tag != 'close_brace':
            statements.append(self.parse_statement())

        self.expect('close_brace')

        return statements

    def parse_function(self):
        name = self.parse_identifier()
        args = []
        product_type = None
        consume_type = None
        return_type = ASTNode('named_type', name = 'void')
        body = None

        self.save()
        symbol = self.parse_symbol()

        if symbol == "<-":
            self.discard()
            while True:
                self.save()
                try:
                    arg_name = self.parse_identifier()
                    self.expect('colon')
                    arg_type = self.parse_type()
                    self.expect('comma')
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

        if symbol == '<=':
            self.discard()
            consume_type = self.parse_type()

            self.save()
            try:
                symbol = self.parse_symbol()
            except ParseError:
                symbol = None

        if symbol == '=>':
            self.discard()
            product_type = self.parse_type()

            self.save()
            try:
                symbol = self.parse_symbol()
            except ParseError:
                symbol = None

        if symbol == "->":
            self.discard()
            return_type = self.parse_type()

            self.save()
            try:
                symbol = self.parse_symbol()
            except ParseError:
                symbol = None

        if symbol != '=':
            raise ParseError('function with no body')

        self.discard()
        self.expect('open_brace')

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

        arg_types = []
        return_type = ASTNode('void')

        self.save()
        try:
            symbol = self.parse_symbol()
        except ParseError:
            symbol = None

        if symbol == "<-":
            self.discard()
            while True:
                self.save()
                try:
                    arg_type = self.parse_type()
                    self.expect('comma')
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

    def parse_constant(self):
        name = self.parse_identifier()
        expr = self.parse_expression()
        return \
            ASTNode(
                'constant',
                name = name,
                expr = expr,
            )

    def parse_top_level_decl(self):
        type = self.parse_keyword()
        if type == 'enum':
            return self.parse_enum()
        elif type == 'struct':
            return self.parse_struct()
        elif type == 'define':
            return self.parse_function()
        elif type == 'extern':
            return self.parse_extern()
        elif type == 'constant':
            return self.parse_constant()
        else:
            raise ParseError('unknown top-level entity: %s' % type)

    def parse_file(self):
        decls = []
        while not self.eof():
            decl = self.parse_top_level_decl()
            decls.append(decl)
        return decls
