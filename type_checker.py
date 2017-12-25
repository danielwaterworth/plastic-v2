class Kind:
    pass

class StarKind(Kind):
    def __eq__(self, other):
        return type(other) == StarKind

star = StarKind()

class FunctionKind(Kind):
    def __init__(self, arg_kinds, return_kind):
        self.arg_kinds = arg_kinds
        self.return_kind = return_kind

    def __eq__(self, other):
        return \
            type(other) == FunctionKind and \
            tuple(self.arg_kinds) == tuple(other.arg_kinds) and \
            self.return_kind == other.return_kind

class TypeLevelExpr:
    @property
    def kind(self):
        raise NotImplementedError()

    def substitutable_for(self, other):
        return self == other

    def substitute(self, substitutions):
        return self

class Void(TypeLevelExpr):
    kind = star

    def __eq__(self, other):
        return type(other) == Void

void = Void()

class Ptr(TypeLevelExpr):
    kind = FunctionKind([star], star)

    def __eq__(self, other):
        return type(other) == Ptr

ptr = Ptr()

def ptr_to(ty):
    return TypeApplication(ptr, [ty])

class OpaquePtr(TypeLevelExpr):
    kind = star

    def substitutable_for(self, other):
        return is_ptr(other)

class Boolean(TypeLevelExpr):
    kind = star

    def __eq__(self, other):
        return type(other) == Boolean

class Number(TypeLevelExpr):
    kind = star

    def __init__(self, signed, width):
        self.signed = signed
        self.width = width

    def __eq__(self, other):
        return \
            type(other) == Number and \
            self.signed == other.signed and \
            self.width == other.width

class OpaqueNumber(TypeLevelExpr):
    kind = star

    def substitutable_for(self, other):
        return type(other) == Number

class TypeApplication(TypeLevelExpr):
    def __init__(self, function, args):
        self.function = function
        self.args = args

    @property
    def kind(self):
        return self.function.kind.return_kind

    def substitute(self, substitutions):
        return \
            TypeApplication(
                self.function.substitute(substitutions),
                [arg.substitute(substitutions) for arg in self.args],
            )

    def __eq__(self, other):
        return \
            type(other) == TypeApplication and \
            self.function == other.function and \
            tuple(self.args) == tuple(other.args)

    def __repr__(self):
        return "TypeApplication(%s, %s)" % (self.function, self.args)

class StructType(TypeLevelExpr):
    kind = star

    def __init__(self, name):
        self.name = name

class EnumType(TypeLevelExpr):
    kind = star

    def __init__(self, name):
        self.name = name

class Coroutine(TypeLevelExpr):
    kind = FunctionKind([star] * 3, star)

    def __eq__(self, other):
        return type(other) == Coroutine

class FunctionType(TypeLevelExpr):
    def __init__(self, calling_convention, arg_types, return_type):
        self.calling_convention = calling_convention
        self.arg_types = arg_types
        self.return_type = return_type

    def substitute(self, substitutions):
        return \
            FunctionType(
                self.calling_convention,
                [arg.substitute(substitutions) for arg in self.arg_types],
                self.return_type.substitute(substitutions),
            )

    @property
    def kind(self):
        return star

class TypeVariable(TypeLevelExpr):
    def __init__(self, name):
        self.name = name

    def substitute(self, substitutions):
        return substitutions.get(self.name, self)

class LambdaType(TypeLevelExpr):
    def __init__(self, args, body):
        self.args = args
        self.body = body

    def apply_type_args(self, args):
        if len(args) != len(self.args):
            raise TypeError()

        return self.body.substitute(dict(zip(self.args, args)))

    def substitute(self, substitutions):
        substitutions = dict(substitutions)
        for arg in self.args:
            del substitutions[arg]
        return self.body.substitute(substitutions)

char = Number(True, 8)
char_ptr = ptr_to(char)

def list_substitutable_for(args, expected):
    return \
        len(args) == len(expected) and \
        all([arg.substitutable_for(exp) for arg, exp in zip(args, expected)])

def is_ptr(x):
    if type(x) == OpaquePtr:
        return True
    if type(x) == TypeApplication:
        if type(x.function) == Ptr:
            return True
    return False

class TypedASTNode:
    def __init__(self, tag, **kwargs):
        self.tag = tag
        self.attributes = kwargs
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __repr__(self):
        return "TypedASTNode(%s, %s)" % (repr(self.tag), repr(self.attributes))

class Environment:
    def __init__(self, type_bindings, term_bindings, parent=None, product_type=None, consume_type=None, return_type=None):
        self.type_bindings = type_bindings
        self.term_bindings = term_bindings
        self.parent = parent
        self.product_type = None
        self.consume_type = None
        self.return_type = None
        if parent:
            self.product_type = parent.product_type
            self.consume_type = parent.consume_type
            self.return_type = parent.return_type
        if product_type:
            self.product_type = product_type
        if consume_type:
            self.consume_type = consume_type
        if return_type:
            self.return_type = return_type

    def lookup_type(self, key):
        try:
            return self.type_bindings[key]
        except KeyError:
            pass

        if self.parent:
            return self.parent.lookup_type(key)

        raise KeyError(key)

    def lookup_term(self, key):
        try:
            return self.term_bindings[key]
        except KeyError:
            pass

        if self.parent:
            return self.parent.lookup_term(key)

        raise KeyError(key)

    def check_type(self, ty):
        if ty.tag == 'named_type':
            return self.lookup_type(ty.name)
        elif ty.tag == 'type_application':
            function = self.check_type(ty.function)
            args = self.check_type_list(ty.args)
            arg_kinds = [arg.kind for arg in args]
            function_kind = function.kind
            if type(function_kind) != FunctionKind:
                raise TypeError()
            if tuple(function_kind.arg_kinds) != tuple(arg_kinds):
                raise TypeError()
            return TypeApplication(function, args)
        raise NotImplementedError()

    def check_type_list(self, types):
        return [self.check_type(type) for type in types]

    def check_struct(self, decl):
        struct_type = StructType(decl.name)
        self.type_bindings[decl.name] = struct_type
        fields = [(name, self.check_type(ty)) for name, ty in decl.fields]
        self.term_bindings[decl.name] = \
            FunctionType('plastic', [ty for _, ty in fields], struct_type)
        struct_type.fields = dict(fields)
        return \
            TypedASTNode(
                'struct',
                name = decl.name,
                fields = fields,
            )

    def check_enum(self, decl):
        enum_type = EnumType(decl.name)
        self.type_bindings[decl.name] = enum_type
        constructors = \
            [(name, self.check_type_list(ty)) for name, ty in decl.constructors]
        for name, args in constructors:
            self.term_bindings[name] = \
                FunctionType('plastic', args, enum_type)
        enum_type.constructors = constructors
        return \
            TypedASTNode(
                'enum',
                name = decl.name,
                constructors = constructors,
            )

    def check_extern(self, decl):
        arg_types = self.check_type_list(decl.arg_types)
        return_type = self.check_type(decl.return_type)
        self.term_bindings[decl.name] = \
            FunctionType('c', arg_types, return_type)
        return \
            TypedASTNode(
                'extern',
                name = decl.name,
                arg_types = arg_types,
                return_type = return_type,
            )

    def check_expression(self, expr):
        if expr.tag == 'yield_expression':
            yielded_expr = self.check_expression(expr.expr)
            if yielded_expr.ty != self.product_type:
                raise TypeError()
            typed_expr = \
                TypedASTNode(
                    'yield_expression',
                    expr = yielded_expr,
                    ty = self.consume_type,
                )
            return typed_expr
        elif expr.tag == 'application':
            function = self.check_expression(expr.function)
            args = self.check_expression_list(expr.args)
            arg_types = [arg.ty for arg in args]
            if type(function.ty) != FunctionType:
                raise TypeError()
            if not list_substitutable_for(arg_types, function.ty.arg_types):
                print(arg_types)
                print(function.ty.arg_types)
                raise TypeError()
            return \
                TypedASTNode(
                    'application',
                    function = function,
                    args = args,
                    ty = function.ty.return_type,
                )
        elif expr.tag == 'variable':
            return \
                TypedASTNode(
                    'variable',
                    name = expr.name,
                    ty = self.lookup_term(expr.name),
                )
        elif expr.tag == 'field_access':
            x = self.check_expression(expr.x)
            if type(x.ty) != StructType:
                print(x.ty)
                raise TypeError()
            if not expr.field in x.ty.fields:
                raise TypeError()
            return \
                TypedASTNode(
                    'field_access',
                    x = x,
                    field = expr.field,
                    ty = x.ty.fields[expr.field],
                )
        elif expr.tag == 'cast':
            to_cast = self.check_expression(expr.expr)
            ty = self.check_type(expr.type)
            castable = \
                type(to_cast.ty) == Number or \
                is_ptr(to_cast.ty)
            if not castable:
                raise TypeError()
            if type(ty) != Number:
                raise TypeError()
            return \
                TypedASTNode(
                    'cast',
                    expr = to_cast,
                    ty = ty,
                )
        elif expr.tag == 'character_literal':
            return \
                TypedASTNode(
                    'number_literal',
                    n = ord(expr.character),
                    ty = Number(False, 8),
                )
        elif expr.tag == '==':
            a = self.check_expression(expr.a)
            b = self.check_expression(expr.b)
            compatible = \
                a.ty.substitutable_for(b.ty) or \
                b.ty.substitutable_for(a.ty)
            if not compatible:
                raise TypeError()
            return \
                TypedASTNode(
                    '==',
                    a = a,
                    b = b,
                    ty = Boolean(),
                )
        elif expr.tag == '+':
            a = self.check_expression(expr.a)
            b = self.check_expression(expr.b)
            compatible = a.ty == b.ty
            if not compatible:
                raise TypeError()
            return \
                TypedASTNode(
                    '+',
                    a = a,
                    b = b,
                    ty = a.ty,
                )
        elif expr.tag == '-':
            a = self.check_expression(expr.a)
            b = self.check_expression(expr.b)
            compatible = a.ty == b.ty
            if not compatible:
                raise TypeError()
            return \
                TypedASTNode(
                    '-',
                    a = a,
                    b = b,
                    ty = a.ty,
                )
        elif expr.tag == 'number_literal':
            return \
                TypedASTNode(
                    'number_literal',
                    n = expr.n,
                    ty = OpaqueNumber(),
                )
        elif expr.tag == 'string_literal':
            return \
                TypedASTNode(
                    'string_literal',
                    string = expr.string,
                    ty = char_ptr,
                )
        elif expr.tag == 'apply_type_args':
            function = self.check_expression(expr.function)
            args = self.check_type_list(expr.args)
            if type(function.ty) != LambdaType:
                raise TypeError()

            new_type = function.ty.apply_type_args(args)
            return \
                TypedASTNode(
                    'apply_type_args',
                    function = function,
                    args = args,
                    ty = new_type,
                )
            raise NotImplementedError()

        print(expr.tag)
        raise NotImplementedError()

    def check_expression_list(self, exprs):
        return [self.check_expression(expr) for expr in exprs]

    def check_let(self, statement):
        expr = self.check_expression(statement.expr)
        statement = \
            TypedASTNode(
                'let_statement',
                expr = expr,
                name = statement.name
            )
        new_env = Environment({}, {statement.name: expr.ty}, self)
        return statement, new_env

    def check_loop(self, statement):
        return \
            TypedASTNode(
                'loop_statement',
                body = self.check_body(statement.body)
            )

    def check_if(self, statement):
        condition = self.check_expression(statement.condition)
        if type(condition.ty) != Boolean:
            raise TypeError()
        true_side = self.check_body(statement.true_side)
        false_side = self.check_body(statement.false_side)
        return \
            TypedASTNode(
                'if_statement',
                condition = condition,
                true_side = true_side,
                false_side = false_side,
            )

    def check_body(self, body):
        if len(body) == 0:
            return []
        else:
            first_statement, rest = body[0], body[1:]
            if first_statement.tag == 'let_statement':
                statement, new_env = self.check_let(first_statement)
            elif first_statement.tag == 'loop_statement':
                statement = self.check_loop(first_statement)
                new_env = self
            elif first_statement.tag == 'if_statement':
                statement = self.check_if(first_statement)
                new_env = self
            elif first_statement.tag == 'expr_statement':
                statement = \
                    TypedASTNode(
                        'expr_statement',
                        expr = self.check_expression(first_statement.expr),
                    )
                new_env = self
            elif first_statement.tag == 'break':
                statement = TypedASTNode('break')
                new_env = self
            elif first_statement.tag == 'assignment':
                expr = self.check_expression(first_statement.expr)
                l_expr = self.check_l_expression(first_statement.l_expr)
                compatible = \
                    expr.ty.substitutable_for(l_expr.ty)
                if not compatible:
                    raise TypeError()
                statement = \
                    TypedASTNode(
                        'assignment',
                        l_expr = l_expr,
                        expr = expr,
                    )
                new_env = self
            elif first_statement.tag == 'return':
                expr = self.check_expression(first_statement.expr)
                if expr.ty != self.return_type:
                    raise TypeError()
                statement = \
                    TypedASTNode(
                        'return',
                        expr = expr,
                        ty = void,
                    )
                new_env = self
            else:
                print(first_statement.tag)
                raise NotImplementedError()
            return [statement] + new_env.check_body(rest)

    def check_l_expression(self, l_expr):
        if l_expr.tag == 'variable':
            ty = self.lookup_term(l_expr.name)
            return \
                TypedASTNode(
                    'variable',
                    name = l_expr.name,
                    ty = ty,
                )
        elif l_expr.tag == 'field_access':
            root = self.check_l_expression(l_expr.l_expr)
            if type(root.ty) != StructType:
                raise TypeError()
            if l_expr.field not in root.ty.fields:
                raise TypeError()
            field_ty = root.ty.fields[l_expr.field]
            return \
                TypedASTNode(
                    'field_access',
                    l_expr = root,
                    field = l_expr.field,
                    ty = field_ty,
                )
        else:
            raise NotImplementedError()

    def check_function(self, decl):
        args = [(name, self.check_type(ty)) for name, ty in decl.args]
        consume_type = None
        product_type = None
        if decl.consume_type:
            consume_type = self.check_type(decl.consume_type)
        if decl.product_type:
            product_type = self.check_type(decl.product_type)
        return_type = self.check_type(decl.return_type)

        if consume_type or product_type:
            self.term_bindings[decl.name] = \
                FunctionType(
                    'plastic',
                    [ty for _, ty in args],
                    TypeApplication(Coroutine(), [
                        consume_type or Void(),
                        product_type or Void(),
                        return_type,
                    ]),
                )
        else:
            self.term_bindings[decl.name] = \
                FunctionType(
                    'plastic',
                    [ty for _, ty in args],
                    return_type,
                )

        new_env = \
            Environment(
                {},
                dict(args),
                self,
                product_type,
                consume_type,
                return_type
            )
        body = new_env.check_body(decl.body)
        return \
            TypedASTNode(
                'function',
                name = decl.name,
                args = args,
                consume_type = consume_type,
                product_type = product_type,
                return_type = return_type,
                body = body,
            )

    def check_top_level_decl(self, decl):
        if decl.tag == 'extern':
            return self.check_extern(decl)
        elif decl.tag == 'struct':
            return self.check_struct(decl)
        elif decl.tag == 'enum':
            return self.check_enum(decl)
        elif decl.tag == 'function':
            return self.check_function(decl)
        raise NotImplementedError()

    def check_top_level_decls(self, decls):
        return [self.check_top_level_decl(decl) for decl in decls]

global_type_environment = {
    'void': void,
    'ptr': ptr,
    'i8': char,
    'i32': Number(True, 32),
    'u32': Number(False, 32),
    'u64': Number(False, 64),
    'bool': Boolean(),
}

global_term_environment = {
    'null': OpaquePtr(),
    'void': void,
    'true': Boolean(),
    'false': Boolean(),
    'resume':
        LambdaType(
            ['a', 'b', 'c'],
            FunctionType(
                'plastic',
                [
                    TypeApplication(
                        Coroutine(),
                        [
                            TypeVariable('a'),
                            TypeVariable('b'),
                            TypeVariable('c'),
                        ]
                    ),
                    TypeVariable('a'),
                ],
                TypeVariable('b'),
            )
        )
}
