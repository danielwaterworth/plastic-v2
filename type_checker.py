import parser
import sys

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

class Void(TypeLevelExpr):
    kind = star

    def __eq__(self, other):
        return type(other) == Void

class Ptr(TypeLevelExpr):
    kind = FunctionKind([star], star)

    def __eq__(self, other):
        return type(other) == Ptr

class OpaquePtr(TypeLevelExpr):
    kind = star

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

class TypeApplication(TypeLevelExpr):
    def __init__(self, function, args):
        self.function = function
        self.args = args

    @property
    def kind(self):
        return self.function.kind.return_kind

    def __eq__(self, other):
        return \
            type(other) == TypeApplication and \
            self.function == other.function and \
            tuple(self.args) == tuple(other.args)

class StructType(TypeLevelExpr):
    kind = star

    def __init__(self, name):
        self.name = name

class EnumType(TypeLevelExpr):
    kind = star

    def __init__(self, name):
        self.name = name

class Constructor(TypeLevelExpr):
    def __init__(self, name, ty, arg_types):
        self.name = name
        self.return_type = ty
        self.arg_types = arg_types

    @property
    def kind(self):
        return FunctionKind([star] * len(self.arg_types), self.return_type.kind)

class Extern(TypeLevelExpr):
    def __init__(self, name, arg_types, ty):
        self.name = name
        self.return_type = ty
        self.arg_types = arg_types

    @property
    def kind(self):
        return FunctionKind([star] * len(self.arg_types), self.return_type.kind)

class TypedASTNode:
    def __init__(self, tag, **kwargs):
        self.tag = tag
        self.attributes = kwargs
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __repr__(self):
        return "ASTNode(%s, %s)" % (repr(self.tag), repr(self.attributes))

class Environment:
    def __init__(self, type_bindings, term_bindings, parent=None, product_type=None, consume_type=None):
        self.type_bindings = type_bindings
        self.term_bindings = term_bindings
        self.parent = parent
        self.product_type = None
        self.consume_type = None
        if parent:
            self.product_type = parent.product_type
            self.consume_type = parent.consume_type
        if product_type:
            self.product_type = product_type
        if consume_type:
            self.consume_type = consume_type

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
        struct_type.fields = dict(fields)
        return \
            TypedASTNode(
                'struct',
                fields = fields,
            )

    def check_enum(self, decl):
        enum_type = EnumType(decl.name)
        self.type_bindings[decl.name] = enum_type
        constructors = \
            [(name, self.check_type_list(ty)) for name, ty in decl.constructors]
        for name, args in constructors:
            self.term_bindings[name] = \
                Constructor(name, enum_type, args)
        enum_type.constructors = constructors
        return \
            TypedASTNode(
                'enum',
                constructors = constructors,
            )

    def check_extern(self, decl):
        arg_types = self.check_type_list(decl.arg_types)
        return_type = self.check_type(decl.return_type)
        self.term_bindings[decl.name] = \
            Extern(decl.name, arg_types, return_type)
        return \
            TypedASTNode(
                'extern',
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
            if type(function.ty.kind) != FunctionKind:
                raise TypeError()
            if tuple(function.ty.arg_types) != tuple(arg_types):
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
            if type(to_cast.ty) != Number:
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
            if a.ty != b.ty:
                raise TypeError()
            return \
                TypedASTNode(
                    '==',
                    a = a,
                    b = b,
                    ty = Boolean(),
                )
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
            else:
                print(first_statement.tag)
                raise NotImplementedError()
            return [statement] + new_env.check_body(rest)

    def check_function(self, decl):
        args = [(name, self.check_type(ty)) for name, ty in decl.args]
        consume_type = self.check_type(decl.consume_type)
        product_type = self.check_type(decl.product_type)
        return_type = self.check_type(decl.return_type)
        new_env = Environment({}, dict(args), self, product_type, consume_type)
        body = new_env.check_body(decl.body)
        return \
            TypedASTNode(
                'function',
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
    'void': Void(),
    'ptr': Ptr(),
    'i8': Number(True, 8),
    'i32': Number(True, 32),
    'u32': Number(False, 32),
    'u64': Number(False, 64),
    'bool': Boolean(),
}

global_term_environment = {
    'null': OpaquePtr()
}

with open(sys.argv[1], 'r') as fd:
    decls = parser.Parser(fd.read()).parse_file()
    env = \
        Environment(
            global_type_environment,
            global_term_environment,
        )
    env.check_top_level_decls(decls)
