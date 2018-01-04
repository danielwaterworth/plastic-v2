from ir import *

star = Node('star')
nat = Node('nat')
module_kind = Node('module')

def function_kind(arg_kinds, return_kind):
    return \
        Node(
            'function',
            arg_kinds = arg_kinds,
            return_kind = return_kind,
        )

def substitutable_for(a, b):
    if a == b:
        return True

    if a == opaque_ptr:
        return is_ptr(b)
    if a == opaque_number_type:
        return b.tag == 'number_type'

    return False

void = Node('void', kind = star)
ptr = Node('void', kind = function_kind([star], star))
array = Node('array', kind = function_kind([star, nat], star))

def tuple_type(n):
    return \
        Node(
            'tuple',
            n = n,
            kind = function_kind([star] * self.n, star),
        )

def nat_literal(n):
    return Node('nat_literal', n = n, kind = nat)

opaque_ptr = Node('opaque_ptr', kind = star)
boolean = Node('boolean', kind = star)

def number_type(signed, width):
    return Node('number_type', signed = signed, width = width, kind = star)

opaque_number_type = Node('opaque_number_type', kind = star)

def type_apply(fn, args):
    if fn.kind.tag != 'function':
        raise \
            TypeError(
                "type function applied to wrong number of arguments"
            )
    fn_arg_kinds = fn.kind.arg_kinds
    arg_kinds = [arg.kind for arg in args]
    if fn_arg_kinds != arg_kinds:
        raise TypeError()
    if fn.tag == 'lambda_tag':
        names = [name for name, _ in fn.args]
        substitutions = dict(zip(names, args))
        return substitute(fn.body, substitutions)
    else:
        return \
            Node(
                'type_application',
                function = fn,
                args = args,
                kind = fn.kind.return_kind
            )

def ptr_to(ty):
    return type_apply(ptr, [ty])

class StructType:
    kind = star

    def __init__(self, module_name, name, type_args):
        self.module_name = module_name
        self.name = name
        self.type_args = type_args

    def __eq__(self, other):
        return \
            type(other) == StructType and \
            self.module_name == other.module_name and \
            self.name == other.name and \
            tuple(self.type_args) == tuple(other.type_args)

    def __str__(self):
        return \
            "StructType(%s, %s, %s)" % (
                self.module_name,
                self.name,
                self.type_args,
            )

class EnumType:
    kind = star

    def __init__(self, module_name, name, type_args, constructors=None):
        self.module_name = module_name
        self.name = name
        self.type_args = type_args
        self.constructors = constructors

    def __eq__(self, other):
        return \
            type(other) == EnumType and \
            self.module_name == other.module_name and \
            self.name == other.name and \
            tuple(self.type_args) == tuple(other.type_args)

    def __str__(self):
        return \
            "EnumType(%s, %s, %s, %s)" % (
                self.module_name,
                self.name,
                self.type_args,
                self.constructors,
            )

coroutine = Node('coroutine', kind = function_kind([star] * 3, star))

def function_type(calling_convention, arg_types, return_type):
    return \
        Node(
            'function_type',
            calling_convention = calling_convention,
            arg_types = arg_types,
            return_type = return_type,
            kind = star,
        )

def type_variable(name, kind):
    return \
        Node(
            'type_variable',
            name = name,
            kind = kind,
        )

def lambda_type(args, body):
    arg_kinds = [kind for _, kind in args]
    kind = function_kind(arg_kinds, body.kind)
    return \
        Node(
            'lambda_type',
            args = args,
            body = body,
            kind = kind,
        )

def substitute(x, substitutions):
    if type(x) == StructType:
        StructType(
            x.module_name,
            x.name,
            [substitute(type_arg, substitutions)
                for type_arg in x.type_args],
        )
    if type(x) == EnumType:
        new_constructors = {}
        for name, tys in x.constructors.items():
            new_constructors[name] = \
                [substitute(ty, substitutions) for ty in tys]
        return \
            EnumType(
                x.module_name,
                x.name,
                [substitute(type_arg, substitutions)
                    for type_arg in x.type_args],
                new_constructors,
            )
    if x.tag == 'function_type':
        return \
            function_type(
                x.calling_convention,
                [substitute(arg, substitutions) for arg in x.arg_types],
                substitute(x.return_type, substitutions),
            )
    if x.tag == 'type_variable':
        return substitutions.get(x.name, x)
    if x.tag == 'lambda_type':
        substitutions = dict(substitutions)
        for arg in x.args:
            del substitutions[arg[0]]
        return \
            lambda_type(
                x.args,
                substitute(x.body, substitutions)
            )
    if x.tag == 'type_application':
        return \
            type_apply(
                substitute(x.function, substitutions),
                [substitute(arg, substitutions) for arg in x.args],
            )
    return x

char = number_type(True, 8)
char_ptr = ptr_to(char)

def list_substitutable_for(args, expected):
    return \
        len(args) == len(expected) and \
        all([substitutable_for(arg, exp) for arg, exp in zip(args, expected)])

def is_ptr(x):
    if x == opaque_ptr:
        return True
    if type(x) == Node and x.tag == 'type_application':
        if x.function == ptr:
            return True
    return False

def is_array(x):
    if type(x) == Node and x.tag == 'type_application':
        if x.function == array:
            return True
    return False

def is_tuple(x):
    if type(x) == Node and x.tag == 'type_application':
        if x.function.tag == 'tuple':
            return True
    return False

def is_number(x):
    return x.tag == 'number_type' or x == opaque_number_type

class ModuleType:
    kind = module_kind

    def __init__(self, name, interface):
        self.name = name
        self.values = interface.values
        self.types = interface.types

class ModuleInterface:
    def __init__(self, values, types):
        self.values = values
        self.types = types

class Environment:
    def __init__(
            self,
            type_bindings,
            term_bindings,
            parent=None,
            product_type=None,
            consume_type=None,
            return_type=None,
            modules={},
            module_name=None):
        self.modules = modules
        self.type_bindings = type_bindings
        self.term_bindings = term_bindings
        self.parent = parent
        self.product_type = None
        self.consume_type = None
        self.return_type = None
        self.module_name = None
        if parent:
            self.product_type = parent.product_type
            self.consume_type = parent.consume_type
            self.return_type = parent.return_type
            self.module_name = parent.module_name
        if product_type:
            self.product_type = product_type
        if consume_type:
            self.consume_type = consume_type
        if return_type:
            self.return_type = return_type
        if module_name:
            self.module_name = module_name

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
            return type_apply(function, args)
        elif ty.tag == 'type_number':
            return nat_literal(ty.n)
        elif ty.tag == 'field_access':
            module = self.check_type(ty.ty)
            if module.kind != module_kind:
                raise TypeError()
            return module.types[ty.field]
        elif ty.tag == 'tuple':
            types = self.check_type_list(ty.types)
            return type_apply(tuple_type(len(types)), types)
        raise NotImplementedError()

    def check_type_list(self, types):
        return [self.check_type(type) for type in types]

    def check_kind(self, kind):
        if kind.tag == 'star':
            return star
        elif kind.tag == 'named_kind':
            if kind.name == 'nat':
                return nat
            raise \
                TypeError(
                    'unknown kind %s' % repr(kind.name)
                )
        raise NotImplementedError()

    def check_type_params(self, parameters):
        return [(name, self.check_kind(kind)) for name, kind in parameters]

    def check_struct(self, decl):
        type_param_kinds = self.check_type_params(decl.type_params)
        type_params = \
            [(name, type_variable(name, kind))
                for name, kind in type_param_kinds]
        type_args = [arg for _, arg in type_params]
        new_env = \
            Environment(
                dict(type_params),
                {},
                self,
            )

        struct_type = StructType(self.module_name, decl.name, type_args)
        if len(type_param_kinds) > 0:
            self.type_bindings[decl.name] = \
                lambda_type(
                    type_param_kinds,
                    struct_type,
                )
        else:
            self.type_bindings[decl.name] = struct_type

        fields = \
            [(name, new_env.check_type(ty)) for name, ty in decl.fields]
        struct_type.fields = dict(fields)

        constructor_type = \
            function_type(
                'plastic',
                [ty for _, ty in fields],
                struct_type,
            )

        if len(type_param_kinds) > 0:
            self.term_bindings[decl.name] = \
                lambda_type(
                    type_param_kinds,
                    constructor_type,
                )
        else:
            self.term_bindings[decl.name] = \
                constructor_type

        return \
            Node(
                'struct',
                name = decl.name,
                fields = fields,
                type_params = type_params,
            )

    def check_enum(self, decl):
        type_param_kinds = self.check_type_params(decl.type_params)
        type_params = \
            [(name, type_variable(name, kind))
                for name, kind in type_param_kinds]
        type_args = [arg for _, arg in type_params]
        new_env = \
            Environment(
                dict(type_params),
                {},
                self,
            )

        enum_type = EnumType(self.module_name, decl.name, type_args)
        if len(type_param_kinds) > 0:
            self.type_bindings[decl.name] = \
                lambda_type(
                    type_param_kinds,
                    enum_type,
                )
        else:
            self.type_bindings[decl.name] = enum_type

        constructors = \
            [(name, new_env.check_type_list(ty))
                for name, ty in decl.constructors]

        for name, args in constructors:
            constructor_type = \
                function_type('plastic', args, enum_type)
            if len(type_param_kinds) > 0:
                self.term_bindings[name] = \
                    lambda_type(
                        type_param_kinds,
                        constructor_type,
                    )
            else:
                self.term_bindings[name] = \
                    constructor_type
        enum_type.constructors = dict(constructors)
        return \
            Node(
                'enum',
                name = decl.name,
                constructors = constructors,
                type_params = type_params,
            )

    def check_extern(self, decl):
        arg_types = self.check_type_list(decl.arg_types)
        return_type = self.check_type(decl.return_type)
        self.term_bindings[decl.name] = \
            function_type('c', arg_types, return_type)
        return \
            Node(
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
                Node(
                    'yield_expression',
                    expr = yielded_expr,
                    ty = self.consume_type,
                )
            return typed_expr
        elif expr.tag == 'application':
            function = self.check_expression(expr.function)
            args = self.check_expression_list(expr.args)
            arg_types = [arg.ty for arg in args]
            if function.ty.tag != 'function_type':
                if function.ty.tag == 'lambda_type':
                    raise TypeError("Need to specify type args first")
                else:
                    raise TypeError()
            if not len(arg_types) == len(function.ty.arg_types):
                raise TypeError()
            new_args = []
            iterator = zip(arg_types, function.ty.arg_types, args)
            for actual, expected, value in iterator:
                if not substitutable_for(actual, expected):
                    raise \
                        TypeError(
                            "can't pass %s for %s" % (
                                repr(actual),
                                repr(expected)
                            )
                        )
                if actual == expected:
                    new_args.append(value)
                else:
                    new_args.append(
                        Node(
                            'cast',
                            expr = value,
                            ty = expected,
                        )
                    )
            return \
                Node(
                    'application',
                    function = function,
                    args = new_args,
                    ty = function.ty.return_type,
                )
        elif expr.tag == 'variable':
            return \
                Node(
                    'variable',
                    name = expr.name,
                    ty = self.lookup_term(expr.name),
                )
        elif expr.tag == 'field_access':
            x = self.check_expression(expr.x)
            if type(x.ty) == ModuleType:
                if not expr.field in x.ty.values:
                    raise \
                        TypeError(
                            "No such value %s in module %s" % (
                                repr(expr.field),
                                repr(x.ty.name)
                            ),
                        )
                return \
                    Node(
                        'module_field_access',
                        x = x,
                        field = expr.field,
                        ty = x.ty.values[expr.field],
                    )
            elif type(x.ty) == StructType:
                if not expr.field in x.ty.fields:
                    raise \
                        TypeError(
                            "No such field %s in struct %s from module %s" % (
                                repr(expr.field),
                                repr(x.ty.name),
                                repr(x.ty.module_name)
                            ),
                        )
                return \
                    Node(
                        'struct_field_access',
                        x = x,
                        field = expr.field,
                        ty = x.ty.fields[expr.field],
                    )
            else:
                print(x.ty)
                raise TypeError()
        elif expr.tag == 'cast':
            to_cast = self.check_expression(expr.expr)
            from_ty = to_cast.ty
            to_ty = self.check_type(expr.type)
            if to_ty.tag == 'number_type':
                castable = \
                    from_ty.tag == 'number_type' or \
                    is_ptr(from_ty) or \
                    from_ty == opaque_number_type
                if not castable:
                    raise TypeError()
            elif is_ptr(to_ty):
                if not is_ptr(from_ty):
                    raise TypeError()
            else:
                raise TypeError()
            return \
                Node(
                    'cast',
                    expr = to_cast,
                    ty = to_ty,
                )
        elif expr.tag == 'character_literal':
            return \
                Node(
                    'number_literal',
                    n = ord(expr.character),
                    ty = number_type(False, 8),
                )
        elif expr.tag == 'number_literal':
            return \
                Node(
                    'number_literal',
                    n = expr.n,
                    ty = opaque_number_type,
                )
        elif expr.tag == 'string_literal':
            return \
                Node(
                    'string_literal',
                    string = expr.string,
                    ty = char_ptr,
                )
        elif expr.tag == 'apply_type_args':
            function = self.check_expression(expr.function)
            if type(function.ty) != lambda_type:
                raise \
                    TypeError(
                        'Expected %s to be a lambda_type' % function.ty
                    )
            args = self.check_type_list(expr.args)
            new_type = type_apply(function.ty, args)

            return \
                Node(
                    'apply_type_args',
                    function = function,
                    args = args,
                    ty = new_type,
                )
        elif expr.tag == 'address_of':
            expr = self.check_l_expression(expr.expr)
            return \
                Node(
                    'address_of',
                    expr = expr,
                    ty = ptr_to(expr.ty),
                )
        elif expr.tag == 'deref':
            expr = self.check_expression(expr.expr)
            if not is_ptr(expr.ty):
                raise TypeError()
            return \
                Node(
                    'deref',
                    expr = expr,
                    ty = expr.ty.args[0],
                )
        elif expr.tag == 'not':
            expr = self.check_expression(expr.expr)
            if type(expr.ty) != Boolean:
                raise TypeError()
            return \
                Node(
                    'not',
                    expr = expr,
                    ty = boolean,
                )
        elif expr.tag == 'uminus':
            expr = self.check_expression(expr.expr)
            if not is_number(expr.ty):
                raise TypeError()
            return \
                Node(
                    'uminus',
                    expr = expr,
                    ty = expr.ty,
                )
        elif expr.tag == 'array_access':
            root = self.check_expression(expr.expr)
            index = self.check_expression(expr.index)
            if not is_array(root.ty):
                raise TypeError()
            if not is_number(index.ty):
                raise TypeError()
            return \
                Node(
                    'array_access',
                    expr = root,
                    index = index,
                    ty = root.ty.args[0],
                )
        elif expr.tag == 'tuple':
            values = self.check_expression_list(expr.values)
            types = [value.ty for value in values]
            return \
                Node(
                    'tuple',
                    values = values,
                    ty = \
                        type_apply(
                            tuple_type(len(values)),
                            types,
                        )
                )
            raise NotImplementedError()
        elif expr.tag == 'equality_operator':
            a = self.check_expression(expr.a)
            b = self.check_expression(expr.b)
            if a.ty == b.ty:
                pass
            if substitutable_for(a.ty, b.ty):
                a = \
                    Node(
                        'cast',
                        expr = a,
                        ty = b.ty,
                    )
            elif substitutable_for(b.ty, a.ty):
                b = \
                    Node(
                        'cast',
                        expr = b,
                        ty = a.ty,
                    )
            else:
                raise TypeError()
            return \
                Node(
                    'equality_operator',
                    operator = expr.operator,
                    a = a,
                    b = b,
                    ty = boolean,
                )
        elif expr.tag == 'comparison_operator':
            a = self.check_expression(expr.a)
            b = self.check_expression(expr.b)
            if a.ty == b.ty:
                pass
            if substitutable_for(a.ty, b.ty):
                a = \
                    Node(
                        'cast',
                        expr = a,
                        ty = b.ty,
                    )
            elif substitutable_for(b.ty, a.ty):
                b = \
                    Node(
                        'cast',
                        expr = b,
                        ty = a.ty,
                    )
            else:
                raise TypeError()
            if not a.ty.tag == 'number_type':
                print(a)
                raise TypeError()
            return \
                Node(
                    'comparison_operator',
                    operator = expr.operator,
                    a = a,
                    b = b,
                    ty = boolean,
                )
        elif expr.tag == 'binary_operator':
            a = self.check_expression(expr.a)
            b = self.check_expression(expr.b)
            if a.ty == b.ty:
                pass
            if substitutable_for(a.ty, b.ty):
                a = \
                    Node(
                        'cast',
                        expr = a,
                        ty = b.ty,
                    )
            elif substitutable_for(b.ty, a.ty):
                b = \
                    Node(
                        'cast',
                        expr = b,
                        ty = a.ty,
                    )
            else:
                raise TypeError()
            if not a.ty.tag == 'number_type':
                print(a)
                raise TypeError()
            return \
                Node(
                    'binary_operator',
                    operator = expr.operator,
                    a = a,
                    b = b,
                    ty = a.ty,
                )
        print(expr.tag)
        raise NotImplementedError()

    def check_expression_list(self, exprs):
        return [self.check_expression(expr) for expr in exprs]

    def check_let(self, statement):
        if statement.expr is None:
            expr = None
        else:
            expr = self.check_expression(statement.expr)
        if statement.ty:
            ty = self.check_type(statement.ty)
            if expr:
                if not substitutable_for(expr.ty, ty):
                    raise TypeError()
                if expr.ty != ty:
                    expr = \
                        Node(
                            'cast',
                            expr = expr,
                            ty = ty,
                        )
        else:
            if expr is None:
                raise TypeError()
            ty = expr.ty
        statement = \
            Node(
                'let_statement',
                expr = expr,
                name = statement.name,
                ty = ty,
            )
        new_env = Environment({}, {statement.name: ty}, self)
        return statement, new_env

    def check_loop(self, statement):
        return \
            Node(
                'loop_statement',
                body = self.check_body(statement.body)
            )

    def check_if(self, statement):
        condition = self.check_expression(statement.condition)
        if condition.ty != boolean:
            raise TypeError()
        true_side = self.check_body(statement.true_side)
        false_side = self.check_body(statement.false_side)
        return \
            Node(
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
                    Node(
                        'expr_statement',
                        expr = self.check_expression(first_statement.expr),
                    )
                new_env = self
            elif first_statement.tag == 'break':
                statement = Node('break')
                new_env = self
            elif first_statement.tag == 'assignment':
                l_expr = self.check_l_expression(first_statement.l_expr)
                expr = self.check_expression(first_statement.expr)
                compatible = \
                    substitutable_for(expr.ty, l_expr.ty)
                equal = \
                    expr.ty == l_expr.ty
                if not compatible:
                    raise TypeError()
                if not equal:
                    expr = \
                        Node(
                            'cast',
                            expr = expr,
                            ty = l_expr.ty,
                        )
                statement = \
                    Node(
                        'assignment',
                        l_expr = l_expr,
                        expr = expr,
                    )
                new_env = self
            elif first_statement.tag == 'return':
                expr = self.check_expression(first_statement.expr)
                if not substitutable_for(expr.ty, self.return_type):
                    raise \
                        TypeError(
                            'can\'t return %s for %s' % (
                                expr.ty,
                                self.return_type,
                            )
                        )
                statement = \
                    Node(
                        'return',
                        expr = expr,
                        ty = void,
                    )
                new_env = self
            elif first_statement.tag == 'match':
                expr = self.check_expression(first_statement.expr)
                matches = self.check_matches(expr.ty, first_statement.matches)
                statement = \
                    Node(
                        'match',
                        expr = expr,
                        matches = matches,
                    )
                new_env = self
            else:
                print(first_statement.tag)
                raise NotImplementedError()
            return [statement] + new_env.check_body(rest)

    def check_constructor_pattern(self, enum_type, pattern):
        if type(enum_type) != EnumType:
            print(enum_type)
            raise TypeError()
        constructor = enum_type.constructors[pattern.name]
        if len(pattern.args) != len(constructor):
            raise TypeError()
        new_env = self
        args = []
        for arg_type, arg_pattern in zip(constructor, pattern.args):
            new_env, arg_pattern = new_env.check_pattern(arg_type, arg_pattern)
            args.append(arg_pattern)
        pattern = \
            Node(
                'constructor',
                name = pattern.name,
                args = args,
                ty = enum_type,
            )
        return new_env, pattern

    def check_wildcard_pattern(self, enum_type, pattern):
        new_env = Environment({}, {pattern.name: enum_type}, self)
        pattern = \
            Node(
                'wildcard',
                name = pattern.name,
                ty = enum_type,
            )
        return new_env, pattern

    def check_pattern(self, enum_type, pattern):
        if pattern.tag == 'wildcard':
            return self.check_wildcard_pattern(enum_type, pattern)
        elif pattern.tag == 'constructor':
            return self.check_constructor_pattern(enum_type, pattern)
        else:
            raise NotImplementedError()

    def check_match(self, enum_type, pattern, body):
        new_env, pattern = self.check_pattern(enum_type, pattern)
        body = new_env.check_body(body)
        return (pattern, body)

    def check_matches(self, enum_type, matches):
        return \
            [self.check_match(enum_type, pattern, body)
                for pattern, body in matches]

    def check_l_expression(self, l_expr):
        if l_expr.tag == 'variable':
            ty = self.lookup_term(l_expr.name)
            return \
                Node(
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
                Node(
                    'field_access',
                    l_expr = root,
                    field = l_expr.field,
                    ty = field_ty,
                )
        elif l_expr.tag == 'deref':
            expr = self.check_expression(l_expr.expr)
            if not is_ptr(expr.ty):
                raise TypeError()
            return \
                Node(
                    'deref',
                    expr = expr,
                    ty = expr.ty.args[0],
                )
        elif l_expr.tag == 'array_access':
            root = self.check_l_expression(l_expr.l_expr)
            index = self.check_expression(l_expr.index)
            if not is_array(root.ty):
                raise TypeError()
            if not is_number(index.ty):
                raise TypeError()
            return \
                Node(
                    'array_access',
                    l_expr = root,
                    index = index,
                    ty = root.ty.args[0],
                )
        else:
            print(l_expr.tag)
            raise NotImplementedError()

    def check_function(self, decl):
        type_param_kinds = self.check_type_params(decl.type_params)
        type_params = \
            [(name, type_variable(name, kind))
                for name, kind in type_param_kinds]
        new_env = \
            Environment(
                dict(type_params),
                {},
                self,
            )

        args = [(name, new_env.check_type(ty)) for name, ty in decl.args]
        consume_type = None
        product_type = None
        if decl.consume_type:
            consume_type = new_env.check_type(decl.consume_type)
        if decl.product_type:
            product_type = new_env.check_type(decl.product_type)
        return_type = new_env.check_type(decl.return_type)

        if consume_type or product_type:
            fn_type = \
                function_type(
                    'plastic',
                    [ty for _, ty in args],
                    type_apply(coroutine, [
                        consume_type or void,
                        product_type or void,
                        return_type,
                    ]),
                )
        else:
            fn_type = \
                function_type(
                    'plastic',
                    [ty for _, ty in args],
                    return_type,
                )

        if len(type_params) > 0:
            self.term_bindings[decl.name] = \
                lambda_type(
                    type_param_kinds,
                    fn_type,
                )
        else:
            self.term_bindings[decl.name] = fn_type

        new_env = \
            Environment(
                {},
                dict(args),
                new_env,
                product_type,
                consume_type,
                return_type
            )
        body = new_env.check_body(decl.body)
        return \
            Node(
                'function',
                name = decl.name,
                type_params = type_params,
                args = args,
                consume_type = consume_type,
                product_type = product_type,
                return_type = return_type,
                body = body,
            )

    def check_constant(self, decl):
        expr = self.check_expression(decl.expr)
        self.term_bindings[decl.name] = expr.ty
        return \
            Node(
                'constant',
                name = decl.name,
                expr = expr,
            )

    def check_import(self, decl):
        module_type = ModuleType(decl.module, self.modules[decl.module])
        self.type_bindings[decl.module] = module_type
        self.term_bindings[decl.module] = module_type
        return \
            Node(
                'import',
                module = decl.module,
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
        elif decl.tag == 'constant':
            return self.check_constant(decl)
        elif decl.tag == 'import':
            return self.check_import(decl)
        raise NotImplementedError()

    def check_top_level_decls(self, decls):
        return [self.check_top_level_decl(decl) for decl in decls]

global_type_environment = {
    'void': void,
    'ptr': ptr,
    'array': array,
    'i8': char,
    'u8': number_type(False, 8),
    'i16': number_type(True, 16),
    'i32': number_type(True, 32),
    'u32': number_type(False, 32),
    'i64': number_type(True, 64),
    'u64': number_type(False, 64),
    'bool': boolean,
}

global_term_environment = {
    'null': opaque_ptr,
    'void': void,
    'true': boolean,
    'false': boolean,
    'resume':
        lambda_type(
            [('a', star), ('b', star), ('c', star)],
            function_type(
                'plastic',
                [
                    type_apply(
                        coroutine,
                        [
                            type_variable('a', star),
                            type_variable('b', star),
                            type_variable('c', star),
                        ]
                    ),
                    type_variable('a', star),
                ],
                type_variable('b', star),
            )
        )
}
