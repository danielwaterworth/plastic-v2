from ir import *
from kind_checker import *
from environment import Environment
from types import SimpleNamespace
import copy
import itertools

class NotImplementedError(Exception):
    pass

types = copy.deepcopy(KindAST.types)

for expr in ['Expr', 'LExpr']:
    del types[expr]['variable']
    types[expr]['primitive'] = {
        'name': Str,
    }
    types[expr]['global'] = {
        'module_name': Str,
        'name': Str,
    }
    types[expr]['local'] = {
        'i': Int,
    }
    types[expr]['arg'] = {
        'name': Str,
    }
    types[expr]['matched_variable'] = {
        'name': Str,
    }

for n in ['Expr', 'LExpr', 'Pattern']:
    for fields in types[n].values():
        fields['ty'] = 'Type'

del types['Statement']['let_statement']['name']
types['Statement']['let_statement']['num'] = Int

TypeAST = Representation('TypeAST', types)

opaque_ptr = Node('opaque_ptr', kind = star)
opaque_number = Node('opaque_number', kind = star)

def struct_constructor_type(module_name, name, type_args, field_types):
    output = \
        Node(
            'struct_type',
            module_name = module_name,
            name = name,
            type_args = type_args,
            kind = star,
        )
    output = \
        function_type(
            'plastic',
            field_types,
            output,
        )
    if len(type_args) > 0:
        args = [(arg.name, arg.kind) for arg in type_args]
        return lambda_type(args, output)
    else:
        return output

def enum_constructor_type(module_name, name, type_args, tys):
    output = \
        Node(
            'enum_type',
            module_name = module_name,
            name = name,
            type_args = type_args,
            kind = star,
        )
    output = \
        function_type(
            'plastic',
            tys,
            output,
        )
    if len(type_args) > 0:
        args = [(arg.name, arg.kind) for arg in type_args]
        return lambda_type(args, output)
    else:
        return output

def is_ptr(x):
    if x == opaque_ptr:
        return True
    if x.tag == 'type_application':
        if x.function == ptr:
            return True
    return False

def is_tuple(x):
    if x.tag == 'type_application':
        if x.function.tag == 'tuple':
            return True
    return False

def is_array(x):
    if x.tag == 'type_application':
        if x.function == array:
            return True
    return False

def is_number(x):
    return x.tag == 'number_type' or x == opaque_number

def substitutable_for(a, b):
    if a == b:
        return True

    if a == opaque_ptr:
        return is_ptr(b)
    if a == opaque_number:
        return b.tag == 'number_type'

    return False

def implicit_cast(expr, ty):
    if not substitutable_for(expr.ty, ty):
        raise TypeError()

    if expr.ty == ty:
        return expr

    return \
        Node(
            'cast',
            expr = expr,
            ty = ty,
        )

def explicit_cast(expr, ty):
    if is_number(expr.ty) and is_number(ty):
        pass
    elif is_ptr(expr.ty) and is_ptr(ty):
        pass
    else:
        raise TypeError()

    if expr.ty == ty:
        return expr

    return \
        Node(
            'cast',
            expr = expr,
            ty = ty,
        )

def cast_to_same_type(a, b):
    if a.ty == b.ty:
        return (a, b)
    elif substitutable_for(a.ty, b.ty):
        return (implicit_cast(a, b.ty), b)
    elif substitutable_for(b.ty, a.ty):
        return (a, implicit_cast(b, a.ty))
    else:
        raise Exception()

def implicit_cast_list(exprs, tys):
    return [implicit_cast(expr, ty) for expr, ty in zip(exprs, tys)]

transformer = KindAST.transformer(TypeAST)

@transformer.case('Decl')
def decl(state, node):
    if node.tag == 'struct':
        ty_vars = make_ty_vars(node.type_params)
        state.env[node.name] = \
            Node(
                'global',
                module_name = state.module_name,
                name = node.name,
                ty = \
                    struct_constructor_type(
                        state.module_name,
                        node.name,
                        [arg for _, arg in ty_vars],
                        [ty for _, ty in node.fields],
                    ),
            )
        state.structs[node.name] = \
            SimpleNamespace(
                fields = node.fields,
                type_params = node.type_params,
            )
    elif node.tag == 'enum':
        ty_vars = make_ty_vars(node.type_params)
        for name, tys in node.constructors:
            state.env[name] = \
                Node(
                    'global',
                    module_name = state.module_name,
                    name = node.name,
                    ty = \
                        enum_constructor_type(
                            state.module_name,
                            node.name,
                            [arg for _, arg in ty_vars],
                            tys,
                        ),
                )
        state.enums[node.name] = \
            SimpleNamespace(
                constructors = node.constructors,
                type_params = node.type_params,
            )
    elif node.tag == 'function':
        fn_type = \
            function_type(
                'plastic',
                [ty for _, ty in node.args],
                node.return_type,
            )
        if len(node.type_params) > 0:
            fn_type = \
                lambda_type(
                    node.type_params,
                    fn_type,
                )
        env = state.env
        args = {}
        for name, ty in node.args:
            args[name] = Node('arg', name = name, ty = ty)
        state.env = Environment(args, env)
        state.return_type = node.return_type
        state.var_nums = itertools.count()
        body = \
            transformer.default_transform(List('Statement'), state, node.body)
        del state.return_type
        state.env = env
        state.env[node.name] = \
            Node(
                'global',
                module_name = state.module_name,
                name = node.name,
                ty = fn_type,
            )
        return \
            Node(
                'function',
                name = node.name,
                type_params = node.type_params,
                constraints = node.constraints,
                args = node.args,
                return_type = node.return_type,
                body = body,
            )
    elif node.tag == 'constant':
        expr = transformer.transform('Expr', state, node.expr)
        state.env[node.name] = \
            Node(
                'global',
                module_name = state.module_name,
                name = node.name,
                ty = expr.ty,
            )
        return \
            Node(
                'constant',
                name = node.name,
                expr = expr,
            )
    elif node.tag == 'extern':
        arg_types = transformer.transform(List('Type'), state, node.arg_types)
        return_type = transformer.transform('Type', state, node.return_type)
        state.env[node.name] = \
            Node(
                'global',
                module_name = state.module_name,
                name = node.name,
                ty = function_type('c', arg_types, return_type),
            )
        return \
            Node(
                'extern',
                name = node.name,
                arg_types = arg_types,
                return_type = return_type,
            )
    elif node.tag == 'import':
        state.env[node.module] = \
            Node(
                'module',
                name = node.module,
                ty = module_type(node.module),
            )
    return transformer.default_transform('Decl', state, node)

def is_writable(l_expr):
    if l_expr.tag == 'local':
        return True
    elif l_expr.tag == 'global':
        return False
    elif l_expr.tag == 'primitive':
        return False
    elif l_expr.tag == 'field_access':
        return is_writable(l_expr.l_expr)
    elif l_expr.tag == 'array_access':
        return is_writable(l_expr.l_expr)
    print(l_expr.tag)
    raise NotImplementedError()

@transformer.case('Statement')
def statement(state, node):
    if node.tag == 'let_statement':
        expr = transformer.transform(OrNone('Expr'), state, node.expr)
        ty = transformer.transform(OrNone('Type'), state, node.ty)
        if expr and ty:
            expr = implicit_cast(expr, ty)
        var_num = next(state.var_nums)
        state.env[node.name] = \
            Node(
                'local',
                i = var_num,
                ty = ty or expr.ty,
            )
        return Node(
            'let_statement',
            num = var_num,
            ty = ty or expr.ty,
            expr = expr,
        )
    elif node.tag == 'if_statement':
        condition = transformer.transform('Expr', state, node.condition)
        if condition.ty != boolean:
            raise TypeError()
        env = state.env
        state.env = Environment({}, env)
        true_side = \
            transformer.transform(List('Statement'), state, node.true_side)

        state.env = Environment({}, env)
        false_side = \
            transformer.transform(List('Statement'), state, node.false_side)

        state.env = env
        return \
            Node(
                'if_statement',
                condition = condition,
                true_side = true_side,
                false_side = false_side,
            )
    elif node.tag == 'loop_statement':
        env = state.env
        state.env = Environment({}, env)

        body = \
            transformer.transform(List('Statement'), state, node.body)

        state.env = env
        return \
            Node(
                'loop_statement',
                body = body,
            )
    elif node.tag == 'match':
        expr = transformer.transform(OrNone('Expr'), state, node.expr)
        state.current_ty = expr.ty
        env = state.env
        matches = []
        for pattern, statements in node.matches:
            state.env = Environment({}, env)
            pattern = transformer.transform('Pattern', state, pattern)
            statements = \
                transformer.transform(List('Statement'), state, statements)
            matches.append((pattern, statements))
        del state.current_ty
        state.env = env
        return \
            Node(
                'match',
                expr = expr,
                matches = matches,
            )
    elif node.tag == 'assignment':
        expr = transformer.transform('Expr', state, node.expr)
        l_expr = transformer.transform('LExpr', state, node.l_expr)
        if not is_writable(l_expr):
            raise TypeError()
        return \
            Node(
                'assignment',
                l_expr = l_expr,
                expr = expr,
            )
    return transformer.default_transform('Statement', state, node)

def lookup_enum(state, module_name, name, type_args):
    if state.module_name == module_name:
        struct = state.enums[name]
    else:
        struct = state.modules[module_name].enums[name]
    constructors = {}
    substitutions = \
        dict(zip([name for name, _ in struct.type_params], type_args))
    for constructor_name, tys in struct.constructors:
        constructors[constructor_name] = \
            [substitute(ty, substitutions) for ty in tys]
    return \
        SimpleNamespace(
            constructors = constructors,
        )

def lookup_struct(state, module_name, name, type_args):
    if state.module_name == module_name:
        struct = state.structs[name]
    else:
        struct = state.modules[module_name].structs[name]
    fields = {}
    substitutions = \
        dict(zip([name for name, _ in struct.type_params], type_args))
    for field_name, field_ty in struct.fields:
        fields[field_name] = substitute(field_ty, substitutions)
    return \
        SimpleNamespace(
            fields = fields,
        )

@transformer.case('Expr')
def expr(state, node):
    if node.tag == 'variable':
        return state.env[node.name]
    elif node.tag == 'application':
        function = transformer.transform('Expr', state, node.function)
        args = transformer.transform(List('Expr'), state, node.args)
        args = implicit_cast_list(args, function.ty.arg_types)
        return \
            Node(
                'application',
                function = function,
                args = args,
                ty = function.ty.return_type,
            )
    elif node.tag == 'number_literal':
        return \
            Node(
                'number_literal',
                n = node.n,
                ty = opaque_number,
            )
    elif node.tag == 'uminus':
        expr = transformer.transform('Expr', state, node.expr)
        return \
            Node(
                'uminus',
                expr = expr,
                ty = expr.ty,
            )
    elif node.tag == 'field_access':
        x = transformer.transform('Expr', state, node.x)
        field = node.field
        if x.ty.tag == 'module_type':
            return state.modules[x.ty.name].values[field]
        elif x.ty.tag == 'struct_type':
            struct = \
                lookup_struct(
                    state,
                    x.ty.module_name,
                    x.ty.name,
                    x.ty.type_args,
                )
            return \
                Node(
                    'field_access',
                    x = x,
                    field = field,
                    ty = struct.fields[field],
                )
        else:
            raise TypeError()
    elif node.tag == 'apply_type_args':
        function = transformer.transform('Expr', state, node.function)
        if function.tag not in ['primitive', 'global']:
            raise TypeError()
        ty = type_apply(function.ty, node.args)
        return \
            Node(
                'apply_type_args',
                function = function,
                args = node.args,
                ty = ty,
            )
    elif node.tag == 'address_of':
        expr = transformer.transform('LExpr', state, node.expr)
        return \
            Node(
                'address_of',
                expr = expr,
                ty = ptr_to(expr.ty),
            )
    elif node.tag == 'string_literal':
        return \
            Node(
                'string_literal',
                string = node.string,
                ty = char_ptr,
            )
    elif node.tag == 'binary_operator':
        a, b = \
            transformer.transform(
                Tuple('Expr', 'Expr'),
                state,
                (node.a, node.b),
            )
        a, b = cast_to_same_type(a, b)
        if a.ty.tag != 'number_type':
            raise TypeError()
        return \
            Node(
                'binary_operator',
                operator = node.operator,
                a = a,
                b = b,
                ty = a.ty,
            )
    elif node.tag == 'comparison_operator':
        a, b = \
            transformer.transform(
                Tuple('Expr', 'Expr'),
                state,
                (node.a, node.b),
            )
        a, b = cast_to_same_type(a, b)
        if a.ty.tag != 'number_type':
            raise TypeError()
        return \
            Node(
                'comparison_operator',
                operator = node.operator,
                a = a,
                b = b,
                ty = boolean,
            )
    elif node.tag == 'equality_operator':
        a, b = \
            transformer.transform(
                Tuple('Expr', 'Expr'),
                state,
                (node.a, node.b),
            )
        a, b = cast_to_same_type(a, b)
        return \
            Node(
                'equality_operator',
                operator = node.operator,
                a = a,
                b = b,
                ty = boolean,
            )
    elif node.tag == 'deref':
        expr = transformer.transform('Expr', state, node.expr)
        if not is_ptr(expr.ty):
            raise TypeError()
        return \
            Node(
                'deref',
                expr = expr,
                ty = expr.ty.args[0],
            )
    elif node.tag == 'array_access':
        expr, index = \
            transformer.transform(
                Tuple('Expr', 'Expr'),
                state,
                (node.expr, node.index)
            )
        if not is_array(expr.ty):
            raise TypeError()
        if not is_number(index.ty):
            raise TypeError()
        return \
            Node(
                'array_access',
                expr = expr,
                index = index,
                ty = expr.ty.args[0],
            )
    elif node.tag == 'cast':
        expr = \
            transformer.transform(
                'Expr',
                state,
                node.expr,
            )
        return explicit_cast(expr, node.ty)
    raise NotImplementedError(node.tag)

@transformer.case('LExpr')
def lexpr(state, node):
    if node.tag == 'variable':
        return state.env[node.name]
    elif node.tag == 'array_access':
        root, index = \
            transformer.transform(
                Tuple('LExpr', 'Expr'),
                state,
                (node.l_expr, node.index)
            )
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
        raise NotImplementedError(node.tag)

@transformer.case('Pattern')
def pattern(state, node):
    if node.tag == 'wildcard':
        state.env[node.name] = \
            Node(
                'matched_variable',
                name = node.name,
                ty = state.current_ty,
            )
        return \
            Node(
                'wildcard',
                name = node.name,
                ty = state.current_ty,
            )
    elif node.tag == 'constructor':
        start_ty = state.current_ty
        if start_ty.tag != 'enum_type':
            raise TypeError()
        enum = \
            lookup_enum(
                state,
                state.current_ty.module_name,
                state.current_ty.name,
                state.current_ty.type_args,
            )
        tys = enum.constructors[node.name]
        args = []
        for arg, ty in zip(node.args, tys):
            state.current_ty = ty
            arg = transformer.transform('Pattern', state, arg)
            args.append(arg)
        node.current_ty = start_ty
        return \
            Node(
                'constructor',
                name = node.name,
                args = args,
                ty = start_ty,
            )
    else:
        raise NotImplementedError()

primitive = define_constructor('primitive', ['name', 'ty'])

def type_check(module_name, modules, decls):
    initial_env = \
        Environment({
            'null': primitive('null', opaque_ptr),
            'void': primitive('void', void),
            'true': primitive('true', boolean),
            'false': primitive('false', boolean),
        })
    env = Environment({}, initial_env)
    structs = {}
    enums = {}
    state = \
        SimpleNamespace(
            module_name = module_name,
            modules = modules,
            env = env,
            structs = structs,
            enums = enums,
        )
    decls = \
        transformer.transform(
            List('Decl'),
            state,
            decls,
        )
    TypeAST.check(List('Decl'), decls)
    interface = \
        SimpleNamespace(
            values = env.attributes,
            structs = structs,
            enums = enums,
        )
    return (decls, interface)
