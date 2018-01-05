from ir import *
from sort_checker import SortAST
from environment import Environment
from types import SimpleNamespace
import copy

class KindError(Exception):
    pass

types = copy.deepcopy(SortAST.types)

types['Type'] = {
    'boolean': {},
    'void': {},
    'ptr': {},
    'array': {},
    'opaque_number': {},
    'opaque_ptr': {},
    'number_type': {
        'width': Int,
        'signed': Boolean,
    },
    'type_application': {
        'function': 'Type',
        'args': List('Type'),
    },
    'nat_literal': {
        'n': Int,
    },
    'tuple': {
        'n': Int,
    },
    'struct_type': {
        'module_name': Str,
        'name': Str,
        'type_args': List('Type'),
    },
    'enum_type': {
        'module_name': Str,
        'name': Str,
        'type_args': List('Type'),
    },
    'module_type': {
        'name': Str,
    },
    'function_type': {
        'calling_convention': Str,
        'arg_types': List('Type'),
        'return_type': 'Type',
    },
    'lambda_type': {
        'args': List(Tuple(Str, 'Kind')),
        'body': 'Type',
    },
    'type_variable': {
        'name': Str,
    },
}

for constructor in types['Type'].values():
    constructor['kind'] = 'Kind'

types['Kind']['function'] = {
    'arg_kinds': List('Kind'),
    'return_kind': 'Kind',
}
types['Kind']['module'] = {}

KindAST = Representation('KindAST', types)

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

void = Node('void', kind = star)
ptr = Node('ptr', kind = function_kind([star], star))
array = Node('array', kind = function_kind([star, nat], star))
boolean = Node('boolean', kind = star)

def number_type(signed, width):
    return Node('number_type', signed = signed, width = width, kind = star)

char = number_type(True, 8)

def lambda_type(args, body):
    return \
        Node(
            'lambda_type',
            args = args,
            body = body,
            kind = function_kind([kind for _, kind in args], body.kind)
        )

def struct_type(module_name, name, type_args):
    output = \
        Node(
            'struct_type',
            module_name = module_name,
            name = name,
            type_args = type_args,
            kind = star,
        )
    if len(type_args) > 0:
        args = [(arg.name, arg.kind) for arg in type_args]
        return lambda_type(args, output)
    else:
        return output

def enum_type(module_name, name, type_args):
    output = \
        Node(
            'enum_type',
            module_name = module_name,
            name = name,
            type_args = type_args,
            kind = star,
        )
    if len(type_args) > 0:
        args = [(arg.name, arg.kind) for arg in type_args]
        return lambda_type(args, output)
    else:
        return output

def module_type(module_name):
    return \
        Node(
            'module_type',
            name = module_name,
            kind = module_kind,
        )

def tuple_type(arity):
    return \
        Node(
            'tuple',
            n = arity,
            kind = function_kind([star] * arity, star),
        )

def type_variable(name, kind):
    return \
        Node(
            'type_variable',
            name = name,
            kind = kind,
        )

def nat_literal(n):
    return Node('nat_literal', n = n, kind = nat)

def function_type(calling_convention, arg_types, return_type):
    return \
        Node(
            'function_type',
            calling_convention = calling_convention,
            arg_types = arg_types,
            return_type = return_type,
            kind = star,
        )

def substitute(x, substitutions):
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

def type_apply(function, args):
    if function.kind.tag != 'function':
        raise KindError()
    if function.kind.arg_kinds != [arg.kind for arg in args]:
        raise KindError()
    if function.tag == 'lambda_type':
        names = [name for name, _ in function.args]
        substitutions = dict(zip(names, args))
        return substitute(function.body, substitutions)
    else:
        return \
            Node(
                'type_application',
                function = function,
                args = args,
                kind = function.kind.return_kind,
            )

def ptr_to(ty):
    return type_apply(ptr, [ty])

char_ptr = ptr_to(char)

def make_ty_vars(type_params):
    return [(name, type_variable(name, kind)) for name, kind in type_params]

transformer = SortAST.transformer(KindAST)

@transformer.case('Decl')
def ty(state, node):
    if node.tag == 'struct':
        ty_vars = make_ty_vars(node.type_params)
        state.env[node.name] = \
            struct_type(
                state.module_name,
                node.name,
                [ty for _, ty in ty_vars],
            )
        state.env = \
            Environment(
                dict(ty_vars),
                state.env,
            )
        output = transformer.default_transform('Decl', state, node)
        state.env = state.env.parent
        return output
    elif node.tag == 'enum':
        ty_vars = make_ty_vars(node.type_params)
        state.env[node.name] = \
            enum_type(
                state.module_name,
                node.name,
                [ty for _, ty in ty_vars],
            )
        state.env = \
            Environment(
                dict(ty_vars),
                state.env,
            )
        output = transformer.default_transform('Decl', state, node)
        state.env = state.env.parent
        return output
    elif node.tag == 'function':
        ty_vars = make_ty_vars(node.type_params)
        state.env = \
            Environment(
                dict(ty_vars),
                state.env,
            )
        output = transformer.default_transform('Decl', state, node)
        state.env = state.env.parent
        return output
    elif node.tag == 'import':
        state.env[node.module] = module_type(node.module)
    return transformer.default_transform('Decl', state, node)

@transformer.case('Type')
def ty(state, node):
    if node.tag == 'named_type':
        return state.env[node.name]
    elif node.tag == 'type_application':
        function = transformer.transform('Type', state, node.function)
        args = transformer.transform(List('Type'), state, node.args)
        return type_apply(function, args)
    elif node.tag == 'tuple':
        types = transformer.transform(List('Type'), state, node.types)
        return type_apply(tuple_type(len(types)), types)
    elif node.tag == 'type_number':
        return nat_literal(node.n)
    elif node.tag == 'field_access':
        ty = transformer.transform('Type', state, node.ty)
        if ty.kind.tag != 'module':
            raise KindError()
        return state.modules[ty.name][node.field]
    print(node)
    raise NotImplementedError()

def kind_check(module_name, modules, decls):
    initial_env = \
        Environment({
            'bool': boolean,
            'void': void,
            'ptr': ptr,
            'array': array,
            'i8': char,
            'u8': number_type(False, 8),
            'i16': number_type(True, 16),
            'u16': number_type(False, 16),
            'i32': number_type(True, 32),
            'u32': number_type(False, 32),
            'i64': number_type(True, 64),
            'u64': number_type(False, 64),
        })
    env = Environment({}, initial_env)
    state = \
        SimpleNamespace(
            module_name = module_name,
            modules = modules,
            env = env,
        )
    decls = \
        transformer.transform(
            List('Decl'),
            state,
            decls,
        )
    KindAST.check(List('Decl'), decls)
    return (decls, env.attributes)
