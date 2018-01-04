import itertools
import type_checker
import collections

class CGASTNode:
    def __init__(self, tag, **kwargs):
        self.tag = tag
        self.attributes = kwargs
        for key, value in kwargs.items():
            setattr(self, key, value)

    @property
    def sorted_attr_tuple(self):
        return tuple(sorted(self.attributes.items()))

    def __repr__(self):
        return "CGASTNode(%s, %s)" % (repr(self.tag), repr(self.attributes))

    def __hash__(self):
        return hash((self.tag, self.sorted_attr_tuple))

    def __eq__(self, other):
        return \
            self.tag == other.tag and \
            self.sorted_attr_tuple == other.sorted_attr_tuple

def number(width):
    return  \
        CGASTNode(
            'number',
            width = width,
        )

def named_type(module_name, name, version=0):
    return  \
        CGASTNode(
            'named_type',
            module_name = module_name,
            name = name,
            version = version,
        )

def ptr_to(ty):
    return \
        CGASTNode(
            'ptr_to',
            ty = ty,
        )

def array_of(of, size):
    return \
        CGASTNode(
            'array',
            of = of,
            size = size,
        )

def tuple_of(types):
    return \
        CGASTNode(
            'tuple',
            types = tuple(types),
        )

def func(arg_types, return_type):
    return \
        CGASTNode(
            'func',
            arg_types = arg_types,
            return_type = return_type,
        )

def nat(n):
    return \
        CGASTNode(
            'nat',
            n = n,
        )

void = \
    CGASTNode(
        'void'
    )
boolean = number(1)
byte = number(8)
byte_ptr = ptr_to(byte)

def concat(xs):
    return [x for ys in xs for x in ys]

def return_(ty, value):
    return \
        CGASTNode(
            'return',
            ty = ty,
            value = value,
        )

def unconditional_branch(block):
    return \
        CGASTNode(
            'unconditional_branch',
            to = block,
        )

def conditional_branch(condition, true_block, false_block):
    return \
        CGASTNode(
            'conditional_branch',
            condition = condition,
            true_block = true_block,
            false_block = false_block,
        )

class GenericFunction:
    def __init__(self, module_name, name, decl):
        self.decl = decl
        self.module_name = module_name
        self.name = name
        self.versions = itertools.count()
        self.specializations = {}

    def specialize(self, code_generator, args):
        args = tuple(args)
        if not args in self.specializations:
            scope = dict(code_generator.type_scope)

            for (name, _), ty in zip(self.decl.type_params, args):
                code_generator.type_scope[name] = ty

            arg_types = \
                [code_generator.generate_type(ty) for _, ty in self.decl.args]

            return_type = code_generator.generate_type(self.decl.return_type)
            code_generator.type_scope = scope

            version = next(self.versions)
            code_generator.generate_function_specialization(
                self.decl,
                args,
                version,
            )

            llvm_name = "@%s$$%s.%d" % (self.module_name, self.name, version)
            self.specializations[args] = \
                ptr_to(func(arg_types, return_type)), llvm_name
        return self.specializations[args]

class FunctionWriter:
    def __init__(self, code_generator):
        self.code_generator = code_generator
        self.current_basic_block = None
        self.basic_blocks = []
        self.variable_names = map(lambda i: "%%var.%d" % i, itertools.count())
        self.block_names = map(lambda i: "block.%d" % i, itertools.count())
        self.new_basic_block()
        self.scope = {}
        self.arg_dict = {}

    def new_basic_block(self):
        self.current_basic_block = \
            CGASTNode(
                'basic_block',
                label = next(self.block_names),
                instructions = [],
                terminator = None,
            )
        self.basic_blocks.append(self.current_basic_block)

    def alloca(self, ty):
        var = next(self.variable_names)
        instruction = \
            CGASTNode(
                'alloca',
                ty = ty,
                dst = var,
            )
        first_basic_block = self.basic_blocks[0]
        first_basic_block.instructions = \
            [instruction] + first_basic_block.instructions
        return var

    def store(self, dst, ty, source):
        self.current_basic_block.instructions.append(
            CGASTNode(
                'store',
                source = source,
                dst = dst,
                ty = ty,
            )
        )

    def load(self, ty, source):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'load',
                source = source,
                dst = dst,
                ty = ty,
            )
        )
        return dst

    def call(self, function, args, return_type):
        if return_type.tag == 'void':
            self.current_basic_block.instructions.append(
                CGASTNode(
                    'call',
                    function = function,
                    args = args,
                    return_type = return_type,
                    dst = None,
                )
            )
        else:
            dst = next(self.variable_names)
            self.current_basic_block.instructions.append(
                CGASTNode(
                    'call',
                    function = function,
                    args = args,
                    return_type = return_type,
                    dst = dst,
                )
            )
            return dst

    def binop(self, op, a, b, ty):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'binop',
                op = op,
                a = a,
                b = b,
                ty = ty,
                dst = dst,
            )
        )
        return dst

    def getelementptr(self, dst_type, source_type, value, offset):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'getelementptr',
                source_type = source_type,
                dst_type = dst_type,
                value = value,
                offset = offset,
                dst = dst,
            )
        )
        return dst

    def cast(self, mode, from_ty, to_ty, value):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'cast',
                mode = mode,
                dst = dst,
                from_ty = from_ty,
                to_ty = to_ty,
                value = value,
            )
        )
        return dst

    def zext(self, from_ty, to_ty, value):
        return self.cast('zext', from_ty, to_ty, value)

    def sext(self, from_ty, to_ty, value):
        return self.cast('sext', from_ty, to_ty, value)

    def truncate(self, from_ty, to_ty, value):
        return self.cast('trunc', from_ty, to_ty, value)

    def bitcast(self, from_ty, to_ty, value):
        return self.cast('bitcast', from_ty, to_ty, value)

    def icmp(self, mode, ty, a, b):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'icmp',
                dst = dst,
                mode = mode,
                ty = ty,
                a = a,
                b = b,
            )
        )
        return dst

    def extractvalue(self, ty, value, indices):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'extractvalue',
                dst = dst,
                ty = ty,
                value = value,
                indices = indices,
            )
        )
        return dst

    def insertvalue(self, struct_ty, struct_val, field_ty, field_val, indices):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'insertvalue',
                dst = dst,
                struct_ty = struct_ty,
                struct_val = struct_val,
                field_ty = field_ty,
                field_val = field_val,
                indices = indices,
            )
        )
        return dst

    def not_(self, value):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'not',
                dst = dst,
                value = value,
            )
        )
        return dst

    def global_string_constant(self, string):
        return self.code_generator.global_string_constant(string)

    def generate_type(self, ty):
        return self.code_generator.generate_type(ty)

    def generate_type_list(self, tys):
        return self.code_generator.generate_type_list(tys)

    def generate_variable(self, name, module_name):
        if name == 'void':
            return void, 'void'
        elif name == 'true':
            return boolean, '1'
        elif name == 'false':
            return boolean, '0'
        elif name == 'null':
            return byte_ptr, 'null'
        elif (module_name, name) in self.code_generator.functions:
            return self.code_generator.functions[(module_name, name)]
        elif (module_name, name) in self.code_generator.constants:
            ty, ptr = self.code_generator.constants[(module_name, name)]
            return ty, self.load(ty, ptr)
        elif name in self.scope:
            ty, ptr = self.scope[name]
            return ty, self.load(ty, ptr)
        elif name in self.arg_dict:
            ty, ptr = self.arg_dict[name]
            return ty, self.load(ty, ptr)
        raise NotImplementedError()

    def generate_variable_l(self, expr):
        module_name = self.code_generator.module_name
        if expr.name in self.scope:
            ty, ptr = self.scope[expr.name]
            return ptr_to(ty), ptr
        elif (module_name, expr.name) in self.code_generator.constants:
            ty, ptr = self.code_generator.constants[(module_name, expr.name)]
            return ptr_to(ty), ptr
        raise NotImplementedError()

    def generate_array_access_l(self, expr):
        array_ty, ptr = self.generate_l_expr(expr.l_expr)
        index_ty, index = self.generate_expression(expr.index)
        value = self.getelementptr(array_ty.ty, array_ty, ptr, ['0', index])
        return ptr_to(array_ty.ty.of), value

    def generate_field_access_l(self, expr):
        struct_ty, ptr = self.generate_l_expr(expr.l_expr)
        field = expr.field
        key = struct_ty.ty.module_name, struct_ty.ty.name, ()
        index, field_ty = self.code_generator.struct_fields[key][field]
        field_ptr = \
            self.getelementptr(struct_ty.ty, struct_ty, ptr, ['0', str(index)])
        return ptr_to(field_ty), field_ptr

    def generate_array_access(self, expr):
        array_ty, value = self.generate_expression(expr.expr)
        index_ty, index = self.generate_expression(expr.index)
        ptr = self.alloca(array_ty)
        self.store(ptr, array_ty, value)
        element_ptr = \
            self.getelementptr(array_ty, ptr_to(array_ty), ptr, ['0', index])
        return array_ty.of, self.load(array_ty.of, element_ptr)

    def generate_application(self, expr):
        ty, function = self.generate_expression(expr.function)
        args = self.generate_expressions(expr.args)
        return_type = ty.ty.return_type
        return return_type, self.call(function, args, return_type)

    def generate_yield_expression(self, expr):
        raise NotImplementedError()

    def generate_struct_field_access(self, expr):
        struct_ty, value = self.generate_expression(expr.x)
        field = expr.field
        key = struct_ty.module_name, struct_ty.name, ()
        index, field_ty = self.code_generator.struct_fields[key][field]
        return field_ty, self.extractvalue(struct_ty, value, [index])

    def generate_module_field_access(self, expr):
        return self.generate_variable(expr.field, expr.x.ty.name)

    def generate_character_literal(self, expr):
        raise NotImplementedError()

    def generate_cast(self, expr):
        from_ty, value = self.generate_expression(expr.expr)
        to_ty = self.generate_type(expr.ty)
        if from_ty.tag == 'number':
            assert to_ty.tag == 'number'
            if from_ty.width == to_ty.width:
                return to_ty, value
            elif from_ty.width > to_ty.width:
                return to_ty, self.truncate(from_ty, to_ty, value)
            else:
                if expr.ty.signed:
                    return to_ty, self.sext(from_ty, to_ty, value)
                else:
                    return to_ty, self.zext(from_ty, to_ty, value)
        else:
            assert from_ty.tag == 'ptr_to'
            return to_ty, self.bitcast(from_ty, to_ty, value)

    def generate_comparison(self, expr):
        a_ty, a = self.generate_expression(expr.a)
        b_ty, b = self.generate_expression(expr.b)
        if expr.tag == 'equality_operator':
            ops = {
                '==': 'eq',
                '!=': 'ne',
            }
            if a_ty.tag == 'number' or a_ty.tag == 'ptr_to':
                return boolean, self.icmp(ops[expr.operator], a_ty, a, b)
            else:
                raise NotImplementedError()
        elif expr.tag == 'comparison_operator':
            if expr.a.ty.signed:
                ops = {
                    '<': 'slt',
                    '>': 'sgt',
                    '<=': 'sle',
                    '>=': 'sge',
                }
            else:
                ops = {
                    '<': 'ult',
                    '>': 'ugt',
                    '<=': 'ule',
                    '>=': 'uge',
                }
            return boolean, self.icmp(ops[expr.operator], a_ty, a, b)
        else:
            raise NotImplementedError()

    def generate_operator(self, expr):
        ops = {
            '+': 'add',
            '-': 'sub',
            '|': 'or',
            '&': 'and',
        }
        ty, a = self.generate_expression(expr.a)
        _, b = self.generate_expression(expr.b)
        return ty, self.binop(ops[expr.operator], a, b, ty)

    def generate_number_literal(self, expr):
        ty = self.generate_type(expr.ty)
        return ty, str(expr.n)

    def generate_string_literal(self, expr):
        ty, value = self.global_string_constant(expr.string)
        return byte_ptr, self.getelementptr(ty, ptr_to(ty), value, ["0", "0"])

    def generate_apply_type_args(self, expr):
        _, function = self.generate_expression(expr.function)
        assert isinstance(function, GenericFunction)
        args = self.generate_type_list(expr.args)
        return function.specialize(self.code_generator, args)

    def generate_address_of(self, expr):
        return self.generate_l_expr(expr.expr)

    def generate_tuple(self, expr):
        exprs = self.generate_expressions(expr.values)
        types = [ty for ty, _ in exprs]
        output_type = tuple_of(types)
        output = 'undef'
        for index, (ty, value) in enumerate(exprs):
            output = \
                self.insertvalue(output_type, output, ty, value, [str(index)])
        return output_type, output

    def generate_expression(self, expr):
        if expr.tag == 'variable':
            output = \
                self.generate_variable(
                    expr.name,
                    self.code_generator.module_name
                )
        elif expr.tag == 'application':
            output = self.generate_application(expr)
        elif expr.tag == 'yield_expression':
            output = self.generate_yield_expression(expr)
        elif expr.tag == 'struct_field_access':
            output = self.generate_struct_field_access(expr)
        elif expr.tag == 'module_field_access':
            output = self.generate_module_field_access(expr)
        elif expr.tag == 'character_literal':
            output = self.generate_character_literal(expr)
        elif expr.tag == 'cast':
            output = self.generate_cast(expr)
        elif expr.tag in ['comparison_operator', 'equality_operator']:
            output = self.generate_comparison(expr)
        elif expr.tag == 'binary_operator':
            output = self.generate_operator(expr)
        elif expr.tag == 'number_literal':
            output = self.generate_number_literal(expr)
        elif expr.tag == 'string_literal':
            output = self.generate_string_literal(expr)
        elif expr.tag == 'apply_type_args':
            output = self.generate_apply_type_args(expr)
        elif expr.tag == 'address_of':
            output = self.generate_address_of(expr)
        elif expr.tag == 'deref':
            ty, ptr = self.generate_expression(expr.expr)
            assert ty.tag == 'ptr_to'
            return ty.ty, self.load(ty.ty, ptr)
        elif expr.tag == 'not':
            ty, value = self.generate_expression(expr.expr)
            return ty, self.not_(value)
        elif expr.tag == 'uminus':
            ty, value = self.generate_expression(expr.expr)
            return ty, self.binop('sub', '0', value, ty)
        elif expr.tag == 'array_access':
            output = self.generate_array_access(expr)
        elif expr.tag == 'tuple':
            output = self.generate_tuple(expr)
        else:
            print(expr.tag)
            raise NotImplementedError()
        assert type(output) == tuple, expr.tag
        assert len(output) == 2, expr.tag
        return output

    def generate_expressions(self, exprs):
        return [self.generate_expression(expr) for expr in exprs]

    def generate_l_expr(self, expr):
        if expr.tag == 'variable':
            output = self.generate_variable_l(expr)
        elif expr.tag == 'field_access':
            output = self.generate_field_access_l(expr)
        elif expr.tag == 'string_literal':
            output = self.generate_string_literal_l(expr)
        elif expr.tag == 'array_access':
            output = self.generate_array_access_l(expr)
        elif expr.tag == 'deref':
            output = self.generate_expression(expr.expr)
        else:
            raise NotImplementedError()
        assert type(output) == tuple, expr.tag
        assert len(output) == 2, expr.tag
        return output

    def generate_let_statement(self, statement):
        ty = self.generate_type(statement.ty)
        ptr = self.alloca(ty)
        self.scope[statement.name] = ty, ptr
        if statement.expr:
            _, value = self.generate_expression(statement.expr)
            self.store(ptr, ty, value)

    def generate_loop_statement(self, statement):
        scope = dict(self.scope)
        prev_block = self.current_basic_block
        self.new_basic_block()
        to_block = self.current_basic_block

        self.new_basic_block()
        after_block = self.current_basic_block

        self.break_to = after_block.label
        self.current_basic_block = to_block
        self.generate_statements(statement.body)
        end_block = self.current_basic_block

        terminator = unconditional_branch(to_block.label)
        prev_block.terminator = terminator
        end_block.terminator = terminator

        self.current_basic_block = after_block
        self.scope = scope

    def generate_assignment(self, statement):
        l_ty, l_expr = self.generate_l_expr(statement.l_expr)
        ty, expr = self.generate_expression(statement.expr)
        self.store(l_expr, ty, expr)

    def generate_if_statement(self, statement):
        scope = dict(self.scope)

        _, condition = self.generate_expression(statement.condition)

        prev_block = self.current_basic_block
        self.new_basic_block()
        true_block = self.current_basic_block
        self.new_basic_block()
        false_block = self.current_basic_block
        self.new_basic_block()
        after_block = self.current_basic_block

        prev_block.terminator = \
            conditional_branch(
                condition,
                true_block.label,
                false_block.label,
            )

        self.scope = dict(scope)
        self.current_basic_block = true_block
        self.generate_statements(statement.true_side)
        if self.current_basic_block:
            self.current_basic_block.terminator = \
                unconditional_branch(after_block.label)

        self.scope = dict(scope)
        self.current_basic_block = false_block
        self.generate_statements(statement.false_side)
        if self.current_basic_block:
            self.current_basic_block.terminator = \
                unconditional_branch(after_block.label)

        self.scope = dict(scope)
        self.current_basic_block = after_block

    def generate_break(self):
        break_block = self.current_basic_block
        self.new_basic_block()
        break_block.terminator = unconditional_branch(self.break_to)

    def generate_return_statement(self, statement):
        ty, value = self.generate_expression(statement.expr)
        self.current_basic_block.terminator = \
            return_(ty, value)
        self.current_basic_block = None

    def generate_match_statement(self, statement):
        ty, expr = self.generate_expression(statement.expr)
        ptr = self.alloca(ty)
        self.store(ptr, ty, expr)
        prev = self.current_basic_block
        self.new_basic_block()
        next_block = self.current_basic_block
        self.new_basic_block()
        after_block = self.current_basic_block
        for pattern, body in reversed(statement.matches):
            self.new_basic_block()
            pattern_block = self.current_basic_block
            cond = self.generate_pattern(pattern, ptr, ty)

            self.new_basic_block()
            body_block = self.current_basic_block
            self.generate_pattern_variables(pattern, ptr, ty)
            self.generate_statements(body)
            self.current_basic_block.terminator = \
                unconditional_branch(after_block.label)
            pattern_block.terminator = \
                conditional_branch(cond, body_block.label, next_block.label)
            next_block = pattern_block
        prev.terminator = unconditional_branch(next_block.label)
        self.current_basic_block = after_block

    def generate_pattern_variables(self, pattern, ptr, ty):
        if pattern.tag == 'wildcard':
            self.scope[pattern.name] = ty, ptr
        elif pattern.tag == 'constructor':
            constructor_ptr = \
                self.getelementptr(ty, ptr_to(ty), ptr, ["0", "1"])
            constructor_ty = named_type(pattern.ty.module_name, pattern.name)
            key = pattern.ty.module_name, pattern.ty.name, ()
            size = \
                self.code_generator.calculate_enum_size(
                    self.code_generator.enums[key]
                )
            data_ptr = \
                self.bitcast(
                    ptr_to(array_of(byte, size)),
                    ptr_to(constructor_ty),
                    constructor_ptr
                )
            for i, arg in enumerate(pattern.args):
                arg_ty = self.generate_type(arg.ty)
                arg_ptr = \
                    self.getelementptr(
                        constructor_ty,
                        ptr_to(constructor_ty),
                        data_ptr,
                        ["0", str(i)]
                    )
                self.generate_pattern_variables(arg, arg_ptr, arg_ty)
        else:
            raise NotImplementedError()

    def generate_pattern(self, pattern, ptr, ty):
        if pattern.tag == 'wildcard':
            return '1'
        elif pattern.tag == 'constructor':
            constructor_tags = self.code_generator.constructor_tags
            expected_tag = str(constructor_tags[pattern.ty.name][pattern.name])
            tag_ptr = self.getelementptr(ty, ptr_to(ty), ptr, ["0", "0"])
            actual_tag = self.load(byte, tag_ptr)
            output = self.icmp('eq', byte, expected_tag, actual_tag)
            constructor_ptr = \
                self.getelementptr(ty, ptr_to(ty), ptr, ["0", "1"])
            constructor_ty = named_type(pattern.ty.module_name, pattern.name)
            key = pattern.ty.module_name, pattern.ty.name, ()
            size = \
                self.code_generator.calculate_enum_size(
                    self.code_generator.enums[key],
                )
            data_ptr = \
                self.bitcast(
                    ptr_to(array_of(byte, size)),
                    ptr_to(constructor_ty),
                    constructor_ptr,
                )
            for i, arg in enumerate(pattern.args):
                arg_ty = self.generate_type(arg.ty)
                arg_ptr = \
                    self.getelementptr(
                        constructor_ty,
                        ptr_to(constructor_ty),
                        data_ptr,
                        ["0", str(i)],
                    )
                res = self.generate_pattern(arg, arg_ptr, arg_ty)
                output = self.binop('and', output, res, boolean)
            return output
        else:
            raise NotImplementedError()

    def generate_statement(self, statement):
        if statement.tag == 'let_statement':
            self.generate_let_statement(statement)
        elif statement.tag == 'loop_statement':
            self.generate_loop_statement(statement)
        elif statement.tag == 'assignment':
            self.generate_assignment(statement)
        elif statement.tag == 'if_statement':
            self.generate_if_statement(statement)
        elif statement.tag == 'expr_statement':
            self.generate_expression(statement.expr)
        elif statement.tag == 'break':
            self.generate_break()
        elif statement.tag == 'return':
            self.generate_return_statement(statement)
        elif statement.tag == 'match':
            self.generate_match_statement(statement)
        else:
            raise NotImplementedError()

    def generate_statements(self, statements):
        for statement in statements:
            self.generate_statement(statement)

class CodeGenerator:
    def __init__(self):
        self.typed_struct_decls = {}
        self.typed_enum_decls = {}
        self.string_names = map(lambda i: "@string.%d" % i, itertools.count())
        self.function_names = \
            map(lambda i: "@function.%d" % i, itertools.count())
        self.strings = []
        self.functions = {}
        self.constants = {}
        self.initializers = []
        self.struct_fields = {}
        self.data_version_counters = \
            collections.defaultdict(lambda : itertools.count())
        self.data_versions = {}
        self.enums = {}
        self.constructor_tags = {}
        self.declare_decls = []
        self.global_decls = []
        self.struct_decls = []
        self.function_decls = []
        self.module_name = ""
        self.type_scope = {}

    def global_string_constant(self, string):
        value = next(self.string_names)
        ty = array_of(byte, len(string))
        self.strings.append(
            CGASTNode(
                'string',
                value = string,
                ty = ty,
                name = value,
            )
        )
        return ty, value

    def generate_type(self, ty):
        if type_checker.is_ptr(ty):
            return ptr_to(self.generate_type(ty.args[0]))
        elif type_checker.is_array(ty):
            of = self.generate_type(ty.args[0])
            n = self.generate_type(ty.args[1])
            return array_of(of, n.n)
        elif type_checker.is_tuple(ty):
            return tuple_of(self.generate_type_list(ty.args))
        elif ty.tag == 'struct_type':
            if len(ty.type_args) > 0:
                type_args = self.generate_type_list(ty.type_args)
                version = \
                    self.struct_version(ty.module_name, ty.name, type_args)
                return named_type(ty.module_name, ty.name, version)
            else:
                return named_type(ty.module_name, ty.name)
        elif ty.tag == 'enum_type':
            if len(ty.type_args) > 0:
                type_args = self.generate_type_list(ty.type_args)
                version = \
                    self.enum_version(ty.module_name, ty.name, type_args)
                return named_type(ty.module_name, ty.name, version)
            else:
                return named_type(ty.module_name, ty.name)
        elif ty == type_checker.void:
            return void
        elif ty == type_checker.boolean:
            return number(1)
        elif ty == type_checker.opaque_number_type:
            return number(64)
        elif ty.tag == 'lambda_type':
            raise NotImplementedError()
        elif ty.tag == 'type_variable':
            return self.type_scope[ty.name]
        elif ty.tag == 'nat_literal':
            return nat(ty.n)
        elif ty.tag == 'number_type':
            return number(ty.width)
        print(ty)
        raise NotImplementedError()

    def generate_type_list(self, tys):
        return [self.generate_type(ty) for ty in tys]

    def generate_extern(self, decl):
        return_type = self.generate_type(decl.return_type)
        arg_types = self.generate_type_list(decl.arg_types)

        key = self.module_name, decl.name
        self.functions[key] = \
            ptr_to(func(arg_types, return_type)), '@' + decl.name

        self.declare_decls.append(
            CGASTNode(
                'declare',
                name = '@' + decl.name,
                return_type = return_type,
                arg_types = arg_types,
            )
        )

    def struct_version(self, module_name, name, type_args):
        key = (module_name, name, tuple(type_args))
        if not key in self.data_versions:
            version = \
                next(self.data_version_counters[(module_name, name)])
            self.data_versions[key] = version
            self.generate_struct_struct(module_name, name, type_args, version)
        return self.data_versions[key]

    def enum_version(self, module_name, name, type_args):
        key = (module_name, name, tuple(type_args))
        if not key in self.data_versions:
            version = \
                next(self.data_version_counters[(module_name, name)])
            self.data_versions[key] = version
            self.generate_enum_structs(module_name, name, type_args, version)
        return self.data_versions[key]

    def generate_struct_constructor(self, module_name, name, type_args):
        decl = self.typed_struct_decls[(module_name, name)]
        if len(type_args) != 0:
            raise NotImplementedError()

        fields = \
            [(name, self.generate_type(ty)) for name, ty in decl.fields]

        return_type = named_type(self.module_name, decl.name)

        arg_types = [ty for _, ty in fields]
        arg_names = ['%' + name for name, _ in fields]

        key = self.module_name, decl.name
        llvm_name = "@%s$$%s" % key

        function_writer = FunctionWriter(self)
        output_ptr = function_writer.alloca(return_type)

        field_descriptors = {}
        for i, (name, ty) in zip(itertools.count(), fields):
            field_descriptors[name] = (i, ty)
        key = module_name, decl.name, ()
        self.struct_fields[key] = field_descriptors

        for i, name, ty in zip(itertools.count(), arg_names, arg_types):
            field_ptr = \
                function_writer.getelementptr(
                    return_type,
                    ptr_to(return_type),
                    output_ptr,
                    ['0', str(i)]
                )
            function_writer.store(field_ptr, ty, name)
        output = function_writer.load(return_type, output_ptr)
        function_writer.current_basic_block.terminator = \
            return_(return_type, output)

        self.function_decls.append(
            CGASTNode(
                'define',
                name = llvm_name,
                return_type = return_type,
                args = list(zip(arg_types, arg_names)),
                basic_blocks = function_writer.basic_blocks,
                linkage = [],
            ),
        )

    def generate_struct_struct(self, module_name, name, type_args, version):
        decl = self.typed_struct_decls[(module_name, name)]

        scope = dict(self.type_scope)
        for (name, _), ty in zip(decl.type_params, type_args):
            self.type_scope[name] = ty

        fields = \
            [(name, self.generate_type(ty)) for name, ty in decl.fields]

        self.type_scope = scope

        field_descriptors = {}
        for i, (name, ty) in zip(itertools.count(), fields):
            field_descriptors[name] = (i, ty)
        key = module_name, decl.name, tuple(type_args)
        self.struct_fields[key] = field_descriptors

        self.struct_decls.append(
            CGASTNode(
                'struct',
                module_name = self.module_name,
                name = decl.name,
                version = version,
                fields = [ty for _, ty in fields],
            ),
        )

    def generate_struct(self, decl):
        self.typed_struct_decls[(self.module_name, decl.name)] = decl
        if len(decl.type_params) == 0:
            self.generate_struct_struct(self.module_name, decl.name, (), 0)
            self.generate_struct_constructor(self.module_name, decl.name, ())
            fields = \
                [(name, self.generate_type(ty)) for name, ty in decl.fields]
            return_type = named_type(self.module_name, decl.name)
            arg_types = [ty for _, ty in fields]
            key = self.module_name, decl.name
            llvm_name = "@%s$$%s" % key
            self.functions[key] = \
                ptr_to(func(arg_types, return_type)), llvm_name

    def size_of(self, ty):
        if ty.tag == 'ptr_to':
            return 8
        elif ty.tag == 'number':
            return ty.width // 8
        elif ty.tag == 'named_type':
            key = (ty.module_name, ty.name, ())
            if key in self.structs:
                fields = self.struct_fields[key]
                return sum([self.size_of(ty) for _, ty in fields.values()])
            if key in self.enums:
                constructors = self.enums[key]
                return self.calculate_enum_size(constructors)
        elif ty.tag == 'tuple':
            return sum([self.size_of(t) for t in ty.types])
        print(ty)
        raise NotImplementedError()

    def calculate_enum_size(self, constructors):
        size = 0
        for name, types in constructors:
            constructor_size = 0
            for ty in types:
                constructor_size += self.size_of(ty)
            size = max(size, constructor_size)
        return size

    def generate_constructor_struct(self, name, version, types):
        self.struct_decls.append(
            CGASTNode(
                'struct',
                module_name = self.module_name,
                name = name,
                version = version,
                fields = types,
            ),
        )

    def generate_constructor_function(self, enum_name, name, tag, types):
        args = \
            list(zip(types, map(lambda i: "%%arg.%d" % i, itertools.count())))
        function_writer = FunctionWriter(self)
        return_type = named_type(self.module_name, enum_name)
        ptr = function_writer.alloca(return_type)
        tag_ptr = \
            function_writer.getelementptr(
                return_type,
                ptr_to(return_type),
                ptr,
                ["0", "0"]
            )
        function_writer.store(tag_ptr, byte, str(tag))
        data_ptr = \
            function_writer.getelementptr(
                return_type,
                ptr_to(return_type),
                ptr,
                ["0", "1"]
            )
        key = self.module_name, enum_name, ()
        size = self.calculate_enum_size(self.enums[key])
        enum_data_type = named_type(self.module_name, name)
        data_ptr = \
            function_writer.bitcast(
                ptr_to(array_of(byte, size)),
                ptr_to(enum_data_type),
                data_ptr
            )
        for index, (type, arg_name) in enumerate(args):
            field_ptr = \
                function_writer.getelementptr(
                    enum_data_type,
                    ptr_to(enum_data_type),
                    data_ptr,
                    ["0", str(index)]
                )
            function_writer.store(field_ptr, type, arg_name)
        output = function_writer.load(return_type, ptr)
        function_writer.current_basic_block.terminator = \
            return_(return_type, output)
        function_key = self.module_name, name
        llvm_name = "@%s$$%s" % function_key
        self.functions[function_key] = \
            ptr_to(func(types, return_type)), llvm_name

        self.function_decls.append(
            CGASTNode(
                'define',
                name = llvm_name,
                return_type = return_type,
                args = args,
                basic_blocks = function_writer.basic_blocks,
                linkage = [],
            ),
        )

    def generate_enum_constructors(self, module_name, name, type_args):
        decl = self.typed_enum_decls[(module_name, name)]

        scope = dict(self.type_scope)
        for (name, _), ty in zip(decl.type_params, type_args):
            self.type_scope[name] = ty

        constructors = \
            [(name, self.generate_type_list(types))
                for name, types in decl.constructors]

        self.type_scope = scope

        if len(type_args) != 0:
            raise NotImplementedError()

        for tag, (name, types) in enumerate(constructors):
            self.generate_constructor_function(
                decl.name,
                name,
                tag,
                types,
            )

    def generate_enum_structs(self, module_name, name, type_args, version):
        decl = self.typed_enum_decls[(module_name, name)]
        scope = dict(self.type_scope)
        for (name, _), ty in zip(decl.type_params, type_args):
            self.type_scope[name] = ty

        constructors = \
            [(name, self.generate_type_list(types))
                for name, types in decl.constructors]

        self.type_scope = scope

        key = self.module_name, decl.name, tuple(type_args)
        self.enums[key] = constructors
        assert len(constructors) <= 256

        for name, types in constructors:
            self.generate_constructor_struct(name, version, types)

        size = self.calculate_enum_size(constructors)

        self.struct_decls.append(
            CGASTNode(
                'struct',
                module_name = self.module_name,
                name = decl.name,
                version = version,
                fields = [
                    byte,
                    CGASTNode(
                        'array',
                        size = size,
                        of = byte,
                    )
                ],
            )
        )

    def generate_enum(self, decl):
        self.typed_enum_decls[(self.module_name, decl.name)] = decl
        self.constructor_tags[decl.name] = {}
        for constructor_tag, (name, _) in enumerate(decl.constructors):
            self.constructor_tags[decl.name][name] = constructor_tag

        if len(decl.type_params) == 0:
            self.generate_enum_structs(self.module_name, decl.name, (), 0)
            self.generate_enum_constructors(self.module_name, decl.name, ())

    def generate_function_specialization(self, decl, type_args, version):
        function_writer = FunctionWriter(self)

        scope = dict(self.type_scope)
        for (name, _), ty in zip(decl.type_params, type_args):
            self.type_scope[name] = ty

        return_type = self.generate_type(decl.return_type)
        args = []
        arg_types = []
        arg_dict = {}
        for arg_name, arg_type in decl.args:
            llvm_name = '%' + arg_name
            llvm_type = self.generate_type(arg_type)
            arg_ptr = function_writer.alloca(llvm_type)
            function_writer.store(arg_ptr, llvm_type, llvm_name)
            args.append((llvm_type, llvm_name))
            arg_types.append(llvm_type)
            arg_dict[arg_name] = llvm_type, arg_ptr

        function_writer.arg_dict = arg_dict
        function_writer.generate_statements(decl.body)

        if decl.name == 'main':
            llvm_name = '@main'
        else:
            llvm_name = "@%s$$%s.%d" % (self.module_name, decl.name, version)

        function_key = self.module_name, decl.name
        self.functions[function_key] = \
            ptr_to(func(arg_types, return_type)), llvm_name
        self.function_decls.append(
            CGASTNode(
                'define',
                linkage = [],
                name = llvm_name,
                args = args,
                basic_blocks = function_writer.basic_blocks,
                return_type = return_type,
            ),
        )

        self.type_scope = scope

    def generate_function(self, decl):
        if len(decl.type_params) == 0:
            self.generate_function_specialization(decl, (), 0)

            return_type = self.generate_type(decl.return_type)
            arg_types = []
            for arg_name, arg_type in decl.args:
                llvm_type = self.generate_type(arg_type)
                arg_types.append(llvm_type)

            if decl.name == 'main':
                llvm_name = '@main'
            else:
                llvm_name = "@%s$$%s.0" % (self.module_name, decl.name)

            function_key = self.module_name, decl.name
            self.functions[function_key] = \
                ptr_to(func(arg_types, return_type)), llvm_name
        else:
            self.functions[(self.module_name, decl.name)] = \
                None, GenericFunction(self.module_name, decl.name, decl)

    def generate_constant(self, decl):
        function_name = next(self.function_names)
        function_writer = FunctionWriter(self)
        ty, value = function_writer.generate_expression(decl.expr)
        function_writer.current_basic_block.terminator = return_(void, 'void')
        llvm_name = "@%s$$%s" % (self.module_name, decl.name)
        function_writer.store(llvm_name, ty, value)
        key = self.module_name, decl.name
        self.constants[key] = ty, llvm_name
        self.initializers.append(function_name)
        self.global_decls.append(
            CGASTNode(
                'global',
                name = llvm_name,
                ty = ty,
            ),
        )
        self.function_decls.append(
            CGASTNode(
                'define',
                linkage = [],
                name = function_name,
                args = [],
                basic_blocks = function_writer.basic_blocks,
                return_type = void,
            ),
        )

    def generate_decl(self, decl):
        if decl.tag == 'extern':
            self.generate_extern(decl)
        elif decl.tag == 'struct':
            self.generate_struct(decl)
        elif decl.tag == 'enum':
            self.generate_enum(decl)
        elif decl.tag == 'function':
            self.generate_function(decl)
        elif decl.tag == 'constant':
            self.generate_constant(decl)
        elif decl.tag == 'import':
            pass
        else:
            raise NotImplementedError()

    def generate(self, module_name, decls):
        self.module_name = module_name
        for decl in decls:
            self.generate_decl(decl)

    def get_decls(self):
        constructors = [
            CGASTNode(
                'global_constructors',
                funcs = self.initializers,
            ),
        ]
        return \
            self.strings + \
            self.struct_decls + \
            self.declare_decls + \
            self.global_decls + \
            self.function_decls + \
            constructors
