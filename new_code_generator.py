import itertools
import type_checker

class CGASTNode:
    def __init__(self, tag, **kwargs):
        self.tag = tag
        self.attributes = kwargs
        for key, value in kwargs.items():
            setattr(self, key, value)

    def __repr__(self):
        return "CGASTNode(%s, %s)" % (repr(self.tag), repr(self.attributes))

def number(width):
    return  \
        CGASTNode(
            'number',
            width = width,
        )

def named_type(name):
    return  \
        CGASTNode(
            'named_type',
            name = name,
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

def func(arg_types, return_type):
    return \
        CGASTNode(
            'func',
            arg_types = arg_types,
            return_type = return_type,
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

class FunctionWriter:
    def __init__(self, code_generator):
        self.code_generator = code_generator
        self.current_basic_block = None
        self.basic_blocks = []
        self.variable_names = map(lambda i: "%%var.%d" % i, itertools.count())
        self.block_names = map(lambda i: "block.%d" % i, itertools.count())
        self.new_basic_block()
        self.scope = {}

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

    def generate_type_list(self, ty):
        return self.code_generator.generate_type_list(ty)

    def generate_variable(self, expr):
        if expr.name == 'void':
            return void, 'void'
        elif expr.name == 'true':
            return boolean, '1'
        elif expr.name == 'false':
            return boolean, '0'
        elif expr.name == 'null':
            return byte_ptr, 'null'
        elif expr.name in self.code_generator.functions:
            return self.code_generator.functions[expr.name]
        elif expr.name in self.scope:
            ty, ptr = self.scope[expr.name]
            return ty, self.load(ty, ptr)
        elif expr.name in self.code_generator.constants:
            ty, ptr = self.code_generator.constants[expr.name]
            return ty, self.load(ty, ptr)
        elif expr.name in self.arg_dict:
            ty, ptr = self.arg_dict[expr.name]
            return ty, self.load(ty, ptr)
        raise NotImplementedError()

    def generate_variable_l(self, expr):
        if expr.name in self.scope:
            ty, ptr = self.scope[expr.name]
            return ptr_to(ty), ptr
        elif expr.name in self.code_generator.constants:
            ty, ptr = self.code_generator.constants[expr.name]
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
        index, field_ty = self.code_generator.structs[struct_ty.ty.name][field]
        return ptr_to(field_ty), self.getelementptr(struct_ty.ty, struct_ty, ptr, ['0', str(index)])

    def generate_array_access(self, expr):
        array_ty, value = self.generate_expression(expr.expr)
        index_ty, index = self.generate_expression(expr.index)
        ptr = self.alloca(array_ty)
        self.store(ptr, array_ty, value)
        element_ptr = self.getelementptr(array_ty, ptr_to(array_ty), ptr, ['0', index])
        return array_ty.of, self.load(array_ty.of, element_ptr)

    def generate_application(self, expr):
        ty, function = self.generate_expression(expr.function)
        args = self.generate_expressions(expr.args)
        return_type = ty.ty.return_type
        return return_type, self.call(function, args, return_type)

    def generate_yield_expression(self, expr):
        raise NotImplementedError()

    def generate_field_access(self, expr):
        struct_ty, value = self.generate_expression(expr.x)
        field = expr.field
        index, field_ty = self.code_generator.structs[struct_ty.name][field]
        return field_ty, self.extractvalue(struct_ty, value, [index])

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
        if expr.tag in ['==', '!=']:
            ops = {
                '==': 'eq',
                '!=': 'ne',
            }
            if a_ty.tag == 'number' or a_ty.tag == 'ptr_to':
                return boolean, self.icmp(ops[expr.tag], a_ty, a, b)
            else:
                raise NotImplementedError()
        elif expr.tag in ['<', '>', '<=', '>=']:
            assert type(expr.a.ty) == type_checker.NumberType
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
            return boolean, self.icmp(ops[expr.tag], a_ty, a, b)

    def generate_operator(self, expr):
        ops = {
            '+': 'add',
            '-': 'sub',
            '|': 'or',
            '&': 'and',
        }
        ty, a = self.generate_expression(expr.a)
        _, b = self.generate_expression(expr.b)
        return ty, self.binop(ops[expr.tag], a, b, ty)

    def generate_number_literal(self, expr):
        ty = self.generate_type(expr.ty)
        return ty, str(expr.n)

    def generate_string_literal(self, expr):
        ty, value = self.global_string_constant(expr.string)
        return byte_ptr, self.getelementptr(ty, ptr_to(ty), value, ["0", "0"])

    def generate_apply_type_args(self, expr):
        raise NotImplementedError()

    def generate_address_of(self, expr):
        return self.generate_l_expr(expr.expr)

    def generate_expression(self, expr):
        if expr.tag == 'variable':
            output = self.generate_variable(expr)
        elif expr.tag == 'application':
            output = self.generate_application(expr)
        elif expr.tag == 'yield_expression':
            output = self.generate_yield_expression(expr)
        elif expr.tag == 'field_access':
            output = self.generate_field_access(expr)
        elif expr.tag == 'character_literal':
            output = self.generate_character_literal(expr)
        elif expr.tag == 'cast':
            output = self.generate_cast(expr)
        elif expr.tag in ['==', '!=', '<', '>', '<=', '>=']:
            output = self.generate_comparison(expr)
        elif expr.tag in ['+', '-', '|', '*', '/', '&']:
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
            self.current_basic_block.terminator = unconditional_branch(after_block.label)
            pattern_block.terminator = conditional_branch(cond, body_block.label, next_block.label)
            next_block = pattern_block
        prev.terminator = unconditional_branch(next_block.label)
        self.current_basic_block = after_block

    def generate_pattern_variables(self, pattern, ptr, ty):
        if pattern.tag == 'wildcard':
            self.scope[pattern.name] = ty, ptr
        elif pattern.tag == 'constructor':
            constructor_ptr = self.getelementptr(ty, ptr_to(ty), ptr, ["0", "1"])
            constructor_ty = named_type(pattern.name)
            size = self.code_generator.calculate_enum_size(self.code_generator.enums[pattern.ty.name])
            data_ptr = self.bitcast(ptr_to(array_of(byte, size)), constructor_ty, constructor_ptr)
            for i, arg in enumerate(pattern.args):
                arg_ty = self.generate_type(arg.ty)
                arg_ptr = self.getelementptr(constructor_ty, ptr_to(constructor_ty), data_ptr, ["0", str(i)])
                self.generate_pattern_variables(arg, arg_ptr, arg_ty)
        else:
            raise NotImplementedError()

    def generate_pattern(self, pattern, ptr, ty):
        if pattern.tag == 'wildcard':
            return '1'
        elif pattern.tag == 'constructor':
            expected_tag = str(self.code_generator.constructor_tags[pattern.ty.name][pattern.name])
            tag_ptr = self.getelementptr(ty, ptr_to(ty), ptr, ["0", "0"])
            actual_tag = self.load(byte, tag_ptr)
            output = self.icmp('eq', byte, expected_tag, actual_tag)
            constructor_ptr = self.getelementptr(ty, ptr_to(ty), ptr, ["0", "1"])
            constructor_ty = named_type(pattern.name)
            size = self.code_generator.calculate_enum_size(self.code_generator.enums[pattern.ty.name])
            data_ptr = self.bitcast(ptr_to(array_of(byte, size)), constructor_ty, constructor_ptr)
            for i, arg in enumerate(pattern.args):
                arg_ty = self.generate_type(arg.ty)
                arg_ptr = self.getelementptr(constructor_ty, ptr_to(constructor_ty), data_ptr, ["0", str(i)])
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
        self.string_names = map(lambda i: "@string.%d" % i, itertools.count())
        self.function_names = map(lambda i: "@function.%d" % i, itertools.count())
        self.strings = []
        self.functions = {}
        self.constants = {}
        self.initializers = []
        self.structs = {}
        self.enums = {}
        self.constructor_tags = {}

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
        if type(ty) == type_checker.NumberType:
            return number(ty.width)
        elif type_checker.is_ptr(ty):
            return ptr_to(self.generate_type(ty.args[0]))
        elif type_checker.is_array(ty):
            return array_of(self.generate_type(ty.args[0]), ty.args[1].n)
        elif type(ty) == type_checker.StructType:
            return named_type(ty.name)
        elif type(ty) == type_checker.EnumType:
            return named_type(ty.name)
        elif type(ty) == type_checker.Void:
            return void
        elif type(ty) == type_checker.Boolean:
            return number(1)
        elif type(ty) == type_checker.OpaqueNumberType:
            return number(64)
        print(ty)
        raise NotImplementedError()

    def generate_type_list(self, tys):
        return [self.generate_type(ty) for ty in tys]

    def generate_extern(self, decl):
        return_type = self.generate_type(decl.return_type)
        arg_types = self.generate_type_list(decl.arg_types)

        self.functions[decl.name] = \
            ptr_to(func(arg_types, return_type)), '@' + decl.name

        return [
            CGASTNode(
                'declare',
                name = '@' + decl.name,
                return_type = return_type,
                arg_types = arg_types,
            )
        ]

    def generate_struct(self, decl):
        fields = \
            [(name, self.generate_type(ty)) for name, ty in decl.fields]

        return_type = named_type(decl.name)

        names = [name for name, _ in fields]
        arg_types = [ty for _, ty in fields]
        arg_names = ['%' + name for name, _ in fields]

        self.functions[decl.name] = \
            ptr_to(func(arg_types, return_type)), '@' + decl.name

        function_writer = FunctionWriter(self)
        output_ptr = function_writer.alloca(return_type)
        self.structs[decl.name] = {}
        for i, name, ty in zip(itertools.count(), names, arg_types):
            self.structs[decl.name][name] = (i, ty)

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

        return [
            CGASTNode(
                'struct',
                name = decl.name,
                fields = [ty for _, ty in fields],
            ),
            CGASTNode(
                'define',
                name = '@' + decl.name,
                return_type = return_type,
                args = list(zip(arg_types, arg_names)),
                basic_blocks = function_writer.basic_blocks,
                linkage = [],
            ),
        ]

    def size_of(self, ty):
        if ty.tag == 'ptr_to':
            return 8
        elif ty.tag == 'number':
            return ty.width // 8
        elif ty.tag == 'named_type':
            if ty.name in self.structs:
                fields = self.structs[ty.name]
                return sum([self.size_of(ty) for _, ty in fields])
            if ty.name in self.enums:
                constructors = self.enums[ty.name]
                return self.calculate_enum_size(constructors)
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

    def generate_constructor_struct(self, name, types):
        return \
            CGASTNode(
                'struct',
                name = name,
                fields = types,
            )

    def generate_constructor_function(self, enum_name, name, constructor_tag, types):
        args = list(zip(types, map(lambda i: "%%arg.%d" % i, itertools.count())))
        function_writer = FunctionWriter(self)
        return_type = named_type(enum_name)
        ptr = function_writer.alloca(return_type)
        tag_ptr = function_writer.getelementptr(return_type, ptr_to(return_type), ptr, ["0", "0"])
        function_writer.store(tag_ptr, byte, str(constructor_tag))
        data_ptr = function_writer.getelementptr(return_type, ptr_to(return_type), ptr, ["0", "1"])
        size = self.calculate_enum_size(self.enums[enum_name])
        enum_data_type = named_type(name)
        data_ptr = function_writer.bitcast(ptr_to(array_of(byte, size)), enum_data_type, data_ptr)
        for index, (type, arg_name) in enumerate(args):
            field_ptr = function_writer.getelementptr(enum_data_type, ptr_to(enum_data_type), data_ptr, ["0", str(index)])
            function_writer.store(field_ptr, type, arg_name)
        output = function_writer.load(return_type, ptr)
        function_writer.current_basic_block.terminator = return_(return_type, output)
        self.functions[name] = \
            ptr_to(func(types, return_type)), '@' + name
        return \
            CGASTNode(
                'define',
                name = '@'+name,
                return_type = return_type,
                args = args,
                basic_blocks = function_writer.basic_blocks,
                linkage = [],
            )

    def generate_enum(self, decl):
        constructors = \
            [(name, self.generate_type_list(types))
             for name, types in decl.constructors]
        assert len(constructors) <= 256

        constructor_structs = \
            [self.generate_constructor_struct(name, types)
             for name, types in constructors]

        self.enums[decl.name] = constructors
        size = self.calculate_enum_size(constructors)

        self.constructor_tags[decl.name] = {}
        for constructor_tag, (name, types) in enumerate(constructors):
            self.constructor_tags[decl.name][name] = constructor_tag

        constructor_functions = \
            [self.generate_constructor_function(decl.name, name, constructor_tag, types)
             for constructor_tag, (name, types) in enumerate(constructors)]
        return [
                   CGASTNode(
                       'struct',
                       name = decl.name,
                       fields = [
                           byte,
                           CGASTNode(
                               'array',
                               size = size,
                               of = byte,
                           )
                       ],
                   )
               ] + constructor_structs + constructor_functions

    def generate_function(self, decl):
        return_type = self.generate_type(decl.return_type)
        function_writer = FunctionWriter(self)
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

        self.functions[decl.name] = \
            ptr_to(func(arg_types, return_type)), '@' + decl.name
        return [
            CGASTNode(
                'define',
                linkage = [],
                name = '@' + decl.name,
                args = args,
                basic_blocks = function_writer.basic_blocks,
                return_type = return_type,
            ),
        ]

    def generate_constant(self, decl):
        function_name = next(self.function_names)
        function_writer = FunctionWriter(self)
        ty, value = function_writer.generate_expression(decl.expr)
        function_writer.current_basic_block.terminator = return_(void, 'void')
        function_writer.store('@' + decl.name, ty, value)
        self.constants[decl.name] = ty, '@' + decl.name
        self.initializers.append(function_name)
        return [
            CGASTNode(
                'global',
                name = '@' + decl.name,
                ty = ty,
            ),
            CGASTNode(
                'define',
                linkage = [],
                name = function_name,
                args = [],
                basic_blocks = function_writer.basic_blocks,
                return_type = void,
            ),
        ]

    def generate_decl(self, decl):
        if decl.tag == 'extern':
            return self.generate_extern(decl)
        elif decl.tag == 'struct':
            return self.generate_struct(decl)
        elif decl.tag == 'enum':
            return self.generate_enum(decl)
        elif decl.tag == 'function':
            return self.generate_function(decl)
        elif decl.tag == 'constant':
            return self.generate_constant(decl)
        raise NotImplementedError()

    def generate(self, decls):
        constructors = [
            CGASTNode(
                'global_constructors',
                funcs = self.initializers,
            ),
        ]
        llvm_decls = concat([self.generate_decl(decl) for decl in decls])
        return self.strings + llvm_decls + constructors
