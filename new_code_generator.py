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
        self.let_allocations = {}
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
        self.current_basic_block.instructions.append(
            CGASTNode(
                'alloca',
                ty = ty,
                dst = var,
            )
        )
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

    def sub(self, a, b, ty):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'sub',
                a = a,
                b = b,
                ty = ty,
                dst = dst,
            )
        )
        return dst

    def add(self, a, b, ty):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'add',
                a = a,
                b = b,
                ty = ty,
                dst = dst,
            )
        )
        return dst

    def getelementptr(self, ty, value, offset):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            CGASTNode(
                'getelementptr',
                source_type = ptr_to(ty),
                value = value,
                offset = offset,
                dst = dst,
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
        elif expr.name in self.code_generator.functions:
            return self.code_generator.functions[expr.name]
        elif expr.name in self.scope:
            ty, ptr = self.scope[expr.name]
            return ty, self.load(ty, ptr)
        raise NotImplementedError()

    def generate_application(self, expr):
        ty, function = self.generate_expression(expr.function)
        args = self.generate_expressions(expr.args)
        return_type = ty.ty.return_type
        return return_type, self.call(function, args, return_type)

    def generate_yield_expression(self, expr):
        raise NotImplementedError()

    def generate_field_access(self, expr):
        raise NotImplementedError()

    def generate_character_literal(self, expr):
        raise NotImplementedError()

    def generate_cast(self, expr):
        raise NotImplementedError()

    def generate_eq(self, expr):
        raise NotImplementedError()

    def generate_plus(self, expr):
        ty, a = self.generate_expression(expr.a)
        _, b = self.generate_expression(expr.a)
        return ty, self.add(a, b, ty)

    def generate_minus(self, expr):
        ty, a = self.generate_expression(expr.a)
        _, b = self.generate_expression(expr.a)
        return ty, self.sub(a, b, ty)

    def generate_number_literal(self, expr):
        raise NotImplementedError()

    def generate_string_literal(self, expr):
        ty, value = self.global_string_constant(expr.string)
        return byte_ptr, self.getelementptr(ty, value, ["0", "0"])

    def generate_apply_type_args(self, expr):
        raise NotImplementedError()

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
        elif expr.tag == '==':
            output = self.generate_eq(expr)
        elif expr.tag == '+':
            output = self.generate_plus(expr)
        elif expr.tag == '-':
            output = self.generate_minus(expr)
        elif expr.tag == 'number_literal':
            output = self.generate_number_literal(expr)
        elif expr.tag == 'string_literal':
            output = self.generate_string_literal(expr)
        elif expr.tag == 'apply_type_args':
            output = self.generate_apply_type_args(expr)
        else:
            raise NotImplementedError()
        assert type(output) == tuple, expr.tag
        assert len(output) == 2, expr.tag
        return output

    def generate_expressions(self, exprs):
        return [self.generate_expression(expr) for expr in exprs]

    def generate_allocations(self, statement):
        if statement.tag == 'let_statement':
            llvm_type = self.generate_type(statement.expr.ty)
            self.let_allocations[id(statement)] = self.alloca(llvm_type)
        elif statement.tag == 'if_statement':
            for s in statement.true_side:
                self.generate_allocations(s)
            for s in statement.false_side:
                self.generate_allocations(s)
        elif statement.tag == 'loop_statement':
            for statement in statement.body:
                self.generate_allocations(statement)

    def generate_let_statement(self, statement):
        ty, value = self.generate_expression(statement.expr)
        ptr = self.let_allocations[id(statement)]
        self.scope[statement.name] = ty, ptr
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
        raise NotImplementedError()

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
        self.current_basic_block.terminator = \
            unconditional_branch(after_block.label)

        self.scope = dict(scope)
        self.current_basic_block = false_block
        self.generate_statements(statement.false_side)
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
        else:
            raise NotImplementedError()

    def generate_statements(self, statements):
        for statement in statements:
            self.generate_statement(statement)

class CodeGenerator:
    def __init__(self):
        self.string_names = map(lambda i: "@string.%d" % i, itertools.count())
        self.strings = []
        self.functions = {}

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
        if type(ty) == type_checker.Number:
            return number(ty.width)
        elif type_checker.is_ptr(ty):
            return ptr_to(self.generate_type(ty.args[0]))
        elif type(ty) == type_checker.StructType:
            return named_type(ty.name)
        elif type(ty) == type_checker.EnumType:
            return named_type(ty.name)
        elif type(ty) == type_checker.Void:
            return void
        elif type(ty) == type_checker.Boolean:
            return number(1)
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
        return []

    def generate_enum(self, decl):
        return []

    def generate_function(self, decl):
        return_type = self.generate_type(decl.return_type)
        function_writer = FunctionWriter(self)
        args = []
        arg_types = []
        for arg_name, arg_type in decl.args:
            llvm_name = '%' + arg_name
            llvm_type = self.generate_type(arg_type)
            arg_ptr = self.alloca(llvm_type)
            function_writer.store(arg_ptr, llvm_type, llvm_name)
            args.append((llvm_name, llvm_type))
            arg_types.append(llvm_type)

        for statement in decl.body:
            function_writer.generate_allocations(statement)

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

    def generate_decl(self, decl):
        if decl.tag == 'extern':
            return self.generate_extern(decl)
        elif decl.tag == 'struct':
            return self.generate_struct(decl)
        elif decl.tag == 'enum':
            return self.generate_enum(decl)
        elif decl.tag == 'function':
            return self.generate_function(decl)
        raise NotImplementedError()

    def generate(self, decls):
        llvm_decls = concat([self.generate_decl(decl) for decl in decls])
        return self.strings + llvm_decls