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

def concat(xs):
    return [x for ys in xs for x in ys]

void = \
    CGASTNode(
        'void'
    )

byte = \
    CGASTNode(
        'number',
        signed = False,
        width = 8,
    )

def ptr_to(ty):
    return \
        CGASTNode(
            'ptr_to',
            ty = ty,
        )

stack_ptr = ptr_to(byte)

def func(arg_types, return_type):
    return \
        CGASTNode(
            'func',
            arg_types = arg_types,
            return_type = return_type,
        )

def continuation(ty):
    if ty.tag == 'void':
        return ptr_to(func([stack_ptr], void))
    else:
        return ptr_to(func([stack_ptr, ty], void))

class FunctionWriter:
    def __init__(self, code_generator):
        self.code_generator = code_generator
        self.var = map(lambda i: "%%var.%i" % i, itertools.count())
        self.stack_ptr = map(lambda i: "%%stackptr.%i" % i, itertools.count())
        self.block = map(lambda i: "block.%i" % i, itertools.count())
        self.variables = {}
        self.instructions = []
        self.basic_blocks = []
        self.functions = []
        self.stack = []

    def copy_context(self):
        writer = \
            FunctionWriter(
                self.code_generator
            )
        writer.variables = dict(self.variables)
        writer.instructions = list(self.instructions)
        writer.basic_blocks = list(self.basic_blocks)
        writer.functions = list(self.functions)
        writer.stack = list(self.stack)
        writer.function_names = self.function_names
        writer.return_type = self.return_type
        return writer

    def terminate_basic_block(self):
        self.basic_blocks.append(
            CGASTNode(
                'basic_block',
                label = next(self.block),
                instructions = [],
                terminator = None,
            ),
        )
        self.instructions = self.basic_blocks[-1].instructions

    def terminate_function(self, ty):
        self.stack_ptr_val = "%$stackptr"
        self.instructions = []
        current_stack_ptr = self.stack_ptr_val
        for arg_name, arg_ty in reversed(self.stack):
            arg_ptr = ptr_to(arg_ty)
            temp0 = next(self.var)
            temp1 = next(self.var)
            new_stack_ptr = next(self.stack_ptr)
            self.instructions.extend([
                CGASTNode(
                    "bitcast",
                    dest_type = arg_ptr,
                    source_type = stack_ptr,
                    value = current_stack_ptr,
                    ret_name = temp0,
                ),
                CGASTNode(
                    "getelementptr",
                    source_type = arg_ptr,
                    value = temp0,
                    offset = "-1",
                    ret_name = temp1,
                ),
                CGASTNode(
                    "load",
                    source_type = arg_ptr,
                    dest_type = arg_ty,
                    value = temp1,
                    ret_name = arg_name,
                ),
                CGASTNode(
                    "bitcast",
                    dest_type = stack_ptr,
                    source_type = arg_ptr,
                    value = temp1,
                    ret_name = new_stack_ptr,
                ),
            ])
            current_stack_ptr = new_stack_ptr

        self.basic_blocks = [
            CGASTNode(
                'basic_block',
                label = next(self.block),
                instructions = self.instructions,
                terminator = None
            ),
        ]

        args = [
            ("%$stackptr", stack_ptr),
        ]
        if ty.tag != 'void':
            args.append(("%$param", ty))

        self.functions.append(
            CGASTNode(
                'define',
                name = next(self.function_names),
                args = args,
                return_type = void,
                basic_blocks = self.basic_blocks,
            )
        )

    def generate_expression(self, expr):
        if expr.tag == 'yield_expression':
            self.generate_expression(expr.expr)
            raise NotImplementedError()
        elif expr.tag == 'application':
            return_type = \
                self.code_generator.generate_llvm_type(
                    expr.ty,
                )
            function = self.generate_expression(expr.function)
            args = [self.generate_expression(arg) for arg in expr.args]
            basic_block = self.basic_blocks[-1]
            stack_ptr_val = self.stack_ptr_val
            self.terminate_function(return_type)
            llvm_function = self.functions[-1]
            args = [
                (stack_ptr_val, stack_ptr),
                ('@' + llvm_function.name, continuation(return_type)),
            ] + args
            basic_block.terminator = \
                CGASTNode(
                    'tail_call',
                    ret_type = void,
                    function = function,
                    args = args,
                )
            return "%$param"
        elif expr.tag == '+':
            a = self.generate_expression(expr.a)
            b = self.generate_expression(expr.b)
            output = next(self.var)
            self.instructions.append(
                CGASTNode(
                    'add',
                    ret_name = output,
                    ty = self.code_generator.generate_llvm_type(expr.ty),
                    a = a,
                    b = b,
                ),
            )
            return output
        elif expr.tag == "variable":
            value = self.code_generator.lookup(expr.name)
            value = self.variables.get(expr.name, value)
            assert value is not None
            return value
        print(expr.tag)
        raise NotImplementedError()

    def generate_statement(self, statement):
        if statement.tag == 'let_statement':
            value = self.generate_expression(statement.expr)
            name = '%' + statement.name
            ty = self.code_generator.generate_llvm_type(statement.expr.ty)
            self.new_stack_variable(name, ty, value)
        elif statement.tag == 'loop_statement':
            raise NotImplementedError()
        elif statement.tag == 'break':
            raise NotImplementedError()
        elif statement.tag == 'assignment':
            raise NotImplementedError()
        elif statement.tag == 'expr_statement':
            self.generate_expression(statement.expr)
        elif statement.tag == 'return':
            value = self.generate_expression(statement.expr)
            args = [("%$stackptr", stack_ptr)]
            if self.return_type.tag != 'void':
                args.append((value, self.return_type))
            self.basic_blocks[-1].terminator = \
                CGASTNode(
                    'tail_call',
                    function = '%$continuation',
                    ret_type = void,
                    args = args,
                )
            self.instructions = None
        elif statement.tag == 'if_statement':
            condition = self.generate_expression(statement.condition)
            true_writer = self.copy_context()
            false_writer = self.copy_context()

            true_writer.terminate_function(void)
            false_writer.terminate_function(void)

            for s in statement.true_side:
                true_writer.generate_statement(s)
            for s in statement.false_side:
                false_writer.generate_statement(s)
            raise NotImplementedError()
        else:
            print(statement.tag)
            raise NotImplementedError()

    def new_stack_variable(self, name, ty, value):
        self.stack.append((name, ty))
        temp0 = next(self.var)
        temp1 = next(self.var)
        new_stack_ptr = next(self.stack_ptr)
        arg_ptr = ptr_to(ty)
        self.instructions.extend([
            CGASTNode(
                "bitcast",
                dest_type = arg_ptr,
                source_type = stack_ptr,
                value = self.stack_ptr_val,
                ret_name = temp0,
            ),
            CGASTNode(
                "store",
                dest_type = arg_ptr,
                source_type = ty,
                value = value,
                dest = temp0,
            ),
            CGASTNode(
                "getelementptr",
                source_type = arg_ptr,
                value = temp0,
                offset = "1",
                ret_name = temp1,
            ),
            CGASTNode(
                "bitcast",
                dest_type = stack_ptr,
                source_type = arg_ptr,
                value = temp1,
                ret_name = new_stack_ptr,
            ),
        ])

        self.stack_ptr_val = new_stack_ptr

    def generate_function(self, decl):
        self.function_names = \
            map(lambda i: "%s.%i" % (decl.name, i), itertools.count())

        self.stack_ptr_val = '%$stackptr'
        self.return_type = \
            self.code_generator.generate_llvm_type(decl.return_type)
        actual_args = \
            [('%' + name, self.code_generator.generate_llvm_type(ty))
                for name, ty in decl.args]
        args_to_persist = \
            [('%$continuation', continuation(self.return_type))] + actual_args
        args = [(self.stack_ptr_val, stack_ptr)] + args_to_persist

        for arg_name, _ in decl.args:
            self.variables[arg_name] = "%" + arg_name

        for arg_name, arg_ty in args_to_persist:
            self.new_stack_variable(arg_name, arg_ty, arg_name)

        self.basic_blocks = [
            CGASTNode(
                'basic_block',
                label = next(self.block),
                instructions = self.instructions,
                terminator = None
            ),
        ]

        self.functions = [
            CGASTNode(
                'define',
                name = decl.name,
                return_type = void,
                args = args,
                basic_blocks = self.basic_blocks,
            )
        ]

        for statement in decl.body:
            self.generate_statement(statement)

        return self.functions

class CodeGenerator:
    def __init__(self):
        self.structs = {}
        self.enums = {}
        self.functions = set()

    def lookup(self, name):
        if name == 'void':
            return 'void'
        elif name == 'true':
            return '1'
        elif name == 'false':
            return '0'
        elif name in self.functions:
            return '@' + name

    def generate_llvm_type(self, ty):
        if type(ty) == type_checker.Number:
            return \
                CGASTNode(
                    'number',
                    width = ty.width,
                )
        elif type_checker.is_ptr(ty):
            return ptr_to(self.generate_llvm_type(ty.args[0]))
        elif type(ty) == type_checker.StructType:
            return \
                CGASTNode(
                    'named_type',
                    name = ty.name,
                )
        elif type(ty) == type_checker.EnumType:
            return \
                CGASTNode(
                    'named_type',
                    name = ty.name,
                )
        elif type(ty) == type_checker.Void:
            return void
        elif type(ty) == type_checker.Boolean:
            return \
                CGASTNode(
                    'number',
                    width = 1,
                )
        print(ty)
        raise NotImplementedError()

    def generate_llvm_types(self, types):
        return [self.generate_llvm_type(ty) for ty in types]

    def generate_extern(self, decl):
        return [
            CGASTNode(
                'declare',
                name = decl.name,
                return_type = self.generate_llvm_type(decl.return_type),
                arg_types = self.generate_llvm_types(decl.arg_types),
            )
        ]

    def generate_struct(self, decl):
        fields = \
            [(name, self.generate_llvm_type(ty)) for name, ty in decl.fields]
        self.structs[decl.name] = fields
        return [
            CGASTNode(
                'struct',
                name = decl.name,
                fields = [ty for _, ty in fields],
            )
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

    def generate_enum(self, decl):
        constructors = \
            [(name, self.generate_llvm_types(types))
                for name, types in decl.constructors]
        assert len(constructors) <= 256

        constructor_structs = \
            [self.generate_constructor_struct(name, types)
                for name, types in constructors]

        self.enums[decl.name] = constructors
        size = self.calculate_enum_size(constructors)
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
        ] + constructor_structs

    def generate_function(self, decl):
        self.functions.add(decl.name)
        return FunctionWriter(self).generate_function(decl)

    def generate_top_level_decl(self, decl):
        if decl.tag == 'extern':
            return self.generate_extern(decl)
        elif decl.tag == 'struct':
            return self.generate_struct(decl)
        elif decl.tag == 'enum':
            return self.generate_enum(decl)
        elif decl.tag == 'function':
            return self.generate_function(decl)
        raise NotImplementedError()

    def generate_top_level_decls(self, decls):
        return concat([self.generate_top_level_decl(decl) for decl in decls])
