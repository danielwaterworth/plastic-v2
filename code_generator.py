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
        self.block = map(lambda i: "block.%i" % i, itertools.count())

    def generate_expression(self, expr):
        if expr.tag == 'yield_expression':
            self.generate_expression(expr.expr)
            raise NotImplementedError()
            return [], next(self.var)
        elif expr.tag == 'application':
            raise NotImplementedError()
            return [], next(self.var)
        elif expr.tag == '+':
            a_instructions, a = self.generate_expression(expr.a)
            b_instructions, b = self.generate_expression(expr.b)
            output = next(self.var)
            instruction = \
                CGASTNode(
                    'add',
                    ty = self.code_generator.generate_llvm_type(expr.ty),
                    a = a,
                    b = b,
                )
            return a_instructions + b_instructions + [instruction], output
        elif expr.tag == "variable":
            pass
        print(expr.tag)
        raise NotImplementedError()

    def generate_statement(self, statement):
        if statement.tag == 'let_statement':
            instructions, name = self.generate_expression(statement.expr)
            return instructions, []
        elif statement.tag == 'loop_statement':
            raise NotImplementedError()
        elif statement.tag == 'break':
            raise NotImplementedError()
        elif statement.tag == 'assignment':
            raise NotImplementedError()
        elif statement.tag == 'expression_statement':
            raise NotImplementedError()
        elif statement.tag == 'return':
            instructions, value = self.generate_expression(statement.expr)
            instructions.append(
                CGASTNode(
                    'tail_call',
                    function = '%$continuation',
                    args = [
                        "%$stack",
                        value,
                    ]
                )
            )
            return instructions, []
        else:
            raise NotImplementedError()

    def generate_function(self, decl):
        stack_ptr_val = '%$stack_ptr'
        return_type = \
            self.code_generator.generate_llvm_type(decl.return_type)
        actual_args = \
            [('%' + name, self.code_generator.generate_llvm_type(ty))
                for name, ty in decl.args]
        args = [
            (stack_ptr_val, stack_ptr),
            ('%$continuation', continuation(return_type))
        ] + actual_args

        arg_name, arg_ty = actual_args[0]
        intermediate = next(self.var)
        instructions = [
            CGASTNode(
                "bitcast",
                dest_type = ptr_to(arg_ty),
                source_type = stack_ptr,
                value = stack_ptr_val,
                ret_name = intermediate,
            ),
            CGASTNode(
                "store",
                dest_type = ptr_to(arg_ty),
                source_type = arg_ty,
                value = arg_name,
                dest = intermediate,
            )
        ]

        blocks = [
            CGASTNode('basic_block', label = next(self.block), instructions=instructions)
        ]

        # for statement in decl.body:
        #     instructions, new_blocks = self.generate_statement(statement)
        #     blocks[-1].instructions.extend(instructions)
        #     blocks.extend(new_blocks)

        return [
            CGASTNode(
                'define',
                name = decl.name,
                return_type = void,
                args = args,
                basic_blocks = blocks,
            )
        ]

class CodeGenerator:
    def __init__(self):
        self.structs = {}
        self.enums = {}

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
