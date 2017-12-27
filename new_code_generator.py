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

void = \
    CGASTNode(
        'void'
    )
byte = number(8)
byte_ptr = ptr_to(byte)

def func(arg_types, return_type):
    return \
        CGASTNode(
            'func',
            arg_types = arg_types,
            return_type = return_type,
        )

def concat(xs):
    return [x for ys in xs for x in ys]

class CodeGenerator:
    def generate_extern(self, decl):
        return []

    def generate_struct(self, decl):
        return []

    def generate_enum(self, decl):
        return []

    def generate_function(self, decl):
        return []

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
        return concat([self.generate_decl(decl) for decl in decls])
