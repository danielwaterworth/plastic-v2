from ir import *
import itertools

class FunctionWriter:
    def __init__(self):
        self.current_basic_block = None
        self.basic_blocks = []
        self.variable_names = \
            map(lambda i: "%%var.%d" % i, itertools.count())
        self.block_names = map(lambda i: "block.%d" % i, itertools.count())
        self.new_basic_block()

    def new_basic_block(self):
        self.current_basic_block = \
            Node(
                'basic_block',
                label = next(self.block_names),
                instructions = [],
                terminator = None,
            )
        self.basic_blocks.append(self.current_basic_block)

    def alloca(self, ty):
        var = next(self.variable_names)
        instruction = \
            Node(
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
            Node(
                'store',
                source = source,
                dst = dst,
                ty = ty,
            )
        )

    def load(self, ty, source):
        dst = next(self.variable_names)
        self.current_basic_block.instructions.append(
            Node(
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
                Node(
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
                Node(
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
            Node(
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
            Node(
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
            Node(
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
            Node(
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
            Node(
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
            Node(
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
            Node(
                'not',
                dst = dst,
                value = value,
            )
        )
        return dst
