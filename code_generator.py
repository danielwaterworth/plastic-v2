import function_writer
from ir import *
from types import SimpleNamespace
import lower_control_flow

types = {}

LL = Representation('LL', types)

class IRWriter:
    def __init__(self):
        self.decls = []

    def struct(self, name, fields):
        pass

transformer = lower_control_flow.LowerControl.transformer(LL)

def transform_type(ty):
    return transformer.transform('Type', (), ty)

@transformer.case('Type')
def ty(state, node):
    if node.tag == 'number_type':
        raise NotImplementedError()
    else:
        print(node.tag)
        raise NotImplementedError()

@transformer.case('Expr')
def expr(state, node):
    if node.tag == 'number_literal':
        state.writer.store(
            state.ty,
            state.pointer,
            str(node.n),
        )
    else:
        print(node.tag)
        raise NotImplementedError()

@transformer.case('Decl')
def value_decl(state, node):
    if node.tag == 'function':
        if len(node.type_args) == 0:
            raise NotImplementedError()
        else:
            raise NotImplementedError()
    elif node.tag == 'constant':
        writer = function_writer.FunctionWriter()
        state.writer = writer
        state.ty = transform_type(node.expr.ty)
        state.pointer = writer.alloca(state.ty)
        transformer.transform('Expr', state, node.expr)
        raise NotImplementedError()
    elif node.tag == 'struct':
        assert len(node.type_params) == 0
        fields = []

        for name, ty in node.fields:
            fields.append((name, transform_type(ty)))

        state.output_writer.struct(
            '%' + node.name,
            fields,
        )
    else:
        print(node.tag)
        raise NotImplementedError()

def generate(things, instances):
    output_writer = IRWriter()
    state = \
        SimpleNamespace(
            instances = instances,
            output_writer = output_writer,
        )
    types = \
        transformer.transform(
            Dict(Tuple(Str, Str), 'Decl'),
            state,
            things.types,
        )
    values = \
        transformer.transform(
            Dict(Tuple(Str, Str), 'Decl'),
            state,
            things.values,
        )
    decls =  output_writer.decls
    LL.check(List('Decl'), decls)
    return decls
