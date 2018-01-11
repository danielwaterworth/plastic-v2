import function_writer
from ir import *
from types import SimpleNamespace
import lower_control_flow

types = {}

LL = Representation('LL', types)

type_transformer = lower_control_flow.LowerControl.transformer(LL)

@type_transformer.case('Decl')
def type_decl(state, node):
    raise NotImplementedError()

value_transformer = lower_control_flow.LowerControl.transformer(LL)

@value_transformer.case('Decl')
def value_decl(state, node):
    if node.tag == 'function':
        if len(node.type_args) == 0:
            raise NotImplementedError()
        else:
            raise NotImplementedError()
    elif node.tag == 'constant':
        raise NotImplementedError()
    else:
        raise NotImplementedError()

def generate(things, instances):
    state = \
        SimpleNamespace(
            instances = instances,
        )
    values = \
        value_transformer.transform(
            Dict(Tuple(Str, Str), 'Decl'),
            state,
            things.values,
        )
    types = \
        type_transformer.transform(
            Dict(Tuple(Str, Str), 'Decl'),
            state,
            things.types,
        )
    return list(values.values()) + list(types.values())
