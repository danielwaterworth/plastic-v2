from ir import *
import type_checker
import copy
from types import SimpleNamespace
import itertools

types = copy.deepcopy(type_checker.TypeAST.types)

del types['Statement']['if_statement']
del types['Statement']['loop_statement']
del types['Statement']['match']
del types['Statement']['return']
del types['Statement']['break']

del types['Decl']['function']['body']
types['Decl']['function']['first_ebb_name'] = Str
types['Decl']['function']['preamble'] = List(Tuple(Str, 'Type'))
types['Decl']['function']['ebbs'] = Dict(Str, 'EBB')

types['EBB'] = {
    'ebb': {
        'label': Str,
        'statements': List('Statement'),
        'terminator': 'Terminator',
    },
}

types['Pattern'] = {
    'tuple': {
        'names': List(Str),
    },
    'constructor': {
        'name': Str,
        'args': List(Str),
    },
    'wildcard': {
        'name': Str,
    }
}

types['Terminator'] = {
    'jump': {
        'to': Str,
    },
    'conditional': {
        'condition': 'Expr',
        'true_side': Str,
        'false_side': Str,
    },
    'match': {
        'expr': 'Expr',
        'cases': List(Tuple('Pattern', Str)),
    },
    'return': {
        'expr': 'Expr',
    },
    'halt': {},
}

LowerControl = Representation('LowerControl', types)

jump = define_constructor('jump', ['to'])
conditional = define_constructor('conditional', ['condition', 'true_side', 'false_side'])
return_ = define_constructor('return', ['expr'])
match = define_constructor('match', ['expr', 'cases'])

class ControlFlowState:
    def __init__(self, arg_names):
        self.preamble = []
        self.first_ebb_name = None
        self.ebbs = {}
        self.ebb_names = map(lambda x: "ebb.%d" % x, itertools.count())
        self.break_blocks = []

    @property
    def current_ebb(self):
        return self.ebbs[self.current_ebb_name]

    def new_ebb(self):
        name = next(self.ebb_names)
        self.ebbs[name] = \
            Node(
                'ebb',
                label = name,
                statements = [],
                terminator = Node('halt'),
            )
        return name

transformer = type_checker.TypeAST.transformer(LowerControl)

@transformer.case('Statement')
def statement(state, node):
    if node.tag == 'return':
        state.current_ebb.terminator = return_(node.expr)
        state.current_ebb_name = None
    elif node.tag == 'break':
        state.current_ebb.terminator = jump(state.break_blocks[-1])
        state.current_ebb_name = None
    elif node.tag == 'if_statement':
        before_block = state.current_ebb_name
        true_block = state.new_ebb()
        false_block = state.new_ebb()
        after_block = state.new_ebb()

        state.ebbs[before_block].terminator = \
            conditional(node.condition, true_block, false_block)

        state.current_ebb_name = true_block
        transformer.transform(List('Statement'), state, node.true_side)
        if state.current_ebb_name:
            state.current_ebb.terminator = jump(after_block)

        state.current_ebb_name = false_block
        transformer.transform(List('Statement'), state, node.false_side)
        if state.current_ebb_name:
            state.current_ebb.terminator = jump(after_block)

        state.current_ebb_name = after_block
    elif node.tag == 'loop_statement':
        before_block = state.current_ebb_name
        start_block = state.new_ebb()
        after_block = state.new_ebb()
        state.break_blocks.append(after_block)
        state.ebbs[before_block].terminator = jump(start_block)
        state.current_ebb_name = start_block
        transformer.transform(List('Statement'), state, node.body)
        state.break_blocks.pop()
        state.current_ebb.terminator = jump(start_block)
        state.current_ebb_name = after_block
    elif node.tag == 'match':
        cases = []
        state.current_ebb.terminator = match(expr, cases)
        raise NotImplementedError()
    else:
        state.current_ebb.statements.append(node)

def transform_function(args, statements):
    state = ControlFlowState([name for name, _, in args])
    label = state.new_ebb()
    state.current_ebb_name = label
    state.first_ebb_name = label
    transformer.transform(List('Statement'), state, statements)
    return state

def transform_decl(node):
    if node.tag == 'function':
        state = transform_function(node.args, node.body)
        return \
            Node(
                'function',
                name = node.name,
                type_params = node.type_params,
                constraints = node.constraints,
                args = node.args,
                return_type = node.return_type,
                preamble = state.preamble,
                ebbs = state.ebbs,
                first_ebb_name = state.first_ebb_name,
            )
    else:
        return node

def lower_control_flow(things):
    raise NotImplementedError()
