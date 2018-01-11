from ir import *
from types import SimpleNamespace
import collections
import lower_control_flow

class MonomorphizationError(Exception):
    pass

find_value_instances = \
    lower_control_flow.LowerControl.transformer(
        lower_control_flow.LowerControl
    )

@find_value_instances.case('Expr')
def expr(state, node):
    if node.tag == 'apply_type_args':
        key = node.function.module_name, node.function.name
        state.instances.values[key].add(node.args)
    find_value_instances.default_transform('Expr', state, node)

def type_produce_instances(ty):
    output = empty_instances()
    return output

def value_produce_instances(value):
    output = empty_instances()
    find_value_instances.transform('Decl', output, value)
    return output

def type_produce_requirements(ty):
    return []

def value_produce_requirements(value):
    return []

def empty_instances():
    return \
        SimpleNamespace(
            values = collections.defaultdict(set),
            types = collections.defaultdict(set),
        )

def copy_instances(instances):
    return \
        SimpleNamespace(
            values = collections.defaultdict(set, instances.values),
            types = collections.defaultdict(set, instances.types),
        )

def update_instances(instances, extra):
    for t in ['types', 'values']:
        extra_1 = getattr(extra, t)
        instances_1 = getattr(instances, t)

        for name, things in extra_1:
            instances_1[name].update(things)

class Instantiator:
    def __init__(self, instances, requirements):
        self.instances = instances
        self.requirements = requirements

    def instantiate(self):
        instances_to_check = copy_instances(self.instances)
        instances_to_check_next = empty_instances()
        for i in range(100):
            for t in ['values', 'types']:
                for name, ty_args in getattr(instances_to_check, t):
                    args, new_requirements = \
                        getattr(self.requirements, t)[name]
                    new_requirements = \
                        substitute(new_requirements, dict(zip(args, ty_args)))
                    for t in ['values', 'types']:
                        for name, ty_args in getattr(new_requirements, t):
                            s = getattr(instances_to_check_next, t)[name]
                            s.add(ty_args)
            instances_to_check = instances_to_check_next
            instances_to_check_next = empty_instances()

            size = \
                len(instances_to_check.values) + len(instances_to_check.types)
            if size == 0:
                break
        else:
            raise MonomorphizationError()

def monomorphize(things):
    instances = empty_instances()
    requirements = SimpleNamespace(values = {}, types = {})

    for name, value in things.values.items():
        update_instances(instances, value_produce_instances(value))

    for name, ty in things.types.items():
        update_instances(instances, type_produce_instances(ty))

    for name, value in things.values.items():
        requirements.values[name] = value_produce_requirements(value)

    for name, ty in things.types.items():
        requirements.types[name] = type_produce_requirements(ty)

    instantiator = \
        Instantiator(
            instances,
            requirements,
        )
    instantiator.instantiate()

    return instantiator.instances
