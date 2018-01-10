from ir import *
from types import SimpleNamespace
import collections

class MonomorphizationError(Exception):
    pass

def type_produce_instances(ty):
    output = empty_instances()
    return output

def value_produce_instances(value):
    output = empty_instances()
    return output

def type_produce_requirements(ty):
    raise NotImplementedError()

def value_produce_requirements(value):
    raise NotImplementedError()

def empty_instances():
    return \
        SimpleNamespace(
            values = collections.defaultdict(set),
            types = collections.defaultdict(set),
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
        instances_to_check = dict(self.instances)
        instances_to_check_next = empty_instances()
        for i in range(100):
            for t in ['values', 'types']:
                for name, ty_args in getattr(instances_to_check, t):
                    args, new_requirements = getattr(self.requirements, t)[name]
                    new_requirements = \
                        substitute(new_requirements, dict(zip(args, ty_args)))
                    for t in ['values', 'types']:
                        for name, ty_args in getattr(new_requirements, t):
                            s = getattr(instances_to_check_next, t)[name]
                            s.add(ty_args)
            instance_to_check = instances_to_check_next
            instances_to_check_next = empty_instances()
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
