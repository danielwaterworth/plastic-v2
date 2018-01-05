import itertools

class Str:
    pass

class Boolean:
    pass

class Int:
    pass

class Tuple:
    def __init__(self, *types):
        self.types = types

class List:
    def __init__(self, of):
        self.of = of

class Dict:
    def __init__(self, k, v):
        self.k = k
        self.v = v

class OrNone:
    def __init__(self, ty):
        self.ty = ty

class OneOf:
    def __init__(self, *args):
        self.alternatives = set(args)

class Node:
    def __init__(self, tag, **attributes):
        self.tag = tag
        self.attributes = attributes
        for key, value in attributes.items():
            setattr(self, key, value)

    def sorted_attribute_tuple(self):
        return tuple(sorted(self.attributes.items()))

    def __eq__(self, other):
        return \
            type(other) == Node and \
            self.tag == other.tag and \
            self.sorted_attribute_tuple() == other.sorted_attribute_tuple()

    def __repr__(self):
        return "Node(%s, %s)" % (self.tag, self.attributes)

class Representation:
    def __init__(self, name, types):
        self.name = name
        self.types = types

    def check(self, ty, data, checked = None):
        checked = checked or set()
        if id(data) in checked:
            return
        checked.add(id(data))
        if ty == Str:
            assert isinstance(data, str)
        elif ty == Boolean:
            assert isinstance(data, bool)
        elif ty == Int:
            assert isinstance(data, int)
        elif isinstance(ty, Tuple):
            assert isinstance(data, tuple)
            assert len(ty.types) == len(data)
            for i, ty, value in zip(itertools.count(), ty.types, data):
                self.check(ty, value, checked)
        elif isinstance(ty, List):
            assert isinstance(data, list)
            for i, x in enumerate(data):
                self.check(ty.of, x, checked)
        elif isinstance(ty, Dict):
            assert isinstance(data, dict)
            for k, v in data.items():
                self.check(ty.k, k, checked)
                self.check(ty.v, v, checked)
        elif isinstance(ty, OrNone):
            if data != None:
                self.check(ty.ty, data, checked)
        elif isinstance(ty, OneOf):
            if data not in ty.alternatives:
                raise Exception()
        elif ty in self.types:
            tag = data.tag
            constructors = self.types[ty]
            if tag not in constructors:
                raise \
                    Exception(
                        'tag %s not in %s' % (tag, ty)
                    )
            expected_attributes = constructors[tag]
            expected_keys = set(expected_attributes.keys())
            actual_keys = set(data.attributes.keys())
            missing = expected_keys - actual_keys
            extra = actual_keys - expected_keys
            if missing or extra:
                raise \
                    Exception(
                        'missing %s, extra %s for %s, %s' % (
                            missing,
                            extra,
                            tag,
                            ty
                        )
                    )
            for key in expected_keys:
                field_ty = expected_attributes[key]
                value = getattr(data, key)
                self.check(field_ty, value, checked)
        else:
            print(ty)
            raise Exception('problem')
