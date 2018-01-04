import itertools

class Str:
    pass

class Int:
    pass

class Tuple:
    def __init__(self, *types):
        self.types = types

class List:
    def __init__(self, of):
        self.of = of

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

    def __repr__(self):
        return "Node(%s, %s)" % (repr(self.tag), repr(self.attributes))

class Representation:
    def __init__(self, name, types):
        self.name = name
        self.types = types

    def check(self, ty, data, path=None):
        path = path or []
        if ty == Str:
            assert isinstance(data, str)
        elif ty == Int:
            assert isinstance(data, int)
        elif isinstance(ty, Tuple):
            assert isinstance(data, tuple)
            assert len(ty.types) == len(data)
            for i, ty, value in zip(itertools.count(), ty.types, data):
                path.append(('tuple', i))
                self.check(ty, value, path)
                path.pop()
        elif isinstance(ty, List):
            assert isinstance(data, list)
            for i, x in enumerate(data):
                path.append(('list', i))
                self.check(ty.of, x, path)
                path.pop()
        elif ty in self.types:
            assert isinstance(data, Node), repr((data, ty))
            tag = data.tag
            constructors = self.types[ty]
            if tag not in constructors:
                raise \
                    Exception(
                        'tag %s not in %s, %s' % (tag, ty, path)
                    )
            expected_attributes = constructors[tag]
            expected_keys = set(expected_attributes.keys())
            actual_keys = set(data.attributes.keys())
            missing = expected_keys - actual_keys
            extra = actual_keys - expected_keys
            if missing or extra:
                raise \
                    Exception(
                        'missing %s, extra %s for %s' % (missing, extra, tag)
                    )
            for key in expected_keys:
                field_ty = expected_attributes[key]
                value = data.attributes[key]
                path.append(('field', key, tag))
                self.check(field_ty, value, path)
                path.pop()
        elif isinstance(ty, OrNone):
            if data != None:
                self.check(ty.ty, data, path)
        else:
            print(ty)
            raise Exception('problem')
