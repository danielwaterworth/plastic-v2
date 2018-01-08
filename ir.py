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

    def __hash__(self):
        return hash(('Tuple', tuple(self.types)))

    def __eq__(self, other):
        return \
            type(other) == Tuple and \
            self.types == other.types

class List:
    def __init__(self, of):
        self.of = of

    def __hash__(self):
        return hash(('List', self.of))

    def __eq__(self, other):
        return \
            type(other) == List and \
            self.of == other.of

class Dict:
    def __init__(self, k, v):
        self.k = k
        self.v = v

    def __hash__(self):
        return hash(('Dict', self.k, self.v))

    def __eq__(self, other):
        return \
            type(other) == Dict and \
            self.k == other.k and \
            self.v == other.v

class OrNone:
    def __init__(self, ty):
        self.ty = ty

    def __hash__(self):
        return hash(('OrNone', self.ty))

    def __eq__(self, other):
        return \
            type(other) == OrNone and \
            self.ty == other.ty

class OneOf:
    def __init__(self, *args):
        self.alternatives = set(args)

    def __hash__(self):
        return hash(('OneOf', tuple(sorted(self.alternatives))))

    def __eq__(self, other):
        return \
            type(other) == OneOf and \
            self.alternatives == other.alternatives

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

    def check(self, ty, data):
        if ty == Str:
            assert isinstance(data, str)
        elif ty == Boolean:
            assert isinstance(data, bool)
        elif ty == Int:
            assert isinstance(data, int)
        elif isinstance(ty, Tuple):
            assert isinstance(data, tuple)
            assert len(ty.types) == len(data)
            for ty, value in zip(ty.types, data):
                self.check(ty, value)
        elif isinstance(ty, List):
            assert isinstance(data, list)
            for x in data:
                self.check(ty.of, x)
        elif isinstance(ty, Dict):
            assert isinstance(data, dict)
            for k, v in data.items():
                self.check(ty.k, k)
                self.check(ty.v, v)
        elif isinstance(ty, OrNone):
            if data != None:
                self.check(ty.ty, data)
        elif isinstance(ty, OneOf):
            if data not in ty.alternatives:
                raise Exception()
        elif ty in self.types:
            assert isinstance(data, Node), repr((ty, data))
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
                self.check(field_ty, value)
        else:
            print(ty)
            raise Exception('problem')

    def transformer(self, into):
        return Transformer(self, into)

class Transformer:
    def __init__(self, out_of, into):
        self.out_of = out_of
        self.into = into
        self.cases = {}

    def case(self, ty):
        def decorator(decorated):
            self.cases[ty] = decorated
            return decorated
        return decorator

    def default_transform(self, ty, state, value):
        if type(ty) == Tuple:
            output = []
            for value, ty in zip(value, ty.types):
                output.append(self.transform(ty, state, value))
            return tuple(output)
        elif type(ty) == List:
            output = []
            for val in value:
                output.append(self.transform(ty.of, state, val))
            return output
        elif type(ty) == Dict:
            output = {}
            for key, val in value.items():
                key = self.transform(ty.k, state, key)
                val = self.transform(ty.v, state, val)
                output[key] = val
            return output
            raise NotImplementedError()
        elif type(ty) == OrNone:
            if value == None:
                return None
            return self.transform(ty.ty, state, value)
        elif type(ty) == OneOf:
            return value
        elif ty == Int:
            return value
        elif ty == Str:
            return value
        elif ty == Boolean:
            return value
        elif ty in self.out_of.types:
            assert isinstance(value, Node)
            field_types = self.out_of.types[ty][value.tag]
            field_values = value.attributes
            ty_keys = set(field_types.keys())
            val_keys = set(field_values.keys())
            missing = ty_keys - val_keys
            if missing:
                raise Exception('missing %s' % missing)
            extra = val_keys - ty_keys
            if extra:
                raise Exception('extra %s' % extra)

            new_attributes = {}
            for key in sorted(field_types.keys()):
                ty = field_types[key]
                val = field_values[key]
                new_attributes[key] = self.transform(ty, state, val)
            return Node(value.tag, **new_attributes)
        else:
            print(ty)
            raise NotImplementedError()

    def transform(self, ty, state, value):
        if ty in self.cases:
            return self.cases[ty](state, value)
        return self.default_transform(ty, state, value)

def pretty(value, indent, output):
    space = ' ' * indent
    space2 = ' ' * (indent + 2)
    if type(value) == Node:
        if len(value.attributes) == 0:
            output.extend(['Node(', repr(value.tag), ')'])
        else:
            output.extend(['Node(\n'])
            output.extend([space2, repr(value.tag), '\n'])
            for key, value in value.attributes.items():
                output.extend([space2, key, ' = '])
                pretty(value, indent + 2, output)
                output.extend([',\n'])
            output.extend([space, ')'])
    elif type(value) == list:
        output.extend(['[\n'])
        for val in value:
            output.append(space2)
            pretty(val, indent + 2, output)
            output.append(',\n')
        output.extend([space, ']'])
    elif type(value) == tuple:
        output.extend(['(\n'])
        for val in value:
            output.append(space2)
            pretty(val, indent + 2, output)
            output.append(',\n')
        output.extend([space, ')'])
    elif type(value) == set:
        output.extend(['{\n'])
        for val in value:
            output.append(space2)
            pretty(val, indent + 2, output)
            output.append(',\n')
        output.extend([space, '}'])
    elif type(value) == dict:
        output.extend(['{\n'])
        for key, val in value.items():
            output.extend([space2, repr(key), ': '])
            pretty(val, indent + 2, output)
            output.append(',\n')
        output.extend([space, '}'])
    else:
        output.append(repr(value))

def pprint(value):
    output = []
    pretty(value, 0, output)
    print(''.join(output))

def define_constructor(tag, fields):
    def constructor(*args):
        assert len(args) == len(fields)
        return Node(tag, **dict(zip(fields, args)))
    return constructor
