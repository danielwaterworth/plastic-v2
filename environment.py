class Environment:
    def __init__(self, attributes, parent = None):
        self.attributes = attributes
        self.parent = parent

    def __getitem__(self, key):
        if key in self.attributes:
            return self.attributes[key]
        if self.parent:
            return self.parent[key]
        else:
            raise KeyError(key)

    def  __setitem__(self, key, value):
        self.attributes[key] = value
