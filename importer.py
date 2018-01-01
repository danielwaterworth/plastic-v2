class Importer:
    def __init__(self, parse):
        self.parse = parse

    def load(self, module):
        if not module in self.loaded_modules:
            filename = self.dir_name + '/' + module + '.plst'
            with open(filename, 'r') as fd:
                src = fd.read()
            decls = self.parse(src)
            for decl in decls:
                if decl.tag == 'import':
                    self.load(decl.module)
            self.modules.append((module, decls))

    def load_all(self, filename):
        assert filename.endswith('.plst')
        self.dir_name, module = filename[:-5].rsplit('/', 1)

        self.modules = []
        self.loaded_modules = set()
        self.load(module)
        return self.modules
