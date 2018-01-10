from ir import *
from types import SimpleNamespace

def reorganize(modules):
    types = {}
    values = {}
    for module_name, decls in modules:
        for decl in decls:
            if decl.tag in ['function', 'constant', 'extern']:
                values[(module_name, decl.name)] = decl
            elif decl.tag == 'struct':
                types[(module_name, decl.name)] = decl
                values[(module_name, decl.name)] = decl
            elif decl.tag == 'import':
                pass
            else:
                print(decl.tag)
                raise NotImplementedError()
    return \
        SimpleNamespace(
            types = types,
            values = values,
        )
