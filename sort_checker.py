from ir import *
import plst_parser
import copy

class SortError(Exception):
    pass

types = copy.deepcopy(plst_parser.AST.types)

types['Kind'] = {
    'star': {},
    'nat': {},
}

SortAST = Representation('SortAST', types)

transformer = plst_parser.AST.transformer(SortAST)

@transformer.case('Kind')
def kind(_, node):
    if node.tag == 'named_kind':
        if node.name == 'nat':
            return Node('nat')
        else:
            raise SortError()
    elif node.tag == 'star':
        return Node('star')
    else:
        print(node)
        raise NotImplementedError()

def sort_check(decls):
    return transformer.transform(List('Decl'), (), decls)
