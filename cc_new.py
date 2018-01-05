import sys

import importer
import lexer
import plst_parser
import sort_checker
import kind_checker
import type_checker_new as type_checker

def parse(src):
    tokens = lexer.Lexer(src).lex()
    return plst_parser.Parser(tokens).parse_file()

def main():
    checked_module_interfaces = {}
    checked_decls = {}
    modules = importer.Importer(parse).load_all(sys.argv[1])

    sort_checked = \
        [(module_name, sort_checker.sort_check(decls))
            for module_name, decls in modules]

    kind_checked = []
    modules = {}
    for module_name, decls in sort_checked:
        module, module_interface = \
            kind_checker.kind_check(module_name, modules, decls)
        modules[module_name] = module_interface
        kind_checked.append((module_name, module))

    type_checked = []
    modules = {}
    for module_name, decls in kind_checked:
        module, module_interface = \
            type_checker.type_check(module_name, modules, decls)
        modules[module_name] = module_interface
        type_checked.append((module_name, module))

main()
