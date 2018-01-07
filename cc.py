import sys

import importer
import lexer
import plst_parser
import sort_checker
import kind_checker
import type_checker

def parse(src):
    tokens = lexer.Lexer(src).lex()
    return plst_parser.Parser(tokens).parse_file()

def map_modules(f, modules):
    return \
        [(module_name, f(decls))
            for module_name, decls in modules]

def apply_module_pass(f, modules):
    output = []
    output_dict = {}
    for module_name, decls in modules:
        module, module_interface = \
            f(module_name, output_dict, decls)
        output_dict[module_name] = module_interface
        output.append((module_name, module))
    return output

def main():
    checked_module_interfaces = {}
    checked_decls = {}
    modules = importer.Importer(parse).load_all(sys.argv[1])

    modules = map_modules(sort_checker.sort_check, modules)
    modules = apply_module_pass(kind_checker.kind_check, modules)
    modules = apply_module_pass(type_checker.type_check, modules)

    raise NotImplementedError()

    with open(sys.argv[2], 'w') as fd:
        writer = output.LLVMWriter(fd)
        writer.writeout_prelude()
        writer.writeout_decls(cg_decls)
main()
