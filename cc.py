import sys

import code_generator
import importer
import kind_checker
import lexer
import lower_control_flow
import monomorphize
import output
import plst_parser
import reorganize
import sort_checker
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
    modules = importer.Importer(parse).load_all(sys.argv[1])

    modules = map_modules(sort_checker.sort_check, modules)
    modules = apply_module_pass(kind_checker.kind_check, modules)
    modules = apply_module_pass(type_checker.type_check, modules)
    things = reorganize.reorganize(modules)
    things = lower_control_flow.lower_control_flow(things)
    instances = monomorphize.monomorphize(things)
    cg_decls = code_generator.generate(things, instances)

    with open(sys.argv[2], 'w') as fd:
        writer = output.LLVMWriter(fd)
        writer.writeout_prelude()
        writer.writeout_decls(cg_decls)
main()
