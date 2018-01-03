import sys

import cps_conversion
import importer
import lexer
import new_code_generator
import output
import plst_parser
import type_checker

def parse(src):
    tokens = lexer.Lexer(src).lex()
    return plst_parser.Parser(tokens).parse_file()

checked_module_interfaces = {}
checked_decls = {}
modules = importer.Importer(parse).load_all(sys.argv[1])

global_env = \
    type_checker.Environment(
        type_checker.global_type_environment,
        type_checker.global_term_environment,
    )

for module_name, decls in modules:
    env = \
        type_checker.Environment(
            {},
            {},
            global_env,
            module_name = module_name,
            modules = checked_module_interfaces,
        )
    decls = env.check_top_level_decls(decls)
    checked_module_interfaces[module_name] = \
        type_checker.ModuleInterface(
            env.term_bindings,
            env.type_bindings,
        )
    checked_decls[module_name] = decls

cg = new_code_generator.CodeGenerator()

for module_name, _ in modules:
    type_checked_decls = checked_decls[module_name]
    cg.generate(module_name, type_checked_decls)

with open(sys.argv[2], 'w') as fd:
    writer = output.LLVMWriter(fd)
    writer.writeout_prelude()
    writer.writeout_decls(cg.get_decls())
