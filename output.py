import sys

import cps_conversion
import importer
import lexer
import new_code_generator
import plst_parser
import type_checker

class LLVMWriter:
    def __init__(self, fd):
        self.fd = fd

    def write(self, string):
        return self.fd.write(string)

    def writeout_value(self, value):
        if isinstance(value, str):
            self.write(value)
        elif len(value) == 0:
            self.write("{}")
        else:
            self.write('{ ')
            self.writeout_arg_list(value)
            self.write(' }')

    def writeout_csl(self, items, function):
        if len(items) == 0:
            return
        for item in items[:-1]:
            function(item)
            self.write(", ")
        function(items[-1])

    def writeout_type_list(self, tys):
        self.writeout_csl(tys, self.writeout_type)

    def writeout_type(self, ty):
        if ty.tag == 'ptr_to':
            self.writeout_type(ty.ty)
            self.write('*')
        elif ty.tag == 'number':
            self.write('i')
            self.write(str(ty.width))
        elif ty.tag == 'array':
            self.write('[')
            self.write(str(ty.size))
            self.write(' x ')
            self.writeout_type(ty.of)
            self.write(']')
        elif ty.tag == 'named_type':
            self.write('%')
            self.write(ty.name)
        elif ty.tag == 'void':
            self.write('void')
        elif ty.tag == 'func':
            self.writeout_type(ty.return_type)
            self.write(' (')
            self.writeout_type_list(ty.arg_types)
            self.write(')')
        else:
            print(ty)
            raise NotImplementedError()

    def writeout_declare(self, decl):
        fd.write('declare ')
        self.writeout_type(decl.return_type)
        self.write(' ')
        self.write(decl.name)
        if len(decl.arg_types) == 0:
            self.write('()')
        else:
            self.write('(')
            self.writeout_type_list(decl.arg_types)
            self.write(')')
        self.write('\n')

    def writeout_struct(self, decl):
        self.write("%")
        self.write(decl.name)
        self.write(" = type { ")
        self.writeout_type_list(decl.fields)
        self.write(" }\n")

    def writeout_string(self, decl):
        self.write(decl.name)
        self.write(" = constant ")
        self.writeout_type(decl.ty)
        self.write(" c")
        self.write('"')
        self.write(decl.value)
        self.write('", align 1\n')

    def writeout_arg(self, arg):
        (ty, name) = arg
        self.writeout_type(ty)
        self.write(' ')
        self.writeout_value(name)

    def writeout_arg_list(self, args):
        self.writeout_csl(args, self.writeout_arg)

    def writeout_define(self, decl):
        self.write('define ')
        self.write(''.join(map(lambda x: x + ' ', decl.linkage)))
        self.writeout_type(decl.return_type)
        self.write(' ')
        self.write(decl.name)
        self.write('(')
        self.writeout_arg_list(decl.args)
        self.write(') {\n')
        for basic_block in decl.basic_blocks:
            self.writeout_basic_block(basic_block)
        self.write('}\n')

    def writeout_basic_block(self, basic_block):
        self.write(basic_block.label)
        self.write(':\n')
        for instruction in basic_block.instructions:
           self.writeout_instruction(instruction)
        self.writeout_terminator(basic_block.terminator)

    def writeout_terminator(self, terminator):
        if not terminator:
            self.write("  call void @llvm.trap()\n")
            self.write("  unreachable\n")
        elif terminator.tag == 'tail_call':
            if terminator.ret_type.tag == 'void':
                self.write("  tail call ")
                self.writeout_type(terminator.ret_type)
                self.write(" ")
                self.write(terminator.function)
                self.write("(")
                self.writeout_arg_list(terminator.args)
                self.write(")\n  ret void\n")
            else:
                raise NotImplementedError()
                self.write("  %$tail_output = tail call ")
                self.writeout_type(terminator.ret_type)
                self.write(" ")
                self.write(terminator.function)
                self.write("(")
                self.writeout_arg_list(terminator.args)
                self.write(")\n  ret %$tail_output\n")
        elif terminator.tag == 'return':
            self.write("  ret ")
            if terminator.ty.tag == 'void':
                self.write('void\n')
            else:
                self.writeout_type(terminator.ty)
                self.write(' ')
                self.writeout_value(terminator.value)
                self.write("\n")
        elif terminator.tag == 'unconditional_branch':
            self.write("  br label %")
            self.write(terminator.to)
            self.write("\n")
        elif terminator.tag == 'conditional_branch':
            self.write("  br i1 ")
            self.writeout_value(terminator.condition)
            self.write(", label %")
            self.write(terminator.true_block)
            self.write(", label %")
            self.write(terminator.false_block)
            self.write("\n")
        else:
            raise NotImplementedError()

    def writeout_instruction(self, instruction):
        self.write("  ")
        if instruction.tag == 'bitcast':
            self.write(instruction.dst)
            self.write(" = bitcast ")
            self.writeout_type(instruction.from_ty)
            self.write(" ")
            self.writeout_value(instruction.value)
            self.write(" to ")
            self.writeout_type(instruction.to_ty)
        elif instruction.tag == 'store':
            self.write("store ")
            self.writeout_type(instruction.ty)
            self.write(" ")
            self.writeout_value(instruction.source)
            self.write(", ")
            self.writeout_type(new_code_generator.ptr_to(instruction.ty))
            self.write(" ")
            self.writeout_value(instruction.dst)
            self.write(', align 1')
        elif instruction.tag == 'getelementptr':
            self.write(instruction.dst)
            self.write(" = getelementptr ")
            assert instruction.source_type.tag == 'ptr_to'
            self.writeout_type(instruction.dst_type)
            self.write(", ")
            self.writeout_type(instruction.source_type)
            self.write(" ")
            self.writeout_value(instruction.value)
            for off in instruction.offset:
                self.write(", i32 ")
                self.writeout_value(off)
        elif instruction.tag == 'binop':
            self.write(instruction.dst)
            self.write(" = ")
            self.write(instruction.op)
            self.write(" ")
            self.writeout_type(instruction.ty)
            self.write(" ")
            self.writeout_value(instruction.a)
            self.write(", ")
            self.writeout_value(instruction.b)
        elif instruction.tag == 'load':
            self.write(instruction.dst)
            self.write(" = load ")
            self.writeout_type(instruction.ty)
            self.write(", ")
            self.writeout_type(new_code_generator.ptr_to(instruction.ty))
            self.write(" ")
            self.writeout_value(instruction.source)
            self.write(', align 1')
        elif instruction.tag == 'select':
            self.write(instruction.dst)
            self.write(" = select i1 ")
            self.writeout_value(instruction.condition)
            self.write(", ")
            self.writeout_type(instruction.type)
            self.write(" ")
            self.writeout_value(instruction.true_value)
            self.write(", ")
            self.writeout_type(instruction.type)
            self.write(" ")
            self.writeout_value(instruction.false_value)
        elif instruction.tag == 'call':
            if instruction.return_type.tag == 'void':
                self.write("call ")
            else:
                self.write(instruction.dst)
                self.write(" = call ")
            self.writeout_type(instruction.return_type)
            self.write(" ")
            self.writeout_value(instruction.function)
            self.write("(")
            self.writeout_arg_list(instruction.args)
            self.write(")")
        elif instruction.tag == 'alloca':
            self.write(instruction.dst)
            self.write(" = alloca ")
            self.writeout_type(instruction.ty)
        elif instruction.tag == 'cast':
            self.write(instruction.dst)
            self.write(" = ")
            self.write(instruction.mode)
            self.write(" ")
            self.writeout_type(instruction.from_ty)
            self.write(" ")
            self.writeout_value(instruction.value)
            self.write(" to ")
            self.writeout_type(instruction.to_ty)
        elif instruction.tag == 'icmp':
            self.write(instruction.dst)
            self.write(" = icmp ")
            self.write(instruction.mode)
            self.write(" ")
            self.writeout_type(instruction.ty)
            self.write(" ")
            self.writeout_value(instruction.a)
            self.write(", ")
            self.writeout_value(instruction.b)
        elif instruction.tag == 'extractvalue':
            self.write(instruction.dst)
            self.write(" = extractvalue ")
            self.writeout_type(instruction.ty)
            self.write(" ")
            self.writeout_value(instruction.value)
            for index in instruction.indices:
                self.write(", ")
                self.write(str(index))
        elif instruction.tag == 'not':
            self.write(instruction.dst)
            self.write(" = sub i1 1, ")
            self.write(instruction.value)
        else:
            print(instruction.tag)
            raise NotImplementedError()
        self.write("\n")

    def writeout_global(self, decl):
        self.write(decl.name)
        self.write(" = global ")
        self.writeout_type(decl.ty)
        self.write(' 0\n')

    def writeout_global_constructors(self, decl):
        self.write("@llvm.global_ctors = appending global [")
        self.write(str(len(decl.funcs)))
        self.write(" x %$constructor] [")
        first = True
        for func in decl.funcs:
            if not first:
                self.write(", ")
            first = False
            self.write("%$constructor { i32 65535, void ()* ")
            self.writeout_value(func)
            self.write(", i8* null }")
        self.write("]\n")

    def writeout_decl(self, decl):
        if decl.tag == 'declare':
            self.writeout_declare(decl)
        elif decl.tag == 'struct':
            self.writeout_struct(decl)
        elif decl.tag == 'define':
            self.writeout_define(decl)
        elif decl.tag == 'string':
            self.writeout_string(decl)
        elif decl.tag == 'global':
            self.writeout_global(decl)
        elif decl.tag == 'global_constructors':
            self.writeout_global_constructors(decl)
        else:
            raise NotImplementedError()

    def writeout_prelude(self):
        self.write("declare void @llvm.trap()\n")
        self.write("%$constructor = type { i32, void ()*, i8* }")
        self.write("\n")

    def writeout_decls(self, decls):
        for decl in decls:
            self.writeout_decl(decl)

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
            modules = checked_module_interfaces,
        )
    decls = env.check_top_level_decls(decls)
    checked_module_interfaces[module_name] = \
        type_checker.ModuleInterface(
            env.term_bindings,
            env.type_bindings,
        )
    checked_decls[module_name] = decls

code_generator = new_code_generator.CodeGenerator()

for module_name, _ in modules:
    type_checked_decls = checked_decls[module_name]
    code_generator.generate(module_name, type_checked_decls)

with open(sys.argv[2], 'w') as fd:
    writer = LLVMWriter(fd)
    writer.writeout_prelude()
    writer.writeout_decls(code_generator.get_decls())
