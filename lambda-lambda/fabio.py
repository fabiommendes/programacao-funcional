(# bf2c - LAMBDA-STYLE

# Importamos o módulo argparse para criar um parser para os argumentos da linha
# de comando do nosso compilador
(lambda
args=(lambda parser:
    parser.add_argument('source', help='Brainfuck input') and
    parser.add_argument('--output', '-o', help='Output C file (empty for stdin)') and
    parser.parse_args()
    )(  
        # Onde define-se:
        parser = __import__('argparse').ArgumentParser(description='bf2c - brainfuck compiler')
    ),

sys=__import__('sys'):

# Agora que temos os argumentos, abrimos os arquivos de leitura e de escrita
(lambda to_source,
write=(open(args.output, 'w') if args.output else sys.stdout).write,
source=open(args.source).read():

[ # Escrevemos o programa na saída usando a função write
write("#include<stdio.h>\n"),
write("\n"),
write("int main () {\n"),
write("    chr data[30000] = {0};  // ANSI Brainfuck uses a 30.000 items array\n"),
write("    int *ptr = chr;\n"),
write("\n"),
write("    // Reference Brainfuck code\n"),
write('\n'.join(f'    // {line}' for line in source.splitlines())),
write("\n\n"),
write("    // Implementation\n"),
write(to_source(source)),
write("\n"),
write("    return 0;\n"), 
write("}\n"),
])( 
    # onde aqui definimos a função que irá salvar o código bf em C
    to_source = 
        (lambda src:
            # Y-combinator
            (lambda f, *args: lambda *args: f(f, *args))(  # (while?)
                # Itera sobre comandos, executando a head.
                (lambda bf_compiler, head, cmds, to_c={'>':'        ++ptr;\n', 
                                                       '<':'        --ptr;\n;',
                                                       '+':'        ++*ptr;\n',
                                                       '-':'        --*ptr;\n',
                                                       '.':'        putchar(*ptr);\n',
                                                       ',':'        *ptr = getchar();\n',
                                                       '[':'    while (*ptr) {;\n',
                                                       ']':'    };\n',}:
                    to_c.get(head, '') + (
                        bf_compiler(bf_compiler, cmds[0], cmds[1:]) if cmds else ''
                    )
                )
            )(
                # Separamos o source na cabeça e cauda
                src and src[0], 
                src and src[1:],
            )
        )
)))()