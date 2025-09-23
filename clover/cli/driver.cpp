#include "clover/ast.h"
#include "clover/load.h"
#include "clover/gen.h"
#include "util/config.h"
#include "asm/arch.h"

using namespace clover;

extern "C" i32 main(i32 argc, i8** argv, i8** envp) {
    process::init(argc, argv, envp);

    parseOptions(argc, argv);

    Compilation compilation;
    const_slice<i8> firstPath, outputFile = cstring("");
    bool compileToObject = false; // Default to executable
    u32 numSourceFiles = 0;

    vec<const_slice<i8>> linkerArgs;

    for (u32 i = 1; i < argc; i ++) {
        auto arg = cstring(argv[i]);
        auto parseMultiCharArgument = [&](const_slice<i8> arg) {
            for (u32 i = 0; i < arg.size(); i ++) switch (arg[i]) {
                case 'c':
                    compileToObject = true;
                    break;
                default:
                    panic("Unexpected char '", arg[i], "' in multi-char argument '-", arg, "'.");
            }
        };
        if (arg[0] == '-') switch (arg[1]) {
            case 'l':
            case 'L':
                linkerArgs.push(cstring(argv[i]));
                break;
            case 'e': {
                if (numSourceFiles > 0)
                    panic("Can't specify inline source with '-e' in same command as file sources.");
                vec<i8> sourceVector;
                for (i ++; i < argc; i ++) {
                    auto cstr = cstring(argv[i]);
                    auto oldSize = sourceVector.size();
                    sourceVector.expandBy(cstr.size() + 1);
                    memory::copy(sourceVector.data() + oldSize, cstr.data(), cstr.size());
                    sourceVector.end()[-1] = ' ';
                }
                addSourceString(&compilation, cstring("<command-line>"), sourceVector.take_slice());
                break;
            }
            case 'o': {
                i ++;
                if (i >= argc)
                    panic("Expected path argument after '-o'.");
                outputFile = cstring(argv[i]);
                break;
            }
            case 'O':
                switch (arg[2]) {
                    case '0' ... '3': compilation.optimizationLevel = arg[2] - '0'; break;
                    default:
                        panic("Unrecognized optimization level '", arg[2], "' (options are 0, 1, 2, 3).");
                }
                break;
            case 'c':
                parseMultiCharArgument(arg.drop(1));
                break;
            default:
                panic("Unrecognized command-line argument '", arg, "'.");
        } else {
            auto path = cstring(argv[i]);
            addSourceFile(&compilation, path);
            numSourceFiles ++;
            firstPath = path;
        }
    }

    if (numSourceFiles > 1 && compileToObject)
        panic("TODO: Implement assembly object concatenation so we can compile multiple sources to a single relocatable object.");

    compile(&compilation);
    if UNLIKELY(config::printProducts)
        compilation.forEachArtifact([](Artifact* artifact) { artifact->print(); });

    if (compileToObject) {
        if (outputFile.size() == 0) { // Implicitly use modified name of first source as object.
            i32 lastDot = -1;
            for (u32 j = 0; j < firstPath.size(); j ++) if (firstPath[j] == '.') lastDot = j;
            vec<i8> newPath;
            for (u32 j = 0; j < (lastDot > 0 ? lastDot : firstPath.size()); j ++)
                newPath.push(firstPath[j]);
            newPath.push('.');
            newPath.push('o');
            outputFile = newPath.take_slice();
        }
        vec<JasmineAssembly> assemblies;
        compilation.forEachArtifact([&](Artifact* artifact) {
            assert(artifact->kind == ArtifactKind::Assembly);
            assemblies.push(getAssembly(artifact));
        });
        JasmineAssembly combined = jasmine_join_assemblies_in_place(assemblies.data(), assemblies.size());
        jasmine_write_relocatable_elf_object(combined, outputFile.data(), outputFile.size());
    } else {
        if (outputFile.size() == 0) { // Implicitly use modified name of first source as object.
            i32 lastDot = -1;
            for (u32 j = 0; j < firstPath.size(); j ++) if (firstPath[j] == '.') lastDot = j;
            vec<i8> newPath;
            for (u32 j = 0; j < (lastDot > 0 ? lastDot : firstPath.size()); j ++)
                newPath.push(firstPath[j]);
            newPath.push('.');
            newPath.push('o');
            outputFile = newPath.take_slice();
        } else {
            // Ensure ends with .o
            vec<i8> newPath;
            newPath.append(outputFile);
            if (newPath.size() < 2 || newPath.back() != 'o' || newPath[newPath.size() - 1] != '.') {
                newPath.push('.');
                newPath.push('o');
            }
            outputFile = newPath.take_slice();
        }
        vec<JasmineAssembly> assemblies;
        assemblies.push(createEntrypoint(&compilation));
        compilation.forEachArtifact([&](Artifact* artifact) {
            assert(artifact->kind == ArtifactKind::Assembly);
            assemblies.push(getAssembly(artifact));
        });
        JasmineAssembly combined = jasmine_join_assemblies_in_place(assemblies.data(), assemblies.size());
        jasmine_write_relocatable_elf_object(combined, outputFile.data(), outputFile.size());

        vec<const_slice<i8>> args;
        args.push(cstring("ld"));
        args.push(cstring("-z"));
        args.push(cstring("noexecstack"));

        // TODO: Actually look this up instead of hardcoding it lol.
        args.push(cstring("-dynamic-linker"));
        args.push(cstring("/lib64/ld-linux-x86-64.so.2"));

        args.push(outputFile);
        args.push(cstring("-o"));
        args.push(outputFile.take(outputFile.size() - 2));
        args.append(linkerArgs);
        process::exec(cstring("/usr/bin/env"), args);
        file::remove(outputFile);
    }

    return 0;
}