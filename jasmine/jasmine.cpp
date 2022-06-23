#include "jasmine/jasmine.h"
#include "jasmine/obj.h"
#include "core/util.h"

bool startswith(const i8* str, const i8* prefix) {
    iptr n = cidx(prefix, '\0');
    for (iptr i = 0; i < n; i ++) {
        if (str[i] != prefix[i]) return false;
    }
    return true;
}

const i8* CMD;

enum ConflictStrategy {
    KEEP_NEWER, KEEP_OLDER, CONFLICT_ERROR
};

ConflictStrategy conflict;

OptLevel opt_level = OPT_1;

struct Path {
    const_slice<i8> obj, mod;
};

bool is_path(const i8* path) {
    for (i32 i = 0; i < cidx(path, '\0') - 1; i ++) if (path[i] == ':') return true;
    return false;
}

Path parse_path(i8* path) {
    auto colon = cidx(path, ':');
    Path p = {
        { path, colon },
        { path + colon + 1, cidx(path, '\0') - colon - 1 }
    };
    path[colon] = '\0';
    return p;
}

i32 help(const i8* cmd) {
    print("Usage: ", cmd, " [options...] <subcommand> files...\n");
    print("\n");
    print("Subcommands:\n");
    print("  run <path>:                Loads and executes the selected module, defaults to first in the object.\n");
    print("  main <path> [module|func]: Sets the entry module or function within the object or module.\n");
    print("  info <path> [module|func]: Prints the contents of the selected module or function.\n");
    print("  ls <path>:                 Lists the defined modules/symbols in the selected object or module.\n");
    print("  cat <path> [files...]:     Combines the objects in [files] into a single object at <path>.\n");
    print("  rm <path> [module|func]:   Removes the selected module or function from the object.\n");
    print("Options:\n");
    print("  -h, --help:                Prints usage information.\n");
    print("  --keep-newer:              Resolve duplicates by keeping the newer version of the module.\n");
    print("  --keep-older:              Resolve duplicates by keeping the older version of the module.\n");
    return 1;
}

i32 run(i32 argc, i8** argv) {
    if (argc == 0) {
        print("Expected object path.\n\n");
        return help(CMD);
    }
    if (argc > 1) {
        print("Unexpected argument '", argv[1], "'.\n\n");
        return help(CMD);
    }

    const Target& target = DEFAULT_TARGET;

    if (is_path(argv[0])) {
        Path path = parse_path(argv[0]);
        stream* file = open(path.obj.ptr, FP_READ);
        if (!file) {
            print("Could not open file '", path.obj, "'.\n\n");
            return 1;
        }
        JasmineObject obj(*file);
        auto mod = obj.modules.find(path.mod);
        if (mod != obj.modules.end()) {
            mod->value->target(DEFAULT_TARGET, opt_level);
        }
        else {
            print("Binary ", path.obj, " contains no such module '", path.mod, "'.");
            return 1;
        }
        close(file);
    }
    else {
        stream* file = open(argv[0], FP_READ);
        if (!file) {
            print("Could not open file '", argv[0], "'.\n\n");
            return 1;
        }
        JasmineObject obj(*file);
        for (const auto& mod : obj.moduleseq) {
            mod->target(DEFAULT_TARGET, opt_level);
        }
        close(file);
    }
    return 0;
}

i32 setmain(i32 argc, i8** argv) {
    //
}

i32 info(i32 argc, i8** argv) {
    //
}

i32 ls(i32 argc, i8** argv) {
    if (argc == 0) {
        print("Expected object path.\n\n");
        return help(CMD);
    }
    if (argc > 1) {
        print("Unexpected argument '", argv[1], "'.\n\n");
        return help(CMD);
    }

    if (is_path(argv[0])) {
        Path path = parse_path(argv[0]);
        stream* file = open(path.obj.ptr, FP_READ);
        if (!file) {
            print("Could not open file '", path.obj, "'.\n\n");
            return 1;
        }
        JasmineObject obj(*file);
        auto mod = obj.modules.find(path.mod);
        if (mod != obj.modules.end()) {
            for (const auto& e : mod->value->data.entries) {
                format_type(mod->value->types, e.type, stdout);
                print(' ', mod->value->strings.strings[e.name], '\n');
            }
            for (const auto& e : mod->value->stat.entries) {
                format_type(mod->value->types, e.type, stdout);
                print(' ', mod->value->strings.strings[e.name], '\n');
            }
            for (const auto& f : mod->value->funcs) {
                f->formatshort(stdout);
                print('\n');
            }
        }
        else {
            print("Binary ", path.obj, " contains no such module '", path.mod, "'.");
            return 1;
        }
        close(file);
    }
    else {
        stream* file = open(argv[0], FP_READ);
        if (!file) {
            print("Could not open file '", argv[0], "'.\n\n");
            return 1;
        }
        JasmineObject obj(*file);
        for (const auto& mod : obj.moduleseq) {
            mod->formatshort(stdout);
            print('\n');
        }
        close(file);
    }

    return 0;
}

i32 cat(i32 argc, i8** argv) {
    vec<JasmineObject*> objs;
    for (i32 i = 0; i < argc; i ++) {
        stream* s = open(argv[i], FP_READ);
        if (!s) {
            print("Could not open file '", argv[i], "'.");
            return 1;
        }
        objs.push(new JasmineObject(*s));
        close(s);
    }
    bool errored = false;
    for (i32 i = 1; i < argc; i ++) {
        for (JasmineModule* m : objs[i]->moduleseq) {
            auto it = objs[0]->modules.find(m->strings.strings[m->meta.modname]);
            if (it == objs[0]->modules.end()) {
                objs[0]->moduleseq.push(m);
                objs[0]->modules.put(m->strings.strings[m->meta.modname], m);
            }
            else {
                if (conflict == CONFLICT_ERROR) {
                    print("Module conflict: ", it->key, ':', it->value->meta.ver, " from ", argv[0], " and ", it->key, ':', m->meta.ver, ".\n");
                    errored = true;
                }
                else if (conflict == KEEP_NEWER) it->value = (it->value->meta.ver >= m->meta.ver ? it->value : m);
                else if (conflict == KEEP_OLDER) it->value = (it->value->meta.ver <= m->meta.ver ? it->value : m);
            }
        }
    }
    if (errored) return 1;

    if (argc) {
        for (auto& m : objs[0]->moduleseq) m = objs[0]->modules[m->strings.strings[m->meta.modname]];

        stream* out = open(argv[0], FP_WRITE);
        objs[0]->write(*out);
        close(out);
    }
    return 0;
}

i32 rm(i32 argc, i8** argv) {
    if (argc == 0) {
        print("Expected object path.\n\n");
        return help(CMD);
    }
    if (argc == 1) {
        print("Expected symbol(s) to delete.\n\n");
        return help(CMD);
    }

    if (is_path(argv[0])) {
        // Path path = parse_path(argv[0]);
        // stream* file = open(path.obj.ptr, FP_READ);
        // if (!file) {
        //     print("Could not open file '", path.obj, "'.\n\n");
        //     return 1;
        // }
        // JasmineObject obj(*file);
        // auto mod = obj.modules.find(path.mod);
        // bool errored = false;
        // if (mod != obj.modules.end()) {
        //     for (u32 i = 1; i < argc; i ++) {
        //         const_slice<i8> name = { argv[i], cidx(argv[i], '\0') };
        //         if (mod->value->data.entrymap.find(name) == mod->value->data.entrymap.end()
        //             && mod->value->stat.entrymap.find(name) == mod->value->stat.entrymap.end()
        //             && mod->value->funcmap.find(name) == mod->value->funcmap.end()) {
        //             print("Module ", mod->key, " does not define '", argv[i], "'.");
        //             errored = true;
        //         }
        //     }
        //     if (errored) return 1;
        //     for (u32 i = 1; i < argc; i ++) {
        //         const_slice<i8> name = { argv[i], cidx(argv[i], '\0') };
        //         if (mod->value->data.entrymap.find(name) == mod->value->data.entrymap.end()) 
        //             mod->value->data.entrymap.erase(name);
        //         if (mod->value->stat.entrymap.find(name) == mod->value->stat.entrymap.end()) 
        //             mod->value->stat.entrymap.erase(name);
        //         if (mod->value->funcmap.find(name) == mod->value->funcmap.end()) 
        //             mod->value->funcmap.erase(name);
        //     }
        // }
        // else {
        //     print("Binary ", path.obj, " contains no such module '", path.mod, "'.");
        //     return 1;
        // }
        // close(file);
    }
    else {
        stream* file = open(argv[0], FP_READ);
        if (!file) {
            print("Could not open file '", (const i8*)argv[0], "'.\n\n");
            return 1;
        }
        JasmineObject obj(*file);
        close(file);
        bool errored = false;
        for (u32 i = 1; i < argc; i ++) {
            const_slice<i8> name = { argv[i], cidx(argv[i], '\0') };
            auto it = obj.modules.find(name);
            if (it == obj.modules.end()) {
                print("Binary ", argv[0], " contains no such module '", argv[i], "'.");
                errored = true;
            }
        }
        if (errored) return 1;
        for (u32 i = 1; i < argc; i ++) {
            const_slice<i8> name = { argv[i], cidx(argv[i], '\0') };
            auto it = obj.modules.find(name);
            if (it != obj.modules.end()) obj.modules.erase(name);
        }
        file = open(argv[0], FP_WRITE);
        obj.write(*file);
        close(file);
    }
    return 0;
}

i32 jasmine_main(i32 argc, i8** argv) {
    if (argc == 1) return help(argv[0]);

    CMD = argv[0];

    // Options
    for (int i = 1; i < argc; i ++) {
        i8* arg = argv[i];
        if (!mcmp(arg, "-h", 3) || !mcmp(arg, "--help", 7) || !mcmp(arg, "help", 5)) {
            return help(argv[0]);
        }
        else if (!mcmp(arg, "-O0", 4)) { opt_level = OPT_0; }
        else if (!mcmp(arg, "-O1", 4)) { opt_level = OPT_1; }
        else if (!mcmp(arg, "-O2", 4)) { opt_level = OPT_2; }
        else if (!mcmp(arg, "-Os", 4)) { opt_level = OPT_SMALL; }
        else if (!mcmp(arg, "--keep-newer", 13)) { conflict = KEEP_NEWER; }
        else if (!mcmp(arg, "--keep-older", 13)) { conflict = KEEP_OLDER; }
        else if (!mcmp(arg, "run", 4)) {
            return run(argc - (i + 1), argv + i + 1);
        }
        else if (!mcmp(arg, "main", 5)) {
            return setmain(argc - (i + 1), argv + i + 1);
        }
        else if (!mcmp(arg, "info", 5)) {
            return info(argc - (i + 1), argv + i + 1);
        }
        else if (!mcmp(arg, "ls", 3)) {
            return ls(argc - (i + 1), argv + i + 1);
        }
        else if (!mcmp(arg, "cat", 4)) {
            return cat(argc - (i + 1), argv + i + 1);
        }
        else if (!mcmp(arg, "rm", 3)) {
            return rm(argc - (i + 1), argv + i + 1);
        }
        else return run(argc - i, argv + i);
    }

    return 0;
}