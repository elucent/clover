#include "rt/common/file.h"

namespace file {
    FileStorage* fd_table[MAX_FDS] ASMLABEL("file.fd_table");

    fd map_new_fd(file::Kind kind, NativeFD sysfd, const_slice<i8> path) {
        if (path.size() + 64 >= FDBUF_SIZE) return -1;
        fd next_file = 0;
        FileStorage* info = nullptr;
        while (next_file < MAX_FDS) {
            if (!fd_table[next_file]) {
                auto pages = memory::map(FDBUF_SIZE / memory::PAGESIZE);
                if (!pages.data()) return -1;
                info = (FileStorage*)pages.data();
            }
            else if (fd_table[next_file]->meta.kind == file::NONE)
                info = fd_table[next_file];

            if (info) {
                info->meta.kind = kind;
                info->meta.mutex = 0;
                info->meta.sysfd = sysfd;
                info->pathlen = path.size();
                bool nulled = path[path.size() - 1] == '\0';
                if (!nulled) info->pathlen ++;
                memory::copy(info->path, path.data(), path.size());
                if (!nulled) info->path[path.size()] = '\0';
                info->meta.start = info->meta.end = 0;
                fd_table[next_file] = info;
                return next_file;
            }
            else next_file ++;
        }
        return -1;
    }

    extern Kind kind(fd file) {
        if (file < 0 || file >= MAX_FDS) return file::NONE;
        return fd_table[file]->meta.kind;
    }
}