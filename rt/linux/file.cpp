#include "rt/linux/common.h"
#include "rt/common/file.h"
#include "sys/fcntl.h"
#include "dirent.h"

using namespace memory;

namespace file {
    enum Syscall {
        Read = 0,
        Write = 1,
        Open = 2,
        Close = 3,
        Stat = 4,
        Rename = 82,
        MkDir = 83,
        RmDir = 84,
        Unlink = 87,
        GetDents = 217
    };

    iword stdin ASMLABEL("file.stdin") = 0;
    iword stdout ASMLABEL("file.stdout") = 1;
    iword stderr ASMLABEL("file.stderr") = 2;

    static i8 buf64k[65536]; // Multi-purpose buffer.

    fd open(const_slice<i8> path, iword flags) ASMLABEL("file.open");
    fd open(const_slice<i8> path, iword flags) {
        if (path.size() >= 65535) return -1;
        memory::copy(buf64k, path.data(), path.size());
        if (buf64k[path.size() - 1] != '\0') buf64k[path.length ++] = '\0';

        iword sysflags = 0;
        if (flags & file::WRITE && flags & file::READ) {
            sysflags = O_RDWR | O_CREAT;
            if (flags & file::APPEND) sysflags |= 1024;
            else sysflags |= O_TRUNC;
        }
        else if (flags & file::WRITE) {
            sysflags = O_RDWR | O_CREAT;
            if (flags & file::APPEND) sysflags |= 1024;
            else sysflags |= O_TRUNC;
        }
        i32 sysfd = os_syscall(Open, (iword)buf64k, sysflags, 0666);
        if (sysfd < 0) return -1;
        else return map_new_fd(file::FILE, sysfd, path);
    }

    iword read(fd file, slice<i8> buf) ASMLABEL("file.read");
    iword read(fd file, slice<i8> buf) {
        if (file < 0 || file >= MAX_FDS) return 0;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE) return 0;
        return os_syscall(Read, info->meta.sysfd, (iword)buf.data(), buf.size());
    }

    iword write(fd file, const_slice<i8> buf) ASMLABEL("file.write");
    iword write(fd file, const_slice<i8> buf) {
        if (file < 0 || file >= MAX_FDS) return 0;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE) return 0;
        return os_syscall(Write, info->meta.sysfd, (iword)buf.data(), buf.size());
    }

    void remove(const_slice<i8> path) ASMLABEL("file.remove");
    void remove(const_slice<i8> path) {
        if (path.size() >= 65535) return;
        memory::copy(buf64k, path.data(), path.size());
        if (buf64k[path.size() - 1] != '\0') buf64k[path.length ++] = '\0';
        os_syscall(Unlink, (iword)buf64k);
    }

    void close(fd file) ASMLABEL("file.close");
    void close(fd file) {
        if (file < 0 || file >= MAX_FDS) return;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE) return;
        if (info->meta.kind == file::FILE) {
            // Flush file.
            file::write(file, {info->path + info->pathlen + info->meta.start, info->meta.end - info->meta.start});
        }
        os_syscall(Close, info->meta.sysfd);
        info->meta.kind = file::NONE;
    }

    FileInfo info(const_slice<i8> path) ASMLABEL("file.info");
    FileInfo info(const_slice<i8> path) {
        FileInfo info = {
            Kind::NONE, 0
        };
        if (path.size() >= 65535)
            return info;
        memory::copy(buf64k, path.data(), path.size());
        if (buf64k[path.size() - 1] != '\0')
            buf64k[path.length ++] = '\0';

        stat64 os_stat;
        if (os_syscall(Stat, (iword)buf64k, (iword)&os_stat) == -1)
            return info;

        if ((os_stat.st_mode & S_IFMT) == S_IFREG) {
            info.kind = Kind::FILE;
            info.size = os_stat.st_size;
        } else if ((os_stat.st_mode & S_IFMT) == S_IFDIR) {
            info.kind = Kind::DIR; 
        }

        return info;
    }
}

namespace dir {
    using NativeDirent = dirent;

    constexpr u32 DIRENTS_PER_64K = 65536 / sizeof(NativeDirent);

    fd open(const_slice<i8> path, iword flags) ASMLABEL("dir.open");
    fd open(const_slice<i8> path, iword flags) {
        if (path.size() >= 65535) return -1;
        memory::copy(buf64k, path.data(), path.size());
        if (buf64k[path.size() - 1] != '\0') buf64k[path.length ++] = '\0';

        iword sysflags = 0;
        if (flags & file::WRITE)
            sysflags = O_RDWR;
        i32 sysfd = os_syscall(Open, (iword)buf64k, sysflags, 0666);
        if (sysfd < 0) {
            if (flags & file::WRITE) sysfd = os_syscall(MkDir, (iword)buf64k, 0666);
            if (sysfd == -1) return -1;
        }
        return map_new_fd(file::DIR, sysfd, path);
    }

    iword read(fd file, slice<entry> buf) ASMLABEL("dir.read");
    iword read(fd file, slice<entry> buf) {
        if (file < 0 || file >= MAX_FDS) return 0;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE) return 0;
        slice<NativeDirent> dirents = { (NativeDirent*)buf64k, DIRENTS_PER_64K };
        if (buf.size() < dirents.size()) dirents.length = buf.size();
        iword read = os_syscall(GetDents, info->meta.sysfd, (iword)dirents.data(), dirents.size());
        for (iword i = 0; i < read; i ++) {
            file::Kind kind = file::NONE;
            switch (dirents[i].d_type) {
                case DT_DIR:
                    kind = file::DIR;
                    break;
                case DT_REG:
                    kind = file::FILE;
                    break;
                case DT_SOCK:
                    kind = file::SOCKET;
                    break;
                default:
                    kind = file::NONE;
                    break;
            }
            buf[i] = dir::entry{ dirents[i].d_name, kind, findc(dirents[i].d_name, '\0') };
        }
        return read;
    }

    void remove(const_slice<i8> path) ASMLABEL("dir.remove");
    void remove(const_slice<i8> path) {
        if (path.size() >= 65535) return;
        memory::copy(buf64k, path.data(), path.size());
        if (buf64k[path.size() - 1] != '\0') buf64k[path.length ++] = '\0';
        os_syscall(RmDir, (iword)buf64k);
    }

    void close(fd file) ASMLABEL("dir.close");
    void close(fd file) {
        if (file < 0 || file >= MAX_FDS) return;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE) return;
        os_syscall(Close, info->meta.sysfd);
        info->meta.kind = file::NONE;
    }
}