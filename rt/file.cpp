#include "rt/sys.h"

#ifdef RT_LINUX
    #include "sys/fcntl.h"
    #include "sys/stat.h"
    #include "dirent.h"
    #include "unistd.h"
#endif

namespace file {
    using NativeFD = i32;
    FileStorage* fd_table[MAX_FDS] ASMLABEL("file.fd_table");
    fd max_file_yet = 2;

    fd map_new_fd(file::Kind kind, NativeFD sysfd, const_slice<i8> path) {
        if (path.size() + 64 >= FDBUF_SIZE) return -1;
        fd next_file = 0;
        FileStorage* info = nullptr;
        while (next_file < MAX_FDS) {
            if (!fd_table[next_file]) {
                auto pages = memory::map(FDBUF_SIZE);
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
                if (next_file > max_file_yet) max_file_yet = next_file;
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

    enum Syscall {
        Read = 0,
        Write = 1,
        Open = 2,
        Close = 3,
        Stat = 4,
        GetCwd = 79,
        Rename = 82,
        MkDir = 83,
        RmDir = 84,
        Unlink = 87,
        GetDents = 217,
        ReadlinkAt = 267
    };

    #ifdef RT_LINUX
        fd stdin ASMLABEL("file.stdin") = 0;
        fd stdout ASMLABEL("file.stdout") = 1;
        fd stderr ASMLABEL("file.stderr") = 2;
    #else
        #error "Unimplemented for platform: stdin, stdout, stderr"
    #endif

    static i8 buf64k[65536]; // Multi-purpose buffer.

    fd open(const_slice<i8> path, iword flags) ASMLABEL("file.open");
    fd open(const_slice<i8> path, iword flags) {
        if (path.size() >= 65535) return -1;
        memory::copy(buf64k, path.data(), path.size());
        if (buf64k[path.size() - 1] != '\0') buf64k[path.length ++] = '\0';

        iword sysflags = 0;
        i32 sysfd;

        #ifdef RT_LINUX
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
            sysfd = ::open(buf64k, sysflags, 0666);
        #else
            #error "Unimplemented syscall for platform: open"
        #endif

        if (sysfd < 0)
            return -1;
        return sysfd;
    }

    iword read(fd file, slice<i8> buf) ASMLABEL("file.read");
    iword read(fd file, slice<i8> buf) {
        if (file < 0 || file >= MAX_FDS)
            return 0;
        #ifdef RT_LINUX
            return ::read(file, buf.data(), buf.size());
        #else
            #error "Unimplemented syscall for platform: read"
        #endif
    }

    iword write(fd file, const_slice<i8> buf) ASMLABEL("file.write");
    iword write(fd file, const_slice<i8> buf) {
        if (file < 0 || file >= MAX_FDS)
            return 0;
        #ifdef RT_LINUX
            return ::write(file, buf.data(), buf.size());
        #else
            #error "Unimplemented syscall for platform: write"
        #endif
    }

    void close(fd file) ASMLABEL("file.close");
    void close(fd file) {
        if (file < 0 || file >= MAX_FDS) return;
        #ifdef RT_LINUX
            ::close(file);
        #else
            #error "Unimplemented syscall for platform: close"
        #endif
    }

    fd openbuf(const_slice<i8> path, iword flags) ASMLABEL("file.openbuf");
    fd openbuf(const_slice<i8> path, iword flags) {
        i32 sysfd = file::open(path, flags);
        if (sysfd < 0)
            return -1;
        return map_new_fd(file::FILE, sysfd, path);
    }

    iword readbuf(fd file, slice<i8> buf) ASMLABEL("file.readbuf");
    iword readbuf(fd file, slice<i8> buf) {
        if (file < 0 || file >= MAX_FDS)
            return 0;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE)
            return 0;
        return file::read(info->meta.sysfd, buf);
    }

    iword writebuf(fd file, const_slice<i8> buf) ASMLABEL("file.writebuf");
    iword writebuf(fd file, const_slice<i8> buf) {
        if (file < 0 || file >= MAX_FDS)
            return 0;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE)
            return 0;
        return file::write(info->meta.sysfd, buf);
    }

    void closebuf(fd file) ASMLABEL("file.closebuf");
    void closebuf(fd file) {
        if (file < 0 || file >= MAX_FDS) return;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE) return;
        if (info->meta.kind == file::FILE) {
            // Flush file.
            file::writebuf(file, {info->path + info->pathlen + info->meta.start, info->meta.end - info->meta.start});
        }
        file::close(info->meta.sysfd);
        info->meta.kind = file::NONE;
    }

    void remove(const_slice<i8> path) ASMLABEL("file.remove");
    void remove(const_slice<i8> path) {
        if (path.size() >= 65535)
            return;
        memory::copy(buf64k, path.data(), path.size());
        if (buf64k[path.size() - 1] != '\0')
            buf64k[path.length ++] = '\0';

        #ifdef RT_LINUX
            unlink(buf64k);
        #else
            #error "Unimplemented syscall for platform: unlink"
        #endif
    }

    FileInfo pathinfo(const_slice<i8> path) ASMLABEL("file.pathinfo");
    FileInfo pathinfo(const_slice<i8> path) {
        FileInfo info = {
            0, Kind::NONE
        };
        if (path.size() >= 65535)
            return info;
        memory::copy(buf64k, path.data(), path.size());
        if (buf64k[path.size() - 1] != '\0')
            buf64k[path.length ++] = '\0';

        #ifdef RT_LINUX
            struct stat os_stat;
            if (::stat(buf64k, &os_stat) == -1)
                return info;

            if ((os_stat.st_mode & S_IFMT) == S_IFREG) {
                info.kind = Kind::FILE;
                info.size = os_stat.st_size;
            } else if ((os_stat.st_mode & S_IFMT) == S_IFDIR) {
                info.kind = Kind::DIR;
            }
        #else
            #error "Unimplemented syscall for platform: stat"
        #endif

        return info;
    }

    FileInfo info(fd file) ASMLABEL("file.info");
    FileInfo info(fd file) {
        FileInfo info = {
            0, Kind::NONE
        };

        #ifdef RT_LINUX
            struct stat os_stat;
            if (::fstat(file, &os_stat) == -1)
                return info;

            if ((os_stat.st_mode & S_IFMT) == S_IFREG) {
                info.kind = Kind::FILE;
                info.size = os_stat.st_size;
            } else if ((os_stat.st_mode & S_IFMT) == S_IFDIR) {
                info.kind = Kind::DIR;
            }
        #else
            #error "Unimplemented syscall for platform: stat"
        #endif

        return info;
    }

    slice<i8> cwd(slice<i8> out) ASMLABEL("file.cwd");
    slice<i8> cwd(slice<i8> out) {
        #ifdef RT_LINUX
            getcwd(buf64k, 65536);
        #else
            #error "Unimplemented syscall for platform: getcwd"
        #endif
        u64 strlen = findc(buf64k, 0);
        if (out.size() < strlen)
            strlen = out.size();
        memory::copy(out.data(), buf64k, strlen);
        return out.take(strlen);
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
        i32 sysfd;

        #ifdef RT_LINUX
            if (flags & file::WRITE)
                sysflags = O_RDWR;
            sysfd = ::open(buf64k, sysflags, 0666);
            if (sysfd < 0) {
                if (flags & file::WRITE) sysfd = mkdir(buf64k, 0666);
                if (sysfd == -1) return -1;
            }
        #else
            #error "Unimplemented syscall for platform: mkdir"
        #endif

        return map_new_fd(file::DIR, sysfd, path);
    }

    iword read(fd file, slice<entry> buf) ASMLABEL("dir.read");
    iword read(fd file, slice<entry> buf) {
        if (file < 0 || file >= MAX_FDS)
            return 0;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE)
            return 0;
        slice<NativeDirent> dirents = { (NativeDirent*)buf64k, DIRENTS_PER_64K };
        if (buf.size() < dirents.size())
            dirents.length = buf.size();

        iword numRead;
        #ifdef RT_LINUX
            numRead = getdents64(info->meta.sysfd, dirents.data(), dirents.size());
            for (iword i = 0; i < numRead; i ++) {
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
                buf[i] = dir::entry{ dirents[i].d_name, (i32)findc(dirents[i].d_name, '\0'), kind };
            }
        #else
            #error "Unimplemented syscall for platform: getdents"
        #endif

        return numRead;
    }

    void remove(const_slice<i8> path) ASMLABEL("dir.remove");
    void remove(const_slice<i8> path) {
        if (path.size() >= 65535)
            return;
        memory::copy(buf64k, path.data(), path.size());
        if (buf64k[path.size() - 1] != '\0')
            buf64k[path.length ++] = '\0';

        #ifdef RT_LINUX
            rmdir(buf64k);
        #else
            #error "Unimplemented syscall for platform: rmdir"
        #endif
    }

    void close(fd file) ASMLABEL("dir.close");
    void close(fd file) {
        if (file < 0 || file >= MAX_FDS)
            return;
        FileStorage* info = fd_table[file];
        if (!info || info->meta.kind == file::NONE)
            return;

        #ifdef RT_LINUX
            ::close(info->meta.sysfd);
        #else
            #error "Unimplemented syscall for platform: close"
        #endif

        info->meta.kind = file::NONE;
    }
}