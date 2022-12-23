#include "core/def.h"
#include "core/sys.h"

enum Syscall {
    READ = 0,
    WRITE = 1,
    OPEN = 2,
    CLOSE = 3,
    POLL = 7,
    MMAP = 9,
    MPROTECT = 10,
    MUNMAP = 11,
    SOCKET = 41,
    CONNECT = 42,
    ACCEPT = 43,
    SHUTDOWN = 48,
    BIND = 49,
    LISTEN = 50,
    RENAME = 82,
    MKDIR = 83,
    RMDIR = 84,
    GETDENTS = 217
};

extern "C" iptr do_syscall(iptr code = 0, iptr a = 0, iptr b = 0, iptr c = 0, iptr d = 0, iptr e = 0) ASM_LABEL("do_syscall");

// Filesystem

extern "C" fd libcore_map_new_fd(FileMeta::Kind kind, i32 sysfd, const_slice<i8> path) {
    if (path.n + 64 >= FDBUF_SIZE) return -1;
    fd next_file = 0;
    FileInfo* info = nullptr;
    while (next_file < MAX_FDS) {
        if (!fd_table[next_file]) {
            auto pages = memory_map(FDBUF_SIZE / PAGESIZE);
            if (!pages.ptr) return -1;
            info = (FileInfo*)pages.ptr;
        }
        else if (fd_table[next_file]->meta.kind == FileMeta::NONE)
            info = fd_table[next_file];

        if (info) {
            info->meta = FileMeta{kind, sysfd};
            info->pathlen = path.n;
            bool nulled = path[path.n - 1] == '\0';
            if (!nulled) info->pathlen ++;
            mcpy(info->path, path.ptr, path.n);
            if (!nulled) info->path[path.n] = '\0';
            info->meta.start = info->meta.end = sizeof(FileInfo) + info->pathlen;
            fd_table[next_file] = info;
            return next_file;
        }
        else next_file ++;
    }
    return -1;
}

constexpr u32 O_RDONLY = 0, O_WRONLY = 1, O_RDWR = 2, O_APPEND = 1024, O_CREAT = 64, O_TRUNC = 512, O_NONBLOCK = 2048;

bool libcore_canopen(fd dir, const_slice<i8> path) {
    return true;
}

static i8 buf64k[65536]; // Multi-purpose buffer.

extern "C" fd libcore_file_open_impl(const_slice<i8> path, iptr flags) {
    if (path.n >= 65535) return -1;
    mcpy(buf64k, path.ptr, path.n);
    if (buf64k[path.n - 1] != '\0') buf64k[path.n ++] = '\0';

    iptr sysflags = 0;
    if (flags & FP_WRITE && flags & FP_READ) {
        sysflags = O_RDWR | O_CREAT;
        if (flags & FP_APPEND) sysflags |= 1024;
        else sysflags |= O_TRUNC;
    }
    else if (flags & FP_WRITE) {
        sysflags = O_RDWR | O_CREAT;
        if (flags & FP_APPEND) sysflags |= 1024;
        else sysflags |= O_TRUNC;
    }
    i32 sysfd = do_syscall(OPEN, (iptr)buf64k, sysflags, 0666);
    if (sysfd < 0) return -1;
    else return libcore_map_new_fd(FileMeta::FILE, sysfd, path);
}

extern "C" iptr libcore_file_read_impl(fd file, slice<i8> buf) {
    if (file < 0 || file >= MAX_FDS) return 0;
    FileInfo* info = fd_table[file];
    if (!info || info->meta.kind == FileMeta::NONE) return 0;
    return do_syscall(READ, info->meta.sysfd, (iptr)buf.ptr, buf.n);
}

extern "C" iptr libcore_file_write_impl(fd file, const_slice<i8> buf) {
    if (file < 0 || file >= MAX_FDS) return 0;
    FileInfo* info = fd_table[file];
    if (!info || info->meta.kind == FileMeta::NONE) return 0;
    return do_syscall(WRITE, info->meta.sysfd, (iptr)buf.ptr, buf.n);
}

extern "C" void libcore_file_close_impl(fd file) {
    if (file < 0 || file >= MAX_FDS) return;
    FileInfo* info = fd_table[file];
    if (!info || info->meta.kind == FileMeta::NONE) return;
    if (info->meta.kind == FileMeta::FILE) {
        // Flush file.
        file_write(file, {info->path + info->pathlen + info->meta.start, info->meta.end - info->meta.start});
    }
    do_syscall(CLOSE, info->meta.sysfd);
    info->meta.kind = FileMeta::NONE;
}

struct sys_dirent {
    i64 inode;
    u64 offset;
    u16 reclen;
    u8 type;
    i8 name[256];
};

constexpr u32 DIRENTS_PER_64K = 65536 / sizeof(sys_dirent);

extern "C" fd libcore_dir_open_impl(const_slice<i8> path, iptr flags) {
    if (path.n >= 65535) return -1;
    mcpy(buf64k, path.ptr, path.n);
    if (buf64k[path.n - 1] != '\0') buf64k[path.n ++] = '\0';

    iptr sysflags = 0;
    if (flags & FP_WRITE)
        sysflags = O_RDWR;
    i32 sysfd = do_syscall(OPEN, (iptr)buf64k, sysflags, 0666);
    if (sysfd < 0) {
        if (flags & FP_WRITE) sysfd = do_syscall(MKDIR, (iptr)buf64k, 0666);
        if (sysfd == -1) return -1;
    }
    return libcore_map_new_fd(FileMeta::DIR, sysfd, path);
}

constexpr u32 DT_DIR = 4, DT_REG = 8, DT_LNK = 10, DT_SOCK = 12;

extern "C" iptr libcore_dir_read_impl(fd file, slice<dir_entry> buf) {
    if (file < 0 || file >= MAX_FDS) return 0;
    FileInfo* info = fd_table[file];
    if (!info || info->meta.kind == FileMeta::NONE) return 0;
    slice<sys_dirent> dirents = { (sys_dirent*)buf64k, DIRENTS_PER_64K };
    if (buf.n < dirents.n) dirents.n = buf.n;
    iptr read = do_syscall(GETDENTS, info->meta.sysfd, (iptr)dirents.ptr, dirents.n);
    for (iptr i = 0; i < read; i ++) {
        FileMeta::Kind kind = FileMeta::Kind::NONE;
        switch (dirents[i].type) {
            case DT_DIR:
                kind = FileMeta::DIR;
                break;
            case DT_REG:
                kind = FileMeta::FILE;
                break;
            case DT_SOCK:
                kind = FileMeta::SOCKET;
                break;
            default:
                kind = FileMeta::NONE;
                break;
        }
        buf[i] = dir_entry{ dirents[i].name, kind, cidx(dirents[i].name, '\0') };
    }
    return read;
}

extern "C" void libcore_dir_remove_impl(const_slice<i8> path) {
    if (path.n >= 65535) return;
    mcpy(buf64k, path.ptr, path.n);
    if (buf64k[path.n - 1] != '\0') buf64k[path.n ++] = '\0';
    do_syscall(RMDIR, (iptr)buf64k);
}

extern "C" void libcore_dir_close_impl(fd file) {
    if (file < 0 || file >= MAX_FDS) return;
    FileInfo* info = fd_table[file];
    if (!info || info->meta.kind == FileMeta::NONE) return;
    do_syscall(CLOSE, info->meta.sysfd);
    info->meta.kind = FileMeta::NONE;
}

// Networking

struct sockaddr {
    u16 family;
    u16 port;
    union {
        u32 ipv4;
        struct { u32 ipv6_flowinfo, ipv6[4], ipv6_scopeid; };
    };
};

constexpr u16 AF_INET = 2, AF_INET6 = 10;
constexpr u32 SOCK_STREAM = 1, SOCK_DGRAM = 2, SOCK_RAW = 3, SOCK_NONBLOCK = 2048;

netport libcore_htons(netport port) {
    union {
        netport port16;
        u8 data[2];
    };
    port16 = port;
    swap(data[0], data[1]);
    return port16;
}

i32 libcore_htonl(i32 word) {
    union {
        i32 word32;
        u8 data[4];
    };
    word32 = word;
    swap(data[0], data[3]);
    swap(data[1], data[2]);
    return word32;
}

void libcore_hton6(netaddr& addr) {
    for (i32 i = 0; i < 4; i ++) addr.data[i] = libcore_htonl(addr.data[i]);
    swap(addr.data[0], addr.data[3]);
    swap(addr.data[1], addr.data[2]);
}

sockaddr libcore_make_sockaddr(netaddr addr, netport port, iptr flags) {
    sockaddr saddr;
    saddr.family = flags & IP_V6 ? AF_INET6 : AF_INET;
    saddr.port = libcore_htons(port);
    if (flags & IP_V6) {
        saddr.ipv6_flowinfo = 0;
        libcore_hton6(addr);
        mcpy(saddr.ipv6, addr.data, 16);
        saddr.ipv6_scopeid = 0;
    }
    else saddr.ipv4 = libcore_htonl(addr.data[0]);
    return saddr;
}

netaddr libcore_get_netaddr(sockaddr saddr, iptr flags) {
    netaddr addr;
    if (flags & IP_V6) {
        mcpy(addr.data, saddr.ipv6, 16);
        libcore_hton6(addr);
    }
    else addr.data[0] = libcore_htonl(saddr.ipv4);
    return addr;
}

extern "C" fd libcore_net_open_impl(iptr flags) {
    u32 socktype = SOCK_STREAM;
    if (flags & IP_DGRAM) socktype = SOCK_DGRAM;
    else if (flags & IP_RAW) socktype = SOCK_RAW;
    i32 sysfd = do_syscall(SOCKET, flags & IP_V6 ? AF_INET6 : AF_INET, socktype | SOCK_NONBLOCK, 0);
    if (sysfd < 0) return -1;
    else {
        fd sockfd = libcore_map_new_fd(FileMeta::SOCKET, sysfd, {"", (iptr)0});
        if (sockfd == -1) return -1;
        FileInfo* info = fd_table[sockfd];
        info->meta.socket_flags = (i8)flags;
        return sockfd;
    }
}

extern "C" void libcore_net_serve_impl(fd socket, netport port, iptr queue) {
    if (socket < 0 || socket >= MAX_FDS) return;
    FileInfo* info = fd_table[socket];
    if (!info || info->meta.kind != FileMeta::SOCKET) return;
    sockaddr saddr = libcore_make_sockaddr(netaddr{{0x7f000001, 0, 0, 0}}, port, info->meta.socket_flags);
    info->meta.addr = netaddr{{0x7f000001, 0, 0, 0}};
    info->meta.port = port;
    do_syscall(BIND, info->meta.sysfd, (iptr)&saddr, info->meta.socket_flags & IP_V6 ? 28 : 16);
    do_syscall(LISTEN, info->meta.sysfd, queue);
}

extern "C" connected libcore_net_connect_impl(fd socket, netaddr addr, netport port) {
    if (socket < 0 || socket >= MAX_FDS) return { true, true };
    FileInfo* info = fd_table[socket];
    if (!info || info->meta.kind != FileMeta::SOCKET) return { true, true };
    sockaddr saddr = libcore_make_sockaddr(addr, port, info->meta.socket_flags);
    info->meta.addr = addr;
    info->meta.port = port;
    iptr result = do_syscall(CONNECT, info->meta.sysfd, (iptr)&saddr, info->meta.socket_flags & IP_V6 ? 28 : 16);
    if (result < 0) {
        constexpr u32 EINPROGRESS = 115;
        if (-result == EINPROGRESS) return { false, false };
        else return { true, true };
    }
    return { false, true };
}

extern "C" accepted libcore_net_accept_impl(fd socket) {
    if (socket < 0 || socket >= MAX_FDS) return { -1, true };
    FileInfo* info = fd_table[socket];
    if (!info || info->meta.kind != FileMeta::SOCKET) return { -1, true };
    do_syscall(POLL, (iptr)&info->meta.sysfd, 1, 0);
    iptr result = do_syscall(ACCEPT, info->meta.sysfd, 0, info->meta.socket_flags & IP_V6 ? 28 : 16);
    if (result < 0) {
        constexpr u32 EAGAIN = 11;
        if (-result == EAGAIN) return { -1, false };
        else return { -1, true };
    }
    fd resultfd = libcore_map_new_fd(FileMeta::SOCKET, result, {"", (iptr)0});
    if (resultfd == -1) return { -1, true };
    FileInfo* result_info = fd_table[resultfd];
    result_info->meta.socket_flags = info->meta.socket_flags;
    // result_info->meta.addr = libcore_get_netaddr(saddr, info->meta.socket_flags);
    result_info->meta.port = info->meta.port;
    return { resultfd, false };
}

extern "C" void libcore_net_close_impl(fd socket) {
    if (socket < 0 || socket >= MAX_FDS) return;
    FileInfo* info = fd_table[socket];
    if (!info || info->meta.kind == FileMeta::NONE) return;
    constexpr u32 SHUT_RDWR = 2;
    do_syscall(SHUTDOWN, SHUT_RDWR);
    do_syscall(CLOSE, info->meta.sysfd);
    info->meta.kind = FileMeta::NONE;
}