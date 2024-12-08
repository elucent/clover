#ifndef RT_COMMON_FILE_H
#define RT_COMMON_FILE_H

#include "rt/def.h"
#include "rt/sys.h"

namespace file {
    using NativeFD = i32;
    extern FileStorage* fd_table[MAX_FDS] ASMLABEL("file.fd_table");

    fd map_new_fd(file::Kind kind, NativeFD sysfd, const_slice<i8> path);
}

#endif