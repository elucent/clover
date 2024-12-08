module memory:
    const PageSize: 4096
    type Page: i8[PageSize]
    type Flags:
        bool read #1
        bool write #1
        bool exec #1

    Page[] map(u32 size)
    void tag(Page[] pages, Flags flags)
    void unmap(Page[] pages)

module path:
    type Path:
        Path* parent
        i8[] name
        bool isDirectory
    type BadPath:
        i8[] path

    Path from(i8[] text) raises BadFormat
    Path from(char[] text) raises BadFormat
    Path subpath(Path parent, Path child)

module fsys:
    use path.Path

    type fd: i32
    type Kind:
        case File
        case Directory
    type Flags:
        bool read #1
        bool write #1
        bool exec #1
        bool create #1
        bool append #1
    type Entity:
        Kind kind #3
        Flags flags #5
        fd id #24
    type Info:
        Kind kind
        u64 size

    type FileNotFound:
        Path path
    type BadPermissions:
        Path path
        Flags givenFlags
        Flags requiredFlags

    Entity open(Path name, Flags flags) raises FileNotFound, BadPermissions
    Info info(Path name) raises FileNotFound
    void rename(Path from, Path to) raises FileNotFound, BadPermissions
    void remove(Path name) raises FileNotFound, BadPermissions
    void close(Entity entity)

    Entity open(type PathLike, PathLike name, Flags flags) raises path.BadPath, FileNotFound, BadPermissions
    Info info(type PathLike, PathLike name) raises path.BadPath, FileNotFound
    void rename(type FromPathLike, type ToPathLike, FromPathLike from, ToPathLike to) raises path.BadPath, FileNotFound, BadPermissions
    void remove(type PathLike, PathLike name) raises path.BadPath, FileNotFound, BadPermissions

module file:
    File open(Path name, Flags flags) raises FileNotFound, BadPermissions, BadCast(Entity):
        fsys.open(name, flags).File()
    i32 read(File file, i8[] buffer)
    i32 write(File file, i8[] text)

module dir:
    type Entry:
        Path path
        case File
        case Directory

    Directory open(Path name, Flags flags) raises FileNotFound, BadPermissions, BadCast(Entity):
        fsys.open(name, flags).Directory()
    i32 read(Directory dir, Entry[] buffer)

module net:
    type Protocol:
        type Network:
            case IPv4
            case IPv6
        type Transport:
            case Stream
            case Datagram
            case Raw
        Network network #4
        Transport transport #4
    type Address:
        case IPv4: i8[4]
        case IPv6: i8[16]
    type Port: u16
    type Socket:
        Protocol protocol
        fsys.fd id #24
    type EmptyQueue

    Socket connect(Address address, Port port, Protocol protocol)
    void serve(Port port, Protocol protocol, u32 queueSize)
    Socket accept(Socket socket) raises EmptyQueue
    i32 receive(Socket socket, i8[] buffer)
    i32 send(Socket socket, i8[] msg)
    void close(Socket socket)

module process:
    type Thread: i32
    type Channel: i32

    void init()
    void deinit()
    void exit(i32 code)
    void lock(i8* ptr)
    void unlock(i8* ptr)
    Thread current() 
    i8* storage()
    Thread spawn(void(iptr) task, iptr parameter, i8* stack, i8* tls, u32 channels)
    void send(Thread thread, u32 channel, iptr msg)
    iptr receive(u32 channel)

module time:
    i64 seconds()
    i64 millis()
    i64 micros()
    i64 nanos()
    i64 cycles()

void crash()
void panic(...):
    file.stderr.write(..., '\n')
    crash()
