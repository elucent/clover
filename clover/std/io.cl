# System-agnostic representation for a file handle, managed by
# the clover runtime.
type fd: int

# File access flags.
int FP_NONE: 0, FP_READ: 1, FP_WRITE: 2, FP_EXEC: 4, FP_APPEND: 8

# Open and close files.
fd open(string path, int flags)
void close(fd file)

# Standard filehandles for input, output, and errors.
fd stdin, stdout, stderr

# Runtime-defined methods for writing primitive types to file.
void int.write(fd file)
void float.write(fd file)
void char.write(fd file)
void bool.write(fd file)
void string.write(fd file)
void unit.write(fd file)

# Byte and byteslice methods for reading and writing raw data.
void i8.putc(fd file)
void i8[].puts(fd file) 
i8 fd.getc()
i8[] fd.gets(int n)

# Reads a line from the file (up to the next occurrence of the newline char).
string fd.readline()

# Reads all lines from the file.
string[] fd.readlines()

# Returns whether all the contents of the provided file have been read.
bool fd.eof()

# Writes a value to the file followed by a newline.
void writeln(v, fd file):
    v.write(file)
    '\n'.write(file)

# Writes a value to standard output.
void print(v):
    v.write(stdout)

# Writes a value to standard output followed by a newline.
void println(v):
    v.print()
    '\n'.print()
