use std/io

const Width: 400, Height: 400

void mandelbrot():
    const Iter: 50, Limit: 2.0

    println("P4")
    print(Width)
    print(" ")
    println(Height)

    var bitnum: 0, byte: 0

    for y < Height:
        for x < Width:
            var zr: 0, zi: 0, tr: 0, ti: 0
            var cr: 2.0 * x / Width - 1.5
            var ci: 2.0 * y / Height - 1.0

            for i < Iter until tr + ti > Limit * Limit:
                zi = 2.0 * zr * zi + ci
                zr = tr - ti + cr
                tr = zr * zr
                ti = zi * zi

            byte <<= 1
            byte |= 1 if tr + ti <= Limit * Limit

            if ++ bitnum == 8:
                stdout.put(byte)
                byte = 0
                bitnum = 0
            else if x == Width - 1:
                stdout.put(byte << (8 - Width % 8))
                byte = 0
                bitnum = 0

mandelbrot()
stdout.flushOutput()

