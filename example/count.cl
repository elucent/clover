void int.print()

type Counter:
    int count

int Counter*.tick():
    this.count ++

Counter c
c.count = 0

(&c).tick()
c.count.print()

(&c).tick()
c.count.print()