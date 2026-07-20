const I_IDLE: 1, I_WORK: 2, I_HANDLERA: 3, I_HANDLERB: 4, I_DEVA: 5, I_DEVB: 6
const PKTBIT: 1, WAITBIT: 2, HOLDBIT: 4
const S_RUN: 0, S_RUNPKT: 1, S_WAIT: 2, S_WAITPKT: 3, S_HOLD: 4, S_HOLDPKT: 5, S_HOLDWAIT: 6, S_HOLDWAITPKT: 7
const K_DEV: 1000, K_WORK: 1001
const MAXINT: 32767

const Count: 1000000, Qpktcountval: 2326410, Holdcountval: 930563

const BUFSIZE: 3

i8[27] alphabet: "0ABCDEFGHIJKLMNOPQRSTUVWXYZ"

use std/io

type PacketList:
    case Packet:
        PacketList* link
        i32 id, kind, a1
        i8[4] a2
    case None
use PacketList.Packet

PacketList* append(Packet* self, PacketList* list):
    self.link = PacketList.None
    if list is Packet* packet:
        while packet.link is Packet* next:
            packet = next
        packet.link = self
        return list
    return self

alias TaskFn: TaskList*(PacketList*)

type TaskData:
    case Idle: i64 v1, v2
    case Work: i64 v1, v2
    case Handler: PacketList* v1, v2
    case Device: PacketList* v1

type TaskList:
    case Task:
        TaskList* link
        i32 id, pri
        PacketList* wkq
        i32 state
        TaskFn fn
        TaskData data
    case None
use TaskList.Task

var data: TaskData.Idle(0, 0)

TaskList*[11] tasktab: [
    TaskList.None,
    TaskList.None,
    TaskList.None,
    TaskList.None,
    TaskList.None,
    TaskList.None,
    TaskList.None,
    TaskList.None,
    TaskList.None,
    TaskList.None,
    TaskList.None
]
TaskList* tasklist: TaskList.None, tcb: TaskList.None

i64 taskid: 0
i32 qpktcount: 0, holdcount: 0, layout: 0

own Task* createTask(i32 id, i32 pri, PacketList* wkq, i32 state, TaskFn fn, TaskData data):
    var t: new Task(tasklist, id, pri, wkq, state, fn, data)
    tasklist = t
    tasktab[id] = t
    return t

own Packet* createPacket(PacketList* link, i32 id, i32 kind):
    new Packet(link, id, kind, 0, [0, 0, 0, 0])

void trace(i8 a):
    if -- layout <= 0:
        println("")
        layout = 50
    stdout.put(a)

const tracing: false

void schedule():
    while tcb is Task* task:
        PacketList* pkt: PacketList.None

        if task.state == S_WAITPKT:
            var Packet* head: task.wkq
            pkt = head
            task.wkq = head.link
            task.state = S_RUN if task.wkq is PacketList.None else S_RUNPKT
        if task.state == S_RUN or task.state == S_RUNPKT or task.state == S_WAITPKT:
            taskid = task.id
            trace(i8(taskid + i8('0'))) if tracing

            data = task.data
            var newtcb: (task.fn)(pkt)
            task.data = data
            tcb = newtcb
        else:
            tcb = task.link

TaskList* wait():
    var Task* task: tcb
    task.state |= WAITBIT
    return tcb

TaskList* holdself():
    holdcount ++
    var Task* task: tcb
    task.state |= HOLDBIT
    return task.link

TaskList* findtcb(i32 id):
    var result: TaskList.None
    if 1 <= id < |tasktab|:
        result = tasktab[id]
    if result is TaskList.None:
        println("Bad task id")
    return result

TaskList* release(i32 id):
    var task: findtcb(id)
    if task is Task* t:
        t.state &= ~HOLDBIT as i32
        var Task* tcbtask: tcb
        if t.pri > tcbtask.pri:
            return task
    return tcb

TaskList* qpkt(PacketList* mpkt):
    var Packet* pkt: mpkt

    var t: findtcb(pkt.id)
    if t is TaskList.None:
        return t

    qpktcount ++
    pkt.link = PacketList.None
    pkt.id = i32(taskid)

    var Task* task: t
    if task.wkq is PacketList.None:
        task.wkq = pkt
        task.state |= PKTBIT
        var Task* tcbtask: tcb
        if task.pri > tcbtask.pri:
            return t
    else:
        pkt.append(task.wkq)
    return tcb

TaskList* idlefn(PacketList* pkt):
    var TaskData.Idle* idle: &data
    if -- idle.v2 == 0:
        return holdself()
    if idle.v1 & 1 == 0:
        idle.v1 = (idle.v1 >> 1) & MAXINT
        return release(I_DEVA)
    idle.v1 = ((idle.v1 >> 1) & MAXINT) ^ 0xD008
    return release(I_DEVB)

TaskList* workfn(PacketList* mpkt):
    var TaskData.Work* work: &data
    if mpkt is Packet* pkt:
        work.v1 = I_HANDLERA + I_HANDLERB - work.v1
        pkt.id = work.v1 as i32
        pkt.a1 = 0
        for i < BUFSIZE:
            work.v2 ++
            if work.v2 > 26:
                work.v2 = 1
            pkt.a2[i] = alphabet[work.v2]
        return qpkt(pkt)
    return wait()

TaskList* handlerfn(PacketList* mpkt):
    var TaskData.Handler* handler: &data
    if mpkt is Packet* pkt:
        var list: &handler.v1 if pkt.kind == K_WORK else &handler.v2
        *list = pkt.append(*list)

    if handler.v1 is Packet* workpkt:
        var count: workpkt.a1

        if count > BUFSIZE:
            handler.v1 = workpkt.link
            return qpkt(workpkt)

        if handler.v2 is Packet* devpkt:
            handler.v2 = devpkt.link
            devpkt.a1 = workpkt.a2[count]
            workpkt.a1 = count + 1
            return qpkt(devpkt)
    return wait()

TaskList* devfn(PacketList* mpkt):
    var TaskData.Device* device: &data
    if mpkt is PacketList.None:
        if device.v1 is Packet* pkt:
            device.v1 = PacketList.None
            return qpkt(pkt)
        return wait()
    else:
        device.v1 = mpkt
        if tracing:
            var Packet* pkt: mpkt
            trace(pkt.a1 as i8)
        return holdself()

void bench():
    println("Bench mark starting")  # Bench mark starting

    var idle: createTask(I_IDLE, 0, PacketList.None, S_RUN, idlefn, TaskData.Idle(1, Count))

    var work1: createPacket(PacketList.None, 0, K_WORK)
    var work2: createPacket(work1, 0, K_WORK)
    var work: createTask(I_WORK, 1000, work2, S_WAITPKT, workfn, TaskData.Work(I_HANDLERA, 0))

    var deva1: createPacket(PacketList.None, I_DEVA, K_DEV)
    var deva2: createPacket(deva1, I_DEVA, K_DEV)
    var deva3: createPacket(deva2, I_DEVA, K_DEV)
    var handlera: createTask(I_HANDLERA, 2000, deva3, S_WAITPKT, handlerfn, TaskData.Handler(PacketList.None, PacketList.None))

    var devb1: createPacket(PacketList.None, I_DEVB, K_DEV)
    var devb2: createPacket(devb1, I_DEVB, K_DEV)
    var devb3: createPacket(devb2, I_DEVB, K_DEV)
    var handlerb: createTask(I_HANDLERB, 3000, devb3, S_WAITPKT, handlerfn, TaskData.Handler(PacketList.None, PacketList.None))

    var deva: createTask(I_DEVA, 4000, PacketList.None, S_WAIT, devfn, TaskData.Device(PacketList.None))
    var devb: createTask(I_DEVB, 5000, PacketList.None, S_WAIT, devfn, TaskData.Device(PacketList.None))

    tcb = tasklist

    qpktcount = 0
    holdcount = 0

    println("Starting") # Starting

    schedule()

    println("finished") # finished
    print("qpkt count = ")
    println(qpktcount)    # qpkt count = 2326410
    print("holdcount = ")
    println(holdcount)  # holdcount = 930563

    print("These results are ")
    if qpktcount == Qpktcountval and holdcount == Holdcountval:
        println("correct")  # These results are correct
    else:
        println("incorrect")

    println("end of run")   # end of run

bench()