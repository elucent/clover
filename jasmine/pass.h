#ifndef BASIL_JASMINE_PASS_H
#define BASIL_JASMINE_PASS_H

#include "jasmine/insn.h"

void foldc(Function& fn);
void dce(Function& fn);
void regalloc(Function& fn, const Target& arch);
void stackalloc(Function& fn, const Target& arch);

#endif