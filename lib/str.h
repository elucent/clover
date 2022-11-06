#ifndef BASIL_LIB_STR_H
#define BASIL_LIB_STR_H

#include "core/def.h"
#include "lib/slice.h"

/*
 * toint(str)
 *
 * Returns the integer value represented by the provided string. Assumes str
 * contains only decimal digit characters.
 */
i64 toint(const_slice<i8> str);


i64 hextoint(const_slice<i8> str);

i64 binarytoint(const_slice<i8> str);

/*
 * tofloat(str)
 *
 * Returns the floating-point value represented by the provided string. Assumes
 * str contains only decimal digit characters or periods.
 */
double tofloat(const_slice<i8> str);

#endif