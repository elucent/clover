1. Introduction
===============

2. Abstract Grammar
===================

We'll start by defining an "abstract grammar" of Clover programs. This is a grammar that describes the structure of a Clover program, including all its expressions and statements, but doesn't necessarily line up with the language syntax. We'll specify in the next section how Clover syntax maps onto this abstract grammar, and also use this abstract grammar in further sections to describe expressions and statements we want to typecheck or evaluate.

2.1. Literals
--------------

A "literal" expression is any of the following:
 - The "unit" literal ().
 - A boolean constant, either *true* or *false*.
 - A character constant with an integer value *C*, between :math:`0` and :math:`2^{21}-1` inclusive, representing a single "code point" per the Unicode specification.
 - A "signed" constant with some integer value *N*, where *N* is between :math:`-2^{63}` and :math:`2^{63}-1`, inclusive.
 - An "unsigned" constant with some integer value *N*, where *N* is between :math:`0` and :math:`2^{64}-1`, inclusive.
 - A floating-point constant with value *F*, one of the values precisely representable in a 64-bit IEEE-754 floating-point number, including positive and negative infinity and any of the "NaN" values representable in 64-bit floating-point format.

.. productionlist::
    Literal : ()
            : true
            : false
            : Char(C) where C is a Unicode code point
            : Signed(N) where N is an integer in [-2^{63}, 2^{63}-1]
            : Unsigned(N) where N is an integer in [0, 2^{64}-1]
            : Float(F) where F is a 64-bit floating-point number
            
2.2. Variables
--------------

A "variable" expression contains a unique symbol *S*. We don't go into a lot of detail here, because really all that matters for variable purposes is the fact that different variables have different symbols. Which source names are permissible, and how they map onto symbols, is a syntax question and not an abstract grammar question.

.. productionlist::
    Variable : Var(S)

2.3. Arithmetic Expressions
---------------------------

Arithmetic expressions correspond to arithmetic operators and generally operate on numbers and compute numerical results. They include the following:
 - The "plus" expression, syntactically :code:`+e`, which acts as an identity function on numbers.
 - The "minus" expression, syntactically :code:`-e`, which negates a number.
 - The "add" expression, syntactically :code:`e1 + e2`, which adds two numbers together.
 - The "subtract" expression, syntactically :code:`e1 - e2`, which computes the difference of two numbers.
 - The "multiply" expression, syntactically :code:`e1 * e2`, which computes the product of two numbers.
 - The "divide" expression, syntactically :code:`e1 / e2`, which computes the quotient of two numbers.
 - The "remainder" expression, syntactically :code:`e1 % e2`, which computes the remainder of one number divided by the other.
 - The "exponent" expression, syntactically :code:`e1 ** e2`, which computes the first number raised to the power of the second number.

.. productionlist:: 
    ArithmeticExpr : Plus(Expr e)
                   : Minus(Expr e)
                   : Add(Expr e1, Expr e2)
                   : Sub(Expr e1, Expr e2)
                   : Mul(Expr e1, Expr e2)
                   : Div(Expr e1, Expr e2)
                   : Rem(Expr e1, Expr e2)
                   : Exp(Expr e1, Expr e2)

2.4. Bitwise Expressions
------------------------

Bitwise expressions correspond to bitwise operators and compute operations between individual bits of their inputs. They include the following:
 - The "bitwise NOT" expression, syntactically :code:`~e`, which computes the bitwise NOT of its input.
 - The "bitwise AND" expression, syntactically :code:`e1 & e2`, which computes the bitwise AND of its two inputs.
 - The "bitwise OR" expression, syntactically :code:`e1 | e2`, which computes the bitwise OR of its two inputs.
 - The "bitwise XOR" expression, syntactically :code:`e1 ^ e2`, which computes the bitwise XOR of its two inputs.
 - The "left shift" expression, syntactically :code:`e1 << e2`, which shifts its first input to the left by its second input.
 - The "right shift" expression, syntactically :code:`e1 >> e2`, which shifts its first input to the right by its second input.
 - The "left rotate" expression, syntactically :code:`e1 /< e2`, which rotates its first input to the left by its second input.
 - The "right rotate" expression, syntactically :code:`e1 /> e2`, which rotates its first input to the right by its second input.

.. productionlist:: 
    BitwiseExpr : BNot(Expr e)
                : BAnd(Expr e1, Expr e2)
                : BOr(Expr e1, Expr e2)
                : BXor(Expr e1, Expr e2)
                : Shl(Expr e1, Expr e2)
                : Shr(Expr e1, Expr e2)
                : Rol(Expr e1, Expr e2)
                : Ror(Expr e1, Expr e2)

2.5. Comparison Expressions
---------------------------

Comparison expressions correspond to relational and equality operators and compare their two inputs together to produce a result. They include the following:
 - The "less than" expression, syntactically :code:`e1 < e2`, which computes if the first input is less than the second.
 - The "greater than" expression, syntactically :code:`e1 > e2`, which computes if the first input is greater than the second.
 - The "less than or equal" expression, syntactically :code:`e1 <= e2`, which computes if the first input is less than or equal to the second.
 - The "greater than or equal" expression, syntactically :code:`e1 >= e2`, which computes if the first input is greater than or equal to the second.
 - The "equal" expression, syntactically :code:`e1 == e2`, which computes if the first input is equal to the second.
 - The "not equal" expression, syntactically :code:`e1 != e2`, which computes if the first input is not equal to the second.

.. productionlist::
    ComparisonExpr : Less(Expr e1, Expr e2)
                   : Greater(Expr e1, Expr e2)
                   : LEqual(Expr e1, Expr e2)
                   : GEqual(Expr e1, Expr e2)
                   : Equal(Expr e1, Expr e2)
                   : NotEqual(Expr e1, Expr e2)

2.6. Logical Expressions
------------------------

Logical expressions operate on boolean values and compute logical relations. They include the following:
 - The "not" expression, syntactically :code:`not e`, which computes the logical negation of its input.
 - The "and" expression, syntactically :code:`e1 and e2`, which computes the logical conjunction of its inputs.
 - The "or" expression, syntactically :code:`e1 or e2`, which computes the logical disjunction of its inputs.

.. productionlist::
    LogicalExpr : Not(Expr e)
                : And(Expr e1, Expr e2)
                : Or(Expr e1, Expr e2)

2.7. Access Expressions
-----------------------

Access expressions access different subparts of a value depending on its type and sometimes additional parameters. They include the following:
 - The "get field" expression, syntactically :code:`e . S` for some symbol *S*, which accesses the field named *S* in its left operand.
 - The "get index" expression, syntactically :code:`e1[e2]`, which accesses the element at index *e2* of *e1*.
 - The "get slice" expression, syntactically :code:`e1[e2:e3]`, which produces a slice of all the elements between indices *e2* and *e3* in *e1*.
 - The "dereference" expression, syntactically :code:`*e`, which loads the value pointed to by its operand.

.. productionlist::
    AccessExpr : Field(Expr e, S)
               : Index(Expr e1, Expr e2)
               : Slice(Expr e1, Expr e2, Expr e3)
               : Deref(Expr e)

2.8. The Conditional Expression
-------------------------------

The "conditional" expression, alternatively the "ternary" expression, syntactically :code:`e2 if e1 else e3`, returns either *e2* or *e3* based on condition *e1*.

.. productionlist::
    ConditionalExpr : Ternary(Expr e1, Expr e2, Expr e3)

2.9. Aggregate Expressions
--------------------------

Aggregate expressions produce an aggregate value containing all of their inputs. They are either:
 - The "array" expression, syntactically :code:`[e1, e2, ..., eN]`, which produces an array value from zero or more values *e1* through *eN*.
 - The "tuple" expression, syntactically :code:`(e1, e2, ..., eN)`, which produces a tuple value from one or more values *e1* through *eN*.

.. productionlist::
    AggregateExpr : Array(Expr* es)
                  : Tuple(Expr+ es)

1. Syntax
=========

1. Type System
==============

4.1. Notion of Type
-------------------

In Clover, a type represents an abstract "domain" of values. We don't require that this domain is finite, or exactly a set. Instead we define a relation on values, where for some value *v* and type *t*, *v* ∈ *t* implies that *v* is an element of the domain of *t*. We also define a "can be typed as" relation, where for an expression *e* and type *t*, *e* : *t* implies that for any value *v* that *e* evaluates to, *v* ∈ *t*. In general, this relation takes place under a "typing context" (usually written as *C*).

.. math:: 
    \frac
        {C\vdash e \Downarrow v \quad C \vdash v \in t}
        {C\vdash e : t}

4.2. Subtyping and Equivalence
------------------------------

Between two types, Clover defines the "subtype" relation (written as "<:"). For two types *t* and *u*, *t* <: *u* implies that any value in the domain of *t* is also in the domain of *u*. This does not mean that the value has to be precisely representable in both types, just that both types are defined to encompass that value. Subtyping is reflexive: a type *t* is always a subtype of itself.

.. math:: 
    \frac {} { C \vdash t <: t }
    \quad
    \frac
        {C \vdash t <: u \quad C \vdash v \in t}
        {C \vdash v \in u}

4.3. Bottom and Top Types
-------------------------

The "bottom type" in Clover is `void`. It is an uninhabited type - we can't have values of type void, its domain is empty. This means that `void` is a subtype of all Clover types.

The "top type" in Clover is `any`. Its domain is all Clover values. Every Clover type is a subtype of `any`.

.. math::
    \frac
        {}
        {C \vdash \forall t . void <: t}
    \quad
    \frac
        {}
        {C \vdash \forall t . t <: any}

4.4. Type Variables
-------------------

A "concrete type" is a type that is neither a type variable nor contains a type variable as any part of its definition. For a Clover program to be well-typed, for every expression *e* in the program, there must be a concrete type *t* for which *e* : *t*.

A "variable type" is a placeholder type used during type inference. It's the job of the typechecker to incrementally refine variable types until either we are forced to pick a concrete type from its range, or it encounters incompatible type judgements. Variable types consist of a globally unique key *k*, and are usually written *var*\(*k*\).

The key for a type variable maps to a type pair in the typing context. This pair consists of a lower bound and upper bound, which may be variable types themselves. For a variable type *t* and a non-concrete type *u*, *t* <: *u* if and only if in the current typing context *t* has lower bound *a* and upper bound *b* such that *a* <: *u* and *u* <: *b*. The reverse, *u* <: *t* holds if and only if the *u* <: *a*.

.. math::
    \frac
        {C[k] = (a, b) \quad C \vdash a <: u \quad C \vdash u <: b}
        {C \vdash var(k) <: u}
    \quad
    \frac
        {C[k] = (a, b) \quad C \vdash a u <: a}
        {C \vdash u <: var(k)}

For two variable types *t* and *t'*, with lower bounds *a* and *a'* and upper bounds *b* and *b'* respectively, *t* = *t'* if and only if *a* = *a'* and *b* = *b'*, and *t* <: *t'* if and only if *a'* <: *a* and *b* <: *b'*.

.. math::
    \frac
        {C[k] = (a, b) \quad C[k'] = (a', b') \quad C \vdash a' <: a \quad C \vdash b <: b'}
        {C \vdash var(k) <: var(k')}

4.5. Primitive Types
--------------------

Clover has several "primitive types". These are the simplest types in the type system, having no internal structure, and no subtyping relations other than with themselves or *void*/*any*. Currently, these include:
 - *unit*, a single-value type, inhabited by the value ().
 - *bool*, the type of boolean values, inhabited by the values *true* and *false*.
 - *char*, the type of text characters, specifically representing a Unicode code point.

4.6. Numeric Types
------------------

A "numeric type" is a type that represents some kind of number value. Numeric types have a "precision", specified as a number of bits in the range [0, 64]. Numeric types are either *signed* or *unsigned*. And numeric types are either *floating* or *not floating*. *floating* implies *signed*.

A numeric type that is *unsigned* is an unsigned integer type. For precision *N*, its domain is all integer values between :math:`0` and :math:`2^N - 1`, inclusive. We write these types as *uN*, i.e. *u32* for an unsigned integer type with 32-bit precision.

A numeric type that is *signed* but not *floating* is a signed integer type. For precision *N*, its domain is all integer values between :math:`-2^{N - 1}` and :math:`2^{N - 1} - 1`, inclusive. We write these types as *iN*, i.e. *i32* for a signed integer type with 32-bit precision. 

A numeric type that is *floating* is a floating-point number type. Its values are all represented in IEEE-754 floating-point format, and thus floating-point types have some amount of imprecision for certain values. Despite this, for typechecking purposes, we consider the domain of floating-point types to contain all real numbers - any real is representable in floating-point, even if it is not possible to do so precisely. We write these types as *fN*, i.e. *f32* for a floating-point type with 32-bit precision.

For an unsigned integer type *uM* with precision *M* and another type *t*, *uM* <: *t* if and only if:
 - *t* is an unsigned integer type *uN* such that *N* >= *M*.
 - *t* is a signed integer type *iN* such that *N* >= *M + 1*.
 - *t* is a floating-point number type.
 - *t* is *any*.

.. math:: 
    \frac
        {M \in [0, 64] \quad N \in [0, 64] \quad M \le N}
        {C \vdash uM <: uN}
    \quad
    \frac
        {M \in [0, 64] \quad N \in [0, 64]}
        {C \vdash iM <: fN}
.. math:: 
    \frac
        {M \in [0, 64] \quad N \in [0, 64] \quad M \le N - 1}
        {C \vdash uM <: iN}

For a signed integer type *iM* with precision *M* and another type *t*, *iM* <: *t* if and only if:
 - *t* is a signed integer type *iN* such that *N* >= *M*.
 - *t* is a floating-point number type.
 - *t* is *any*.

.. math:: 
    \frac
        {M \in [0, 64] \quad N \in [0, 64] \quad M \le N}
        {C \vdash iM <: iN}
    \quad
    \frac
        {M \in [0, 64] \quad N \in [0, 64]}
        {C \vdash iM <: fN}

For a floating-point number type *fM* with precision *M* and another type *t*, *fM* <: *t* if any only if:
 - *t* is a floating-point number type *fN* such that *N* >= *M*.
 - *t* is *any*.

.. math:: 
    \frac
        {M \in [0, 64] \quad N \in [0, 64] \quad M \le N}
        {C \vdash fM <: fN}

4.7. Unification and Occurrence
-------------------------------

Unification is an operation that attempts to unify two types into a common type. A general property of unification is that any value in the domains of either type must also be in the domain of the resulting type. Unification is also effectful - it can lead to changes in the typing context, as unifying a type variable with another type may refine its bounds.

For two concrete types *t* and *u*, *unify*\(*t*, *u*\) yields *t* if *u* <: *t*, otherwise *u* if *t* <: *t*, and otherwise fails.

.. math:: 
    \frac
        {C \vdash t <: u \quad isconcrete(t) \quad isconcrete(u)}
        {C \vdash unify(t, u) = u \dashv C}
    \quad
    \frac
        {C \vdash unify(u, t) = t' \dashv C'}
        {C \vdash unify(t, u) = t' \dashv C'}

If only one type *t* is concrete, and the other is a variable type *var*\(*k*\) with lower bound *a* and upper bound *b*, *unify*\(*t*, *var*\(*k*\)) yields *a* if *t* <: *var*\(*k*\), otherwise *t* if *var*\(*k*\) <: *t*, and otherwise fails. In the second case, where *var*\(*k*\) is a subtype of *t*, the lower bound of *var*\(*k*\) is replaced with *t* in the typing context.

.. math:: 
    \frac
        {C[k] = (a, b) \quad C \vdash t <: var(k)}
        {C \vdash unify(var(k), t) = a \dashv C}
    \quad
    \frac
        {C \vdash unify(var(k), t) = t' \dashv C'}
        {C \vdash unify(t, var(k)) = t' \dashv C'}
.. math::
    \frac
        {C[k] = (a, b) \quad C \vdash var(k) <: t \quad C \vdash t \cancel{<:} var(k) }
        {C \vdash unify(var(k), t) = t \dashv C[k := (t, b)]}

If both types are variables, 

5. Typechecking
===============

5.1. Inference
--------------

Typechecking in Clover is divided into two stages, "inference" and "checking".

In the inference phase, types are inferred for expressions based on available information, often defining type variables which are widened through successive uses. We don't require that any type have a concrete type during inference, its purpose is just to do preliminary discovery of known type constraints. One additional part of inference is limited, but required constant folding. For an expression *e*, *infer*\(*e*\) returns a tuple (*e'*, *t*) of an expression *e'* and optional type *t* (a missing type is written as ∅). *e'* is a potentially distinct expression, which for the purposes of future analysis *replaces* the original expression *e*. In most cases, where *t* is present, *e'* = *e*. But in cases where *e* can be totally evaluated at compile time, we generally propagate that result to the parent expression via *e'*, so we can infer more-precise types.

As part of inference, we introduce a *totype*\(*e*, *t*\) function. In cases where compile-time evaluation is impossible, we use *totype* to pick plausible types for constant operands. To compute *totype* for some evaluation \(*e*, *t*\):
 - If the type *t* is not ∅, return *t*.
 - If the expression is the unit literal (), return *unit*.
 - If the expression is the literal *true* or *false*, return *bool*.
 - If the expression is a character literal Char\(*C*\), return *char*.
 - If the expression is a signed literal Signed\(*N*\) where *N* >= 0, return the signed integer type *iM* where *M* is the smallest positive integer such that :math:`2^M` > *N*.
 - If the expression is a signed literal Signed\(*N*\) where *N* < 0, return the signed integer type *iM* where *M* is the smallest positive integer such that :math:`-2^M` <= *N*.
 - If the expression is an unsigned literal Unsigned\(*N*\), return the unsigned integer type *uM* where *M* is the smallest positive integer such that :math:`2^M` > *N*.
 - If the expression is a floating-point literal Float\(*F*\), return the bottom floating-point type :math:`f0`.
  
.. math:: 
    \frac
        {t \ne \varnothing}
        {C \vdash totype(e, t) = t}
.. math::
    \frac
        {}
        {C \vdash totype(\texttt{()}, \varnothing) = unit}
    \quad
    \frac
        {}
        {C \vdash totype(\texttt{true}, \varnothing) = bool}
    \quad
    \frac
        {}
        {C \vdash totype(\texttt{false}, \varnothing) = bool}
.. math:: 
    \frac
        {}
        {C \vdash totype(\texttt{Char}(c), \varnothing) = char}
    \quad
    \frac
        {}
        {C \vdash totype(\texttt{Float}(f), \varnothing) = f0}
.. math::
    \frac
        {N \ge 0 \quad M = min\{x \;|\; x \in [0, 64] \,\land\, 2^x \gt N\}}
        {C \vdash totype(\texttt{Signed}(N), \varnothing) = iM}
    \quad
    \frac
        {N \lt 0 \quad M = min\{x \;|\; x \in [0, 64] \,\land\, -2^x \le N\}}
        {C \vdash totype(\texttt{Signed}(N), \varnothing) = iM}
.. math::
    \frac
        {M = min\{x \;|\; x \in [0, 64] \,\land\, 2^x \gt N\}}
        {C \vdash totype(\texttt{Unsigned}(N), \varnothing) = uM}

In the checking phase we enforce the requirement that for an expression *e*, if it is well-typed, *check*\(*e*\) returns a concrete type. Variable types may undergo some final unification, but afterwards we select a compatible concrete type and lock the variable to that type going forwards. The existence of a checking phase that may operate on wholly-concrete types means we are able to use it to typecheck expressions that are otherwise difficult to embed as constraints in the type system, such as calls to overloaded functions.

In the rest of this section, we'll go over the inference and checking rules for all expressions and statements in Clover.

5.2. Literals
--------------

Any literal expression *e* folds to itself during the inference phase - that is to say, *infer*\(*e*\) = (*e*, ∅).

.. math:: 
    \frac
        {}
        {C \vdash infer(\texttt{unit}) = (\texttt{unit}, \varnothing) \dashv C}
.. math:: 
    \frac
        {}
        {C \vdash infer(\texttt{true}) = (\texttt{true}, \varnothing) \dashv C}
    \quad
    \frac
        {}
        {C \vdash infer(\texttt{false}) = (\texttt{false}, \varnothing) \dashv C}
.. math::
    \frac
        {}
        {C \vdash infer(\texttt{Char}(c)) = (\texttt{Char}(c), \varnothing) \dashv C}
.. math::
    \frac
        {}
        {C \vdash infer(\texttt{Signed}(N)) = (\texttt{Signed}(N), \varnothing) \dashv C}
.. math::
    \frac
        {}
        {C \vdash infer(\texttt{Unsigned}(N)) = (\texttt{Unsigned}(N), \varnothing) \dashv C}
.. math::
    \frac
        {}
        {C \vdash infer(\texttt{Float}(F)) = (\texttt{Float}(F), \varnothing) \dashv C}

During checking, a literal expression *e* that is a unit, boolean, or character literal returns the corresponding primitive type.

.. math:: 
    \frac
        {}
        {C \vdash check(\texttt{()}) = unit \dashv C}
.. math:: 
    \frac
        {}
        {C \vdash check(\texttt{true}) = bool \dashv C}
    \quad
    \frac
        {}
        {C \vdash check(\texttt{false}) = bool \dashv C}
.. math::
    \frac
        {}
        {C \vdash check(\texttt{Char}(c)) = char \dashv C}

To check an integer or floating-point literal, we not only need to pick a sound type based on the value of the expression, but also "legalize" the type - simplify it so it's easier to describe at runtime. For signed and unsigned integer types, legalization means rounding up to the nearest legal word size - either 8, 16, 32, or 64. For floating-point types, we round up similarly, but the only legal word sizes are 32 and 64.

.. math::
    \frac
        {C \vdash totype(e) = uN \quad M = min\{x \;|\; x \in \{8, 16, 32, 64\} \land x \ge N\}}
        {C \vdash legalize(e) = uM}
.. math::
    \frac
        {C \vdash totype(e) = iN \quad M = min\{x \;|\; x \in \{8, 16, 32, 64\} \land x \ge N\}}
        {C \vdash legalize(e) = iM}
.. math::
    \frac
        {C \vdash totype(e) = fN \quad M = min\{x \;|\; x \in \{32, 64\} \land x \ge N\}}
        {C \vdash legalize(e) = fM}
.. math::
    \frac
        {}
        {C \vdash check(\texttt{Signed}(N)) = legalize(\texttt{Signed}(N)) \dashv C}
.. math::
    \frac
        {}
        {C \vdash check(\texttt{Unsigned}(N)) = legalize(\texttt{Unsigned}(N)) \dashv C}
.. math::
    \frac
        {}
        {C \vdash check(\texttt{Float}(F)) = legalize(\texttt{Float}(F)) \dashv C}

5.3. Variables
--------------

To both infer and check a variable expression Var\(*S*\) in typing context *C*, we simply return the type in *C* mapped to by symbol *S*.

.. math:: 
    \frac
        {}
        {C \vdash infer(\texttt{Var}(s)) = (\texttt{Var}(s), C[s]) \dashv C}
    \quad
    \frac
        {}
        {C \vdash check(\texttt{Var}(s)) = C[s] \dashv C}

5.4. Arithmetic Expressions
---------------------------

In the inference phase, we fold arithmetic expressions if and only if all of their operands are folded, no observable behavior (such as dividing by zero, or overflowing a 64-bit integer) occurs while computing the result at compile time. Otherwise, we give the expression a variable type *var*\(*u0*, *any*\), unify it with both of the inferred types of our operands, and return that.

The below semantics use a primitive *asfloat64*\(*N*\) function. This means we round an integer *N* to the nearest precisely-representable number in the 64-bit floating-point number space.

.. math:: 
    \frac
        {}
        {toint(\texttt{Signed}(N)) = N}
    \quad
    \frac
        {}
        {toint(\texttt{Unsigned}(N)) = N}
.. math:: 
    \frac
        {}
        {tofloat(\texttt{Float}(F)) = F}
    \quad
    \frac
        {}
        {tofloat(\texttt{Signed}(N)) = asfloat64(N)}
.. math:: 
    \frac
        {}
        {tofloat(\texttt{Unsigned}(N)) = asfloat64(N)}
.. math::
    \frac
        {C \vdash infer(e_1) = (e_1', \varnothing) \dashv C' \quad toint(e_1') = M \quad C' \vdash infer(e_2) = (e_2', \varnothing) \dashv C'' \quad toint(e_2') = N \quad 0 \le M + N \lt 2^{64}}
        {C \vdash infer(\texttt{Add}(e_1, e_2)) = (\texttt{Unsigned}(M + N), \varnothing) \dashv C''}
.. math::
    \frac
        {C \vdash infer(e_1) = (e_1', \varnothing) \dashv C' \quad toint(e_1') = M \quad C' \vdash infer(e_2) = (e_2', \varnothing) \dashv C'' \quad toint(e_2') = N \quad -2^{63} \le M + N \lt 0}
        {C \vdash infer(\texttt{Add}(e_1, e_2)) = (\texttt{Signed}(M + N), \varnothing) \dashv C''}
.. math::
    \frac
        {C \vdash infer(e_1) = (e_1', \varnothing) \dashv C' \quad tofloat(e_1') = M \quad C' \vdash infer(e_2) = (e_2', \varnothing) \dashv C'' \quad tofloat(e_2') = N}
        {C \vdash infer(\texttt{Add}(e_1, e_2)) = (\texttt{Float}(M + N), \varnothing) \dashv C''}