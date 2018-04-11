CLP(SMT)-miniKanren
===================

Canonical miniKanren implementation, augmented with CLP(SMT).

Added SMT hooks:
z/fresh  (declare-fun x () Int)
z/assert (assert (= x 1)) (assert (>= x 0))
(check-sat) is called implicitly, if the result is unsat, the procedure fails.

Starts with the language described in the paper:

William E. Byrd, Eric Holk, and Daniel P. Friedman.
miniKanren, Live and Untagged: Quine Generation via Relational Interpreters (Programming Pearl).
To appear in the Proceedings of the 2012 Workshop on Scheme and Functional Programming, Copenhagen, Denmark, 2012.


CORE LANGUAGE

Logical operators:

==
fresh
conde

Interface operators:

run
run*


EXTENDED LANGUAGE

Constraint operators:

=/=
symbolo
numbero
absento
