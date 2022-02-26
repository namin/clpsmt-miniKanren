CLP(SMT)-miniKanren
===================

Canonical miniKanren implementation, augmented with CLP(SMT).

Added SMT hooks: `z/assert` takes a boolean arithmetic expression with variables (integer by default), `z/` takes an SMT statement, `z/purge` manually purges the SMT constraints into an enumeration of models, `z/check` only fails if the constraints are unsatisfiable.

See also:
- https://github.com/chansey97/faster-minikanren/tree/smt-assumptions-full-integration
- https://github.com/namin/faster-miniKanren/tree/smt-assumptions
- https://github.com/webyrd/Barliman/blob/will-clpsmt/cocoa/Barliman/mk-and-rel-interp/test-interp.scm

Background on miniKanren
------------------------

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
