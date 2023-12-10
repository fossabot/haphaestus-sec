# Add animation infrastructure
This would require callers to seperate logic for desugaring CSS properties
(and resolving relative units) from parsing CSS tokens to a style tree! This
would also help implement calc(), and somewhat generically handle `inherit`.

Can be implemented as a PropertyParser decorator.

---

It occurs to me that this could be easily hand-tested by writing a commandline
program which displays computed values (relative to initial values) for a style
declaration.

That might even be useful program! Though ofcourse it shouldn't replace
automated testing.
